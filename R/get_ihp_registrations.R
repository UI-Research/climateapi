# Author: Will Curran-Groome

#' @importFrom magrittr %>%

#' @title Get Individuals and Households Program (IHP) registrations
#'
#' @param geography Included only for API consistency; this must be NULL.
#' @param state_fips A character vector of two-letter state abbreviations. If NULL (default), return data for all 51 states. Otherwise return data for the specified states.
#' @param file_name The name (not the full path) of the Box file containing the raw data.
#' @param api If TRUE (default), query the API.
#' @param outpath The path to save the parquet-formatted datafile. Applicable only when `api = FALSE`.
#'
#' @returns A dataframe comprising county-level data on current NFIP policies
#' @export
#'
#' @examples
#' \dontrun{
#' get_ihp_registrations(
#'    state_fips = "NJ",
#'    api = TRUE)
#' }

get_ihp_registrations = function(
    geography = NULL,
    state_fips = NULL,
    file_name = "IndividualsAndHouseholdsProgramValidRegistrations_2025_06_10.csv",
    api = TRUE,
    outpath = NULL) {

    xwalks_path = file.path(get_box_path(), "crosswalks")

    if (isFALSE(api)) {
      inpath = file.path(
        get_box_path(), "hazards", "fema", "individual-households-program", "raw", file_name)

      if (! file.exists(inpath)) {
        stop("The provided `file_name` is invalid.") }

      if (! (stringr::str_detect(inpath, "parquet"))) {

        convert_delimited_to_parquet(
          inpath = inpath,
          delimit_character = ",",
          dataset = "ihp_registrations") }

      if (is.null(state_fips)) {
        state_fips = c(datasets::state.abb, "DC") }

      df1 = arrow::read_parquet(file = inpath) %>%
        dplyr::select(dplyr::all_of(get_dataset_columns("ihp_registrations"))) %>%
        janitor::clean_names() %>%
        dplyr::filter(damaged_state_abbreviation %in% state_fips)
      } else {

      df1 = purrr::map_dfr(
          state_fips,
          ~ rfema::open_fema(
              data_set = "individualsandhouseholdsprogramvalidregistrations",
              filters = list(damagedStateAbbreviation = .x),
              ask_before_call = FALSE)) %>%
        dplyr::select(dplyr::all_of(get_dataset_columns("ihp_registrations"))) %>%
        janitor::clean_names() %>%
        dplyr::filter(damaged_state_abbreviation %in% state_fips)
    }

    zip_county_xwalk = readr::read_csv(
      file.path(xwalks_path, "geocorr2022_2020_zip_zcta_to_county.csv")) %>%
      dplyr::slice(2:nrow(.)) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        afact = as.numeric(afact),
        dplyr::across(.cols = c(county_name, zip_name), .fns = ~ stringr::str_remove_all(.x, '\\"'))) %>%
      dplyr::select(
        zcta_code = zcta,
        county_code = county,
        county_name,
        population_2020 = pop20,
        afact)

    ihp_registrations = df1 %>%
      dplyr::rename(
        substate_geography = county,
        state_abbreviation = damaged_state_abbreviation,
        city_name = damaged_city,
        zip_code = damaged_zip_code,
        househole_residents = household_composition,
        household_income_gross = gross_income,
        household_tenure = own_rent,
        amount_individual_housing_program = ihp_amount,
        amount_flood_insurance_premium_paid_by_fema = fip_amount,
        amount_housing_assistance = ha_amount,
        amount_other_needs_assistance = ona_amount,
        emergency_needs_flag = emergency_needs,
        food_needs_flag = food_need,
        shelter_needs_flag = shelter_need,
        special_accommodations_needs_flag = access_functional_needs,
        fema_determined_value_real_property_damage = rpfvl,
        fema_determined_value_personal_property_damage = ppfvl,
        amount_rental_assistance = rental_assistance_amount,
        amount_repairs = repair_amount,
        amount_replacement = replacement_amount,
        amount_personal_property = personal_property_amount,
        max_individual_households_program_flag = ihp_max,
        max_housing_assistance_flag = ha_max,
        max_other_needs_assistance_flag = ona_max,
        date_last_updated = last_refresh) %>%
      dplyr::mutate(
        unique_id = uuid::UUIDgenerate(n = nrow(.)),
        zip_code = stringr::str_pad(zip_code, width = 5, pad = "0", side = "left"),
        substate_geography = substate_geography %>% stringr::str_remove_all("\\(|\\)")) %>%
      dplyr::left_join(
        tidycensus::fips_codes %>%
          dplyr::select(state, state_code, state_name) %>% dplyr::distinct(),
        by = c("state_abbreviation" = "state")) %>%
      dplyr::select(
        unique_id,
        dplyr::matches("^state|^county|^zip|^zcta|name|code|geography"),
        dplyr::everything())

    warning("
FEMA does not provide consistent county-level identifiers such as GEOIDs. As such, some 'counties'--in particular,
independent cities and tribal lands, but also other non-county, sub-state geographies--have missing GEOIDs but valid
records of IHP registrations. Users have multiple options for summarizing data at various geographies:

(1) Use relatively non-missing fields included in the raw data by FEMA, including state and zip code.
(2) Use fields with moderate missingness, such as `substate_geography`, which is often (but not always) the county.
(3) Crosswalk zip codes to counties using the `zip_county_xwalk` data frame, which is the second item in the list returned by this function.
    This will produce consistent, standardized county-level results, though statistics at this geography will be estimates of county-level figures
    based on areal interpolation. Analysts will need to use the `afact` variable to calculate these estimates, akin to:

    df %>%
      dplyr::group_by(county_code) %>%
      dplyr::mutate(afact = dplyr::if_else(is.na(afact), 1, afact)) %>%
      dplyr::summarize(valid_registrations = sum(afact, na.rm = TRUE))")

    message("
These data are from: https://www.fema.gov/openfema-data-page/individuals-and-households-program-valid-registrations-v1.
Per FEMA: This dataset contains IA applications from DR1439 (declared in 2002) to those declared over 30 days ago.
The full data set is refreshed on an annual basis; the last 18 months of data are refreshed weekly.
This dataset includes all major disasters and includes only valid registrants
(applied in a declared county, within the registration period, having damage due to the incident and damage within the incident period).
IHP is intended to meet basic needs and supplement disaster recovery efforts.")

  message(stringr::str_c(
"The unit of observation is individual households' IHP registrations. ",
"The `unique_id` field is a unique identifier for each observation. ",
"Note that a zip-county crosswalk is included as the second item in the list returned by this function."))

  return(list(ihp_registrations, zip_county_xwalk))
}

utils::globalVariables(c(
  "substate_geography", "state_abbreviation", "city_name", "zip_code", "househole_residents",
  "household_income_gross", "household_tenure", "amount_individual_housing_program",
  "amount_flood_insurance_premium_paid_by_fema", "amount_housing_assistance", "other_needs_assistance",
  "emergency_needs_flag", "food_needs_flag", "shelter_needs_flag", "special_accommodations_needs_flag",
  "fema_determined_value_real_property_damage", "fema_determined_value_personal_property_damage",
  "amount_rental_assistance", "amount_repairs", "amount_replacement", "amount_personal_property",
  "max_individual_households_program_flag", "max_housing_assistance_flag", "max_other_needs_assistance_flag",
  "date_last_updated", "zip_county_xwalk", "access_functional_needs", "afact",
  "damaged_city", "damaged_state_abbreviation", "damaged_zip_code", "emergency_needs",
  "fip_amount", "food_need", "gross_income", "ha_amount", "ha_max", "household_composition",
  "ihp_amount", "ihp_max", "last_refresh", "ona_amount", "ona_max", "own_rent", "personal_property_amount",
  "ppfvl", "rpfvl", "repair_amount", "replacement_amount", "rental_assistance_amount", "shelter_need",
  "uuid", "zip_name", "zcta", "pop20", "state.abb"))
