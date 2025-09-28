#' @importFrom magrittr %>%

#' @title Get Individuals and Households Program (IHP) registrations
#'
#' @param state_fips A character vector of two-letter state abbreviations. If NULL (default), return data for all 51 states. Otherwise return data for the specified states.
#' @param file_name The name (not the full path) of the Box file containing the raw data.
#' @param api If TRUE, query the API. If FALSE (default), read from disk.
#' @param outpath The path to save the parquet-formatted datafile. Applicable only when `api = FALSE`.
#'
#' @returns A dataframe comprising IHP registrations
#' @export
#'
#' @examples
#' \dontrun{
#' get_ihp_registrations(
#'    state_fips = "NJ",
#'    api = TRUE)
#' }

get_ihp_registrations = function(
    state_fips = NULL,
    file_name = "IndividualsAndHouseholdsProgramValidRegistrationsV2_2025_09_26.parquet",
    api = FALSE,
    outpath = NULL) {

    if (isFALSE(api)) {
      inpath = file.path(
        get_box_path(), "hazards", "fema", "individual-households-program", "raw", file_name)

      if (! file.exists(inpath)) {
        stop("The provided `file_name` is invalid.") }

      if (! (stringr::str_detect(inpath, "parquet")) &
          !file.exists(inpath %>% stringr::str_replace("csv", "parquet"))) {

        convert_delimited_to_parquet(
          inpath = inpath,
          delimit_character = ",",
          dataset = "ihp_registrations") }

      if (is.null(state_fips)) {
        state_fips = c(datasets::state.abb, "DC") }

      if (!stringr::str_detect(inpath, "parquet")) {
        inpath = inpath %>% stringr::str_replace("csv", "parquet") }

      ihp_vars_all = get_dataset_columns("ihp_registrations")
      ihp_vars = ihp_vars_all[!stringr::str_detect(ihp_vars_all, "Need|Max|fvl|Refresh|^id")]

      df1 = arrow::read_parquet(
          file = inpath,
          col_select = dplyr::any_of(ihp_vars)) %>%
        janitor::clean_names() %>%
        dplyr::filter(damaged_state_abbreviation %in% state_fips) %>%
        clean_ihp()

    } else {

      df1 = purrr::map_dfr(
          state_fips,
          ~ rfema::open_fema(
              data_set = "individualsandhouseholdsprogramvalidregistrations",
              filters = list(damagedStateAbbreviation = .x),
              ask_before_call = FALSE)) %>%
        dplyr::select(dplyr::any_of(ihp_vars)) %>%
        janitor::clean_names() %>%
        dplyr::filter(damaged_state_abbreviation %in% state_fips) %>%
        clean_ihp()
    }

    zip_county_xwalk = readr::read_csv(
      file.path(get_box_path(), "crosswalks", "geocorr2022_2020_zip_zcta_to_county.csv")) %>%
      dplyr::slice(2:nrow(.)) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        afact = as.numeric(afact),
        dplyr::across(
          .cols = c(county_name, zip_name),
          .fns = ~ stringr::str_remove_all(.x, '\\"'))) %>%
      dplyr::select(
        zcta_code = zcta,
        county_code = county,
        county_name,
        allocation_factor_zcta_to_county = afact) %>%
      dplyr::filter(!is.na(zcta_code))

    df2 = df1 %>%
      dplyr::left_join(
        zip_county_xwalk,
        by = c("zip_code" = "zcta_code"),
        relationship = "many-to-many")

    df3 = df2 %>%
      dplyr::transmute(
        unique_id,
        allocation_factor_zcta_to_county = dplyr::case_when(
          ## in some cases the crosswalk doesn't join but the raw data has a valid county geoid
          ## in which case we assume we attribute all of the given record to non-missing county
          is.na(allocation_factor_zcta_to_county) & !is.na(geoid_county) ~ 1,
          TRUE ~ allocation_factor_zcta_to_county),
        state_name, state_abbreviation, state_code,
        geoid_county = dplyr::if_else(is.na(geoid_county), county_code, geoid_county),
        zcta_code = zip_code, geoid_tract, geoid_block_group,
        amount_individual_housing_program, amount_housing_assistance, amount_other_needs_assistance,
        amount_rental_assistance, amount_repairs, amount_replacement, amount_personal_property,
        amount_flood_insurance_premium_paid_by_fema)

    warning("
County identifiers in the raw data from FEMA have high missingness. For this reason, the data returned by this function reflect a many-to-many
join between the raw data's zip code identifiers and a crosswalk of ZCTAs to counties. This means that many original records are represented as
multiple observations, and it is critical to summarize or otherwise de-deuplicate these data using the `allocation_factor_zcta_to_county` field.

What your workflow should look like:

(1) You can obtain the original data by running: `dplyr::distinct(df, unique_id, .keep_all = TRUE)`.
(2) You can summarize the data to obtain county-level estimates as follows:

    df |>
      dplyr::group_by(geoid_county) |>
      dplyr::mutate(afact = dplyr::if_else(is.na(afact), 1, afact)) |>
      dplyr::summarize(valid_registrations = sum(afact, na.rm = TRUE))")

    message("
These data are from: https://www.fema.gov/openfema-data-page/individuals-and-households-program-valid-registrations-v2.
Per FEMA: This dataset contains IA applications from DR1439 (declared in 2002) to those declared over 30 days ago.
The full data set is refreshed on an annual basis; the last 18 months of data are refreshed weekly.
This dataset includes all major disasters and includes only valid registrants
(applied in a declared county, within the registration period, having damage due to the incident and damage within the incident period).
IHP is intended to meet basic needs and supplement disaster recovery efforts.")

  message(stringr::str_c(
"The unit of observation is individual households' IHP registrations. ",
"However, these observations have been duplicated as a result of joining a ",
"county-level crosswalk; see above for additional details on this. ",
"The `unique_id` field is a unique identifier for each original observation."))

  return(df3)
}

#' Clean IHP data from OpenFEMA
#'
#' @param data The raw IHP data
#'
#' @return IHP data with desired variables and cleaned variable names
#' @noRd
clean_ihp = function(data) {
  data %>%
    dplyr::select(
      disaster_number,
      geoid_block_group = census_geoid,
      #substate_geography = county,
      state_abbreviation = damaged_state_abbreviation,
      #city_name = damaged_city,
      zip_code = damaged_zip_code,
      household_residents = household_composition,
      household_income_gross = gross_income,
      household_tenure = own_rent,
      amount_individual_housing_program = ihp_amount,
      amount_flood_insurance_premium_paid_by_fema = fip_amount,
      amount_housing_assistance = ha_amount,
      amount_other_needs_assistance = ona_amount,
      #emergency_needs_flag = emergency_needs,
      #food_needs_flag = food_need,
      #shelter_needs_flag = shelter_need,
      #special_accommodations_needs_flag = access_functional_needs,
      #fema_determined_value_real_property_damage = rpfvl,
      #fema_determined_value_personal_property_damage = ppfvl,
      amount_rental_assistance = rental_assistance_amount,
      amount_repairs = repair_amount,
      amount_replacement = replacement_amount,
      amount_personal_property = personal_property_amount,
      #max_individual_households_program_flag = ihp_max,
      #max_housing_assistance_flag = ha_max,
      #max_other_needs_assistance_flag = ona_max,
      #date_last_updated = last_refresh
    ) %>%
    dplyr::mutate(
      geoid_tract = stringr::str_sub(geoid_block_group, 1, 11),
      geoid_county = stringr::str_sub(geoid_block_group, 1, 5),
      unique_id = uuid::UUIDgenerate(n = nrow(.)),
      zip_code = stringr::str_pad(zip_code, width = 5, pad = "0", side = "left")) %>%
    dplyr::left_join(
      tidycensus::fips_codes %>%
        dplyr::select(state, state_code, state_name) %>% dplyr::distinct(),
      by = c("state_abbreviation" = "state")) %>%
    dplyr::select(
      unique_id,
      dplyr::matches("^state|^county|^zip|^zcta|name|code|geography"),
      dplyr::everything())
}

#' Standardize county names for matching, to the extent possible
#'
#' @param county
#'
#' @return Cleaned county names more suitable for matching on
#' @noRd
clean_county = function(county) {

  county %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("parish|county|\\.|\\(|\\)|municipality|city and borough|borough|\\|in pmsa.*|'") %>%
    stringr::str_replace_all(c(
      "census area" = "ca",
      "^e " = "east ",
      "^w " = "west ",
      "de kalb" = "dekalb",
      "la salle" = "lasalle",
      "la porte" = "laporte",
      "prince of wales-hyder" = "prince of wales-hyder ca",
      "prince of wales-outer" = "prince of wales-outer ca",
      "state wide" = "statewide",
      "ca ca" = "ca")) %>%
    stringr::str_trim() %>%
    stringr::str_squish()
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
  "uuid", "zip_name", "zcta", "pop20", "state.abb", "ihp_registrations",
  "amount_other_needs_assistance", "census_geoid",  "geoid_block_group", "geoid_county",
  "geoid_tract", "zcta_code"))
