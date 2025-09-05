# Author: Will Curran-Groome

#' @importFrom magrittr %>%

#' @title Access county-level data on NFIP policies
#' @param county_geoids A character vector of five-digit county codes. NULL by default; must be non-NULL if `api = TRUE`.
#' @param file_name The name (not the full path) of the Box file containing the raw data.
#' @param api If TRUE, query the API. FALSE by default.
#'
#' @returns A dataframe comprising county-level data on current NFIP policies
#' @export
#'
#' @examples
#' \dontrun{
#' get_nfip_claims(
#'    county_geoids = c("01001", "01003"),
#'    api = TRUE)
#' }
get_nfip_claims = function(
    county_geoids = NULL,
    file_name = "fima_nfip_claims_2025_06_08.parquet",
    api = FALSE) {

  if (isFALSE(api)) {
    inpath = file.path(
      get_box_path(), "hazards", "fema", "national-flood-insurance-program", "raw", file_name)

    if (! file.exists(inpath)) {
      stop("The provided `file_name` is invalid.") }

    if (! (stringr::str_detect(inpath, "parquet"))) {

      convert_delimited_to_parquet(
        inpath = inpath,
        delimit_character = ",",
        dataset = "nfip_claims") }

    df1a = arrow::read_parquet(file = inpath) %>%
      janitor::clean_names()

  } else {
    if (is.null(county_geoids)) {
      stop("No `county_geoids` value was supplied; for querying the API, you must supply this argument.")
      county_geoids = tidycensus::fips_codes %>%
        dplyr::mutate(county_geoid = stringr::str_c(state_code, county_code)) %>%
        dplyr::distinct(county_geoid) %>%
        dplyr::pull(county_geoid)
    }

    df1a = purrr::map_dfr(
      county_geoids,
      ~ rfema::open_fema(
        data_set = "fimanfippolicies",
        filters = list(countyCode = .x),
        ask_before_call = FALSE) %>%
        janitor::clean_names())
  }

  df1b = df1a %>%
    dplyr::mutate(county_geoid = stringr::str_sub(census_tract, 1, 5)) %>%
    {if (!is.null(county_geoids)) dplyr::filter(., county_geoid %in% county_geoids) else . } %>%
    dplyr::left_join(
      tidycensus::fips_codes %>%
        dplyr::select(state_fips = state_code, state_abb = state, county_fips = county_code, county_name = county) %>%
        dplyr::mutate(county_geoid = stringr::str_c(state_fips, county_fips)),
      by = c("county_geoid"))

  ## to save space:
  rm(df1a)

  result = df1b %>%
    dplyr::select(
      nfip_policy_id = id,
      state_fips,
      state_abbreviation = state,
      county_geoid,
      county_name,
      occupancy_type,
      count_units_insured = policy_count, ## if a condo, the number of units is stored separately (see below)
      count_units_condo_insured = number_of_units, ## "the number of residential AND NONRESIDENTIAL units covered by the conominium master policy"
      cause_of_damage,
      flood_event_name = flood_event,
      building_deductible_code,
      contents_deductible_code,
      building_property_value,
      contents_property_value,
      contents_replacement_cost,
      building_replacement_cost,
      flood_zone_firm_current = flood_zone_current,
      total_building_insurance_coverage,
      total_contents_insurance_coverage,
      year_of_loss,
      dplyr::matches("amount"))

  ## NEED TO ADDRESS THE TWO COUNT VARIABLES SO THAT EACH RECORD REFLECTS A SINGLE CLAIM
  ## OR HAS A SINGLE VARIABLE DENOTING THE NUMBER OF CLAIMS IT ENCOMPASSES
  # result %>%
  #   dplyr::select(dplyr::matches("count")) %>%
  #   dplyr::count(count_units_condo_insured, count_units_insured, sort = TRUE)

  message("
These data are from: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-policies-v2.
Per FEMA: This dataset represents more than 80,000,000 policy transactions. It is
derived from the NFIP system of record, staged in the NFIP reporting platform and
redacted to protect policy holder personally identifiable information.")

  return(result)

}

utils::globalVariables(c(
  "census_tract", "county_geoid", "state_code", "state", "county_code", "county",
  "state_fips", "county_fips", "id", "state_abb", "county_name", "policy_cost",
  "policy_count",  "building_deductible_code", "building_property_value", "cause_of_damage",
  "contents_deductible_code", "contents_property_value", "contents_replacement_cost",
  "count_units_condo_insured", "count_units_insured", "flood_event", "flood_zone_current",
  "number_of_units", "total_building_insurance_coverage", "total_contents_insurance_coverage",
  "year_of_loss"))
