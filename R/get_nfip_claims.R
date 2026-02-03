#' @title Access county-level data on NFIP claims
#' @param county_geoids A character vector of five-digit county codes. NULL by default; must be non-NULL if `api = TRUE`.
#' @param file_name The name (not the full path) of the Box file containing the raw data.
#' @param api If TRUE, query the API. FALSE by default.
#'
#' @details
#' These data are from: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims-v2.
#' Per FEMA: This data set represents more than 2,000,000 NFIP claims transactions. It is
#' derived from the NFIP system of record, staged in the NFIP reporting platform and
#' redacted to protect policy holder personally identifiable information. The
#' dataset includes the 50 states + DC and the following territories: Puerto Rico,
#' US Virgin Islands, and Guam.
#'
#' In order to filter to residential claims, filter out occupancy type: "non-residential".
#'
#' Some claims (from multi-unit buildings / condos) are associated with multiple insured units.
#' When calculating the number of units covered by a claim, the analyst should use the count_units_insured column.
#'
#' The example below illustrates and example of how the data set can be summarized to show the total
#' number of residential claims submitted in two different counties, as well as the total damages
#' and payments in the same time period.
#'
#' @returns A data frame comprising county-level data on current NFIP policies
#'  \describe{
#'    \item{state_fips}{A two-digit state identifier.}
#'    \item{state_abbreviation}{The name of the state.}
#'    \item{county_geoid}{A five-digit county identifier.}
#'    \item{county_name}{The name of the county.}
#'    \item{year_construction}{The original year of the construction of the building.}
#'    \item{year_loss}{The year in which the flood loss occurred.}
#'    \item{occupancy_type}{The occupancy type of the primary building associated with the claim.}
#'    \item{count_units_insured}{The number of insured units associated with the claim.}
#'    \item{deductible_building}{The total deductible for buildings, main and appurtenant.}
#'    \item{deductible_contents}{The total deductible for contents.}
#'    \item{value_building}{The value of the main building as estimated by an adjuster.}
#'    \item{value_contents}{The value of the contents as estimated by an adjuster.}
#'    \item{replacement_cost_building}{Estimated cost to replace the building as reported by the insurer.}
#'    \item{replacement_cost_contents}{Estimated cost to replace the contents as reported by the insurer.}
#'    \item{insurance_coverage_building}{The total insurance amount on the building.}
#'    \item{insurance_coverage_contents}{The total insurance amount on the contents.}
#'    \item{damage_building}{The amount of damage to a main property.}
#'    \item{damage_contents}{The value of damage to contents.}
#'    \item{net_payment_building}{Net building payment amount.}
#'    \item{net_payment_contents}{Net contents payment amount.}
#'    \item{net_payment_increased_compliance}{Net Increased Cost of Compliance (ICC) payment amount.}
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#'
#' test <- get_nfip_claims(county_geoids = c("01001", "48201")) |>
#'   dplyr::filter(
#'     year_of_loss >= 2015,  ### in the past 10 years
#'     !occupancy_type %in% c("non-residential")) |> ### only residential claims
#'   dplyr::summarize(
#'     .by = county_geoid,
#'     dplyr::across(dplyr::matches("payment"), sum, na.rm = TRUE),
#'     residential_claims = dplyr::n_distinct(nfip_claim_id))
#'}

get_nfip_claims = function(
  county_geoids = NULL,
  file_name = "fima_nfip_claims_2025_09_09.parquet",
  api = FALSE) {

  ## if reading from disk
  if (isFALSE(api)) {
    if (is.na(file_name)) stop("If `api = FALSE`, you must provide a `file_name` argument.")

    inpath = file.path(
      get_box_path(), "hazards", "fema", "national-flood-insurance-program", "raw", file_name)

    ## check that the cached file exists
    if (! file.exists(inpath)) {
      stop("The provided `file_name` is invalid.") }

    ## if the cached file isn't a parquet file, convert it to one
    if (! (stringr::str_detect(inpath, "parquet"))) {

      convert_delimited_to_parquet(
        inpath = inpath,
        delimit_character = ",",
        dataset = "nfip_claims") }

    ## read the parquet file
    df1a = arrow::read_parquet(file = inpath) |>
      janitor::clean_names()

  } else {

  ## if querying the API:

    ## if no county_geoids, throw an error
    if (is.null(county_geoids)) {
      stop("No `county_geoids` value was supplied; for querying the API, you must supply this argument.") }

    if (length(county_geoids > 10)) {
      warning(stringr::str_c(
"These data are very large; given the number of supplied `county_geoids`, anticipate ",
"a slow query. An alternate approach is to download the full dataset and then supply that ",
"filepath to the parameter `file_name`.")) }

    df1a = purrr::map_dfr(
      county_geoids,
      ~ rfema::open_fema(
        data_set = "fimaNfipClaims",
        filters = list(countyCode = .x),
        ask_before_call = FALSE) |>
        janitor::clean_names())
  }

  ## data cleaning:
  df1b = df1a |>
    tidytable::mutate(
      ### taking county_code if it exists, if not extract from census_tract
      county_geoid = tidytable::if_else(!is.na(county_code), county_code, stringr::str_sub(census_tract, 1, 5)))

  if (!is.null(county_geoids)) {
    df1b = df1b |>
      ### filtering to only the selected county_geoids
      tidytable::filter(county_geoid %in% county_geoids) }

  df1c = df1b |>
    tidytable::left_join(
      tidycensus::fips_codes |>
        tidytable::select(
          state_fips = state_code, state_abb = state, county_fips = county_code,
          county_name = county) |>
        tidytable::mutate(county_geoid = stringr::str_c(state_fips, county_fips)),
      by = c("county_geoid"),
      relationship = "many-to-one")

  result = df1c |>
    dplyr::transmute(
      #nfip_claim_id = id,
      state_fips,
      #state_abbreviation = state, ## this is unreliable--other fields appear to be more consistent
      county_geoid,
      county_name,
      occupancy_type = dplyr::case_when(occupancy_type %in% c(1, 11) ~ "single family",
                                        occupancy_type %in% c(2, 3, 12, 13, 16, 15) ~ "multi-family",
                                        occupancy_type %in% c(14) ~ "mobile/manufactured home",
                                        occupancy_type %in% c(4, 6, 17, 18, 19) ~ "non-residential"),
      year_loss = year_of_loss,
      year_construction = lubridate::year(original_construction_date),
      count_units_insured = policy_count, ## number of insured units associated with the claim
      #cause_of_damage,
      #flood_event_name = flood_event,
      #flood_zone_firm_current = flood_zone_current,
      deductible_building = dplyr::case_when(building_deductible_code == "0" ~ 500,
                                      building_deductible_code == "1" ~ 1000,
                                      building_deductible_code == "2" ~ 2000,
                                      building_deductible_code == "3" ~ 3000,
                                      building_deductible_code == "4" ~ 4000,
                                      building_deductible_code == "5" ~ 5000,
                                      building_deductible_code == "9" ~ 750,
                                      building_deductible_code == "A" ~ 10000,
                                      building_deductible_code == "B" ~ 15000,
                                      building_deductible_code == "C" ~ 20000,
                                      building_deductible_code == "D" ~ 25000,
                                      building_deductible_code == "E" ~ 50000,
                                      building_deductible_code == "F" ~ 1250,
                                      building_deductible_code == "G" ~ 500,
                                      building_deductible_code == "H" ~ 200),
      deductible_contents = dplyr::case_when(contents_deductible_code == "0" ~ 500,
                                      contents_deductible_code == "1" ~ 1000,
                                      contents_deductible_code == "2" ~ 2000,
                                      contents_deductible_code == "3" ~ 3000,
                                      contents_deductible_code == "4" ~ 4000,
                                      contents_deductible_code == "5" ~ 5000,
                                      contents_deductible_code == "9" ~ 750,
                                      contents_deductible_code == "A" ~ 10000,
                                      contents_deductible_code == "B" ~ 15000,
                                      contents_deductible_code == "C" ~ 20000,
                                      contents_deductible_code == "D" ~ 25000,
                                      contents_deductible_code == "E" ~ 50000,
                                      contents_deductible_code == "F" ~ 1250,
                                      contents_deductible_code == "G" ~ 500,
                                      contents_deductible_code == "H" ~ 200),
      value_building = building_property_value,
      value_contents = contents_property_value,
      replacement_cost_building = building_replacement_cost,
      replacement_cost_contents = contents_replacement_cost,
      insurance_coverage_building = total_building_insurance_coverage,
      insurance_coverage_contents = total_contents_insurance_coverage,
      damage_building = building_damage_amount,
      damage_contents = contents_damage_amount,
      net_payment_building = net_building_payment_amount,
      net_payment_contents = net_contents_payment_amount,
      net_payment_icc = net_icc_payment_amount)

  return(result)
}

utils::globalVariables(c(
    "building_deductible_code", "building_property_value",
    "building_replacement_cost", "cause_of_damage", "census_tract",
    "contents_deductible_code", "contents_damage_amount",
    "contents_property_value", "contents_replacement_cost",
    "county", "county_code", "county_fips", "county_geoid",
    "county_name", "flood_event", "flood_zone_current",
    "id", "net_building_payment_amount", "net_contents_payment_amount",
    "net_icc_payment_amount", "occupancy_type", "original_construction_date",
    "original_construction_year", "policy_count", "state", "state_abb",
    "state_abbreviation", "state_code", "state_fips", "total_building_insurance_coverage",
    "total_contents_insurance_coverage", "year_of_loss", "building_damage_amount"
  ))
