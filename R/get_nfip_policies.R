#' @title Access county-level data on NFIP policies
#' @param state_abb A 2 letter state abbreviation (e.g. TX). This parameter is required.
#' @param county_geoids A character vector of five-digit county codes.
#' @param file_name The name (not the full path) of the Box file containing the raw data.
#' @param api If TRUE (default), query the API.
#'
#' @details
#'
#'The following dataset houses information on NFIP policies (both historic and current).
#'In order to filter to current policies, the analyst will need to filter on the
#'policy_date_termination and policy_date_effective columns.
#'
#'The dataset also contains both residential and commercial policies. In order to filter
#'to residential policies, the analyst can filter out the "non-residential" occupancy type.
#'
#' @return A dataframe of project-level funding requests and awards, along with variables that can be aggregated to the county level.
#' \describe{
#'         \item{nfip_policy_id}{A unique identifier for each policy in the data.}
#'         \item{state_fips}{A two-digit state identifier.}
#'         \item{state_abb}{The abbreviation of the state.}
#'         \item{county_code}{The five-digit county identifier.}
#'         \item{county_name}{The name of the county.}
#'         \item{census_tract}{The _ digit census tract code}
#'         \item{policy_cost}{The cost of the policy calculated in dollars by adding together the calculated premium, reserve fund assessment, federal policy fee, and HFIAA surcharge.}
#'         \item{policy_count}{The number of insured units in active status associated with the policy}
#'         \item{policy_rated_flood_zone}{The NFIP flood zone}
#'         \item{policy_emergency_program_flag}{The phase of NFIP in which the community is participating (R - regular, E - emergency program)}
#'         \item{policy_premium_total_cost}{The total insurance premium of the policy in whole dollars. Note negative values indicate a refund.}
#'         \item{policy_date_termination}{The date up on which the cancellation of a flood insurance becomes effective - either because it was cancelled or lapsed.}
#'         \item{policy_date_effective}{The effective date of the flood policy.}
#'         \item{building_occupancy_type}{The occupancy type of the building}
#'         \item{building_replacement_cost}{The estimated cost in whole dollars to replace the building as reported by the insurer.}
#'         }
#'
#' @examples
#' \dontrun{
#' test <- get_nfip_policies(state_abb = "TX", county_geoids = c("48201", "48339", "48071", "48167", "48033", "48157", "48473"),
#' file_name = "fima_nfip_policies_2025_10_14.parquet",
#' api = FALSE)|>
#'   dplyr::filter(!occupancy_type %in% c("non-residential"), ### only residential claims,
#'   policy_date_termination >= as.Date("2025-10-15"),
#'   policy_date_effective <= as.Date("2025-10-15")) |>
#'   dplyr::group_by(county_geoid)|>
#'   dplyr::summarise(avg_policy_cost = mean(policy_cost))}
#'
#'@export

## add column name parameter - set to default NULL - return small subset of columns

get_nfip_policies = function(
    state_abb,
    county_geoids = NULL,
    file_name = "fima_nfip_policies_2025_10_14.parquet",
    api = FALSE) {

  if (isFALSE(api)) {

    file <- paste0(state_abb, "_", file_name)

    inpath = file.path(
      get_box_path(),"hazards", "fema", "national-flood-insurance-program", "intermediate", file)

    if (! file.exists(inpath)) {
      stop("The provided `file_name` is invalid.") }

    if (! (stringr::str_detect(inpath, "parquet"))) {

      convert_delimited_to_parquet(
        inpath = inpath,
        delimit_character = ",",
        dataset = "nfip_policies") }

    df1a = arrow::read_parquet(file = inpath) |>
      janitor::clean_names()

  } else {
    df1a = purrr::map_dfr(
      county_geoids,
      ~ rfema::open_fema(
        data_set = "fimanfippolicies",
        filters = list(countyCode = .x),
        ask_before_call = FALSE) |>
        janitor::clean_names())
  }


  df1b = df1a |>
    tidytable::mutate(county_geoid = tidytable::if_else(
      !is.na(county_code),
      county_code,
      stringr::str_sub(census_tract, 1, 5) ### taking county_code if it exists, if not extract from census_tract
    ))


  if (!is.null(county_geoids)) {
    df1b = df1b |>
      tidytable::filter(county_geoid %in% county_geoids)
  }

  rm(df1a)

  df1c = df1b |>
    tidytable::left_join(
      tidycensus::fips_codes |>
        tidytable::select(
          state_fips = state_code, state_abb = state,
          county_fips = county_code, county_name = county) |>
        tidytable::mutate(county_geoid = stringr::str_c(state_fips, county_fips)),
      by = c("county_geoid"))

  result = df1c |>
    dplyr::transmute(
      nfip_policy_id = id,
      state_fips,
      state_abb,
      county_geoid,
      county_name,
      census_tract,
      policy_cost,
      policy_count,
      policy_rated_flood_zone = rated_flood_zone,
      policy_premium_total_cost = total_insurance_premium_of_the_policy,
      policy_date_termination = policy_termination_date,
      policy_date_effective = policy_effective_date,
      building_occupancy_type = dplyr::case_when(occupancy_type %in% c(1, 11, 14) ~ "single family",
                                                           occupancy_type %in% c(2, 12, 3, 13, 16, 15) ~ "multi-family",
                                                           occupancy_type %in% c(14) ~ "mobile/manufactured home",
                                                           occupancy_type %in% c(4, 6, 17, 18, 19) ~ "non-residential"),
      building_replacement_cost = building_replacement_cost)


  message("
These data are from: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-policies-v2.
Per FEMA: This dataset represents more than 80,000,000 policy transactions. It is
derived from the NFIP system of record, staged in the NFIP reporting platform and
redacted to protect policy holder personally identifiable information.")

  return(result)
}

utils::globalVariables(c(
  "id", "census_tract", "county_geoid", "county_code", "state_code", "state",
  "county", "state_fips", "county_fips", "state_abb", "county_name",
  "policy_cost", "policy_count", "rated_flood_zone", "regular_emergency_program_indicator",
  "total_insurance_premium_of_the_policy", "policy_termination_date", "policy_effective_date",
  "occupancy_type", "original_construction_date", "building_replacement_cost",
  "primary_residence_indicator", "small_business_indicator_building", "floodproofed_indicator",
  "rental_property_indicator", "tenant_indicator", "disaster_assistance_coverage_required_code",
  "mandatory_purchase_flag", "id", "censusTract", "policyCost", "policyCount", "ratedFloodZone",
  "totalInsurancePremiumOfThePolicy", "policyTerminationDate", "policyEffectiveDate", "occupancyType",
  "originalConstructionDate", "buildingReplacementCost", "primaryResidenceIndicator", "floodproofedIndicator",
  "rentalPropertyIndicator", "tenantIndicator"
))


### Printing out by state

# outpath <- fs::path(get_box_path(), "hazards", "fema", "national-flood-insurance-program", "intermediate")
#
# df1a %>%
#   group_by(property_state) %>%
#   group_map(~ {
#     # extracting state abbreviation for naming
#     state_abbr <- .y$property_state
#
#     out_path <- file.path(
#       outpath,
#       paste0(state_abbr, "_fima_nfip_policies_2025_10_14.parquet")
#     )
#
#     arrow::write_parquet(.x, out_path)
#
#   })



