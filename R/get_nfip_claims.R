# Author: Will Curran-Groome

#' @importFrom magrittr %>%

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
#' In order to filter to residential claims, filter out occupancy types 4, 6, 17, 18, 19.
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
#'         \item{nfip_claim_id}{A unique identifier assigned to the claim record}
#'         \item{state_fips}{A two-digit state identifier.}
#'         \item{state_abbreviation}{The name of the state.}
#'         \item{county_geoid}{A five-digit county identifier.}
#'         \item{county_name}{The name of the county.}
#'         \item{occupancy_type}{The occupancy type ( 1=single family residence; 2 = 2 to 4 unit residential building; 3 = residential building with more than 4 units; 4 = Non-residential building; 6 = Non Residential - Business; 11 = Single-family residential building with the exception of a mobile home or a single residential unit within a multi unit building; 12 = A residential non-condo building with 2, 3, or 4 units seeking insurance on all units; 13 = A residential non-condo building with 5 or more units seeking insurance on all units; 14 = Residential mobile/manufactured home; 15 = Residential condo association seeking coverage on a building with one or more units; 16 = Single residential unit within a multi-unit building; 17 = Non-residential mobile/manufactured home; 18 = A non-residential building; 19 = a non-residential unit within a multi-unit building)}
#'         \item{count_units_insured}{The number of insured units associated with the claim. For example, if a condo submits a claim for the 5-unit building, the count_units_insured will be 5.}
#'         \item{cause_of_damage}{The method by which the property/contents were damaged. Note that there may be multiple causes of loss for one claim. (0 : Other causes; 1 : Tidal water overflow; 2 : Stream, river, or lake overflow; 3 : Alluvial fan overflow; 4 : Accumulation of rainfall or snowmelt; 7 : Erosion-demolition; 8 : Erosion-removal; 9 : Earth movement, landslide, land subsidence, sinkholes, etc.; A : Closed basin lake; B : Expedited claim handling process without site inspection; C : Expedited claim handling process follow-up site inspection; D : Expedited claim handling process by Adjusting Process Pilot Program (Remote Adjustment).)}
#'         \item{flood_event_name}{The name of the flood event.}
#'         \item{building_deductible_code}{The total deductible amount in dollars for buildings, both main and appurtenant that can be applied against a loss (0 : $500; 1 : $1,000; 2 : $2,000; 3 : $3,000; 4 : $4,000; 5 : $5,000; 9 : $750; A : $10,000; B : $15,000; C : $20,000; D : $25,000; E : $50,000; F : $1,250; G : $1,500; H : $200 - Used only in Group Flood Insurance Policies)}
#'         \item{contents_deductible_code}{The total deductibel amount in dollars for contents in  both main and apartment structures that can be applied against a loss (0 : $500; 1 : $1,000; 2 : $2,000; 3 : $3,000; 4 : $4,000; 5 : $5,000; 9 : $750; A : $10,000; B : $15,000; C : $20,000; D : $25,000; E : $50,000; F : $1,250; G : $1,500; H : $200 - Used only in Group Flood Insurance Policies)}
#'         \item{building_property_value}{The actual cash value of the main building in whole dollars as estimated by an adjuster.}
#'         \item{contents_property_values}{The actual cash value of the contents in whole dollars as estimated by an adjuster.}
#'         \item{contents_replacement_cost}{Estimated cost in whole dollars to replace the contents as reported by the insurer.}
#'         \item{building_replacement_cost}{Estimated cost in whole dollars to replace the building as reported by the insurer.}
#'         \item{flood_zone_firm_current}{Flood zone derived from the Flood Insurance Rate Map (FIRM) where the insured property is currently located. Refer to FEMA documentation for value descriptions.}
#'         \item{total_building_insurance_coverage}{The total insurance amount in whole dollars on the building.}
#'         \item{total_contents_insurance_coverage}{The total insurance amount in whole dollars on the contents.}
#'         \item{year_of_loss}{The year in which the flood loss occurred.}
#'         \item{original_construction_year}{The original year of the construction of the building.}
#'         \item{building_damage_amount}{The actual cash value amount of damage to a main property in whole dollars.}
#'         \item{net_building_payment_amount}{Net building payment amount made to insured in dollars}
#'         \item{contents_damage_amount}{The actual cash value amount of damage to contents in whole dollars}
#'         \item{net_contents_payment_amount}{Net contents payment amount made to insured in dollars}
#'         \item{net_icc_payment_amount}{Net Increased Cost of Compliance (ICC) payment amount made to insured in dollars.}
#'     }
#' @export
#'
#' @examples
#' \dontrun{
#'
#'test <- get_nfip_claims(county_geoids = c("01001", "48201"),
#'file_name = "fima_nfip_claims_2025_09_09.parquet",
#'api = FALSE) %>%
#'   filter(year_of_loss >= 2015) %>% ### in the past 10 years
#'   filter(!occupancy_type %in% c(4, 6, 17, 18, 19))%>% ### only residential claims
#'   group_by(county_geoid)%>%
#'   summarise(building_damage = sum(building_damage_amount, na.rm = TRUE),
#'             contents_damage = sum(contents_damage_amount, na.rm = TRUE),
#'             number_res_claims = n_distinct(nfip_claim_id),
#'             building_payment = sum(net_building_payment_amount, na.rm = TRUE),
#'             contents_payment = sum(net_contents_payment_amount, na.rm = TRUE),
#'             icc_payment = sum(net_icc_payment_amount, na.rm = TRUE)) %>%
#'   mutate(total_damages = building_damage + contents_damage,
#'          total_payments = building_payment + contents_payment + icc_payment)}


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

  df1b = df1a  |>
    tidytable::mutate(county_geoid = tidytable::if_else(
      !is.na(county_code),
      county_code,
      stringr::str_sub(census_tract, 1, 5) ### taking county_code if it exists, if not extract from census_tract
    ))

  if (!is.null(county_geoids)) {
    df1b = df1b |>
      tidytable::filter(county_geoid %in% county_geoids)
  } ### filtering to only the selected county_geoids

  df1c = df1b |>
    tidytable::left_join(
      tidycensus::fips_codes |>
        tidytable::select(state_fips = state_code, state_abb = state, county_fips = county_code, county_name = county) |>
        tidytable::mutate(county_geoid = stringr::str_c(state_fips, county_fips)),
      by = c("county_geoid")) |>
    tidytable::mutate(id = tidytable::map_chr(id, function(x) paste0(format(x), collapse = ""))) ### converting the unique IDs to character values

  ## to save space:
  rm(df1a)

  result = df1c |>
    dplyr::transmute(
      nfip_claim_id = id,
      state_fips,
      state_abbreviation = state,
      county_geoid,
      county_name,
      occupancy_type,
      count_units_insured = policy_count, ## number of insured units associated with the claim
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
      original_construction_year = lubridate::year(original_construction_date),
      building_damage_amount,
      contents_damage_amount,
      net_building_payment_amount,
      net_contents_payment_amount,
      net_icc_payment_amount)


  message("These data are from: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims-v2.
Per FEMA: This dataset represents more than 2,000,000 claims transactions. It is
derived from the NFIP system of record, staged in the NFIP reporting platform and
redacted to protect policy holder personally identifiable information."

)

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

# inpath_default = file.path(
#   get_box_path(), "hazards", "fema", "national-flood-insurance-program", "intermediate", "fima_nfip_claims_2025_09_09_cleaned.parquet")
#
# write_parquet(result, inpath_default)

#
# if(isFALSE(api)) {
#
#   cleaned_path <- file.path(
#     get_box_path(), "hazards", "fema", "national-flood-insurance-program", "final", "fima_nfip_claims_2025_09_09_cleaned.parquet")
#
#   if(file.exists(cleaned_path)) {
#
#     df1 = arrow::read_parquet(file = cleaned_path) %>%
#       janitor::clean_names()
#
#     if(!is.null(county_geoids)) {
#       df1 = df1 |>
#         dplyr::filter(county_geoid %in% county_geoids)
#     }
#
#     message("These data are from: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims-v2.
# Per FEMA: This dataset represents more than 2,000,000 claims transactions. It is
# derived from the NFIP system of record, staged in the NFIP reporting platform and
# redacted to protect policy holder personally identifiable information.")
#
#     return(df1)
#
#   } else {
#
#     inpath = file.path(
#       get_box_path(), "hazards", "fema", "national-flood-insurance-program", "raw", file_name)
#
#     if (! file.exists(inpath)) {
#       stop("The provided `file_name` is invalid.") }
#
#     if (! (stringr::str_detect(inpath, "parquet"))) {
#
#       convert_delimited_to_parquet(
#         inpath = inpath,
#         delimit_character = ",",
#         dataset = "nfip_claims") }
#
#     df1 = arrow::read_parquet(file = inpath) %>%
#       janitor::clean_names()
#
#   }
# }
#
# else {
#   if (is.null(county_geoids)) {
#     stop("No `county_geoids` value was supplied; for querying the API, you must supply this argument.")
#     county_geoids = tidycensus::fips_codes %>%
#       dplyr::mutate(county_geoid = stringr::str_c(state_code, county_code)) %>%
#       dplyr::distinct(county_geoid) %>%
#       dplyr::pull(county_geoid)
#   }
#
#   df1a = purrr::map_dfr(
#     county_geoids,
#     ~ rfema::open_fema(
#       data_set = "fimanfippolicies",
#       filters = list(countyCode = .x),
#       ask_before_call = FALSE) %>%
#       janitor::clean_names())
# }
#
#
