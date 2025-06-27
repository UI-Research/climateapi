#' @title Access county-level data on NFIP policies
#' @param county_geoids A character vector of five-digit county codes.
#' @param file_name The name (not the full path) of the Box file containing the raw data.
#' @param api If TRUE (default), query the API.
#'
#' @returns A dataframe comprising county-level data on current NFIP policies
#' @export
#'
#' @examples
#' \dontrun{
#' get_nfip_policies(
#'    county_geoids = c("01001", "01003"),
#'    api = TRUE)
#' }
get_nfip_policies = function(
	county_geoids,
	file_name = "fima_nfip_policies_2024_10_13.parquet",
	api = TRUE) {

  if (isFALSE(api)) {
    inpath = file.path(
      get_box_path, "hazards", "fema", "national-flood-insurance-program", "raw", file_name)

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
		dplyr::mutate(county_geoid = stringr::str_sub(census_tract, 1, 5)) |>
		dplyr::filter(county_geoid %in% county_geoids) |>
		dplyr::left_join(
			tidycensus::fips_codes |>
				dplyr::select(
				  state_fips = state_code, state_abb = state,
				  county_fips = county_code, county_name = county) |>
				dplyr::mutate(county_geoid = stringr::str_c(state_fips, county_fips)),
			by = c("county_geoid"))

	rm(df1a)

	result = df1b |>
		dplyr::select(
			nfip_policy_id = id,
			state_fips,
			state_abb,
			county_geoid,
			county_fips,
			county_name,
			census_tract,
			policy_cost,
			policy_count,
			policy_rated_flood_zone = rated_flood_zone,
			policy_crs_code = crs_class_code,
			policy_emergency_program_flag = regular_emergency_program_indicator,
			policy_premium_total_cost = total_insurance_premium_of_the_policy,
			policy_date_termination = policy_termination_date,
			policy_date_effective = policy_effective_date,
			building_occupancy_type = occupancy_type,
			building_date_original_construction = original_construction_date,
			building_replacement_cost = building_replacement_cost,
			building_primary_residence_flag = primary_residence_indicator,
			building_small_business_flag = small_business_indicator_building,
			building_floodproofed_flag = floodproofed_indicator,
			building_rental_flag = rental_property_indicator,
			building_tenant_flag = tenant_indicator,
			dplyr::everything())

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
  "policy_count", "rated_flood_zone", "crs_class_code", "regular_emergency_program_indicator",
  "total_insurance_premium_of_the_policy", "policy_termination_date", "policy_effective_date",
  "occupancy_type", "original_construction_date", "building_replacement_cost",
  "primary_residence_indicator", "small_business_indicator_building", "floodproofed_indicator",
  "rental_property_indicator", "tenant_indicator", "DataFrameCallback"))
