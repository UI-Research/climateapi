# Author: Will Curran-Groome

#' @importFrom magrittr %>%

#' @title Convert raw data to parquet to conserve memory / speed subsequent operations.
#' @param inpath The local path to read CSV data from.
#' @param outpath The local path to write parquet data to.
#' @param delimit_character The delimiting character of the raw data.
#' @param subsetted_columns The columns to include in the outputted parquet data.
#' @returns Nothing. Parquet data are written to local path.
raw_nfip_policies_to_parquet = function(
		inpath,
		outpath = NULL,
		delimit_character = ",",
		subsetted_columns = NULL) {

	## write to the same location (but as .parquet), by default
	if (is.null(outpath)) {
		outpath = inpath %>% stringr::str_replace("csv", "parquet")
	}

	if (!file.exists(outpath)) {
		## get username to configure Box path less rigidly
		username = getwd() %>% stringr::str_match("Users/.*?/") %>% stringr::str_remove_all("Users|/")
		box_path = file.path(
			"C:", "Users", username, "Box", "METRO Climate and Communities Practice Area",
			"github-repository", "hazards", "fema", "national-flood-insurance-program", "raw")

		inpath = file.path(box_path, "fima_nfip_policies_2024_10_13.csv")

		if (is.null(subsetted_columns)) {
			#The relevant columns
			subsetted_columns = c(
				"id",
				"longitude",
				"latitude",
				"censusTract",
				"crsClassCode",
				"ratedFloodZone",
				"occupancyType",
				"originalConstructionDate",
				"policyCost",
				"policyCount",
				"policyEffectiveDate",
				"policyTerminationDate",
				"primaryResidenceIndicator",
				"regularEmergencyProgramIndicator",
				"smallBusinessIndicatorBuilding",
				"totalInsurancePremiumOfThePolicy",
				"buildingReplacementCost",
				"floodproofedIndicator",
				"rentalPropertyIndicator",
				"tenantIndicator")
		}

		## a quick test prior to reading in full file
		raw_txt_test_delimit_character = tryCatch(
			{ readr::read_delim(inpath, delim = delimit_character, n_max = 5) },
			error = function(e) { stop("Error reading inpath file. Did you provide the correct `delimit_character` value for the input file-type?") })

		## a quick test prior to reading in full file
		raw_txt_test_subsetted_columns = tryCatch(
			{ readr::read_delim(inpath, delim = delimit_character, col_select = dplyr::all_of(subsetted_columns), n_max = 5) },
			error = function(e) { stop("Error reading inpath file. The subsetted columns may not be present in the inpath file.")})

		## a callback function to read the full file in chunks
		read_chunk_callback = function(x, cols) { x %>% dplyr::select(dplyr::all_of(subsetted_columns)) }

		## reading the file in chunks
		raw_text_subsetted = tryCatch(
			{ readr::read_delim_chunked(inpath, delim = delimit_character, callback = DataFrameCallback$new(read_chunk_callback), chunk_size = 1000000) },
			error = function(e) { stop(e)})

		arrow::write_parquet(raw_text_subsetted, sink = outpath)
	}
}

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
#' get_nfip_policies_county(
#'    county_geoids = c("01001", "01003"),
#'    api = TRUE)
#' }
get_nfip_policies_county = function(
	county_geoids,
	file_name = "fima_nfip_policies_2024_10_13.parquet",
	api = TRUE) {

  if (!api) {
  	username = getwd() %>% stringr::str_match("Users/.*?/") %>% stringr::str_remove_all("Users|/")
  	box_path = file.path(
  		"C:", "Users", username, "Box", "METRO Climate and Communities Practice Area",
  		"github-repository", "hazards", "fema", "national-flood-insurance-program", "raw")

  	inpath = file.path(box_path, file_name)

  	stopifnot(file.exists(inpath))

  	if (stringr::str_detect(inpath, "csv")) {
  		outpath = inpath %>% stringr::str_replace("csv", "parquet")

  		if (! file.exists(outpath)) {
  			raw_nfip_policies_to_parquet(inpath = inpath, outpath = outpath)
  		}
  		inpath = inpath %>% stringr::str_replace("csv", "parquet")
  	}

  	df1a = arrow::read_parquet(file = inpath) %>%
  		janitor::clean_names()
  } else {
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
		dplyr::filter(county_geoid %in% county_geoids) %>%
		dplyr::left_join(
			tidycensus::fips_codes %>%
				dplyr::select(state_fips = state_code, state_abb = state, county_fips = county_code, county_name = county) %>%
				dplyr::mutate(county_geoid = stringr::str_c(state_fips, county_fips)),
			by = c("county_geoid"))

	rm(df1a)

	df2 = df1b %>%
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

	result = df2

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
