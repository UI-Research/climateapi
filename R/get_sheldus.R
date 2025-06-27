#' @title Access temporal county-level SHELDUS hazard damage data.
#' @param file_path The path to the raw SHELDUS data.
#'
#' @returns A dataframe comprising month x year x county observations of hazard events.
#' @export
#'
#' @examples
#' \dontrun{
#' get_sheldus()
#' }

get_sheldus = function(
	file_path = file.path(
		get_box_path(), "hazards", "sheldus",
		"SHELDUS_22.0_01012916_01012023_AllCounties_CountyAggregate_YearMonth_2022USD",
		"UID14148f_AGG_A.csv")) {

	## is the file path valid/accessible?
	if (!file.exists(file_path)) {
		stop(stringr::str_c(
			"The path to the dataset does not point to a valid file. ",
			"Please ensure there is a file located at this path: ", file_path, ".")) }

  ## the data date back to ~1960, so there are some valid observations for counties that
  ## no longer exist as of 2010; we drop those counties
  ## counties that exist in 2010 or 2022
  benchmark_geographies = dplyr::bind_rows(
    tigris::counties(cb = TRUE, year = 2010) |>
      dplyr::transmute(GEOID = stringr::str_c(STATE, COUNTY)) |>
      sf::st_drop_geometry(),
    tigris::counties(cb = TRUE, year = 2022) |>
      dplyr::select(GEOID) |>
      sf::st_drop_geometry()) |>
    dplyr::pull(GEOID) |>
    unique()

	df1 = file_path |>
		readr::read_csv() |>
		janitor::clean_names()

	df2 = df1 |>
		dplyr::mutate(
			state_name = state_name |> stringr::str_to_sentence(),
			GEOID = stringr::str_remove_all(county_fips, "'"),
			unique_id = uuid::UUIDgenerate(n = nrow(df1))) |>
		dplyr::rename_with(
		  .cols = dplyr::everything(),
		  .fn = ~ stringr::str_replace_all(.x,
		    c("dmg" = "damage", "adj" = "adjusted"))) |>
		dplyr::select(
			unique_id,
			GEOID,
			state_name,
			county_name,
			year,
			month,
			property_damage_adjusted_2022,
			property_damage_per_capita_adjusted_2022,
			crop_damage_adjusted_2022,
			crop_damage_per_capita_adjusted_2022,
			dplyr::matches("injuries|fatalities|duration"),
			records) |>
		dplyr::arrange(GEOID, year, month) |>
		dplyr::filter(GEOID %in% benchmark_geographies)

message(stringr::str_c(
"The unit of observation is: county x year x month. ",
"The `unique_id` field is a unique identifier for each observation. ",
"Not all counties have observations for each month x year. ",
"That is, only counties with a disaster event have an observation for a given month x year. ",
"The `records` field reflects the number of events that were aggregated to ",
"calculate the values reflected in the given observation."))

	return(df2)
}

utils::globalVariables(c(
  "state_name", "county_fips", "unique_id", "GEOID", "county_name", "year", "month",
  "property_damage_adjusted_2022", "crop_damage_adjusted_2022", "crop_damage_per_capita_adjusted_2022",
  "property_damage_per_capita_adjusted_2022", "records", "benchmark_geographies",
  "STATE", "COUNTY", "."))
