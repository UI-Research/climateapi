#' @title Access temporal county-level SHELDUS hazard damage data.
#' @param file_path The path to the raw SHELDUS data.
#'
#' @returns A dataframe comprising hazard x month x year x county observations of hazard events.
#' @export
#'
#' @examples
#' \dontrun{
#' get_sheldus()
#' }

get_sheldus = function(
	file_path = file.path(
		get_box_path(),"hazards", "sheldus",
		"SHELDUS_23.0_12312023_AllCounties_CountyAggregate_YearMonthHazard_2023USD",
		"direct_loss_aggregated_output_24075.csv")) {

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
			### confirmed in data documentation the per capita values are calculated using the most recent year inflation
			### all values are in the latest year's data
			damage_property = property_damage_adjusted_2023,
			damage_property_per_capita = property_damage_per_capita,
			damage_crop = crop_damage_adjusted_2023,
			damage_crop_per_capita = crop_damage_per_capita,
			dplyr::matches("injuries|fatalities"),
			records) |>
		dplyr::arrange(GEOID, year, month) |>
		dplyr::filter(GEOID %in% benchmark_geographies)

message(stringr::str_c(
"The unit of observation is: county x year x month x hazard. ",
"The `unique_id` field is a unique identifier for each observation. ",
"Not all counties have observations for each month x year x hazard. ",
"That is, only counties with a disaster event have an observation for a given month x year x hazard. ",
"The `records` field reflects the number of events that were aggregated to ",
"calculate the values reflected in the given observation.",
"All dollar-denominated values are in 2023 dollars."))

	return(df2)
}

utils::globalVariables(c(
  "state_name", "county_fips", "unique_id", "GEOID", "county_name", "year", "month",
  "damage_property", "damage_property_per_capita", "damage_crop",
  "damage_crop_per_capita", "records", "benchmark_geographies",
  "STATE", "COUNTY", "."))
