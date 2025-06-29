# Author: Kameron Lloyd

#' @title Get major disaster declarations by county
#' @param file_path The path (on Box) to the file containing the raw data.
#' @param api If TRUE (default), access data from the API. Else, read locally from `file_path`.
#' @returns A dataframe comprising Major Disaster Declarations by month by year by county.
#' @export
#' @examples
#' \dontrun{
#' get_fema_disaster_declarations(api = TRUE)
#' }
get_fema_disaster_declarations = function(
	file_path = file.path(
	  get_box_path(), "hazards", "fema", "disaster-declarations",
		"raw", "fema_disaster_declarations_county_2024_10_25.csv"),
	api = TRUE) {

	## is the file path valid/accessible?
	if (!file.exists(file_path) & !api) {
		stop(stringr::str_c(
			"The path to the dataset does not point to a valid file. ",
			"Please ensure there is a file located at this path: ", file_path, ".")) }

	if (!api) {
		disaster_declarations1 <- readr::read_csv(file_path) |>
			janitor::clean_names()
	} else {
		disaster_declarations1 = rfema::open_fema(
				data_set = "DisasterDeclarationsSummaries",
				# Major Disaster Declarations Only
				filters = list(declarationType = "=DR"),
				ask_before_call = FALSE) |>
			janitor::clean_names() }

	# Defining natural hazards so we can filter out disaster declarations for things like terrorist attacks/biological
	natural_hazards <- paste0(
		  "incidents_",
		  c(
		    "Fire", "Flood", "Hurricane", "Severe Storm", "Winter Storm", "Tornado", "Snowstorm",
		    "Earthquake", "Mud/Landslide", "Coastal Storm", "Severe Ice Storm",
		    "Tropical Storm", "Typhoon", "Volcanic Eruption", "Tsunami", "Freezing", "Drought")) |>
		janitor::make_clean_names()

	## the data date back multiple decades, so there are some valid observations for counties that
	## no longer exist as of 2010 or 2022; we drop those counties
	## counties that exist in 2010 or 2022
	benchmark_geographies = dplyr::bind_rows(
	  tigris::counties(cb = TRUE, year = 2010, progress_bar = FALSE) |>
	    dplyr::transmute(GEOID = stringr::str_c(STATE, COUNTY)) |>
	    sf::st_drop_geometry(),
	  tigris::counties(cb = TRUE, year = 2022, progress_bar = FALSE) |>
	    dplyr::select(GEOID) |>
	    sf::st_drop_geometry()) |>
	  dplyr::pull(GEOID) |>
	  unique()

	# Counting disaster declarations by county by year and month
	disaster_declarations2 <- disaster_declarations1 |>
		dplyr::mutate(
			fips_state_code = stringr::str_pad(as.character(fips_state_code), width = 2, side = "left", pad = "0"),
			fips_county_code = stringr::str_pad(as.character(fips_county_code), width = 3, side = "left", pad = "0"),
			GEOID = stringr::str_c(fips_state_code, fips_county_code),
			year_declared = lubridate::year(declaration_date),
			month_declared = lubridate::month(declaration_date)) |>
		## Major Disaster Declarations only
		dplyr::filter(declaration_type == "DR") |>
		## produce counts at the county x incident-type x year x month level
		dplyr::group_by(GEOID, incident_type, year_declared, month_declared) |>
			dplyr::summarise(
			  count = dplyr::n(),
			  declaration_title = paste(declaration_title, collapse = ", ")) |>
			dplyr::ungroup() |>
	  dplyr::arrange(GEOID, year_declared, month_declared) |>
		## widen the data so that there is one row per county x year x month, with
		## incident-level counts in columns
		tidyr::pivot_wider(names_from = incident_type, values_from = count, values_fill = list(count = 0), names_prefix = "incidents_") |>
	  dplyr::rename_with(.cols = dplyr::matches("incidents_"), janitor::make_clean_names)

	disaster_declarations = disaster_declarations2 |>
	  dplyr::mutate(
			unique_id = uuid::UUIDgenerate(n = nrow(disaster_declarations2)),
			incidents_all = rowSums(dplyr::select(disaster_declarations2, dplyr::matches("incidents")), na.rm = TRUE),
			incidents_natural_hazard = rowSums(dplyr::select(disaster_declarations2, dplyr::all_of(natural_hazards)), na.rm = TRUE)) |>
	  dplyr::rename_with(.cols = -GEOID, .fn = ~ .x |> stringr::str_to_lower()) |>
	  dplyr::select(
			unique_id,
			GEOID,
			year_declared,
			month_declared,
			declaration_title,
			incidents_all,
			incidents_natural_hazard,
			dplyr::everything()) |>
	  dplyr::arrange(
			GEOID,
			year_declared,
			month_declared) |>
	  dplyr::filter(GEOID %in% benchmark_geographies)

	message(stringr::str_c(
"The unit of observation is: county x year x month. ",
"The `unique_id` field is a unique identifier for each observation. ",
"Note that these data only describe Major Disaster Declarations."))

	return(disaster_declarations)
}

utils::globalVariables(c(
  "get_box_data_path", "fips_state_code", "fips_county_code", "declaration_date",
  "declaration_type", "incident_type", "GEOID", "year_declared", "month_declared",
  ".", "n", "count", "incidents_all", "incidents_natural_hazard", "benchmark_geographies",
  "declaration_title"))
