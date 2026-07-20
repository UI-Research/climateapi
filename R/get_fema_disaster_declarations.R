#' @title Get major disaster declarations by county
#'
#' @description Retrieves FEMA Major Disaster Declarations at the county level,
#'   aggregated by year and month. Tribal declarations are stored separately as
#'   an attribute.
#'
#' @param file_path The path to the raw data. If NULL (default), reads the most recently
#'   cached file for this dataset from `get_openfema_cache_path()`.
#' @param api If TRUE, access data from the API. Else (default), read from `file_path`
#'   (or the local OpenFEMA cache, if `file_path` is NULL).
#'
#' @details Data are from FEMA's OpenFEMA API. See
#'   \url{https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2}.
#'   Statewide declarations are expanded to all counties in the state.
#'
#'   Connecticut's pre-2022 counties (FIPS 09001-09015) are crosswalked onto the 2022-vintage
#'   planning regions (09110-09190) using a binary any-overlap assumption: a planning region
#'   is treated as having received a declaration if ANY overlapping pre-2022 county received
#'   it, even if only part of that county's area falls within the region. This is not a
#'   proportional/population-weighted allocation (unlike the crosswalks this package uses for
#'   dollar-valued FEMA datasets), since a declaration is a binary event, not a divisible amount.
#'
#' @returns A dataframe comprising Major Disaster Declarations by month by year by county.
#'   Tribal declarations are stored as an attribute (`tribal_declarations`). Columns include:
#'   \describe{
#'     \item{unique_id}{Unique identifier for each observation.}
#'     \item{GEOID}{Five-digit county FIPS code.}
#'     \item{year_declared}{Year the disaster was declared.}
#'     \item{month_declared}{Month the disaster was declared (1-12).}
#'     \item{declaration_title}{Title(s) of the disaster declaration(s).}
#'     \item{incidents_all}{Total count of disaster declarations in the county-month.}
#'     \item{incidents_natural_hazard}{Count of natural hazard declarations.}
#'     \item{incidents_*}{Additional columns for other incident types, each of which reflects the count of the given incident type.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' get_fema_disaster_declarations(api = TRUE)
#' }
get_fema_disaster_declarations = function(
	file_path = NULL,
	api = FALSE) {

	if (!api) {
		if (is.null(file_path)) {
			disaster_declarations1 <- arrow::read_parquet(
					find_openfema_cache_file("DisasterDeclarationsSummaries")) |>
				janitor::clean_names()
		} else {

			## is the file path valid/accessible?
			if (!file.exists(file_path)) {
				stop(stringr::str_c(
					"The path to the dataset does not point to a valid file. ",
					"Please ensure there is a file located at this path: ", file_path, ".")) }

			disaster_declarations1 <- readr::read_csv(file_path) |>
				janitor::clean_names() }
	} else {
		disaster_declarations1 = rfema::open_fema(
				data_set = "DisasterDeclarationsSummaries",
				# Major Disaster Declarations Only
				filters = list(declarationType = "=DR"),
				ask_before_call = FALSE) |>
			janitor::clean_names() }

	## api=TRUE and api=FALSE otherwise return different column types for these two fields
	disaster_declarations1 = disaster_declarations1 |>
		dplyr::mutate(
			place_code = as.character(place_code),
			tribal_request = as.logical(tribal_request))

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
	benchmark_geographies1 = dplyr::bind_rows(
	  tigris::counties(cb = TRUE, year = 2010, progress_bar = FALSE) |>
	    dplyr::transmute(GEOID = stringr::str_c(STATE, COUNTY), year = 2010) |>
	    sf::st_drop_geometry(),
	  tigris::counties(cb = TRUE, year = 2022, progress_bar = FALSE) |>
	    dplyr::transmute(GEOID, year = 2022) |>
	    sf::st_drop_geometry())

	## counties that exist in 2010 or 2022
	benchmark_geographies = benchmark_geographies1 |>
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
		## `place_code` is included in the grouping key so that distinct tribes (each
		## identified by their own place_code) aggregate independently, instead of collapsing
		## same-state/same-month/same-incident-type tribal records into one via
		## `dplyr::first(place_code)`
		tidytable::summarise(
		  .by = c(fips_state_code, fips_county_code, GEOID, place_code, incident_type, year_declared, month_declared),
		  count = dplyr::n(),
		  tribal_request = dplyr::first(tribal_request),
		  declaration_title = paste(declaration_title, collapse = ", ")) |>
	  dplyr::arrange(GEOID, year_declared, month_declared) |>
	  dplyr::mutate(id = dplyr::row_number()) |>
		## widen the data so that there is one row per county x year x month, with
		## incident-level counts in columns
		tidyr::pivot_wider(names_from = incident_type, values_from = count, values_fill = list(count = 0), names_prefix = "incidents_") |>
	  dplyr::rename_with(.cols = dplyr::matches("incidents_"), janitor::make_clean_names)

	## These are all non-tribal, statewide declarations
	## For these records, we join them to a list of counties (by state) so that we
	## can accurately measure declarations at the county level, including counties
	## impacted by declarations that are declared statewide
	##
	## We omit tribal declarations here because these are fundamentally at a non-county
	## level. Instead, we attach tribal declarations to the primary object returned by this
	## function as an attribute so that users can work with these declarations in tandem
	## and incorporate them into their analyses as appropriate
	statewide_declarations1 = disaster_declarations2 |>
	  dplyr::filter(fips_county_code == "000", tribal_request == FALSE) |>
	  dplyr::select(-GEOID) |>
	  dplyr::arrange(year_declared)

	tribal_declarations1 = disaster_declarations2 |>
	  dplyr::filter(fips_county_code == "000", tribal_request == TRUE) |>
	  dplyr::mutate(GEOID = place_code) |>
	  dplyr::arrange(year_declared)

	statewide_declarations2 = statewide_declarations1 |>
	  dplyr::left_join(
	    tibble::tibble(
	      GEOID = benchmark_geographies1 %>%
	        dplyr::filter(year == 2022) %>%
	        dplyr::pull(GEOID)) |>
	      dplyr::mutate(fips_state_code = stringr::str_sub(GEOID, 1, 2)),
	    by = "fips_state_code",
	    relationship = "many-to-many")

	disaster_declarations3a = disaster_declarations2 |>
	  dplyr::filter(!(id %in% c(statewide_declarations1$id))) |>
	  dplyr::bind_rows(statewide_declarations2)

	## Connecticut's real (non-statewide, non-tribal) county-level declarations still carry
	## legacy pre-2022 county FIPS (09001-09015), mixing two incompatible FIPS vintages into
	## one GEOID column alongside the rest of the country's current-vintage GEOIDs (statewide
	## CT declarations, above, are already expanded onto the 2022-vintage regions). Crosswalk
	## these onto the 2022 planning regions via any-overlap propagation -- see @details.
	incident_columns = disaster_declarations3a |>
	  dplyr::select(dplyr::starts_with("incidents_")) |>
	  colnames()

	is_ct_county_specific = disaster_declarations3a$fips_state_code == "09" &
	  disaster_declarations3a$fips_county_code != "000" &
	  disaster_declarations3a$tribal_request == FALSE

	if (any(is_ct_county_specific, na.rm = TRUE)) {

	  message(stringr::str_c(
	    "Connecticut's pre-2022 counties are crosswalked onto 2022-vintage planning regions ",
	    "using an any-overlap assumption: a planning region is treated as having a declaration ",
	    "if ANY overlapping pre-2022 county did, even if only part of that county's area falls ",
	    "within the region. This is not a proportional/population-weighted allocation."))

	  ct_overlap_table = crosswalk::get_crosswalk(
	      source_geography = "county",
	      target_geography = "county",
	      source_year = 2020,
	      target_year = 2022,
	      silent = TRUE)$crosswalks$step_1 |>
	    dplyr::filter(state_fips == "09") |>
	    dplyr::select(source_geoid, target_geoid)

	  ct_crosswalked = disaster_declarations3a |>
	    dplyr::filter(is_ct_county_specific) |>
	    dplyr::left_join(ct_overlap_table, by = c("GEOID" = "source_geoid"), relationship = "many-to-many") |>
	    dplyr::select(-GEOID, -fips_county_code) |>
	    dplyr::rename(GEOID = target_geoid) |>
	    dplyr::mutate(fips_county_code = stringr::str_sub(GEOID, 3, 5)) |>
	    tidytable::summarise(
	      .by = c(GEOID, fips_state_code, fips_county_code, place_code, year_declared, month_declared),
	      id = dplyr::first(id),
	      dplyr::across(dplyr::all_of(incident_columns), ~ sum(.x, na.rm = TRUE)),
	      declaration_title = paste(unique(declaration_title), collapse = ", "))

	  disaster_declarations3 = disaster_declarations3a |>
	    dplyr::filter(!is_ct_county_specific) |>
	    dplyr::bind_rows(ct_crosswalked)

	} else { disaster_declarations3 = disaster_declarations3a }

	prep_data = function(data) {
	  data |>
  	  dplyr::mutate(
  	    unique_id = uuid::UUIDgenerate(n = nrow(data)),
  	    incidents_all = rowSums(
  	      dplyr::select(data, dplyr::matches("incidents")), na.rm = TRUE),
  	    incidents_natural_hazard = rowSums(
  	      dplyr::select(data, dplyr::all_of(natural_hazards)), na.rm = TRUE)) |>
  	    dplyr::rename_with(.cols = -GEOID, .fn = ~ .x |> stringr::str_to_lower()) |>
  	    dplyr::select(
  	      unique_id,
  	      GEOID,
  	      fips_state_code,
  	      year_declared,
  	      month_declared,
  	      declaration_title,
  	      incidents_all,
  	      incidents_natural_hazard,
  	      dplyr::everything()) |>
  	    dplyr::arrange(
  	      GEOID,
  	      year_declared,
  	      month_declared) }

	disaster_declarations_nontribal = disaster_declarations3 |>
	  prep_data() |>
	  dplyr::select(-dplyr::matches("fips")) |>
	  dplyr::filter(GEOID %in% benchmark_geographies)

	disaster_declarations_tribal = tribal_declarations1 |>
	  prep_data() |>
	  dplyr::select(-fips_county_code)

	result = disaster_declarations_nontribal
	attr(result, "tribal_declarations") = disaster_declarations_tribal

warning(stringr::str_c(
"Some counties have observations in the data but those counties no longer exist. ",
"We drop those counties from the dataset and only include counties that existed in ",
"either 2010 or 2020."))

warning(stringr::str_c(
  "Some disasters are declared for the entire state, but such events are not represented ",
  "at the county level in the raw data. We create records for each county in such cases, ",
  "but users should ensure that they reflect only the counties that actually existed at the ",
  "time, as it is possible that in our process we create records for counties that did not exist, ",
  "since we create records for counties that exist as of 2022."))

warning(stringr::str_c(
"Records for disaster declarations for tribes are included as an attribute of the
primary dataframe because these declarations do not pertain to counties (the primary unit
of analysis). Users should ensure they include tribal declarations as appropriate, for
example, for purposes of describing these data at the state level (tribal declarations
are nested within states in these data). The `GEOID` column in this data contains a
unique identifier created by FEMA for identifying tribes; across disasters, the same
tribe should have the same GEOID. However, this GEOID will not connect to other
identifiers in other datasets.

To access tribal declarations: `attr(df, 'tribal_declarations')`"))

message(stringr::str_c(
"The unit of observation is: county x year x month. ",
"The `unique_id` field is a unique identifier for each observation. ",
"Note that these data only describe Major Disaster Declarations."))

	return(result)
}

utils::globalVariables(c(
  "get_box_data_path", "fips_state_code", "fips_county_code", "declaration_date",
  "declaration_type", "incident_type", "GEOID", "year_declared", "month_declared",
  ".", "n", "count", "incidents_all", "incidents_natural_hazard", "benchmark_geographies",
  "declaration_title", "place_code", "tribal_request", "id", "source_geoid", "target_geoid",
  "state_fips"))
