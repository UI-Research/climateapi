# Author: Kameron Lloyd

#' @title Get major disaster declarations by county
#' @param file_path The path (on Box) to the file containing the raw data.
#' @param api If TRUE (default), access data from the API. Else, read locally from `file_path`.
#' @returns A dataframe comprising Major Disaster Declarations by month by year by county.
#'  Tribal declarations are stored as an attribute of the primary dataframe called `tribal_declarations`.
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
				ask_before_call = FALSE) |>
			janitor::clean_names() }

	natural_hazards <- c(
    "Fire", "Flood", "Hurricane", "Severe Storm", "Winter Storm", "Tornado", "Snowstorm",
    "Earthquake", "Mud/Landslide", "Coastal Storm", "Severe Ice Storm",
    "Tropical Storm", "Typhoon", "Volcanic Eruption", "Tsunami", "Freezing", "Drought",
    "Tropical Depression", "Straight-Line Winds", "Other Natural Hazards")

	temp = disaster_declarations1 %>%
	  dplyr::mutate(
	    designated_area = designated_area %>% stringr::str_squish() %>% stringr::str_trim(),
	    county_fips_code = stringr::str_c(fips_state_code, fips_county_code))

	## most declarations where the county code == 000 are "Statewide" designations
	temp %>%
	  dplyr::filter(fips_county_code == "000") %>%
	  dplyr::count(designated_area, sort = TRUE)

	## seemingly all such declarations have a "designated_area" corresponding to an Indigenous community
	## (i.e., a tribe, tribal area, or pacific island area)
	temp %>%
	  dplyr::filter(
	    fips_county_code == "000",
	    designated_area != "Statewide") %>%
	  dplyr::count(designated_area, sort = TRUE) %>%
	  print(n = Inf)

	## let's double check that's right
	## TDSA: Tribally-Designated Service Area
	## OTSA: Oklahoma Tribal Statistical Area
	## ANV: Alaskan Native Village
	## Joint Area: areas shared by multiple tribes
	temp %>%
	  dplyr::filter(
	    fips_county_code == "000",
	    designated_area != "Statewide",
	    !stringr::str_detect(designated_area, "OTSA|Reservation|Tribe|Native|ANV|Band|Indians|Trust|Indian|Pueblo|TDSA|Joint Area")) %>%
	  dplyr::count(designated_area, sort = TRUE) %>%
	  print(n = Inf)
	## the few remaining areas are all tribal areas as well

	## disaster declaration observations are fundamentally not at the county level
	## they include townships, independent cities, educational areas, remote maine areas,
	## and more
	disaster_declarations2 <- disaster_declarations1 |>
		dplyr::mutate(
			fips_state_code = stringr::str_pad(as.character(fips_state_code), width = 2, side = "left", pad = "0"),
			fips_county_code = stringr::str_pad(as.character(fips_county_code), width = 3, side = "left", pad = "0"),
			GEOID = stringr::str_c(fips_state_code, fips_county_code),
			year_declared = lubridate::year(declaration_date),
			month_declared = lubridate::month(declaration_date),
			tribal_designated_area = dplyr::case_when(
			  fips_county_code == "000" & designated_area != "Statewide" ~ 1,
			  TRUE ~ 0),
			incident_type = dplyr::case_when(
			  incident_type == "Other" & stringr::str_detect(declaration_title, "STRAIGHT-LINE") ~ "Straight-Line Winds",
			  incident_type == "Other" & stringr::str_detect(declaration_title, "WIND STORM|TIDAL WAVE|SEISMIC SEA WAVE|SEVERE WEATHER CONDITIONS|HIGH WINDS") ~ "Other Natural Hazards",
			  TRUE ~ incident_type)) %>%
	  dplyr::transmute(
	    disaster_number,
	    declaration_title,
	    type_declaration = dplyr::case_when(
	      declaration_type == "DR" ~ "Major disaster declaration",
	      declaration_type == "EM" ~ "Emergency declaration",
	      declaration_type == "FM" ~ "Fire management declaration",
	      TRUE ~ declaration_type),
	    type_incident = incident_type,
	    type_incident_natural_hazard = dplyr::case_when(
        type_incident %in% natural_hazards ~ TRUE,
        TRUE ~ FALSE),
	    designated_area,
	    state_fips_code = stringr::str_sub(GEOID, 1, 2),
	    GEOID,
	    date_declaration = declaration_date,
	    date_incident_begin = incident_begin_date,
	    date_incident_end = incident_end_date,
	    tribal_request,
	    tribal_designated_area,
	    program_declared_individuals_households = ih_program_declared,
	    program_declared_individual_assistance = ia_program_declared,
	    program_declared_public_assistance = pa_program_declared,
	    program_declared_hazard_mitigation_assistance = hm_program_declared) %>%
	  dplyr::mutate(dplyr::across(.cols = dplyr::matches("tribal|program_declared"), as.logical)) %>%
	  dplyr::arrange(dplyr::desc(date_declaration))

	disaster_declarations2 %>%
	  dplyr::count(type_incident, type_incident_natural_hazard) %>%
	  dplyr::filter(type_incident_natural_hazard == FALSE)

	disaster_declarations2 %>%
	  dplyr::filter(!stringr::str_detect(designated_area, "County|Parish|Municipio")) %>%
	  dplyr::count(designated_area, tribal_designated_area, sort = TRUE) %>%
	  dplyr::arrange(dplyr::desc(tribal_designated_area), dplyr::desc(n))
		## Major Disaster Declarations only
		dplyr::filter(declaration_type == "DR") |>
		## produce counts at the county x incident-type x year x month level
		dplyr::group_by(fips_state_code, fips_county_code, GEOID, incident_type, year_declared, month_declared) |>
			dplyr::summarise(
			  count = dplyr::n(),
			  place_code = dplyr::first(place_code),
			  tribal_request = dplyr::first(tribal_request),
			  declaration_title = paste(declaration_title, collapse = ", ")) |>
			dplyr::ungroup() |>
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

	## all tribal declarations have fips_county_code == "000"
	# disaster_declarations1 %>%
	#   dplyr::filter(tribal_request == TRUE) %>%
	#   dplyr::count(fips_county_code)

	tribal_declarations1 = disaster_declarations2 |>
	  dplyr::filter(fips_county_code == "000", tribal_request == TRUE) |>
	  dplyr::mutate(GEOID = place_code) |>
	  dplyr::arrange(year_declared)

	statewide_declarations2 = statewide_declarations1 |>
	  dplyr::left_join(
	    tibble::tibble(GEOID = benchmark_geographies) |>
	      dplyr::mutate(fips_state_code = stringr::str_sub(GEOID, 1, 2)),
	    by = "fips_state_code",
	    relationship = "many-to-many")

	disaster_declarations3 = disaster_declarations2 |>
	  dplyr::filter(!(id %in% c(statewide_declarations1$id))) |>
	  dplyr::bind_rows(statewide_declarations2)

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
  	      month_declared)
	}

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
"Records for disaster declarations for tribes are included as an attribute of the
primary dataframe because these declarations do not pertain to counties (the primary unit
of analysis). Users should ensure they include tribal declarations as appropriate, for
example, for purposes of describing these data at the state level (tribal declarations
are nested within states in these data). The `GEOID` column in this data contains a
unique identifier created by FEMA for identifying tribes; across disasters, the same
tribe should have the same GEOID. However, this GEOID will not connect to other
identifiers in other datasets.

To access tribal declarations: 	`attr(df, 'tribal_declarations')`"))

warning(stringr::str_c(
"Some disasters are declared for the entire state, but such events are not represented ",
"at the county level in the raw data. We create records for each county in such cases, ",
"but users should ensure that they reflect only the counties that actually existed at the ",
"time, as it is possible that in our process we create records for counties that did not exist."))

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
  "declaration_title", "place_code", "tribal_request"))

