#' Get FEMA Public Assistance (PA) funding
#'
#' Project- and county-level data on PA funding over time
#'
#' @param state_abbreviations A character vector of state abbreviations. NULL by default, which returns records for all 51 states. Only the 51 states are supported at this time.
#' @param file_path The file path to the raw data contained in a .parquet file.
#'
#' @details
#' These data have been crosswalked so that estimates can be  aggregated at the county level.
#' This is necessary (for county-level estimates) because many projects are statewide
#' projects and do not have county-level observations in the data.
#'
#' Analysts thus have two options for working with these data:
#'   (1) De-select the variables suffixed with `_split` and then run `distinct(df)`.
#'       This will provide unique observations for projects; projects are both county-level
#'       and statewide. These data can be aggregated to the state level but cannot be
#'       comprehensively aggregated to the county level.
#'   (2) Group the data at the county level and summarize to produce county-level
#'       characterizations of PA projects and funding, using the `_split`-suffixed
#'       variables to calculate funding totals. For example, this might look like:
#'
#' The attribution of statewide projects to the county level occurs by proportionally attributing
#' project costs based on county-level populations. For example, in a fictional state with two
#' counties, one of population 10 and one of population 90, 10% of a statewide project's funding
#' would be attributed to the first county and 90% of the project's funding to second county.
#' Roughly 62 percent of the total PA funding returned by this function is for county-specific
#' projects, and the remaining 38 percent is for statewide projects (as of 2025).
#'
#' @return A dataframe of project-level funding requests and awards, along with variables that can be aggregated to the county level.
#'     \describe{
#'         \item{id}{A unique identifier for each project in the raw data. This field is not unique in the returned data due to crosswalking statewide projects to the county level. Refer to the details for additional information}
#'         \item{state_fips}{A two-digit state identifier.}
#'         \item{state_name}{The name of the state.}
#'         \item{state_abbreviation}{The two-character USPS abbreviation for the state.}
#'         \item{county_fips}{A five-digit county identifier.}
#'         \item{county_name}{The name of the county.}
#'         \item{declaration_year}{The year when the authorizing disaster declaration was made.}
#'         \item{disaster_number}{The FEMA-created disaster number. This is unique at the disaster-state level; for example, Hurricane Helene has multiple disaster numbers associated with it, one per state that received an associated disaster declaration.}
#'         \item{incident_type}{The type of disaster, e.g., "Hurricane".}
#'         \item{project_status}{The current status of the funded PA project, e.g., "Active".}
#'         \item{damage_category_code}{A letter code identifying the category of damages/what funds may be used for.}
#'         \item{damage_category_description}{A descriptive characteristization of the damage category.}
#'         \item{pa_federal_funding_obligated}{Obligated federal funding at the project `id` level.}
#'         \item{pa_federal_funding_obligated_split}{Obligated federal attributed to the `id`-by-county level. Refer to the details for additional information.}
#'     }
#' @export
#'
#' @examples
#' \dontrun{
#'   get_public_assistance(state_abbreviations = "NJ")
#' }

.datatable.aware=TRUE

get_public_assistance= function(
    file_path = file.path(
      get_box_path(), "hazards", "fema", "public-assistance", "raw",
      "PublicAssistanceFundedProjectsDetailsV2_2025_09_26.parquet"),
    state_abbreviations = NULL) {

  if (is.null(state_abbreviations)) { state_abbreviations = c(state.abb, "DC") }
  if (any(!state_abbreviations %in% c(state.abb, "DC"))) { stop("Only the 50 states and DC are supported at this time.") }

  state_fips = get_geography_metadata("state") |>
    dplyr::filter(state_abbreviation %in% state_abbreviations) |>
    dplyr::pull(state_code)

  ## data on counties and their populations
  suppressMessages({suppressWarnings({
    state_county_xwalk = get_geography_metadata(geography_type = "county") |>
      dplyr::filter(state_code %in% state_fips) })})

  public_assistance_raw = arrow::read_parquet(file_path) |>
    janitor::clean_names() |>
    dplyr::transmute(
      id = dplyr::row_number(),
      state_fips = stringr::str_pad(state_number_code, side = "left", width = 2, pad = "0"),
      county_name = county,
      county_fips = stringr::str_c(
        state_fips,
        stringr::str_pad(as.character(county_code), side = "left", width = 3, pad = "0")),
      disaster_number,
      declaration_year = lubridate::year(declaration_date),
      incident_type,
      project_status,
      damage_category_code,
      damage_category_description = damage_category_descrip,
      pa_category = stringr::str_c(damage_category_code, damage_category_description, sep = " - "),
      pa_federal_funding_obligated = federal_share_obligated) |>
    dplyr::filter(state_fips %in% !!state_fips, as.numeric(state_fips) < 60)

  interpolate_columns = c("pa_federal_funding_obligated")

  ## Connecticut counties are dated -- we need to crosswalk them to current county-
  ## equivalents (planning regions)
  if ("09" %in% state_fips) {
    ct_county_crosswalk1 = readr::read_csv(
      file.path(get_box_path(), "crosswalks", "ctdata_2022_tractcrosswalk_connecticut.csv")) |>
      janitor::clean_names() |>
      dplyr::select(
        ## each tract has a single observation
        tract_fips_2020,
        ## each tract has a single observation
        tract_fips_2022,
        county_fips_2020,
        county_fips_2022 = ce_fips_2022)

    suppressMessages({
      ct_tract_populations = tidycensus::get_acs(
        year = 2021,
        geography = "tract",
        state = "CT",
        variable = "B01003_001",
        output = "wide") |>
        dplyr::select(
          tract_fips_2020 = GEOID,
          population_2020 = B01003_001E) })

    ct_county_crosswalk = ct_county_crosswalk1 |>
      dplyr::left_join(ct_tract_populations, by = "tract_fips_2020") |>
      dplyr::summarize(
        .by = c("county_fips_2020", "county_fips_2022"),
        population_2020 = sum(population_2020)) |>
      dplyr::mutate(
        .by = "county_fips_2020",
        population_2020_total = sum(population_2020, na.rm = TRUE)) |>
      dplyr::mutate(allocation_factor = population_2020 / population_2020_total) |>
      dplyr::select(-dplyr::matches("population"))

    crosswalked_ct_counties = public_assistance_raw |>
      dplyr::filter(state_fips == "09") |>
      dplyr::left_join(
        ct_county_crosswalk,
        by = c("county_fips" = "county_fips_2020"),
        relationship = "many-to-many") |>
      tidytable::summarize(
        .by = c("id"),
        dplyr::across(
          .cols = dplyr::all_of(interpolate_columns),
          .fns = \(x) base::sum(x * allocation_factor, na.rm = TRUE)),
        dplyr::across(
          .cols = -dplyr::all_of(interpolate_columns),
          .fns = \(x) dplyr::first(x))) |>
      tibble::as_tibble() |>
      dplyr::select(-county_fips) |>
      dplyr::rename(county_fips = county_fips_2022)

    #joining the non CT states with the CT crosswalk to create the updated dataset
    public_assistance1 = public_assistance_raw |>
      dplyr::filter(state_fips != "09") |>
      dplyr::bind_rows(crosswalked_ct_counties) |>
      dplyr::select(-allocation_factor) } else { public_assistance1 = public_assistance_raw }

  public_assistance2 <- public_assistance1 |>
    dplyr::filter(
      !(incident_type %in% c(
        "Biological", "Fishing Losses", "Human Cause", "Terrorist Attack",
        "Dam/Levee Break", "Toxic Substances", "National Special Security Event",
        "Chemical"))) |>
    dplyr::mutate(
      statewide_flag = dplyr::case_when(
        stringr::str_detect(county_name, "tatewide") ~ 1,
        #for the REAAs in Alaska, we are making them statewide
        stringr::str_detect(county_name, "REAA") ~ 1,
        ## these are county_fips placeholder values--no real county FIPS is 000
        ## these are in fact statewide awards
        stringr::str_sub(county_fips, 3, 5) == "000" ~ 1,
        TRUE ~ 0),
      # In these cases, the disaster number was associated with either 00, 1001, or 1003
      # and a normal state_fips--changing the state codes to match the other state
      # attached to that disaster_number
      state_fips = dplyr::case_when(
        disaster_number %in% c(4570, 4590) ~ "22",
        disaster_number %in% c(4587, 4776) ~ "40",
        disaster_number == 4781   ~ "48",
        TRUE ~ state_fips))

  suppressMessages({
    county_populations = get_geography_metadata(geography_type = "county", year = 2023) |>
      dplyr::select(
        county_fips = county_code,
        county_population,
        county_name) })

  public_assistance3a = public_assistance2 |>
    dplyr::mutate(
      county_fips = dplyr::case_when(
        ## aligning now-dissolved counties with their contemporary counterparts,
        ## or with the seemingly-intended county of reference, to the best possible
        county_fips == "02270" ~ "02158",
        county_fips == "02232" ~ "02105",
        county_fips == "02261" ~ "02063",
        county_fips == "46155" ~ "46109",
        county_fips == "22203" ~ "22017",
        county_fips == "46113" ~ "46102",
        county_fips == "46175" ~ "46127",
        county_fips == "44071" ~ "06071",
        county_fips == "51515" ~ "51019",
        county_fips == "55161" ~ "26161",
        county_fips == "02001" ~ "41001",
        county_fips == "35101" ~ "08101",
        county_fips == "38109" ~ "46109",
        county_name == "Jasper County" & disaster_number %in% c(4318,4226) ~ "05101",
        TRUE ~ county_fips),
      county_name = dplyr::case_when(
        stringr::str_detect(county_name, "Wade Hampton") ~ "Kusilvak Census Area",
        stringr::str_detect(county_name, "Valdez-Cordova") ~ "Chugach Census Area",
        stringr::str_detect(county_name, "Traverse") & county_fips == "46109" ~ "Roberts County",
        stringr::str_detect(county_name, "Harrison") & county_fips == "22017" ~ "Caddo Parish",
        county_name == "Bedford (city)" ~ "Bedford County",
        county_name == "Jasper County" & disaster_number %in% c(4318,4226) ~ "Newton County",
        TRUE ~ county_name),
      state_fips = dplyr::case_when(
        county_fips == "26161" ~ "26",
        county_fips == "41001" ~ "41",
        county_fips == "08101" ~ "08",
        county_fips == "46109" ~ "46",
        county_name == "Jasper County" & disaster_number %in% c(4318,4226) ~ "05",
        TRUE ~ state_fips),
      ## for projects with missing county_fips but non-missing states, we assert
      ## these are statewide projects
      imputed_statewide_flag = dplyr::case_when(
        is.na(county_fips) & !is.na(state_fips) & statewide_flag == 0 ~ 1,
        TRUE ~ 0),
      statewide_flag = dplyr::case_when(
        imputed_statewide_flag == 1 ~ 1,
        TRUE ~ statewide_flag),
      ## we don't want a weird pseudo-county identifier; statewide projects should
      ## have no county attributes
      county_fips = dplyr::case_when(
        statewide_flag == 1 ~ NA,
        TRUE ~ county_fips)) |>
    dplyr::select(-county_name) |>
    dplyr::left_join(
      county_populations |> dplyr::select(county_fips, county_population),
      by = "county_fips",
      relationship = "many-to-one")

  imputed_statewide_projects = sum(public_assistance3a$imputed_statewide_flag, na.rm = TRUE)
  dropped_projects = public_assistance3a |>
    dplyr::filter(
      is.na(state_fips)) |>
    dplyr::pull(id)

  if (imputed_statewide_projects > 0) {
    message(stringr::str_c(
      imputed_statewide_projects, " projects have been attributed",
      " as statewide projects due to missing county-level identifiers.")) }
  if (length(dropped_projects) > 0) {
    message(stringr::str_c(
      length(dropped_projects), " projects have been omitted due to missing state-level identifiers.")) }

  public_assistance3b = public_assistance3a |>
    dplyr::filter(!id %in% dropped_projects)

  public_assistance_4a = public_assistance3b |>
    dplyr::select(-county_population, -imputed_statewide_flag) |>
    dplyr::filter(statewide_flag == 0) |>
    dplyr::left_join(
      state_county_xwalk |>
        dplyr::transmute(
          county_fips = county_code,
          county_population),
      by = c("county_fips"))

  public_assistance_4b = public_assistance3b |>
    dplyr::select(-county_population, -county_fips, -imputed_statewide_flag) |>
    dplyr::filter(statewide_flag == 1) |>
    ## we join all counties in the state to projects that are listed as "statewide"
    dplyr::left_join(
      state_county_xwalk |>
        dplyr::transmute(
          state_fips = state_code,
          county_fips = county_code,
          county_population,
          statewide_flag = 1),
      by = c("state_fips", "statewide_flag"),
      relationship = "many-to-many")

  public_assistance_5 = dplyr::bind_rows(
      public_assistance_4a,
      public_assistance_4b) |>
    ## obtain a count of counties per project and then create a denominator for summed
    ## county populations across all the counties per project
    tidytable::mutate(
      .by = "id",
      county_count = dplyr::n_distinct(county_fips, na.rm = TRUE),
      project_counties_population = sum(county_population, na.rm = TRUE)) |>
    ## this is how we attribute project-level costs to  statewide awards
    ## it's 1 for the given county when a project is only in one county
    tidytable::mutate(
      allocation_factor = county_population / project_counties_population) |>
    ## and these are our project costs attributed to the county level, with observations
    ## representing one record per project-county (but potentially multiple records per project)
    tidytable::mutate(
      .by = c("id", "county_fips"),
      tidytable::across(
        .cols = tidytable::all_of(interpolate_columns),
        .fns = ~ .x * allocation_factor, .names = "{.col}_split")) |>
    tibble::as_tibble() |>
    dplyr::select(
      id,
      state_fips,
      county_fips,
      declaration_year,
      disaster_number,
      incident_type,
      project_status,
      dplyr::matches("damage_category"),
      pa_category,
      statewide_flag,
      dplyr::matches("funding"),
      dplyr::matches("amount"))

  ## checking for records that don't align -- there should be none
  test = public_assistance_5 |>
    tidytable::summarize(
      .by = c("state_fips", "id"),
      pa_federal_funding_obligated = tidytable::first(pa_federal_funding_obligated),
      pa_federal_funding_obligated_split = base::sum(pa_federal_funding_obligated_split, na.rm = TRUE)) |>
    tibble::as_tibble()

  ## ensure that we haven't lost any projects
  stopifnot(nrow(test) == nrow(public_assistance3b))

  ## total project costs are the same across both formulations (original and county-level)
  nonaligned_ids = test |>
    dplyr::filter(
      !dplyr::near(
        pa_federal_funding_obligated,
        pa_federal_funding_obligated_split)) |>
    dplyr::mutate(
      difference = abs(pa_federal_funding_obligated - pa_federal_funding_obligated_split)) |>
    dplyr::filter(difference > 1) |>
    dplyr::pull(id)

  stopifnot(length(nonaligned_ids) == 0)

  years = public_assistance_5 |> dplyr::pull(declaration_year)

  public_assistance_6 = public_assistance_5 |>
    dplyr::left_join(
      state_county_xwalk |> dplyr::select(county_fips = county_code, county_name, state_name, state_abbreviation),
      by = "county_fips",
      relationship = "many-to-one") |>
    dplyr::relocate(dplyr::matches("county"), .after = id) |>
    dplyr::relocate(dplyr::matches("state_"), .after = id)

  ## checking row counts
  stopifnot(
    public_assistance_6 |> dplyr::distinct(id) |> nrow() == (
      public_assistance_4a |> nrow() + ## non-statewide projects
      public_assistance_4b |> dplyr::distinct(id) |> nrow()))

  ## there should always be the same number of observations per id for statewide project
  ## ids within the same state
  stopifnot(
    public_assistance_4b |>
    dplyr::count(id, state_fips, sort = TRUE) |>
    tidytable::summarize(
      .by = state_fips,
      distinct_state_records_per_id = dplyr::n_distinct(n)) |>
    tibble::as_tibble() |>
    dplyr::arrange(dplyr::desc(distinct_state_records_per_id)) |>
    dplyr::slice(1) |>
    dplyr::pull(distinct_state_records_per_id) == 1)

  message(glue::glue("Public assistance records are filtered to include only 'natural' `incident_type`
  values, with records from the years {min(years)}-{max(years)}, inclusive. Tribal records are not included."))

  warning(glue::glue("These data have been crosswalked so that estimates can be  aggregated at the county level.
  This is necessary for county-level estimates because many projects are statewide
  projects and do not have county-level observations in the data.

  Analysts thus have two options for working with these data:
  (1) De-select the variables suffixed with `_split` and then run `distinct(df)`.
      This will provide unique observations for projects.
  (2) Group the data at the county level and summarize to produce county-level
      characterizations of PA projects and funding, using the `_split`-suffixed
      variables to calculate funding totals.
  "))

  return(public_assistance_6)
}

utils::globalVariables(c(
  "tract_fips_2020", "tract_fips_2022", "county_fips_2020", "ce_fips_2022", "B01003_001E",
  "population_2020", "population_2020_total", "state_number_code", "dcc", "damage_category",
  "federal_share_obligated", "county_fips_2022", "allocation_factor", "county_population",
  "statewide_flag", "project_counties_population", "pa_federal_funding_obligated",
  "pa_federal_funding_obligated_split", "difference", "declaration_year", "disaster_number",
  "damage_category_code", "damage_category_descrip", "damage_category_description",
  "distinct_state_records_per_id", "imputed_statewide_flag", "pa_category", "project_status"))
