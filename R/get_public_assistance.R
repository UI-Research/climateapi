#' Get FEMA Public Assistance (PA) funding
#'
#' Project- and county-level data on PA funding over time
#'
#' @param state_abbreviations A character vector of state abbreviations. NULL by default, which returns records for all 51 states.
#' @param file_path The file path to the raw data file, preferably a .parquet file.
#'
#' @return A dataframe of project-level funding requests and awards, along with variables that can be aggregated to the county level.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_public_assistance(state_abbreviations = "NJ")
#' }

get_public_assistance= function(
    file_path = file.path(
      get_box_path(), "hazards", "fema", "public-assistance", "raw",
      "public_assistance_funded_projects_county_2024_10_25.parquet"),
    state_abbreviations = NULL) {

  if (is.null(state_abbreviations)) { state_abbreviations = c(state.abb, "DC") }
  state_fips = get_geography_metadata("state") %>%
    dplyr::filter(state_abbreviation %in% state_abbreviations) %>%
    dplyr::pull(state_code)

  ## Connecticut counties are dated -- we need to crosswalk them to current county-
  ## equivalents (planning regions)
  ct_county_crosswalk1 = readr::read_csv(file.path(get_box_path(), "crosswalks", "ctdata_2022_tractcrosswalk_connecticut.csv")) %>%
    janitor::clean_names() %>%
    dplyr::select(
      ## each tract has a single observation
      tract_fips_2020,
      ## each tract has a single observation
      tract_fips_2022,
      county_fips_2020,
      county_fips_2022 = ce_fips_2022)

  ct_tract_populations = tidycensus::get_acs(
      year = 2021,
      geography = "tract",
      state = "CT",
      variable = "B01003_001",
      output = "wide") %>%
    dplyr::select(
      tract_fips_2020 = GEOID,
      population_2020 = B01003_001E)

  ct_county_crosswalk = ct_county_crosswalk1 %>%
    dplyr::left_join(ct_tract_populations) %>%
    tidytable::summarize(
      .by = c("county_fips_2020", "county_fips_2022"),
      population_2020 = sum(population_2020)) %>%
    tidytable::mutate(
      .by = "county_fips_2020",
      population_2020_total = sum(population_2020, na.rm = TRUE)) %>%
    tidytable::mutate(allocation_factor = population_2020 / population_2020_total) %>%
    tidytable::select(-dplyr::matches("population"))

  ## data on counties and their populations
  state_county_xwalk <- get_geography_metadata(geography_type = "county") %>%
    dplyr::mutate(GEOID = stringr::str_c(state_code, county_code)) %>%
    dplyr::left_join(
      tidycensus::get_acs(
        year = 2023,
        output = "wide",
        variables = "B01003_001",
        geography = "county") %>%
        dplyr::select(GEOID, county_population = B01003_001E),
      by = c("GEOID")) %>%
    dplyr::filter(state_code %in% state_fips)

  public_assistance_raw <- arrow::read_parquet(file_path) |>
    janitor::clean_names() |>
    dplyr::transmute(
      id = dplyr::row_number(),
      state_fips = stringr::str_pad(state_number_code, side = "left", width = 2, pad = "0"),
      county_name = county,
      county_fips = stringr::str_c(state_fips, stringr::str_pad(as.character(county_code), side = "left", width = 3, pad = "0")),
      disaster_number,
      declaration_year = lubridate::year(declaration_date),
      incident_type,
      damage_category_code = dcc,
      damage_category_description = damage_category,
      pa_category = stringr::str_c(dcc, damage_category, sep = " - "),
      pa_federal_funding_obligated = federal_share_obligated) %>%
    dplyr::filter(state_fips %in% state_fips, as.numeric(state_fips) < 60)

  interpolate_columns = c("pa_federal_funding_obligated")

  if ("09" %in% state_fips) {
    crosswalked_ct_counties = public_assistance_raw %>%
      dplyr::filter(state_fips == "09") %>%
      dplyr::left_join(ct_county_crosswalk, by = c("county_fips" = "county_fips_2020"), relationship = "many-to-many") %>%
      tidytable::summarize(
        .by = c("id"),
        dplyr::across(
          .cols = dplyr::all_of(interpolate_columns),
          .fns = ~ sum(.x * allocation_factor, na.rm = TRUE)),
        dplyr::across(
          .cols = -dplyr::all_of(interpolate_columns),
          .fns = ~ dplyr::first(.x))) %>%
      dplyr::select(-county_fips) %>%
      dplyr::rename(county_fips = county_fips_2022)

    #joining the non CT states with the CT crosswalk to create the updated dataset
    public_assistance1 = public_assistance_raw %>%
      dplyr::filter(state_fips != "09") %>%
      dplyr::bind_rows(crosswalked_ct_counties) %>%
      dplyr::select(-allocation_factor)
  } else {
    public_assistance1 = public_assistance_raw
  }

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
        TRUE ~ 0),
      #In these cases, the disaster number was associated with either 00, 1001, or 1003
      #and a normal state_fips. In these cases, I changed the state codes to match the
      #other state attached to that disaster_number
      state_fips = dplyr::case_when(
        disaster_number %in% c(4570, 4590) ~ "22",
        disaster_number %in% c(4587, 4776) ~ "40",
        disaster_number == 4781   ~ "48",
        TRUE ~ state_fips))

  suppressMessages({
    county_populations = purrr::map_dfr(
      state_fips,
      ~ tidycensus::get_acs(
        year = 2023,
        geography = "county",
        state = .x,
        variable = "B01003_001",
        output = "wide")) %>%
      dplyr::select(
        county_fips = GEOID,
        county_population = B01003_001E,
        NAME) })

  public_assistance3a = public_assistance2 %>%
    dplyr::mutate(
      county_fips = dplyr::case_when(
        ## these aren't perfect but they align now-dissolved counties with their contemporary counterparts
        ## or with the seemingly-intended county of reference
        county_fips == "02270" ~ "02158",
        county_fips == "02232" ~ "02105",
        county_fips == "02261" ~ "02063",
        county_fips == "46155" ~ "46109",
        county_fips == "22203" ~ "22017",
        county_fips == "46113" ~ "46102",
        county_fips == "46175" ~ "46127",
        county_fips == "44071" ~ "06071",
        county_fips == "51515" ~ "51019",
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
        county_name == "Jasper County" & disaster_number %in% c(4318,4226) ~ "05",
        TRUE ~ state_fips)) %>%
    dplyr::left_join(county_populations, by = "county_fips")

  public_assistance3b = public_assistance3a %>%
    dplyr::filter(
      is.na(county_population),
      state_fips %in% !!state_fips,
      county_name != "Statewide",
      !stringr::str_detect(county_name, "REAA"))

  public_assistance3c = public_assistance3a %>%
    dplyr::filter(!is.na(county_population))

  if(nrow(public_assistance3b) > 0) {
    warning(stringr::str_c(
    nrow(public_assistance3b), " records have been dropped because they lack accurate information ",
    "about their locations."))
  }

  public_assistance_4a = public_assistance3c %>%
    dplyr::select(-county_population) %>%
    dplyr::filter(statewide_flag == 0) %>%
    dplyr::left_join(
      state_county_xwalk %>%
        dplyr::transmute(
          county_fips = county_geoid,
          county_population),
      by = c("county_fips"))

  public_assistance_4b = public_assistance3c %>%
    dplyr::select(-county_population, -county_fips) %>%
    dplyr::filter(statewide_flag == 1) %>%
    ## we join all counties in the state to projects that are listed as "statewide"
    dplyr::left_join(
      state_county_xwalk %>%
        dplyr::transmute(
          state_fips = state_code,
          county_fips = county_geoid,
          county_population,
          statewide_flag = 1),
      by = c("state_fips", "statewide_flag"),
      relationship = "many-to-many")

  public_assistance_5 = dplyr::bind_rows(
    public_assistance_4a,
    public_assistance_4b) %>%
    ## obtain a count of counties per project and then create a denominator for summed
    ## county populations across all the counties per project
    tidytable::mutate(
      .by = "id",
      county_count = dplyr::n_distinct(county_fips, na.rm = TRUE),
      project_counties_population = sum(county_population, na.rm = TRUE)) %>%
    ## this is how we attribute project-level costs to  statewide awards
    ## it's 1 for the given county when a project is only in one county
    dplyr::mutate(
      allocation_factor = county_population / project_counties_population) %>%
    ## and these are our project costs attributed to the county level, with observations
    ## representing one record per project-county
    tidytable::mutate(
      .by = c("id", "county_fips"),
      dplyr::across(dplyr::matches("funding"), ~ .x * allocation_factor, .names = "{.col}_split"))

  ## checking for records that don't align -- there should be none
  test = public_assistance_5 %>%
    tidytable::summarize(
      .by = c("state_fips", "id"),
      pa_federal_funding_obligated = dplyr::first(pa_federal_funding_obligated),
      pa_federal_funding_obligated_split = sum(pa_federal_funding_obligated_split, na.rm = TRUE))

  ## ensure that we haven't lost any projects
  stopifnot(nrow(test) == nrow(public_assistance3c))

  ## total project costs are the same across both formulations (original and
  ## county-level)
  nonaligned_ids = test %>%
    dplyr::filter(
      !dplyr::near(
        pa_federal_funding_obligated,
        pa_federal_funding_obligated_split)) %>%
    dplyr::mutate(
      difference = abs(pa_federal_funding_obligated - pa_federal_funding_obligated_split)) %>%
    dplyr::filter(difference > 1) %>%
    dplyr::pull(id)

  stopifnot(length(nonaligned_ids) == 0)

  years = public_assistance_5 %>% dplyr::pull(declaration_year)

  message(glue::glue("Public assistance records are filtered to include only 'natural' incident_type
  values, with records from the years {min(years)}-{max(years)}, inclusive. Tribal records are not included."))

  warning("These data are at the project x county level, with one row per project-county.
  Only the `project_amount_total_no_administrative_costs_split` variable should
          be used (for purposes of aggregating costs to the county level).")

  return(public_assistance_5)
}

utils::globalVariables(c(
  "tract_fips_2020", "tract_fips_2022", "county_fips_2020", "ce_fips_2022", "B01003_001E",
  "population_2020", "population_2020_total", "state_number_code", "dcc", "damage_category",
  "federal_share_obligated", "county_fips_2022", "allocation_factor", "county_population",
  "statewide_flag", "project_counties_population", "pa_federal_funding_obligated",
  "pa_federal_funding_obligated_split", "difference", "declaration_year", "disaster_number"))
