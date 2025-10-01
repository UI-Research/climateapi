#' Standardize county names for matching, to the extent possible
#'
#' @param county
#'
#' @return Cleaned county names more suitable for matching on
#' @noRd
clean_county = function(county) {
  county %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("parish|county|\\.|\\(|\\)|municipality|city and borough|borough|\\|in pmsa.*|'") %>%
    stringr::str_replace_all(c(
      "census area" = "ca",
      "census are" = "ca",
      "^e " = "east ",
      "^w " = "west ",
      "de kalb" = "dekalb",
      "la salle" = "lasalle",
      "la porte" = "laporte",
      "prince of wales-hyder" = "prince of wales-hyder ca",
      "prince of wales-outer" = "prince of wales-outer ca",
      "state wide" = "statewide",
      "ca ca" = "ca")) %>%
    stringr::str_trim() %>%
    stringr::str_squish()
}

#' Get Hazard Mitigation Assistance (HMA) Project Details
#'
#' @param file_path_old_grant_system The file path to raw data for HMA applications from the older grant-reporting system. These data are typically available from: https://www.fema.gov/openfema-data-page/hazard-mitigation-assistance-projects-v4
#' @param file_path_new_grant_system The file path to raw data for HMA applications from the newer (FEMA GO) grant-reporting system. These data are typically available from: https://www.fema.gov/openfema-data-page/hma-subapplications-v2
#' @param state_abbreviations NULL by default, in which case data are returned for all 51 states. Provide a vector of two-character USPS state abbreviations to obtain data for a sub-selection of states.
#'
#' @return A dataframe of project-county HMA application data, aggregated across both old and new grant reporting systems.
#' @export
#'
#' @examples
#' \dontrun{
#' get_hazard_mitigation_assistance()
#' }
get_hazard_mitigation_assistance = function(
  file_path_old_grant_system = file.path(
    get_box_path(), "hazards", "fema", "hazard-mitigation-assistance", "raw",
    "HazardMitigationAssistanceProjects_2025_09_27.parquet"),
  file_path_new_grant_system = file.path(
    get_box_path(), "hazards", "fema", "hazard-mitigation-assistance", "raw",
    "HmaSubapplications_2025_09_27.parquet"),
  state_abbreviations = NULL) {

  if (is.null(state_abbreviations)) { state_abbreviations = c(state.abb, "DC") }
  if (any(!state_abbreviations %in% c(state.abb, "DC"))) { stop("Only the 50 states and DC are supported at this time.") }
  if (!file.exists(file_path_old_grant_system)) stop("Error: there is no file at the specified `file_path_old_grant_system`.")
  if (!file.exists(file_path_new_grant_system)) stop("Error: there is no file at the specified `file_path_new_grant_system`.")

  state_fips = get_geography_metadata("state") %>%
    dplyr::filter(state_abbreviation %in% state_abbreviations) %>%
    dplyr::pull(state_code)

  ## data on counties and their populations
  suppressMessages({
    state_county_xwalk = get_geography_metadata(geography_type = "county") %>%
      dplyr::filter(state_code %in% state_fips) %>%
      dplyr::mutate(county_clean = clean_county(county_name)) })

  hma_projects00 = arrow::read_parquet(file_path_old_grant_system) %>%
    janitor::clean_names()

  hma_projects0 = hma_projects00 %>%
    dplyr::select(
      project_id = project_identifier,
      project_program_area = program_area,
      status,
      ## this has no missingness, so we prefer it over date-stamped data
      project_fiscal_year = program_fy,
      state_name = state,
      state_code = state_number_code,
      county_name = county,
      project_counties,
      disaster_number,
      initial_obligation_amount,
      project_application_cost = project_amount,
      federal_share = cost_share_percentage,
      #project_federal_share_obligated = federal_share_obligated,
      project_number_of_properties_mitigated_final = number_of_final_properties) %>%
    ## only the specified states
    ## only approved projects
    dplyr::filter(state_code %in% !!state_fips) %>%
    dplyr::mutate(
      ## for some records, the project_application_cost is zero but there is an
      ## initial_obligation_amount. for these cases, we use the initial_obligation_amount
      ## but do not multiply it by the federal_share (because this already reflects
      ## only federal dollars)
      project_cost_federal = dplyr::if_else(
        project_application_cost > 0,
        project_application_cost * federal_share,
        initial_obligation_amount),
      state_code = as.character(stringr::str_pad(state_code, side = "left", pad = "0", width = 2)),
      ## standardizing county names
      project_counties = project_counties %>%
        stringr::str_replace_all(c(";" =  ",")) %>%
        stringr::str_trim() %>%
        stringr::str_squish() %>%
        stringr::str_to_lower(),
      project_counties = dplyr::case_when(
        project_id == "DR-1008-4031-R" ~ "los angeles",
        project_id == "DR-1008-4020-R" ~ "ventura, orange, los angeles",
        project_id %in% c("DR-1119-0012-R", "DR-1119-0013-R") ~ "matanuska-susitna borough",
        project_id == "DR-1603-0434-R" ~ "statewide, west feliciana",
        project_id == "DR-1009-0003-R" ~ "desoto",
        project_id == "DR-1018-0001-R" ~ NA_character_,
        project_id == "DR-0961-0013-R" ~ "honolulu",
        project_id == "DR-0928-0017-R" ~ "statewide",
        project_id == "DR-0864-0019-R" ~ "honolulu",
        project_id == "PDMC-PJ-10-AKIT003-2018-001" ~ "prince of wales-outer ca ketchikan ca",
        project_id == "DR-1249-0063-R" ~ NA_character_,
        project_id == "DR-1059-0001-R" ~ "buena vista city",
        TRUE ~ project_counties %>% tolower),
      project_counties_multiple_flag = dplyr::if_else(
        stringr::str_detect(project_counties, ","), 1, 0),
      row_id = dplyr::row_number())

  ## we lengthen records for projects with multiple counties
  hma_projects0a = hma_projects0 %>%
    dplyr::filter(project_counties_multiple_flag == 1) %>%
    tidyr::separate_longer_delim(project_counties, delim = ",")

  hma_projects0b = dplyr::bind_rows(
    hma_projects0 %>%
      dplyr::filter(is.na(project_counties_multiple_flag) | project_counties_multiple_flag == 0),
    hma_projects0a) %>%
    dplyr::select(-c(project_application_cost, federal_share, initial_obligation_amount)) %>%
    ## convert comma-separated values into new rows
    tidyr::separate_longer_delim(project_counties, delim = ",") %>%
    dplyr::mutate(
      ## more county name standardization
      project_counties = project_counties %>%
        clean_county %>%
        stringr::str_replace_all(c(
          "census a$|census are$" = "ca",
          "unincorporated|various|several|see comments" = NA_character_)) %>%
        stringr::str_to_lower(),
      project_counties = dplyr::case_when(
        stringr::str_detect(project_counties, "prince of wales-outer") ~ "prince of wales-outer ca ketchikan ca",
        stringr::str_detect(project_counties, "mat su") ~ "matanuska-susitna",
        stringr::str_detect(project_counties, "wrangell-petersburg census are") ~ "wrangell-petersburg ca",
        stringr::str_detect(project_counties, "kusilvak") ~ "kusilvak ca",
        stringr::str_detect(project_counties, "ammany") ~ "st tammany",
        !is.na(county_name) & is.na(project_counties) ~ county_name,
        ## when no county-level information, we split the values across the entire state
        ## based on population at the county level
        is.na(project_counties) ~ "statewide",
        TRUE ~ project_counties %>% stringr::str_to_lower()),
      project_counties = project_counties %>% stringr::str_to_lower(),
      project_counties = dplyr::case_when(
        stringr::str_detect(project_counties, "ammany") ~ "st tammany",
        project_counties %in% c("miami - dade", "dade") & state_name == "Florida" ~ "miami-dade",
        TRUE ~ project_counties))

  hma_projects1a = hma_projects0b %>%
    ## join county codes and populations
    ## we have ~15 records for which there's no county information--for these, we attribute
    ## them across all counties in the state
    dplyr::left_join(
      state_county_xwalk %>% dplyr::select(state_name, county_code, county_population, county_clean),
      by = c("state_name", "project_counties" = "county_clean"))

  ## projects without county-matches--we treat these as statewide projects
  no_county_match_projects = hma_projects1a %>%
    dplyr::filter(
      is.na(county_population),
      county_name != "Statewide") %>%
    nrow()

  message(stringr::str_c(
    no_county_match_projects, " projects do not have valid county identifiers and are treated as if they were statewide projects."))

  hma_projects1b = hma_projects1a %>%
    dplyr::mutate(
      project_counties = dplyr::if_else(is.na(county_population), "statewide", project_counties)) %>%
    dplyr::select(-county_name)

  ## all statewide projects match:
  stopifnot(
    hma_projects1b %>%
      dplyr::filter(project_counties == "statewide") %>%
      dplyr::anti_join(
        state_county_xwalk %>%
          dplyr::transmute(
            state_code,
            county_code,
            county_population,
            statewide = "statewide"),
        by = c("state_code", "project_counties" = "statewide")) %>%
      nrow() == 0)

  hma_projects2 = hma_projects1b %>%
    ## we join all counties in the state to projects that are listed as "statewide"
    ## or where county_geoid is missing
    dplyr::left_join(
      state_county_xwalk %>%
        dplyr::transmute(
          state_code,
          county_geoid = county_code,
          county_population,
          statewide = "statewide"),
      by = c("state_code", "project_counties" = "statewide"),
      relationship = "many-to-many") %>%
    ## consolidate duplicated county code and county population variables
    dplyr::mutate(
      county_geoid = dplyr::if_else(is.na(county_code), county_geoid, county_code),
      county_population = dplyr::if_else(
        is.na(county_population.x),
        county_population.y, county_population.x)) %>%
    dplyr::select(-c(county_code, county_population.x, county_population.y)) %>%
    ## obtain a count of counties per project and then create a denominator for summed
    ## county populations across all the counties per project
    tidytable::mutate(
      .by = c("project_id", "row_id"),
      county_count = dplyr::n_distinct(county_geoid, na.rm = TRUE),
      project_counties_population = sum(county_population, na.rm = TRUE)) %>%
    ## this is how we attribute project-level costs to counties for projects that
    ## are in more than one county (including statewide projects)
    ## it's 1 for the given county when a project is only in one county
    dplyr::mutate(
      allocation_factor = county_population / project_counties_population) %>%
    ## and these are our project costs attributed to the county level, with observations
    ## representing one record per project-county
    tidytable::mutate(
      .by = c("project_id", "county_geoid", "row_id"),
      project_cost_federal_split = project_cost_federal * allocation_factor)

  ## the project-county-level costs should be the same as the original project-level costs
  stopifnot(
    hma_projects2 %>%
      dplyr::select(project_id, state_name, county_geoid, project_cost_federal_split, project_cost_federal) %>%
      tidytable::summarize(
        .by = c("project_id", "state_name"),
        project_cost_federal = dplyr::first(project_cost_federal),
        project_cost_federal_split = sum(project_cost_federal_split, na.rm = TRUE)) %>%
      dplyr::mutate(difference = project_cost_federal - project_cost_federal_split) %>%
      dplyr::filter(!(abs(difference) < 1)) %>%
      nrow == 0)

  ## same number of distinct projects
  stopifnot(
    hma_projects1b %>%
      dplyr::summarize(dplyr::n_distinct(project_id)) %>% nrow() ==
      hma_projects2 %>%
      dplyr::summarize(dplyr::n_distinct(project_id)) %>% nrow())

  ####FEMA GO APPLICATIONS####
  ## FEMA GO Applications
  femago_df1 = arrow::read_parquet(file.path(file_path_new_grant_system)) %>%
    janitor::clean_names() %>%
    dplyr::select(
      program,
      state_abbreviation = subapplicant_state_abbreviation,
      counties = benefiting_counties,
      state_name = subapplicant_state,
      fiscal_year,
      selection_status,
      ## the amount approved during the selection phase -- this appears to be the
      ## most accurate reflection of intended federal funding
      selection_federal_share_amount,
      disaster_number,
      project_id = id) %>%
    dplyr::filter(state_abbreviation %in% !!state_abbreviations)

  femago_df2 = femago_df1  %>%
    ## convert comma-separate values into new rows
    tidyr::separate_longer_delim(counties, delim = ",") %>%
    dplyr::mutate(
      program = dplyr::case_when(
        stringr::str_detect(program, "Building Resilient") ~ "BRIC",
        ## note this includes 150 FMA "Swift Current" records in 2024
        stringr::str_detect(program, "Flood Mitigation") ~ "FMA",
        ## note this includes one HMGP "Post Fire" record in 2024
        stringr::str_detect(program, "Hazard Mitigation") ~ "HMGP",
        TRUE ~ "ERROR"),
      counties = clean_county(counties))

  interpolate_columns = c("selection_federal_share_amount")

  ## Connecticut counties are dated -- we need to crosswalk them to current county-
  ## equivalents (planning regions)
  if ("CT" %in% state_abbreviations) {
    ct_county_crosswalk1 = readr::read_csv(
      file.path(get_box_path(), "crosswalks", "ctdata_2022_tractcrosswalk_connecticut.csv")) %>%
      janitor::clean_names() %>%
      dplyr::select(
        ## each tract has a single observation
        tract_fips_2020,
        ## each tract has a single observation
        tract_fips_2022,
        county_name,
        county_fips_2020,
        county_fips_2022 = ce_fips_2022)

    suppressMessages({
      ct_tract_populations = tidycensus::get_acs(
        year = 2021,
        geography = "tract",
        state = "CT",
        variable = "B01003_001",
        output = "wide") %>%
        dplyr::select(
          tract_fips_2020 = GEOID,
          population_2020 = B01003_001E) })

    ct_county_crosswalk = ct_county_crosswalk1 %>%
      dplyr::left_join(ct_tract_populations, by = "tract_fips_2020") %>%
      tidytable::summarize(
        .by = c("county_fips_2020", "county_name", "county_fips_2022"),
        population_2020 = sum(population_2020)) %>%
      tidytable::mutate(
        .by = "county_fips_2020",
        population_2020_total = sum(population_2020, na.rm = TRUE)) %>%
      tidytable::mutate(
        allocation_factor = population_2020 / population_2020_total,
        county_name = county_name %>% stringr::str_to_lower()) %>%
      tidytable::select(-dplyr::matches("population"))

    crosswalked_ct_counties = femago_df2 %>%
      dplyr::filter(state_abbreviation == "CT") %>%
      dplyr::left_join(
        ct_county_crosswalk,
        by = c("counties" = "county_name"),
        relationship = "many-to-many") %>%
      tidytable::summarize(
        .by = c("project_id"),
        dplyr::across(
          .cols = dplyr::all_of(interpolate_columns),
          .fns = ~ sum(.x * allocation_factor, na.rm = TRUE)),
        dplyr::across(
          .cols = -dplyr::all_of(interpolate_columns),
          .fns = ~ dplyr::first(.x))) %>%
      dplyr::select(-county_fips_2020, -counties) %>%
      dplyr::rename(county_code_ct = county_fips_2022) %>%
      dplyr::left_join(
        tidycensus::fips_codes %>%
          dplyr::transmute(
            county_code_ct = stringr::str_c(state_code, county_code),
            counties = county %>% stringr::str_to_lower() %>% stringr::str_c(" planning region")),
        by = "county_code_ct") %>%
      dplyr::select(-county_code_ct)

    #joining the non CT states with the CT crosswalk to create the updated dataset
    femago_df3 = femago_df2 %>%
      dplyr::filter(state_abbreviation != "CT") %>%
      dplyr::bind_rows(crosswalked_ct_counties) %>%
      dplyr::select(-allocation_factor) } else {

    femago_df3 = femago_df2 }

  femago_df4 = femago_df3 %>%
    ## join county codes and populations
    dplyr::left_join(
      state_county_xwalk %>% dplyr::select(state_name, county_code, county_population, county_clean),
      by = c("state_name", "counties" = "county_clean"))

  projects_with_missing_county_identifiers = femago_df4 %>%
    dplyr::filter(is.na(county_code)) %>%
    dplyr::pull(project_id) %>% unique()

  warning(stringr::str_c(length(projects_with_missing_county_identifiers), " projects have been omitted due to missing county-level identifiers."))

  femago_df5 = femago_df4 %>%
    dplyr::filter(! project_id %in% projects_with_missing_county_identifiers) %>%
    ## obtain a count of counties per project and then create a denominator for summed
    ## county populations across all the counties per project
    tidytable::mutate(
      .by = "project_id",
      county_count = dplyr::n_distinct(county_code, na.rm = TRUE),
      project_counties_population = sum(county_population, na.rm = TRUE)) %>%
    ## this is how we attribute project-level costs to counties for projects that
    ## are in more than one county;
    ## it's 1 for the given county when a project is only in one county
    dplyr::mutate(
      allocation_factor = county_population / project_counties_population) %>%
    ## and these are our project costs attributed to the county level, with observations
    ## representing one record per project-county
    dplyr::mutate(
      .by = c("project_id", "county_code"),
      selection_federal_share_amount_split = selection_federal_share_amount * allocation_factor) %>%
    dplyr::rename(county_geoid = county_code)

  ## ensure the same number of projects
  stopifnot(
    nrow(femago_df1) == nrow(femago_df2 %>% dplyr::distinct(project_id)))

  all_hma_projects = dplyr::bind_rows(
    hma_projects2 %>%
      dplyr::transmute(
        data_source = "hma-projects",
        project_id,
        disaster_number = as.character(disaster_number),
        project_program_area,
        project_fiscal_year,
        state_name,
        county_geoid,
        county_population,
        project_status = status,
        project_cost_federal,
        project_cost_federal_split),
    femago_df5 %>%
      dplyr::transmute(
        data_source = "hma-subapplications",
        project_id,
        disaster_number = as.character(disaster_number),
        project_program_area = program,
        project_fiscal_year = fiscal_year,
        state_name,
        county_geoid,
        county_population,
        project_status = selection_status,
        project_cost_federal = selection_federal_share_amount,
        project_cost_federal_split = selection_federal_share_amount_split))

  warning(
    "These data are at the project x county level, with one row per project-county.
Only the `project_cost_federal_split` variable should be used (for purposes of
aggregating costs to the county level).")

  return(all_hma_projects)
}

utils::globalVariables(c(
  "benefiting_counties", "cost_share_percentage", "counties", "county_clean", "county_code_ct",
  "county_population.x", "county_population.y", "federal_share", "fiscal_year", "hazard",
  "number_of_final_properties", "observation_date", "program", "program_area", "program_fy",
  "project_amount", "project_application_cost", "project_cost_federal", "project_cost_federal_split",
  "project_counties", "hma_project00", "initial_obligation_amount", "project_counties_multiple_flag",
  "project_fiscal_year", "project_id", "project_identifier", "project_program_area",
  "selection_federal_share_amount", "selection_federal_share_amount_split", "status",
  "subapplicant_state", "subapplicant_state_abbreviation"))
