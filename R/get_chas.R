#' Register a HUD API key
#' This utility facilitates setup for accessing the HUD API for, e.g., querying CHAS data
#'
#' @param key
#'
#' @return Nothing. A key will be saved to your user-level .Renviron.
#' @export
#'
#' @examples register_hud_api_key("HUD_API_KEY_HERE")

register_hud_api_key = function(key) {
  Sys.setenv(HUD_API_KEY = key)
  readRenviron("~/.Renviron")
}

#' Convert unzipped CHAS raw data folders into combined parquet files
#'
#' @param box_path The directory where unzipped raw CHAS data folders are stored.
#' @param years One or more years (numeric) from the period 2009-2021. The years correspond to the last year of the five-year ACS period.
#'
#' @return Nothing. Results are written to `box_path`.
#' @internal
write_chas_acs_tract = function(
    box_path = file.path(climateapi::get_box_path(), "built-environment", "hud", "comprehensive-housing-affordability-strategies"),
    years = c(2009:2021)) {

  years = years %>% map_chr(~ str_c(.x - 4, "thru", .x))

  walk(
    years,
    function(year) {
      box_path %>%
        list.files(recursive = TRUE, full.names = TRUE) %>%
        purrr::keep(~str_detect(.x, str_c(year, ".*csv$"))) %>%
        map(
          function(path) {
            df = read_csv(path) %>%
              mutate(year = source %>% stringr::str_extract("thru[0-9]{4}") %>% stringr::str_remove("thru")) %>%
              select(-any_of(c("sumlevel", "name", "source", "st", "cnty", "tract"))) }) %>%
        reduce(
          left_join,
          by = c("geoid", "year")) %>%
        arrow::write_parquet(sink = file.path(box_path, str_c(year, ".parquet"))) })
}

#' Obtain CHAS data from the HUD API
#'
#' @param geography One of c("nation", "state", "county", "mcd", "place").
#' @param end_year Any year from 2009-2021. Refers to the last year of the five-year ACS period.
#' @param state_code A two-digit state FIPS code.
#' @param entity_code A FIPS identifier for the specified sub-state geography.
#'
#' @return A dataframe with the CHAS results.
#' @internal
get_chas_api = function(end_year, geography, state_code, entity_code) {

  ## converting geography names to type codes accepted by the API
  type = tibble::tribble(
    ~code, ~geography,
    1, "nation",
    2, "state",
    3, "county",
    4, "mcd",
    5, "place") %>%
    dplyr::filter(geography == !!geography) %>%
    dplyr::pull(code)

  ## prepping API parameters
  end_year = as.numeric(end_year)
  start_year = end_year - 4
  year = stringr::str_c(start_year, "-", end_year)
  entityId = as.numeric(entity_code)
  stateId = as.numeric(state_code)

  if (!is.null(state_code) & as.numeric(state_code) > 56) {
    stop("`state_code` must less than 57.") }

  if (type %in% c(3:5)) {
    if (is.null(entityId)) {
      stop("An `entity_code` is required for this `geography`.") }}

  params = list(
    type = type,
    year = year,
    stateId = stateId,
    entityId = entityId) %>%
    purrr::compact()

  key = Sys.getenv("HUD_API_KEY")

  if (key == "") {
    stop(str_c(
      "No key found. Ensure you've stored your API key in your .Renviron.",
      "Register for a key via HUD's website and store the key using `register_hud_api_key()`")) }

  base_url = "https://www.huduser.gov/hudapi/public/chas"

  chas_request = httr2::request(base_url = base_url) |>
    httr2::req_auth_bearer_token(key) |>
    httr2::req_url_query(!!!params)

  chas_result = chas_request |>
    httr2::req_perform()

  chas_body = chas_result |>
    httr2::resp_body_json()

  chas_tibble = chas_body[[1]] |> tibble::as_tibble()
}

#' Obtain housing affordability data from CHAS
#' If geography does not equal "tract", query the data from the API or from data stored
#' on disk. If the geography equals "tract", read from disk.
#'
#' @param geography One of c("nation", "state", "county", "mcd", "place", "tract").
#' @param end_year Any year from 2009-2021. Refers to the last year of the five-year ACS period.
#' @param state_code A two-digit state FIPS code.
#' @param entity_code A FIPS identifier for the specified sub-state geography.
#'
#' @return A dataframe of CHAS results.
#' @export
#'
#' @examples
get_chas = function(
    geography, end_year = 2009, state_code = NULL, entity_code = NULL, api = NULL,
    directory_path = file.path(climateapi::get_box_path(), "built-environment", "hud", "comprehensive-housing-affordability-strategies"),
    columns = NULL) {
#
#   end_year = 2016
#   state_code = 1
#   geography = "county"
#   entity_code = 1
#   api = TRUE
#   directory_path = file.path(climateapi::get_box_path(), "built-environment", "hud", "comprehensive-housing-affordability-strategies")
#   columns = NULL
#
  api_geographies = c("nation", "state", "county", "mcd", "place")

  if (!geography %in% c(api_geographies, "tract")) {
    stop('`geography` must be one of c("nation", "state", "county", "mcd", "place", "tract").') }

  if (api == TRUE) {
    df1 = get_chas_api(
      end_year = end_year,
      geography = geography,
      state_code = state_code,
      entity_code = entity_code)
  }

  if (is.null(api) | api == FALSE) {
    if (geography %in% api_geographies) {
      stop('This geography is currently only supported via the API; set API = TRUE.') }

    file_path = file.path(directory_path, stringr::str_c(end_year - 4, "thru", end_year, ".parquet"))
    if (is.null(columns)) {
      df1 = arrow::read_parquet(file = file_path) } else {
        df1 = arrow::read_parquet(file = file_path, col_select = dplyr::any_of(columns)) }

    ## for these years, data are returned for tract parts, which need to be aggregated up
    ## to the tract level
    if (end_year %in% c(2009:2012)) {
      df1 = df1 %>%
        tidytable::mutate(
          tract_geoid = str_sub(geoid, 8, 19)) %>%
        tidytable::summarize(
          .by = tract_geoid,
          across(-c(geoid, year), .fns = ~ sum(.x, na.rm = TRUE))) }
  }

  ## standard workflow across results from API / disk
  # obtain the codebook path (from disk)
  codebook_path = directory_path %>%
    list.dirs(full.names = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, stringr::str_c("thru", end_year))) %>%
    list.files(full.names = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "dictionary"))

  ## read in the codebook, select relevant columns, and reformat variable definitions
  ## so they can be used as variable names
  codebook1 = readxl::read_excel(codebook_path, sheet = 2, guess_max = 5000) %>%
    janitor::clean_names()

  codebooke2 = codebook1 %>%
    dplyr::filter(stringr::str_detect(file_name, "Table")) %>%
    tidyr::unite(
      column_name_new,
      description_1, description_2, description_3, description_4, description_5,
      sep = "_", na.rm = TRUE, remove = TRUE) %>%
    dplyr::mutate(
      column_name_new = column_name_new %>%
        stringr::str_to_lower() %>%
        stringr::str_squish() %>%
        stringr::str_replace_all(c(
          " " = "_",
          ":|\\/|,|-|\\(|\\)|%" = "",
          "_in_|_with_|_and_|_is_|_to_|or_|_the_|_per_|_but_|_has_|_of_" = "_",
          "raceethnicity" = "race",
          "has_1_more_4_housing_unit_problems" = "1ormore_housing_problems",
          "household_income" = "income",
          "africanamerican" = "",
          "american_indian_alaska_native" = "aian",
          "greater_than" = "_gt",
          "less_than_equal" = "_lte",
          "with_either" = "",
          "\\+" = "plus",
          "__" = "_")),
      column_name_new = stringr::str_c(
        "table_",
        stringr::str_extract(column_name, "[0-9]{1,2}_"),
        column_name_new,
        stringr::str_replace(stringr::str_extract(column_name, "est.*"), "est", "_estimate_")),
      length  = nchar(column_name_new)) %>%
    dplyr::select(
      column_name_old = column_name,
      column_name_new)

  df2a = df1 %>%
    dplyr::filter()
    rename()
}



