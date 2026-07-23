#' Convert unzipped CHAS raw data folders into combined parquet files
#'
#' Reads the per-table CHAS CSVs for each five-year period found under `box_path`,
#' joins them into a single wide table per period (keyed on `geoid` and `year`), and
#' writes one `.parquet` file per period back to `box_path`. This is a one-time data
#' preparation utility used to build the on-disk mirror that [get_chas_housing_affordability()] reads from;
#' it is not called by `get_chas_housing_affordability()` itself.
#'
#' @param box_path The directory where unzipped raw CHAS data folders are stored.
#' @param years One or more years (numeric) from the period 2009-2021. Each year is the
#'   last year of the corresponding five-year ACS period.
#'
#' @return Nothing. Results are written to `box_path`.
#' @noRd
write_chas_acs_tract = function(
    box_path = file.path(
      climateapi::get_box_path(), "built-environment", "hud",
      "comprehensive-housing-affordability-strategies"),
    years = 2009:2021) {

  year_ranges = years |> purrr::map_chr(~ stringr::str_c(.x - 4, "thru", .x))

  purrr::walk(
    year_ranges,
    function(year_range) {
      box_path |>
        list.files(recursive = TRUE, full.names = TRUE) |>
        purrr::keep(~ stringr::str_detect(.x, stringr::str_c(year_range, ".*csv$"))) |>
        purrr::map(
          function(path) {
            readr::read_csv(path, show_col_types = FALSE) |>
              dplyr::mutate(
                year = source |>
                  stringr::str_extract("thru[0-9]{4}") |>
                  stringr::str_remove("thru")) |>
              dplyr::select(-dplyr::any_of(c("sumlevel", "name", "source", "st", "cnty", "tract"))) }) |>
        purrr::reduce(
          dplyr::left_join,
          by = c("geoid", "year"),
          relationship = "one-to-one") |>
        arrow::write_parquet(
          sink = file.path(box_path, stringr::str_c(year_range, ".parquet"))) })
}

#' Obtain CHAS data from the HUD API
#'
#' @param end_year Any year from 2009-2021. Refers to the last year of the five-year ACS period.
#' @param geography One of c("nation", "state", "county", "mcd", "place").
#' @param state_code A two-digit state FIPS code. Required for all geographies except
#'   "nation"; `NULL` otherwise.
#' @param entity_code A FIPS identifier for the specified sub-state geography. Required
#'   for "county", "mcd", and "place"; `NULL` otherwise.
#'
#' @return A tibble with the CHAS results (using the API's own column names).
#' @noRd
get_chas_api = function(end_year, geography, state_code = NULL, entity_code = NULL) {

  warning("This function has not been fully QCed.")
  ## converting geography names to type codes accepted by the API
  api_types = tibble::tribble(
    ~code, ~geography,
    1L, "nation",
    2L, "state",
    3L, "county",
    4L, "mcd",
    5L, "place")

  type = api_types |>
    dplyr::filter(geography == !!geography) |>
    dplyr::pull(code)

  if (length(type) == 0) {
    stop("`geography` must be one of nation, state, county, mcd, or place for API queries.") }

  ## county/mcd/place require a sub-state entity; any geography but nation requires a state
  if (type %in% 3:5 && is.null(entity_code)) {
    stop("An `entity_code` is required when `geography` is 'county', 'mcd', or 'place'.") }
  if (type >= 2 && is.null(state_code)) {
    stop("A `state_code` is required when `geography` is not 'nation'.") }
  if (!is.null(state_code) && as.numeric(state_code) > 56) {
    stop("`state_code` must be a state FIPS code less than 57.") }

  end_year = as.numeric(end_year)

  params = list(
    type = type,
    year = stringr::str_c(end_year - 4, "-", end_year),
    stateId = if (!is.null(state_code)) as.numeric(state_code) else NULL,
    entityId = if (!is.null(entity_code)) as.numeric(entity_code) else NULL) |>
    purrr::compact()

  chas_response = httr2::request(base_url = "https://www.huduser.gov/hudapi/public/chas") |>
    httr2::req_auth_bearer_token(get_hud_api_key()) |>
    httr2::req_url_query(!!!params) |>
    ## surface HUD's error body (e.g. an invalid-key message) rather than a bare status code
    httr2::req_error(
      body = function(response) tryCatch(httr2::resp_body_string(response), error = function(e) NULL)) |>
    httr2::req_perform()

  chas_body = chas_response |> httr2::resp_body_json()

  if (length(chas_body) == 0 || length(chas_body[[1]]) == 0) {
    warning("The HUD API returned no CHAS records for the requested parameters.")
    return(tibble::tibble()) }

  chas_body[[1]] |> tibble::as_tibble()
}

#' Obtain housing affordability data from HUD's CHAS
#'
#' @description Retrieves HUD Comprehensive Housing Affordability Strategy (CHAS) data,
#'   which cross-tabulate ACS housing-need measures (cost burden, overcrowding, and
#'   related housing problems) by tenure, household income relative to HUD Area Median
#'   Family Income (HAMFI), race/ethnicity, and household type. Data are drawn either
#'   from HUD's CHAS API (for "nation", "state", "county", "mcd", and "place"
#'   geographies) or from an on-disk parquet mirror on Box (for "tract"). Where a data
#'   dictionary is available, the source's terse column codes are expanded into
#'   descriptive snake_case names (see Details).
#'
#' @param geography The geographic summary level. One of "nation", "state", "county",
#'   "mcd" (minor civil division), "place", or "tract". All levels except "tract" are
#'   served by the API (`api = TRUE`); "tract" is read from disk (`api = FALSE`).
#' @param end_year The last year of the five-year ACS period, from 2009 to 2022.
#'   Defaults to 2022 (the most recent period).
#' @param state_code A two-digit state FIPS code. Required by the API for every
#'   geography except "nation"; ignored for the disk (tract) path.
#' @param entity_code A FIPS identifier for the requested sub-state geography (county,
#'   MCD, or place). Required by the API for those geographies; ignored for the disk
#'   (tract) path.
#' @param api Logical. If `TRUE`, query the HUD API (requires a registered key; see
#'   [register_hud_api_key()]). If `FALSE` (the default), read from the on-disk parquet
#'   mirror -- currently only "tract" is available this way.
#' @param directory_path The directory containing the on-disk CHAS parquet files and the
#'   data dictionary. Defaults to the CHAS folder under the C&C Box path. Used for the
#'   disk (tract) path and for locating the dictionary used to rename columns.
#' @param columns An optional character vector of raw column names to read from the
#'   parquet file (disk path only). Names refer to the source's original column codes,
#'   i.e. before the codebook renaming described in Details. If `NULL` (the default),
#'   all columns are read.
#'
#' @details CHAS releases cover five-year ACS periods; `end_year` is the last year of
#'   that period (e.g. `end_year = 2021` is the 2017-2021 release).
#'
#'   For 2009-2012, the source publishes counts for census tract *parts* rather than
#'   whole tracts; these are summed up to the tract level (keyed on a `tract_geoid`
#'   derived from the source geoid) before being returned.
#'
#'   Column renaming uses the CHAS data dictionary found under `directory_path` (an
#'   `.xlsx` file whose name contains "dictionary", matched to `end_year`). The
#'   dictionary's hierarchical descriptions are collapsed into a single descriptive
#'   snake_case name per column and applied to any matching columns; columns without a
#'   dictionary match (including margin-of-error columns, which the dictionary does not
#'   describe the same way) keep their original names. If no dictionary is found (for
#'   example, when querying the API without the Box mirror synced), the data are
#'   returned with the source's original column names.
#'
#' @return A tibble of CHAS results, one row per geography. Estimate columns whose codes
#'   appear in the data dictionary are renamed to descriptive snake_case names of the
#'   form `table_<n>_<description>_estimate_<k>`; all other columns (identifiers such as
#'   `geoid`/`tract_geoid`, the `year` label, and margin-of-error columns) retain their
#'   original names. API results are returned with HUD's own column names unless they
#'   happen to match dictionary codes.
#' @export
#'
#' @examples
#' \dontrun{
#' ## tract-level data for the 2017-2021 period, read from the on-disk Box mirror
#' get_chas_housing_affordability(geography = "tract", end_year = 2021)
#'
#' ## county-level data from the HUD API (requires a registered key)
#' register_hud_api_key("your-hud-api-key")
#' get_chas_housing_affordability(geography = "county", end_year = 2021, state_code = "01", entity_code = "001", api = TRUE)
#' }
get_chas_housing_affordability = function(
    geography,
    end_year = 2022,
    state_code = NULL,
    entity_code = NULL,
    api = FALSE,
    directory_path = file.path(
      climateapi::get_box_path(), "built-environment", "hud",
      "comprehensive-housing-affordability-strategies"),
    columns = NULL) {

  warning("This function has not been fully QCed.")

  api_geographies = c("nation", "state", "county", "mcd", "place")
  valid_geographies = c(api_geographies, "tract")

  if (length(geography) != 1 || !geography %in% valid_geographies) {
    stop(stringr::str_c(
      '`geography` must be one of ', stringr::str_c('"', valid_geographies, '"', collapse = ", "), ".")) }

  if (!is.numeric(end_year) || length(end_year) != 1) {
    stop("`end_year` must be a single numeric year (the last year of a five-year ACS period).") }
  if (!end_year %in% 2009:2021) {
    warning("`end_year` is outside the documented 2009-2021 range; the requested data may be unavailable.") }

  api = isTRUE(api)

  if (api) {
    if (geography == "tract") {
      stop('`geography = "tract"` is not available via the API; use `api = FALSE` to read it from disk.') }
    df1 = get_chas_api(
      end_year = end_year,
      geography = geography,
      state_code = state_code,
      entity_code = entity_code)
  } else {
    if (geography %in% api_geographies) {
      stop(stringr::str_c(
        '`geography = "', geography, '"` is currently only available via the API; set `api = TRUE`.')) }

    file_path = file.path(
      directory_path, stringr::str_c(end_year - 4, "thru", end_year, ".parquet"))
    if (!file.exists(file_path)) {
      stop(stringr::str_c("No CHAS parquet file found at: ", file_path)) }

    df1 = if (is.null(columns)) {
      arrow::read_parquet(file = file_path)
    } else {
      arrow::read_parquet(file = file_path, col_select = dplyr::any_of(columns)) }

    ## for these years, data are published for tract parts, which are summed up to the
    ## tract level (the source geoid embeds a summary-level prefix ahead of the tract id)
    if (end_year %in% 2009:2012) {
      df1 = df1 |>
        tidytable::mutate(tract_geoid = stringr::str_sub(geoid, 8, 19)) |>
        tidytable::summarize(
          .by = c(tract_geoid, year),
          tidytable::across(-c(geoid, tract_geoid, year), .fns = ~ sum(.x, na.rm = TRUE))) |>
        tibble::as_tibble() }
  }

  ## ---- standardize column names using the on-disk data dictionary -------------------
  ## locate the dictionary for this period; if none is found (e.g. API use without the
  ## Box mirror), return the data with its original column names rather than erroring
  codebook_paths = directory_path |>
    list.dirs(full.names = TRUE) |>
    purrr::keep(~ stringr::str_detect(.x, stringr::str_c("thru", end_year))) |>
    list.files(full.names = TRUE) |>
    purrr::keep(~ stringr::str_detect(.x, "dictionary"))

  if (length(codebook_paths) == 0) {
    message(
      "No CHAS data dictionary found under `directory_path`; returning data with the ",
      "source's original column names.")
    return(df1) }
  if (length(codebook_paths) > 1) {
    message(
      "Multiple candidate CHAS data dictionaries found; using the first: ",
      basename(codebook_paths[[1]])) }

  ## read the dictionary and reformat each hierarchical variable definition into a single
  ## descriptive snake_case name usable as a column name
  codebook = readxl::read_excel(codebook_paths[[1]], sheet = 2, guess_max = 5000) |>
    janitor::clean_names() |>
    dplyr::filter(stringr::str_detect(file_name, "Table")) |>
    tidyr::unite(
      column_name_new,
      description_1, description_2, description_3, description_4, description_5,
      sep = "_", na.rm = TRUE, remove = TRUE) |>
    dplyr::mutate(
      column_name_new = column_name_new |>
        stringr::str_to_lower() |>
        stringr::str_squish() |>
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
        stringr::str_replace(stringr::str_extract(column_name, "est.*"), "est", "_estimate_"))) |>
    dplyr::select(column_name_old = column_name, column_name_new) |>
    dplyr::filter(!is.na(column_name_new), column_name_new != "") |>
    dplyr::distinct(column_name_old, column_name_new)

  ## a dictionary label that maps to more than one raw column would make the rename
  ## ambiguous (and dplyr::rename() error), so leave any such columns with their raw names
  ambiguous_labels = codebook |>
    dplyr::filter(duplicated(column_name_new) | duplicated(column_name_new, fromLast = TRUE))

  if (nrow(ambiguous_labels) > 0) {
    message(
      dplyr::n_distinct(ambiguous_labels$column_name_new),
      " CHAS dictionary label(s) map to multiple columns and were left with their ",
      "original names to avoid ambiguity.") }

  rename_lookup = codebook |>
    dplyr::anti_join(ambiguous_labels, by = "column_name_new")

  rename_vector = rlang::set_names(rename_lookup$column_name_old, rename_lookup$column_name_new)

  df2 = df1 |> dplyr::rename(dplyr::any_of(rename_vector))

  return(df2)
}

utils::globalVariables(c(
  "source", "code", "geoid", "year", "tract_geoid", "file_name",
  "description_1", "description_2", "description_3", "description_4", "description_5",
  "column_name", "column_name_new", "column_name_old"))
