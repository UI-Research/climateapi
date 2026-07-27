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
#'   i.e. before the codebook renaming described in Details. `geoid` and `year` are always
#'   read, whether or not they are listed. If `NULL` (the default), all columns are read.
#'
#' @details CHAS releases cover five-year ACS periods; `end_year` is the last year of
#'   that period (e.g. `end_year = 2021` is the 2017-2021 release).
#'
#'   The source writes every identifier with its summary level in front, separated by
#'   `"US"` -- `"1400000US01001020100"` for a tract in recent releases, `"14000US..."` in
#'   releases through 2015-2019, and `"08000US..."` in the tract-part files. That prefix
#'   is removed, so `geoid` is the plain eleven-digit tract identifier used by
#'   [tigris::tracts()] and by `tidycensus`, and joins to them without further work.
#'
#'   For 2009-2012, the source publishes counts for census tract *parts* rather than
#'   whole tracts. Each part identifier is state, county, county subdivision, place, and
#'   tract, so the tract is taken from the state, county, and final six digits, and the
#'   parts are summed to whole-tract counts. These years return the same `geoid` column as
#'   every other year (earlier versions of this function returned a separate `tract_geoid`
#'   column that was not in fact a tract identifier).
#'
#'   Column renaming uses the CHAS data dictionary found under `directory_path`, read via
#'   [get_chas_codebook()]. The dictionary's hierarchical descriptions are collapsed into
#'   a single descriptive snake_case name per column and applied to any matching columns;
#'   columns without a dictionary match (including margin-of-error columns, which the
#'   dictionary does not describe separately) keep their original names. If no dictionary
#'   is found (for example, when querying the API without the Box mirror synced), the data
#'   are returned with the source's original column names. If a dictionary is found for a
#'   different period than the one requested, [get_chas_codebook()] warns and the
#'   substituted period is recorded in the codebook's `vintage` column.
#'
#'   That codebook is attached to the returned data as an attribute named
#'   `"chas_codebook"`, so the definition of any column can be looked up without reading
#'   the dictionary from disk again:
#'
#'   ```
#'   chas = get_chas_housing_affordability(geography = "tract", end_year = 2021)
#'   codebook = attr(chas, "chas_codebook")
#'   ```
#'
#'   The attribute holds the whole codebook for the period, including variables not
#'   present in the returned columns. Most data-manipulation functions drop attributes
#'   they do not recognize, so save the codebook to its own object before reshaping,
#'   joining, or filtering the data.
#'
#'   Descriptive names are unique within a CHAS table but repeat across tables, since each
#'   table has its own "owner occupied" total and the like. A name is therefore treated as
#'   ambiguous only when it describes more than one of the columns actually being
#'   returned; those columns keep their original names and a message reports how many. In
#'   practice this means requesting a single table always renames cleanly, while
#'   requesting the whole file leaves a small number of top-level totals unrenamed. Use
#'   [get_chas_codebook()] to look up what any column means.
#'
#' @return A tibble of CHAS results, one row per geography. Estimate columns whose codes
#'   appear in the data dictionary are renamed to descriptive snake_case names built from
#'   the variable's definition, for example
#'   `owner_occupied_income_lte_30_hamfi_cost_burden_gt_50`. See [get_chas_codebook()] for
#'   the abbreviations these names use (`lte`, `hh`, `ppr`, and so on). All other columns
#'   (the `geoid` identifier, the `year` label, margin-of-error columns, and any column
#'   whose descriptive name is ambiguous among those returned) retain their original
#'   names. API results are returned with HUD's own column names unless they happen to
#'   match dictionary codes.
#'
#'   The result carries a `"chas_codebook"` attribute holding the tibble returned by
#'   [get_chas_codebook()] for the period used -- one row per CHAS estimate variable,
#'   giving its descriptive name, source code, table, and definition. When no dictionary
#'   is available the attribute is a zero-row tibble of that same shape.
#' @seealso [get_chas_codebook()] for the descriptive name and definition of every CHAS
#'   variable.
#' @export
#'
#' @examples
#' \dontrun{
#' ## tract-level data for the 2017-2021 period, read from the on-disk Box mirror
#' chas = get_chas_housing_affordability(geography = "tract", end_year = 2021)
#'
#' ## the definitions of the columns returned above
#' codebook = attr(chas, "chas_codebook")
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

    ## `geoid` and `year` identify the rows, so they are read whether or not they were
    ## asked for; without them the counts could not be attributed to a place or a period
    df_read = if (is.null(columns)) {
      arrow::read_parquet(file = file_path)
    } else {
      arrow::read_parquet(
        file = file_path,
        col_select = dplyr::any_of(unique(c("geoid", "year", columns)))) }

    ## the source writes each identifier with its summary level in front, separated by
    ## "US": "1400000US" for tracts from the 2016-2020 release on, "14000US" before that,
    ## and "08000US" for the tract-part files. Dropping it leaves the identifier itself.
    df_stripped = df_read |>
      dplyr::mutate(geoid = stringr::str_remove(geoid, "^[0-9]+US"))

    df1 = if (end_year %in% 2009:2012) {
      ## these releases publish counts for census tract *parts* rather than whole tracts.
      ## Each identifier is state (2 digits), county (3), county subdivision (5), place
      ## (5), and tract (6), so the tract is the state and county followed by the last six
      ## digits. The parts are then summed to give whole-tract counts.
      df_stripped |>
        tidytable::mutate(
          geoid = stringr::str_c(
            stringr::str_sub(geoid, 1, 5), stringr::str_sub(geoid, 16, 21))) |>
        tidytable::summarize(
          .by = c(geoid, year),
          tidytable::across(-c(geoid, year), .fns = ~ sum(.x, na.rm = TRUE))) |>
        tibble::as_tibble()
    } else {
      df_stripped }
  }

  ## ---- standardize column names using the on-disk data dictionary -------------------
  ## if no dictionary is available (for example, when the Box mirror is not synced),
  ## return the data with its original column names rather than erroring
  codebook = get_chas_codebook(end_year = end_year, directory_path = directory_path)

  if (nrow(codebook) == 0) {
    message("Returning data with the source's original column names.")
    attr(df1, "chas_codebook") = codebook
    return(df1) }

  ## descriptive names are unique within a CHAS table but repeat across tables (each has
  ## its own "owner occupied" total, for instance), so a name is only genuinely ambiguous
  ## if it maps to more than one of the columns actually being returned. Restricting the
  ## check to those columns means a request for a single table always renames cleanly.
  rename_lookup = codebook |>
    dplyr::filter(column_name_source %in% names(df1))

  ambiguous_labels = rename_lookup |>
    dplyr::filter(duplicated(column_name) | duplicated(column_name, fromLast = TRUE))

  if (nrow(ambiguous_labels) > 0) {
    message(
      dplyr::n_distinct(ambiguous_labels$column_name), " descriptive name(s) describe more ",
      "than one of the requested columns (the same total appears in several CHAS tables); ",
      "those ", nrow(ambiguous_labels), " columns keep their original names.") }

  rename_lookup = rename_lookup |>
    dplyr::anti_join(ambiguous_labels, by = "column_name")

  rename_vector = rlang::set_names(rename_lookup$column_name_source, rename_lookup$column_name)

  df2 = df1 |> dplyr::rename(dplyr::any_of(rename_vector))

  ## the codebook travels with the data so that callers can look up what a column means
  ## without re-reading the dictionary from disk. Attributes are dropped by many
  ## data-manipulation functions, so save it separately before reshaping the result.
  attr(df2, "chas_codebook") = codebook

  return(df2)
}

utils::globalVariables(c(
  "source", "code", "geoid", "year", "tract_geoid", "column_name", "column_name_source"))
