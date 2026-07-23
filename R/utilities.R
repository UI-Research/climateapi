#' @title Get the user's username
#'
#' @return A character string containing the system username. Uses `Sys.info()["user"]`
#'   which works reliably across Windows, Mac, and Linux.
#' @export
#'
#' @examples
#' \dontrun{
#' get_system_username()
#' }
get_system_username = function() {
  username <- Sys.info()[["user"]]
  if (is.null(username) || username == "") {
    stop("Could not determine system username from Sys.info()")
  }

  username
}

#' @title Register a HUD API key
#'
#' @description Stores a HUD API key so that [get_chas_housing_affordability()] can authenticate against
#'   HUD's CHAS API. By default the key is set for the current R session only; set
#'   `install = TRUE` to also persist it to your user-level `.Renviron` so that it is
#'   available in future sessions.
#'
#' @param key A HUD API key: a single, non-empty character string. Obtain one by
#'   registering at <https://www.huduser.gov/portal/dataset/fmr-api.html>.
#' @param install Logical. If `TRUE`, write the key to your user-level `.Renviron`
#'   (replacing any existing `HUD_API_KEY` entry) so it persists across sessions.
#'   Defaults to `FALSE`, which sets the key for the current session only.
#'
#' @return Invisibly returns `key`. Called for the side effect of setting the
#'   `HUD_API_KEY` environment variable (and, when `install = TRUE`, writing it to
#'   `.Renviron`).
#' @export
#'
#' @examples
#' \dontrun{
#' register_hud_api_key("your-hud-api-key")
#' register_hud_api_key("your-hud-api-key", install = TRUE)
#' }
register_hud_api_key = function(key, install = FALSE) {

  if (!is.character(key) || length(key) != 1 || is.na(key) || key == "") {
    stop("`key` must be a single, non-empty character string.") }

  Sys.setenv(HUD_API_KEY = key)

  if (isTRUE(install)) {
    renviron_path = path.expand(file.path("~", ".Renviron"))
    existing_lines = if (file.exists(renviron_path)) readLines(renviron_path) else character(0)
    ## drop any prior HUD_API_KEY entry so repeated calls don't accumulate duplicates
    retained_lines = existing_lines %>%
      purrr::discard(~ stringr::str_detect(.x, "^HUD_API_KEY="))
    writeLines(c(retained_lines, stringr::str_c("HUD_API_KEY=", key)), renviron_path)
    readRenviron(renviron_path)
    message("HUD API key written to ", renviron_path, ".") }

  invisible(key)
}

#' @title Retrieve the stored HUD API key
#'
#' @description Returns the HUD API key stored in the `HUD_API_KEY` environment
#'   variable, erroring with setup guidance if none is found. Used internally by
#'   [get_chas_housing_affordability()]'s API path.
#'
#' @return The HUD API key, as a character string.
#' @export
#'
#' @examples
#' \dontrun{
#' get_hud_api_key()
#' }
get_hud_api_key = function() {
  key = Sys.getenv("HUD_API_KEY")
  if (key == "") {
    stop(stringr::str_c(
      "No HUD API key found. Register for one at ",
      "https://www.huduser.gov/portal/dataset/fmr-api.html and store it with ",
      "`register_hud_api_key()`.")) }
  key
}

#' Query an OpenFEMA v2 dataset filtered to a single quoted field value
#'
#' Bypasses a bug in `rfema:::gen_api_query()`, which coerces all-digit filter values
#' (e.g. `countyCode`) to unquoted numbers -- stripping leading zeros and producing a
#' 400 Bad Request, since FEMA's API schema requires fields like `countyCode` as a
#' quoted string. Constructs the OData query directly instead, and paginates by
#' incrementing `$skip` until a page returns fewer than the page size, rather than via
#' `$inlinecount=allpages` (which `rfema::open_fema()` uses internally to pre-compute an
#' exact total record count) -- that exact count times out on FEMA's largest datasets
#' (verified: reliably produces a 503 on FimaNfipPolicies, an ~80M-row table).
#'
#' @param data_set_endpoint The full OpenFEMA v2 API base URL for the dataset (e.g.
#'   "https://www.fema.gov/api/open/v2/FimaNfipClaims").
#' @param field_name The OData field name to filter on (e.g. "countyCode").
#' @param field_value The (string-typed) value to filter to; always quoted in the request.
#'
#' @return A tibble of all matching records, paginated as needed. Zero-row tibble if
#'   no records match.
#' @noRd
query_openfema_quoted_filter = function(data_set_endpoint, field_name, field_value) {

  page_size = 1000

  base_query = stringr::str_c(
      data_set_endpoint, "?$top=", page_size, "&$filter=(",
      field_name, " eq '", field_value, "')") |>
    stringr::str_replace_all(" ", "%20")

  fetch_page = function(skip) {
    response = httr::GET(stringr::str_c(base_query, "&$skip=", format(skip, scientific = FALSE)))
    if (response$status_code != 200) {
      stop(httr::http_status(response)$message) }

    json_data = httr::content(response)[[2]]
    if (length(json_data) == 0) { return(NULL) }

    max_list_length = max(sapply(json_data, length))
    json_data = lapply(json_data, function(x) c(x, rep(NA, max_list_length - length(x))))
    page_df = data.frame(do.call(rbind, json_data))
    ## each column is still list-typed at this point (one scalar per cell, wrapped in a
    ## length-1 list); as.character() (called implicitly by gsub()) unwraps these to plain
    ## atomic character vectors, matching what rfema::open_fema() does internally
    page_df = as.data.frame(lapply(page_df, function(x) gsub("\n", "", x)))
    tibble::as_tibble(page_df) }

  pages = list()
  skip = 0
  repeat {
    page = fetch_page(skip)
    if (is.null(page)) { break }
    pages[[length(pages) + 1]] = page
    if (nrow(page) < page_size) { break }
    skip = skip + page_size }

  if (length(pages) == 0) { return(tibble::tibble()) }
  dplyr::bind_rows(pages)
}

#' Locate the root of the user's Box folder
#'
#' Shared OS-detection logic used by both `get_box_path()` (the C&C practice
#' area subfolder) and `get_openfema_cache_path()` (the `data-cache` subfolder
#' at the Box root).
#'
#' @return A character string containing the full path to the root of the user's Box folder.
#' @noRd
locate_box_root = function() {
  username <- get_system_username()
  os <- Sys.info()[["sysname"]]

  if (os == "Windows") {
    box_root <- file.path("C:", "Users", username, "Box")
  } else if (os == "Darwin") {
    # Mac: Check common Box locations
    box_locations <- c(
      file.path("/Users", username, "Box"),
      file.path("/Users", username, "Library", "CloudStorage", "Box-Box")
    )
    box_root <- NULL
    for (loc in box_locations) {
      if (dir.exists(loc)) {
        box_root <- loc
        break
      }
    }
    if (is.null(box_root)) {
      stop(
        "Could not find Box folder. Checked:\n",
        paste("  -", box_locations, collapse = "\n")
      )
    }
  } else {
    stop("Unsupported operating system: ", os, ". Only Windows and Mac are supported.")
  }

  box_root
}

#' @title Get the path to the C&C Box folder
#'
#' @return A character string containing the full file path to the Climate and Communities (C&C) Box folder.
#'   On Windows, returns "C:/Users/\{username\}/Box/METRO Climate and Communities Practice Area/github-repository".
#'   On Mac, checks for Box at "/Users/\{username\}/Box" or "/Users/\{username\}/Library/CloudStorage/Box-Box",
#'   using whichever exists. Throws an error if the Box folder cannot be found.
#' @export
#'
#' @examples
#' \dontrun{
#' get_box_path()
#' }
get_box_path = function() {
  box_path <- file.path(
    locate_box_root(), "METRO Climate and Communities Practice Area", "github-repository")

  if (!dir.exists(box_path)) {
    warning("Box path does not exist: ", box_path)
  }

  box_path
}

#' @title Get the path to the local OpenFEMA dataset cache
#'
#' @return A character string containing the full path to the local cache of
#'   OpenFEMA datasets, populated via `download_openfema_datasets()`. This
#'   cache lives at the root of the user's Box folder (`data-cache/openfema`),
#'   not under the C&C practice area subfolder returned by `get_box_path()`.
#' @export
#'
#' @examples
#' \dontrun{
#' get_openfema_cache_path()
#' }
get_openfema_cache_path = function() {
  file.path(locate_box_root(), "data-cache", "openfema")
}

#' Locate the most recently cached OpenFEMA dataset file
#'
#' Package `get_*` functions use this to default to the freshest local mirror
#' of a given OpenFEMA dataset instead of a hardcoded, inevitably stale filename.
#'
#' @param dataset_name The OpenFEMA API endpoint name (e.g. "DisasterDeclarationsSummaries"),
#'   matching the file-naming convention produced by `download_openfema_datasets()`
#'   (`<dataset_name>_YYYY_MM_DD.parquet`).
#' @param cache_path Directory to search. Defaults to `get_openfema_cache_path()`.
#'
#' @return The full path to the most recently dated cached file for `dataset_name`.
#' @noRd
find_openfema_cache_file = function(dataset_name, cache_path = get_openfema_cache_path()) {

  if (!dir.exists(cache_path)) {
    stop(stringr::str_c(
      "No local OpenFEMA cache found at: ", cache_path, ". Use `download_openfema_datasets()` ",
      "to populate it, or supply an explicit file path.")) }

  pattern = stringr::str_c("^", dataset_name, "_\\d{4}_\\d{2}_\\d{2}\\.parquet$")
  cached_files = list.files(cache_path, pattern = pattern, full.names = TRUE)

  if (length(cached_files) == 0) {
    stop(stringr::str_c(
      "No cached file found for '", dataset_name, "' in: ", cache_path, ". Use ",
      "`download_openfema_datasets()` to populate it, or supply an explicit file path.")) }

  file_dates = cached_files |>
    basename() |>
    stringr::str_extract("\\d{4}_\\d{2}_\\d{2}") |>
    stringr::str_replace_all("_", "-") |>
    as.Date()

  most_recent_file = cached_files[which.max(file_dates)]
  message("Reading cached OpenFEMA file: ", basename(most_recent_file))
  most_recent_file
}


#' Get the raw column names for a specified dataset
#'
#' @param dataset The name of the dataset. One of c('nfip_policies', 'nfip_claims', 'ihp_registrations').
#'
#' @return A character vector containing the raw column names (in camelCase format as they appear in the source data) to be selected when reading the specified dataset. The columns returned are curated subsets of the full dataset columns, excluding administrative/metadata fields. For "nfip_policies": 11 columns matching the current per-state parquet schema. For "nfip_claims": 19 columns needed by `get_nfip_claims()`'s downstream `transmute()`. For "ihp_registrations": ~20 columns including disaster info, geographic identifiers, and assistance amounts.
get_dataset_columns = function(dataset) {

  if (length(dataset) > 1 | !is.character(dataset)) {
    stop("The `dataset` argument must be a character of length one.") }

  if (! dataset %in% c('nfip_policies', 'nfip_claims', 'ihp_registrations')) {
    stop("The `dataset` argument must be one of c('nfip_policies', 'nfip_claims', 'ihp_registrations').") }

  if (dataset == "nfip_policies") {
    columns = c(
      "id",
      "countyCode",
      "censusTract",
      "policyCost",
      "policyCount",
      "ratedFloodZone",
      "totalInsurancePremiumOfThePolicy",
      "policyTerminationDate",
      "policyEffectiveDate",
      "occupancyType",
      "buildingReplacementCost") }

  if (dataset == "nfip_claims") {
    columns = c(
      "countyCode",
      "censusTract",
      "occupancyType",
      "yearOfLoss",
      "originalConstructionDate",
      "policyCount",
      "buildingDeductibleCode",
      "contentsDeductibleCode",
      "buildingPropertyValue",
      "contentsPropertyValue",
      "buildingReplacementCost",
      "contentsReplacementCost",
      "totalBuildingInsuranceCoverage",
      "totalContentsInsuranceCoverage",
      "buildingDamageAmount",
      "contentsDamageAmount",
      "netBuildingPaymentAmount",
      "netContentsPaymentAmount",
      "netIccPaymentAmount") }

  if (dataset == "ihp_registrations") {
    columns = c(
      "censusGeoid",
      # "incidentType",
      "disasterNumber",
      # "declarationDate",
      #"county",
      "damagedStateAbbreviation",
      #"damagedCity",
      "damagedZipCode",
      "householdComposition",
      "grossIncome",
      "ownRent",
      "residenceType",
      # "homeOwnersInsurance",
      # "floodInsurance",
      "ihpAmount",
      "fipAmount",
      "haAmount",
      "onaAmount",
      "homeDamage",
      "autoDamage",
      # "emergencyNeeds",
      # "foodNeed",
      # "shelterNeed",
      # "accessFunctionalNeeds",
      # "sbaApproved",
      # "rpfvl",
      # "ppfvl",
      "destroyed",
      "rentalAssistanceAmount",
      "repairAmount",
      "replacementAmount",
      "personalPropertyAmount"
      # "ihpMax",
      # "haMax",
      # "onaMax",
      # "lastRefresh",
      # "id"
      ) }

  return(columns)
}

#' @title Convert raw data to parquet to conserve memory / speed subsequent operations
#'
#' @param inpath The local path to read CSV data from.
#' @param outpath The local path to write parquet data to.
#' @param delimit_character The delimiting character of the raw data.
#' @param subsetted_columns The columns to include in the outputted parquet data.
#' @param dataset NULL by default. Alternately, one of c("nfip_policies", "nfip_claims", "ihp_registrations"). If not null, this will be used to select the columns that are returned.
#'
#' @return NULL (invisibly). This function is called for its side effect of writing a parquet file to disk at the specified `outpath` (or a path derived from `inpath` with a .parquet extension). The function reads the input file in chunks to handle large files efficiently, optionally subsets to specified columns, and writes the result in Apache Parquet format using `arrow::write_parquet()`.
convert_delimited_to_parquet = function(
    inpath,
    outpath = NULL,
    delimit_character = ",",
    subsetted_columns = NULL,
    dataset = NULL) {

  ## write to the same location (but as .parquet)
  if (is.null(outpath)) {
    outpath = inpath |> stringr::str_replace("\\..*$", "\\.parquet") }

  if (file.exists(outpath)) {
    stop("A file already exists at the specified `outpath`.") }

  ## a quick test prior to reading in full file
  raw_txt_test_delimit_character = tryCatch(
    { readr::read_delim(inpath, delim = delimit_character, n_max = 5) },
    error = function(e) { stop("Error reading inpath file. Did you provide the correct `delimit_character` value for the input file-type?") })

  ## assign default columns to retain
  if (dataset == "ihp_registrations") {
    subsetted_columns = get_dataset_columns("ihp_registrations") }
  if (dataset == "nfip_policies") {
    subsetted_columns = get_dataset_columns("nfip_policies") }
  if (dataset == "nfip_claims") {
    subsetted_columns = get_dataset_columns("nfip_claims") }
  if (is.null(dataset)) {
    subsetted_columns = colnames(raw_txt_test_delimit_character) }
  if (!(dataset %in% c("ihp_registrations", "nfip_policies", "nfip_claims"))) {
    stop("The `dataset` argument must be one of c('ihp_registrations', 'nfip_policies', 'nfip_claims').") }

  ## a quick test prior to reading in full file
  raw_txt_test_subsetted_columns = tryCatch(
    { readr::read_delim(inpath, delim = delimit_character, col_select = dplyr::all_of(subsetted_columns), n_max = 5) },
    error = function(e) { stop("Error reading inpath file. The subsetted columns may not be present in the inpath file.") })

  ## a callback function to read the full file in chunks
  read_chunk_callback = function(x, cols) { x |> dplyr::select(dplyr::all_of(subsetted_columns)) }

  ## reading the file in chunks
  raw_text_subsetted = tryCatch(
    { readr::read_delim_chunked(inpath, delim = delimit_character, callback = readr::DataFrameCallback$new(read_chunk_callback), chunk_size = 1000000) },
    error = function(e) { stop(e) })

  arrow::write_parquet(raw_text_subsetted, sink = outpath)
}

#' @title Get the Census geographies that overlap with the input spatial dataset
#'
#' @param data An sf-formatted dataframe
#' @param return_geometry Logical. Include the geometries of returned geographies?
#' @param projection The EPSG code of the desired projection. Default is 5070 (Albers Equal Area).
#'
#' @return A tibble (or `sf` object if `return_geometry = TRUE`) containing Census geographies that overlap with the input spatial data. The structure depends on the geographic extent:
#' \describe{
#'   \item{When multiple states overlap}{Returns state-level data with columns: `state_geoid` (2-digit FIPS), `geography` ("state").}
#'   \item{When a single state overlaps}{Returns tract-level data with columns: `state_geoid` (2-digit FIPS), `county_geoid` (5-digit FIPS), `geography` ("tract").}
#' }
#' If `return_geometry = TRUE`, the geometry column is retained; otherwise it is dropped.
#' @export
get_spatial_extent_census = function(data, return_geometry = FALSE, projection = 5070) {

  data1 = data |> sf::st_transform(projection)
  data_union = data1 |> sf::st_union()

  ## computes the % of each candidate geography's own area that overlaps `data_union`,
  ## retaining only geographies exceeding a 5% overlap threshold--this avoids keeping
  ## geographies that only barely touch/border the input boundary
  filter_by_overlap = function(candidates, threshold = 0.05) {
    candidates |>
      dplyr::mutate(geography_area = sf::st_area(geometry)) |>
      sf::st_intersection(data_union) |>
      dplyr::mutate(overlap_pct = as.numeric(sf::st_area(geometry) / geography_area)) |>
      sf::st_drop_geometry() |>
      dplyr::filter(overlap_pct > threshold) |>
      dplyr::pull(GEOID) }

  all_counties = tigris::counties(cb = TRUE, year = 2023, progress_bar = FALSE) |>
    sf::st_transform(projection)

  overlapping_county_geoids = all_counties |> filter_by_overlap()

  if (length(overlapping_county_geoids) == 0) {
    stop("No county overlaps `data` by more than 5% of the county's area.") }

  state_geoids = all_counties |>
    sf::st_drop_geometry() |>
    dplyr::filter(GEOID %in% overlapping_county_geoids) |>
    dplyr::pull(STATEFP) |>
    unique()

  if (length(state_geoids) > 1) {

    states_sf = tigris::states(
        cb = TRUE,
        resolution = "20m",
        year = 2023,
        progress_bar = FALSE,
        refresh = TRUE) |>
      sf::st_transform(projection)

    result = states_sf |>
      dplyr::filter(GEOID %in% state_geoids) |>
      dplyr::transmute(
        state_geoid = GEOID,
        geography = "state") } else {

    all_tracts = state_geoids |>
      purrr::map_dfr(
        ~ tigris::tracts(
          state = .x,
          county = NULL,
          cb = TRUE,
          year = 2023,
          progress_bar = FALSE,
          refresh = TRUE)) |>
      sf::st_transform(projection)

    overlapping_tract_geoids = all_tracts |> filter_by_overlap()

    result = all_tracts |>
      dplyr::filter(GEOID %in% overlapping_tract_geoids) |>
      dplyr::transmute(
        state_geoid = stringr::str_sub(GEOID, 1, 2),
        county_geoid = stringr::str_sub(GEOID, 1, 5),
        geography = "tract") }

  if (isFALSE(return_geometry)) { result = sf::st_drop_geometry(result) }

  return(result)
}

#' Download a .xlsx file(s) from a URL(s)
#'
#' @param urls A character vector of URLs of length one or greater.
#' @param directory The path to a single directory--not to a file--where the .xlsx file(s) will be saved.
#' @param file_names Optionally, a character vector of the same length as `urls` containing only the file names (not the full paths) with which the downloaded files should be named. If NULL (default), file names are extracted from `urls`.
#' @param silent If TRUE (default), files are saved silently. If FALSE, downloaded files are read and returned as a list.
#'
#' @return When `silent = TRUE` (default): Returns NULL invisibly. Files are downloaded and saved to `directory`.
#' When `silent = FALSE`: Returns a list of data frames, one per URL, containing the contents of each downloaded .xlsx file as read by `openxlsx::read.xlsx()`. List elements are in the same order as the input `urls`.
#' @export
read_xlsx_from_url = function(urls, directory, file_names = NULL, silent = TRUE) {

  if (any(!is.null(file_names)) & length(file_names) != length(urls)) {
    stop("`urls` and `file_names` must be of the same length.") }
  if (any(is.null(file_names))) {
    file_names = purrr::map_chr(
      urls,
      ~ file.path(
        directory,
        .x |>
          stringr::str_split("\\/") |>
          purrr::map_chr(~ length(.x) %>% as.character())))
    }
  if (any(stringr::str_detect(directory, "\\.xlsx$"))) {
    stop("`directory` must point to a directory (folder), not a file. Provide file names via `file_names`.") }

  result = purrr::imap(
    urls,
    function(url, i) {
      file_path = file_names[i]
      utils::download.file(
        url = url,
        destfile = file_path,
        mode = "wb")

      result = openxlsx::read.xlsx(file_path)
      return(result) })

  if (isTRUE(silent)) { return() } else return(result)
}

#' Get geography metadata about states or counties
#'
#' @param geography_type One of c("state", "county").
#' @param year The year for which to obtain state/county metadata. Cannot be greater than the most recent year supported by `library(tidycensus)` for the 5-year ACS.
#'
#' @return A tibble containing geographic metadata. The structure varies by `geography_type`:
#' \describe{
#'   \item{For "county"}{Returns county-level data with columns: `state_code` (2-digit FIPS), `state_name`, `state_abbreviation` (2-letter USPS), `state_population`, `county_code` (5-digit FIPS), `county_name`, `county_population`.}
#'   \item{For "state"}{Returns state-level data with columns: `state_abbreviation`, `state_code`, `state_name` (one row per state, no county information).}
#' }
#' Population data are sourced from the ACS 5-year estimates for the specified `year`.

get_geography_metadata = function(
    geography_type = c("state", "county"),
    year = 2023) {

  geography_type = match.arg(geography_type)

  ## tidycensus::get_acs() only covers the 50 states, DC, and Puerto Rico; American Samoa,
  ## Guam, the Northern Mariana Islands, and the US Virgin Islands are not part of the ACS,
  ## so their geography metadata is sourced from tidycensus::fips_codes instead. Population
  ## is unavailable for these territories from this source and is left NA.
  territory_codes = c("60", "66", "69", "78")

  suppressMessages({
    df1 = tidycensus::get_acs(
        year = year,
        output = "wide",
        variables = "B01003_001",
        geography = "county") |>
      tidyr::separate_wider_delim(NAME, delim = ", ", names = c("county_name", "state_name")) |>
      dplyr::transmute(
        state_code = stringr::str_sub(GEOID, 1, 2),
        state_name,
        county_code = GEOID,
        county_name,
        county_population = B01003_001E) |>
      dplyr::left_join(
        tidycensus::fips_codes %>%
          dplyr::select(state_abbreviation = state, state_code) %>%
          dplyr::distinct(),
        by = "state_code") })

  territory_geographies = tidycensus::fips_codes |>
    dplyr::filter(state_code %in% territory_codes) |>
    dplyr::transmute(
      state_code,
      state_name,
      state_abbreviation = state,
      ## fips_codes' `county_code` is only the 3-digit county part; pair it with
      ## `state_code` to match df1's 5-digit GEOID-based county_code
      county_code = stringr::str_c(state_code, county_code),
      county_name = county,
      county_population = NA_real_)

  df2 = dplyr::bind_rows(df1, territory_geographies) |>
    dplyr::mutate(
      .by = c(state_code),
      state_population = dplyr::if_else(
        all(is.na(county_population)), NA_real_, sum(county_population, na.rm = TRUE))) |>
    dplyr::select(dplyr::matches("state"), dplyr::matches("county"))

  if (geography_type == "state") {
    df2 = df2 |> dplyr::distinct(state_abbreviation, state_code, state_name) }

  return(df2)
}

#' Convert named month-including dates to standardized date-type variables
#'
#' @param date_string A text-based date of the form January 01 2000
#'
#' @return A date-type variable
#' @noRd
date_string_to_date = function(date_string) {
  date_string %>%
    stringr::str_replace_all(c(
      "January" = "01",
      "February" = "02",
      "March" = "03",
      "April" = "04",
      "May" = "05",
      "June" = "06",
      "July" = "07",
      "August" = "08",
      "September" = "09",
      "October" = "10",
      "November" = "11",
      "December" = "12",
      " " = "-")) %>%
    stringr::str_extract("[0-9]{2}-[0-9]{1,2}-[0-9]{4}") %>%
    lubridate::mdy()
}

#' Inflation adjust dollar values using annual PCE Index
#'
#' The Personal Consumption Expenditures Price Index (PCE Index) is from the
#' Federal Reserve Bank of St. Louis's FRED tool.
#'
#' @param df The dataframe with values to inflation-adjust
#' @param year_variable The name of the column denoting the year that existing dollar-denominated figures are based in.
#' @param dollar_variables The variables to inflation-adjust.
#' @param base_year The year to use as the base for inflation adjustment. If NULL, defaults to the most recent year in the PCE index data.
#' @param names_suffix A suffix to add to the names of the inflation-adjusted variables. If NULL, defaults to "_<base_year>". If "", columns are renamed in place.
#'
#' @return A tibble identical to the input `df` with additional inflation-adjusted columns. For each column specified in `dollar_variables`, a new column is created with the same name plus `names_suffix` (default: "_\{base_year\}"). The adjusted values are calculated by multiplying original values by an inflation factor derived from the PCE Price Index ratio between the base year and each observation's year. Original columns are preserved unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' df = tibble::tribble(
#'   ~ year, ~ amount,
#'   1990, 1,
#'   1991, 1,
#'   1992, 1)
#'
#' df |>
#'   inflation_adjust(
#'     year_variable = year,
#'     dollar_variables = amount,
#'     names_suffix = "inflation_adjusted")
#' }

inflation_adjust = function(
    df,
    year_variable,
    dollar_variables,
    names_suffix = NULL,
    base_year = 2024) {

  inflation_data = readr::read_csv(file.path(get_box_path(), "utilities", "pce_index_annual_fred_2025_03_27.csv")) %>%
    dplyr::transmute(
      inflation_year_ = lubridate::year(observation_date) %>% as.numeric(),
      pce_index = DPCERG3A086NBEA)

  if (is.null(base_year)) base_year = max(inflation_data$year)
  if (is.null(names_suffix)) names_suffix = paste0("_", base_year)

  inflation_data = inflation_data %>%
    dplyr::mutate(
      inflation_factor = (pce_index[inflation_year_ == dplyr::if_else(is.null(base_year), max(inflation_year_), base_year)]) / pce_index)

  df1 = df %>%
    dplyr::mutate(
      inflation_year_ = .data[[year_variable]] %>% as.numeric) %>%
    dplyr::left_join(inflation_data, by = c("inflation_year_")) %>%
    dplyr::mutate(
      dplyr::across(.cols = dplyr::all_of(dollar_variables), ~ .x * inflation_factor, .names = "{.col}{names_suffix}")) %>%
    dplyr::select(-pce_index, -inflation_year_, -inflation_factor)
}

utils::globalVariables(c(
  "DPCERG3A086NBEA", "crop_damage_adjusted_2023", "inflation_factor", "inflation_year_",
  "pce_index", "property_damage_adjusted_2023", "B01003_001E", "geometry", "geography_area",
  "overlap_pct", "STATEFP"))
