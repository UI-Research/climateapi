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

#' @title Get the path to the C&C Box folder
#'
#' @return A character string containing the full file path to the Climate and Communities (C&C) Box folder.
#'   On Windows, returns "C:/Users/{username}/Box/METRO Climate and Communities Practice Area/github-repository".
#'   On Mac, checks for Box at "/Users/{username}/Box" or "/Users/{username}/Library/CloudStorage/Box-Box",
#'   using whichever exists. Throws an error if the Box folder cannot be found.
#' @export
#'
#' @examples
#' \dontrun{
#' get_box_path()
#' }
get_box_path = function() {
  username <- get_system_username()
  os <- Sys.info()[["sysname"]]
  box_subfolder <- file.path("METRO Climate and Communities Practice Area", "github-repository")

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

  box_path <- file.path(box_root, box_subfolder)

  if (!dir.exists(box_path)) {
    warning("Box path does not exist: ", box_path)
  }

  box_path
}


#' Get the raw column names for a specified dataset
#'
#' @param dataset The name of the dataset. One of c('nfip_policies', 'ihp_registrations').
#'
#' @return A character vector containing the raw column names (in camelCase format as they appear in the source data) to be selected when reading the specified dataset. The columns returned are curated subsets of the full dataset columns, excluding administrative/metadata fields. For "nfip_policies": 20 columns including location, policy details, and building characteristics. For "ihp_registrations": ~20 columns including disaster info, geographic identifiers, and assistance amounts.
get_dataset_columns = function(dataset) {

  if (length(dataset) > 1 | !is.character(dataset)) {
    stop("The `dataset` argument must be a character of length one.") }

  if (! dataset %in% c('nfip_policies', 'ihp_registrations')) {
    stop("The `dataset` argument must be one of c('nfip_policies', 'ihp_registrations').") }

  if (dataset == "nfip_policies") {
    columns = c(
      "id",
      "longitude",
      "latitude",
      "censusTract",
      "crsClassCode",
      "ratedFloodZone",
      "occupancyType",
      "originalConstructionDate",
      "policyCost",
      "policyCount",
      "policyEffectiveDate",
      "policyTerminationDate",
      "primaryResidenceIndicator",
      "regularEmergencyProgramIndicator",
      "smallBusinessIndicatorBuilding",
      "totalInsurancePremiumOfThePolicy",
      "buildingReplacementCost",
      "floodproofedIndicator",
      "rentalPropertyIndicator",
      "tenantIndicator") }

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
#' @param dataset NULL by default. Alternately, one of c("nfip_policies", "ihp_registrations"). If not null, this will be used to select the columns that are returned.
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
  if (is.null(dataset)) {
    subsetted_columns = colnames(raw_txt_test_delimit_character) }
  if (!(dataset %in% c("ihp_registrations", "nfip_policies"))) {
    stop("The `dataset` argument must be one of c('ihp_registrations', 'nfip_policies').") }

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
  warning("This leverages `sf::st_overlaps()` and does not provide the desired results consistently.")

  data = tigris::counties()
  data = data |>
    sf::st_transform(projection)

  states_sf = tigris::states(
      cb = TRUE,
      resolution = "20m",
      year = 2023,
      progress_bar = FALSE,
      refresh = TRUE) |>
    sf::st_transform(projection)

  state_geoids = states_sf |>
    sf::st_filter(data, .predicate = sf::st_overlaps) |>
    _[["GEOID"]]

  if (length(state_geoids) > 1) {
    result = states_sf |>
      dplyr::filter(GEOID %in% state_geoids) |>
      dplyr::transmute(
        state_geoid = GEOID,
        geography = "state") } else {

    result = state_geoids |>
      purrr::map_dfr(
        ~ tigris::tracts(
          state = .x,
          county = NULL,
          cb = TRUE,
          year = 2023,
          progress_bar = FALSE,
          refresh = TRUE)) |>
      sf::st_transform(projection) |>
      sf::st_filter(data) |>
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

  suppressMessages({
    df = tidycensus::get_acs(
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
        by = "state_code") |>
      dplyr::mutate(
        .by = c(state_code),
        state_population = sum(county_population, na.rm = TRUE)) |>
    dplyr::select(dplyr::matches("state"), dplyr::matches("county")) })

  if (geography_type == "state") {
    df = df |> dplyr::distinct(state_abbreviation, state_code, state_name) }

  return(df)
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
#' @return A tibble identical to the input `df` with additional inflation-adjusted columns. For each column specified in `dollar_variables`, a new column is created with the same name plus `names_suffix` (default: "_{base_year}"). The adjusted values are calculated by multiplying original values by an inflation factor derived from the PCE Price Index ratio between the base year and each observation's year. Original columns are preserved unchanged.
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

  inflation_data = readr::read_csv(here::here(
    "data", "data-raw", "pce_index_annual_fred_2025_03_27.csv")) %>%
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
  "pce_index", "property_damage_adjusted_2023"))
