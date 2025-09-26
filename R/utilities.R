## Author: Will Curran-Groome

#' @title Get the user's username
#'
#' @return The username of the user running the script
#' @export
#'
#' @examples
#' \dontrun{
#' get_system_username()
#' }
get_system_username = function() {
  here::here() |>
    stringr::str_match("Users/.*?/") |>
    stringr::str_remove_all("Users|/")
}

#' @title Get the path to the C&C Box folder
#'
#' @return The filepath to the C&C Box folder
#' @export
#'
#' @examples
#' \dontrun{
#' get_box_path()
#' }
get_box_path = function() {
  username = get_system_username()
  file.path(
    "C:", "Users", username, "Box", "METRO Climate and Communities Practice Area",
    "github-repository")
}


#' Get the raw column names for a specified dataset
#'
#' @param dataset The name of the dataset. One of c('nfip_policies', 'ihp_registrations').
#'
#' @return A vector of raw column names to be selected from the specified dataset
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
      "incidentType",
      "disasterNumber",
      "declarationDate",
      "county",
      "damagedStateAbbreviation",
      "damagedCity",
      "damagedZipCode",
      "householdComposition",
      "grossIncome",
      "ownRent",
      "residenceType",
      "homeOwnersInsurance",
      "floodInsurance",
      "ihpAmount",
      "fipAmount",
      "haAmount",
      "onaAmount",
      "homeDamage",
      "autoDamage",
      "emergencyNeeds",
      "foodNeed",
      "shelterNeed",
      "accessFunctionalNeeds",
      "sbaApproved",
      "rpfvl",
      "ppfvl",
      "destroyed",
      "rentalAssistanceAmount",
      "repairAmount",
      "replacementAmount",
      "personalPropertyAmount",
      "ihpMax",
      "haMax",
      "onaMax",
      "lastRefresh",
      "id") }

  return(columns)
}

# Author: Will Curran-Groome

#' @title Convert raw data to parquet to conserve memory / speed subsequent operations
#'
#' @param inpath The local path to read CSV data from.
#' @param outpath The local path to write parquet data to.
#' @param delimit_character The delimiting character of the raw data.
#' @param subsetted_columns The columns to include in the outputted parquet data.
#' @param dataset One of c("nfip_policies", "ihp_registrations"). If not null, this will be used to select the columns that are returned.
#'
#' @returns Nothing. Parquet data are written to local path.
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
  if (!(dataset %in% c("ihp_registrations", "nfip_policies", "ia_registrations"))) {
    stop("The `dataset` argument must be one of c('ihp_registrations', 'nfip_policies', 'ia_registrations')") }

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
#' @returns A dataframe (optionally, an sf-dataframe) comprising Census geographies
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
#' @returns Either nothing (silent == TRUE) or a list of dataframes from the specified URLs.
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
#'
#' @return A data frame containing metadata about the specified geography type and area.

get_geography_metadata = function(
    geography_type = c("state", "county")) {

  geography_type = match.arg(geography_type)

  df = tidycensus::fips_codes |>
    dplyr::select(
      state_abbreviation = state,
      state_code,
      state_name,
      county_code,
      county_name = county) |>
    dplyr::mutate(
      county_geoid = stringr::str_c(state_code, county_code))

  if (geography_type == "state") {
    df = df |> dplyr::distinct(state_abbreviation, state_code, state_name) }

  return(df)
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
#' @return A dataframe with inflation-adjusted values
#' @export
#'
#' @examples
#'
# df = tibble::tribble(
#   ~ year, ~ amount,
#   1990, 1,
#   1991, 1,
#   1992, 1)
#
# df %>%
#   inflation_adjust(
#     year_variable = year,
#     dollar_variables = amount,
#     names_suffix = "inflation_adjusted")

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
      inflation_factor = (pce_index[inflation_year_ == if_else(is.null(base_year), max(inflation_year_), base_year)]) / pce_index)

  df1 = df %>%
    dplyr::mutate(
      inflation_year_ = .data[[year_variable]] %>% as.numeric) %>%
    dplyr::left_join(inflation_data, by = c("inflation_year_")) %>%
    dplyr::mutate(
      across(.cols = dplyr::all_of(dollar_variables), ~ .x * inflation_factor, .names = "{.col}{names_suffix}")) %>%
    dplyr::select(-pce_index, -inflation_year_, -inflation_factor)

}



