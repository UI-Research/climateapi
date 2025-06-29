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
