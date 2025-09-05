## Authors: Original code from Aaron R. Williams, extended by Will Curran-Groome

#' @importFrom magrittr %>%

## This script wraps a standard ipumsr::read_ipums*() query workflow, addressing
## two common challenges:
##    (1) the default workflow downloads arbitrarily named raw data files that
##        are sequentially numbered and dependent on the total number of extracts
##        submitted by a given user; and
##    (2) the default workflow does not provide an inbuilt capacity to check for
##        a local version of the query before re-submitting to the API.
##
## This script addresses these challenges by taking a user-supplied filename and
## file directory, checking if there is an existing file at that path, and otherwise
## downloading the extract (again user-specified) to the given filepath.

#' @title Read cached IPUMS data
#'
#' @param filename The name of the file (not the full file path)
#' @param download_directory A relative path specifying where to download the data
#' @param extract_definition A `define_extract_micro()` or `define_extract_agg()` object
#' @param refresh If true, execute the API query, even if data are already stored locally. Defaults to FALSE
#'
#' @return A dataframe corresponding to the supplied extract_definition
#' @export
#'
#' @examples
#' \dontrun{
#' read_ipums_cached(
#'   filename = "acs_insurance_race_2022_1yr_repweights",
#'   download_directory = "data",
#'   extract_definition = define_extract_micro(
#'     collection = "usa",
#'     description = "2022 ACS 1-year sample with replicate weights - insurance and race",
#'     samples = c("us2022a"),
#'     variables = list(
#'       "HCOVANY",
#'       var_spec("RACE", case_selections = c("1", "2")),
#'       "REPWT"),
#'   refresh = FALSE))
#' }

read_ipums_cached = function(filename, download_directory, extract_definition, refresh = FALSE) {

  if (!is.character(filename)) {
    stop("The `filename` argument must be a character string.") }
  if (!is.character(download_directory)) {
    stop("The `download_directory` argument must be a character string.") }
  if (!is.logical(refresh)) {
    stop("The `refresh` argument must be either `TRUE` or `FALSE`.") }
  if (!dir.exists(here::here(download_directory))) {
    stop("The specified `download_directory` does not exist. Specify an existing directory
         relative to your root directory.") }

  ## could be either a .xml (for microdata) or a .zip (nhigs, ihgis)
  possible_files = here::here(download_directory, stringr::str_c(filename, c(".xml", ".zip")))
  file_exists = any(file.exists(possible_files))

  ## if the file doesn't already exist, submit the extract definition to the api
  if (!file_exists | refresh == TRUE) {

    # submit the extract to IPUMS USA for processing
    submitted_extract <- ipumsr::submit_extract(extract_definition)

    # access the extract number, stored in the return value of submit_extract
    extract_number <- stringr::str_pad(
      submitted_extract$number,
      width = 5,
      side = "left",
      pad = "0")

    # pause the code until the extract is prepared
    ipumsr::wait_for_extract(submitted_extract)
    # This will save the extract files to the current directory
    # use the download_dir argument to specify a different location
    # The return value is the path to the DDI codebook file, which can then be passed to read_ipums_*() to read the data
    path_to_ddi_file <- ipumsr::download_extract(submitted_extract, download_dir = download_directory)

    ## the code for the "collection", e.g., "usa", "cps", etc.
    collection_code = extract_definition$collection

    # rename files so they don't depend on the extract number, which changes from
    # extract to extract and user to user
    if (!(collection_code %in% c("nhgis", "ihgis"))) {
      ## rename the data file
      file.rename(
        from = here::here(
          download_directory,
          stringr::str_glue("{collection_code}_{extract_number}.dat.gz", extract_number = extract_number)),
        to = here::here(download_directory, stringr::str_c(filename, ".dat.gz")))

      ## rename the ddi file
      file.rename(
        from = here::here(
          download_directory,
          stringr::str_glue("{collection_code}_{extract_number}.xml", extract_number = extract_number)),
        to = here::here(download_directory, stringr::str_c(filename, ".xml"))) }

    ## for some reason, nhgis data are downloaded to a different file type and using a slightly
    ## different naming convention
    if (collection_code %in% c("nhgis", "ihgis")) {
      ## bizzarely, the collection code appears to sometimes (?) have one of three leading zeros removed
      ## so we read in a corresponding file at the given location
      file.rename(
        from = here::here(
          download_directory,
          stringr::str_glue(
            "{collection_code}{extract_number}_csv.zip",
            extract_number = extract_number |> stringr::str_replace("000", "00"))),
        to = here::here(download_directory, stringr::str_c(filename, ".zip")))
    }
  } else {
    warning("Data are being read from a local path. If you have changed the arguments
            to your define_*_extract() call, you should delete the existing data
            file at the specified local path and then re-execute this function, which
            will then query the IPUMS API for the updated data and save it to disk.")

    collection_code = extract_definition$collection

    if (!collection_code %in% c("nhgis", "ihgis")) {
      data = ipumsr::read_ipums_micro(
        ddi = here::here(download_directory, stringr::str_c(filename, ".xml")),
        data_file = here::here(download_directory, stringr::str_c(filename, ".dat.gz"))) }
    if (collection_code %in% c("nhgis", "ihgis")) {
      zip_path = here::here(download_directory, stringr::str_c(filename, ".zip"))
      data = ipumsr::read_ipums_agg(data_file = zip_path) |>
        ipumsr::set_ipums_var_attributes(
          var_info = { if (collection_code == "nhgis") {
            ipumsr::read_nhgis_codebook(zip_path) } else { ipumsr::read_ihgis_codebook(zip_path) }})
    }}

  return(data)
}
