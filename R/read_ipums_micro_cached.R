## This script wraps a standard ipumsr::read_ipums_micro() query workflow, addressing
## two common challenges:
##    (1) the default workflow downloads arbitrarily named raw data files that
##        are sequentially numbered and dependent on the total number of extracts
##        submitted by a given user; and
##    (2) the default workflow does not provide an inbuilt capacity to check for
##        a local version of the query before re-submitting to the API.
##
## This script addresses these challenges by taking a user-supplied filename and
## file directory, checking if there is an existing file at that path, and otherwise
## downloading the microdata extract (again user-specified) to the given filepath.

## Authors: Original code from Aaron R. Williams, extended by Will Curran-Groome
## Last Updated: 01/08/2025

#' @importFrom magrittr %>%
#' @title Read Cached IPUMS Microdata
#'
#' @param filename name of the file (not the full path)--do not include a file extension
#' @param download_directory wherever the data will be downloaded to--must be a relative path
#' @param extract_definition the object resulting from define_extract_micro()
#' @param refresh if true, execute the API query, even if data are already stored locally
#'
#' @return A dataframe corresponding to the supplied extract_definition
#' @export
#'
#' @examples
#' \dontrun{
#' read_ipums_micro_cached(
#'   filename = "acs_insurance_race_2022_1yr_repweights",
#'   download_directory = "data",
#'   extract_definition = define_micro_extract(
#'     collection = "usa",
#'     description = "2022 ACS 1-year sample with replicate weights - insurance and race",
#'     samples = c("us2022a"),
#'     variables = list(
#'       "HCOVANY",
#'       var_spec("RACE", case_selections = c("1", "2")),
#'       "REPWT"),
#'   refresh = FALSE))
#' }
#'
read_ipums_micro_cached = function(
    filename,
    download_directory,
    extract_definition,
    refresh = FALSE) {

  if (!is.character(filename)) {
    stop("The `filename` argument must be a character string.")
  }
  if (!is.character(download_directory)) {
    stop("The `download_directory` argument must be a character string.")
  }
  if (!is.logical(refresh)) {
    stop("The `refresh` argument must be either `TRUE` or `FALSE`.")
  }
  if (!dir.exists(here::here(download_directory))) {
    stop("The specified `download_directory` does not exist. Specify an existing directory
         relative to your root directory.")
  }

  ## if the file doesn't already exist, submit the extract definition to the api
  if (!file.exists(here::here(download_directory, stringr::str_c(filename, ".xml"))) | refresh == TRUE) {

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
    # The return value is the path to the DDI codebook file, which can then be passed to read_ipums_micro to read the data
    path_to_ddi_file <- ipumsr::download_extract(submitted_extract, download_dir = download_directory)

    # rename files so they don't depend on the extract number, which changes from
    # extract to extract and user to user
    file.rename(
      from = here::here(
        download_directory,
        stringr::str_glue("usa_{extract_number}.dat.gz", extract_number = extract_number)),
      to = here::here(download_directory, stringr::str_c(filename, ".dat.gz")))

    file.rename(
      from = here::here(
        download_directory,
        stringr::str_glue("usa_{extract_number}.xml", extract_number = extract_number)),
      to = here::here(download_directory, stringr::str_c(filename, ".xml")))

  } else {
    warning("Data are being read from a local path. If you have changed the arguments
            to your define_micro_extract() call, you should delete the existing data
            file at the specified local path and then re-execute this function, which
            will then query the IPUMS API for the updated data and save it to disk.")
    path_to_ddi_file <- here::here(download_directory, stringr::str_c(filename, ".xml"))
  }

  data <- ipumsr::read_ipums_micro(
    ddi = here::here(download_directory, stringr::str_c(filename, ".xml")),
    data_file = here::here(download_directory, stringr::str_c(filename, ".dat.gz")))

  return(data)
}
