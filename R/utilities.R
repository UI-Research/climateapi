## Author: Will Curran-Groome

#' @importFrom magrittr %>%

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
  here::here() %>%
    stringr::str_match("Users/.*?/") %>%
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

#' @importFrom magrittr %>%

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
    outpath = inpath %>% stringr::str_replace("\\..*$", "\\.parquet") }

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
  read_chunk_callback = function(x, cols) { x %>% dplyr::select(dplyr::all_of(subsetted_columns)) }

  ## reading the file in chunks
  raw_text_subsetted = tryCatch(
    { readr::read_delim_chunked(inpath, delim = delimit_character, callback = DataFrameCallback$new(read_chunk_callback), chunk_size = 1000000) },
    error = function(e) { stop(e) })

  arrow::write_parquet(raw_text_subsetted, sink = outpath)
}
