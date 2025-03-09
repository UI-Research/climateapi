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
