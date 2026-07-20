#' Get the share of residential structures covered by NFIP
#'
#' @param states NULL by default.
#' @param file_name The name (not full path) of the raw dataset. If NULL (default),
#'   reads the most recently cached file for this dataset from `get_openfema_cache_path()`.
#'
#' @return A tibble with the following columns:
#'     \describe{
#'         \item{state_name}{State name}
#'         \item{couty_name}{County name}
#'         \item{GEOID}{Five digits county FIPS code}
#'         \item{year_data}{The year of the data}
#'         \item{penetration_rate}{Share of all residential structures insured by NFIP}
#'         \item{penetration_rate_sfha}{Share of all residential structures in the Special Flood Hazard Area insured by NFIP}
#'         \item{contracts_in_force}{Residential NFIP contracts currently in effect ("in force")}
#'         \item{contracts_in_force_sfha}{Residential NFIP contracts in the Special Flood Hazard Area currently in effect ("in force")}
#'         \item{residential_structures}{The number of residential structures, derived from the 2022 National Structure Inventory}
#'         \item{residential_structures_sfha}{The number of residential structures in the Special Flood Hazard Area, derived from the 2022 National Structure Inventory}
#'     }
#'
#' @export
#' @examples
#' \dontrun{
#' get_nfip_residential_penetration()
#' }
get_nfip_residential_penetration <- function(
    states = NULL,
    file_name = NULL) {

  if (is.null(file_name)) {
    df0 <- arrow::read_parquet(find_openfema_cache_file("NfipResidentialPenetrationRates"))
  } else {
    inpath <- file.path(
      get_box_path(), "hazards", "fema", "national-flood-insurance-program", "raw", file_name)

    if (!file.exists(inpath)) { stop("The provided `file_name` is invalid.") }

    df0 <- readr::read_csv(file = inpath)
  }

  df1 <- df0 %>%
    janitor::clean_names() %>%
    dplyr::transmute(
      state_name = state, 
      county_name = county, 
      GEOID = stringr::str_pad(fips_code, side = "left", pad = "0", width = 5),
      year_data = lubridate::year(as_of_date),
      dplyr::across(
        .cols = dplyr::matches("res"), 
        .fns = ~ .x)) %>%
    dplyr::rename_with(
      .cols = dplyr::matches("res"),
      .fn = ~ .x %>% stringr::str_replace_all(c(
        "res_penetration" = "penetration",
        "res_contracts" = "contracts",
        "res_structures" = "residential_structures",
        "total_" = ""))) %>%
    tibble::as_tibble()
  
  # Apply filter only when states is not NULL
  if (!is.null(states)) {
    ## this column holds full state names (e.g. "Florida"), not abbreviations; warn if
    ## any supplied value doesn't match, since an abbreviation would otherwise silently
    ## return 0 rows
    unmatched_states = setdiff(states, unique(df1$state_name))
    if (length(unmatched_states) > 0) {
      stop(stringr::str_c(
        "The following `states` values do not match any state_name in the data (this ",
        "column holds full state names, e.g. 'Florida', not abbreviations): ",
        stringr::str_c(unmatched_states, collapse = ", "), ".")) }

    df1 <- df1 %>%
      dplyr::filter(state_name %in% states) }

  return(df1)
}

utils::globalVariables(c("fips_code", "as_of_date"))