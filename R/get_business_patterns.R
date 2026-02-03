#' @importFrom magrittr %>%

#' @title Get NAICS Codes for County Business Patterns
#'
#' @description A utility function to programmatically identify and select NAICS
#'     codes for use with `get_business_patterns()`. This is a wrapper around
#'     `censusapi::listCensusMetadata(name = "cbp")`.
#'
#' @param year The vintage year for NAICS codes. Data are available from 1986 through 2023.
#'     Default is 2022.
#' @param digits The number of digits for desired NAICS codes. Must be between 2 and 6.
#'     Default is 3. Two-digit codes represent broad industry sectors (20 codes),
#'     while six-digit codes represent detailed industries.
#'
#' @return A tibble with the following columns:
#'     \describe{
#'         \item{naics_code}{The NAICS code (character)}
#'         \item{naics_label}{The descriptive label for the NAICS code}
#'         \item{year}{The vintage year of the NAICS codes}
#'     }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all 2-digit NAICS codes
#' get_naics_codes(year = 2022, digits = 2)
#'
#' # Get all 3-digit NAICS codes (default)
#' get_naics_codes(year = 2022)
#'
#' # Get 4-digit NAICS codes for a specific year
#' get_naics_codes(year = 2020, digits = 4)
#' }
get_naics_codes <- function(year = 2022, digits = 3) {
  if (year < 1986) { stop("Year must be 1986 or later.") }
  if (year > 2023) { stop("Most recent year for data is 2023.") }
  if (!digits %in% 2:6) { stop("`digits` must be between 2 and 6.") }

  naics_metadata <- censusapi::listCensusMetadata(
      name = "cbp",
      vintage = as.character(year),
      type = "variables",
      include_values = TRUE) |>
    dplyr::filter(
      !is.na(values_code),
      nchar(values_code) == digits) |>
    dplyr::transmute(
      naics_code = values_code,
      naics_label = values_label,
      year = year) |>
    dplyr::distinct() |>
    dplyr::arrange(naics_code) |>
    tibble::as_tibble()

}

#' @title Obtain County Business Patterns (CBP) Estimates per County
#'
#' @param year The vintage of CBP data desired. Data are available from 1986,
#'     though this function likely only supports more recent years (it it tested on 2022-vintage data only).
#'     Default is 2022.
#' @param geo The level of geography of CBP data desired. Either "county" or "zipcode". Zipcode
#'     level data only The ZIP Code Business Patterns (ZBP) dataset includes the number of establishments,
#'     employment during the week of March 12th, first quarter and annual payroll for NAICS 00 (total for all sectors).
#'     Additionally, the number of establishments (but not employment or payroll) are available by employment
#'     size of the establishment for 2- through 6-digit NAICS.
#' @param naics_code_digits One of c(2, 3). Default is 2. NAICS codes range in
#'     specificity; 2-digit codes describe the highest groupings of industries,
#'     while six-digit codes are exceedingly detailed. There are 20 2-digit NAICS
#'     codes and 196 3-digit codes. If more specific codes are desired, leave this
#'     argument as NULL and supply the desired codes as the argument to `naics_codes`.
#' @param naics_codes A vector of NAICS codes to query. If NULL, the function will
#'     query all available codes with the specified number of digits. If not NULL,
#'     this argument overrides the `naics_code_digits` argument.
#'
#' @details
#' County Business Patterns (CBP) is an annual series that provides subnational
#' economic data for establishments with paid employees by industry and employment size.
#' This series includes the number of establishments, employment during the week of
#' March 12, first quarter payroll, and annual payroll. Industry classification of business
#' establishments in CBP is according to the North American Industry Classification System (NAICS)
#' https://www.census.gov/naics/
#'
#' CBP data are useful for studying economic activity of small areas. Federal agencies
#' use the data to determine employee concentrations and trends by industry.
#' State and local government offices use the data to assess business changes, develop
#' fiscal policies, and plan future policies and programs. CBP data are used to benchmark
#' public and private sector statistical series, surveys, and databases between economic census years.
#'
#' While similar to LEHD Origin-Destination Employment Statistics (LODES) data in it's coverage of employment
#' statistics, CBP differs mainly due to its broader geographies (county vs. tract) and
#' focus on framing the statistics at an establishment/company level rather than at the individual/job
#' level found in LODES data. CBP also does not offer information on locations of the jobs in relation to
#' where the employee actually resides.
#'
#' The series excludes data on self-employed individuals, employees of private households,
#' railroad employees, agricultural production employees, and most government employees.
#' A certain amount of undercoverage occurs in the universe, as the Census Bureau does
#' not create a multi-unit company structure in the Business Register for very small employers
#' (less than 10 employees) identified in the Economic Census.
#'
#' CBP covers most NAICS industries excluding Crop and Animal Production (NAICS 111,112);
#' Rail Transportation (NAICS 482); Postal Service (NAICS 491); Pension, Health, Welfare,
#' and Other Insurance Funds (NAICS 525110, 525120, 525190); Trusts, Estates, and Agency
#' Accounts (NAICS 525920); Offices of Notaries (NAICS 541120); Private Households (NAICS 814);
#' and Public Administration (NAICS 92)
#'
#' @return A tibble with data on county-level employees, employers, and aggregate
#'     annual payrolls by industry and employer size
#'     \describe{
#'         \item{year}{the year for which CBP data is pulled from}
#'         \item{state}{A two-digit state identifier.}
#'         \item{county}{A three-digit county identifier.}
#'         \item{employees}{number of individual employees employed in that particular industry
#'              and establishment size combination}
#'         \item{employers}{number of establishments of each employment size}
#'         \item{annual_payroll}{total annual payroll expenditures measured in $1,000's of USD}
#'         \item{industry}{industry classification according to North American Industry Classification System.
#'              Refer to details for additional information}
#'         \item{employee_size_range_label}{range for the employment size of establishments included in each
#'              given grouping}
#'         \item{employee_size_range_code}{three-digit code used to categorize employment sizes}
#'         \item{naics_code}{two to six-digit code used by the NAICS to categorize and sub-categorize industries}
#'         }
#' @export
#'
#' @examples
#' \dontrun{
#' get_business_patterns(
#'  year = 2022,
#'  naics_code_digits = 3)
#'
#' get_business_patterns(
#'  year = 2017,
#'  naics_codes = c(221111, 221112))
#' }

get_business_patterns = function(year = 2022, geo = "county", naics_code_digits = 2, naics_codes = NULL) {
  if (year < 1986) { stop("Year must be 1986 or later.") }
  if (year > 2023) { stop("Most recent year for data is 2023.") }
  if (! geo %in% c("county", "zipcode")) { stop("`geo` must be one of 'county' or 'zipcode'.") }
  if (! naics_code_digits %in% c(2, 3)) {
    stop("`naics_code_digits` must be one of c(2, 3). For more detailed codes, explicitly pass desired codes to the `naics_codes` parameter.") }

  naics_codes_metadata = censusapi::listCensusMetadata(
      name = "cbp",
      vintage = "2022",
      type = "variables",
      include_values = TRUE) %>%
    #filter out codes 92 and 95 which do not appear to have data associated and
    #don't appear on the census list of naics codes at
    #https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/naics-descriptions/naics2017.txt
    dplyr::filter(!stringr::str_starts(values_code, "92|95"))

  if (!is.null(naics_codes)) {
    naics_code_check = naics_codes_metadata %>%
      dplyr::filter(values_code %in% naics_codes) %>%
      nrow()

    if (naics_code_check < 1) {
      stop("The provided `naics_codes` do not appear to be valid NAICS codes. Refer to https://www.census.gov/naics/ for the relevant vintage's NAICS codes.") }

    if (naics_code_check < length(naics_codes)) {
      warning("Some, but not all, supplied `naics_codes` appear to be valid NAICS codes. Returning data only for valid codes.") }

    naics_code_digits = NULL }

  if (!is.null(naics_code_digits)) {
    naics_codes_to_query = naics_codes_metadata %>%
      dplyr::filter(
        nchar(values_code) == naics_code_digits,
        !is.na(values_code)) %>%
      dplyr::pull(values_code) } else {
    naics_codes_to_query = naics_codes_metadata %>%
      dplyr::filter(
        values_code %in% naics_codes,
        !is.na(values_code)) %>%
      dplyr::pull(values_code) }

  cbp = purrr::map_dfr(
    naics_codes_to_query,
    ## some high-level codes (92 and 94) error with a "No data to return"
    ## message, so we wrap this in a tryCatch(). this doesn't appear to be an
    ## error with our query--there's no data for these codes on data.census.gov
    ## either
    ~ tryCatch({
      censusapi::getCensus(
        name = "cbp",
        vintage = year,
        vars = c(
          "EMP",
          "YEAR",
          "ESTAB",
          "PAYANN",
          "EMPSZES",
          "NAICS2017_LABEL"),
        region = paste0(geo, ":*"),
        NAICS2017 = .x) %>%
      dplyr::mutate(naics_code = .x)},
      error = function(e) { return(tibble::tibble()) })) %>%
    dplyr::mutate(
      # state,
      # county,
      employees = EMP,
      employers = ESTAB,
      annual_payroll = PAYANN,
      employee_size_range = EMPSZES,
      industry = NAICS2017_LABEL,
      naics_code) %>%
    dplyr::mutate(
      industry = industry %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(c(" " = "_", ",|\\(|\\)|_for_all_sectors|and_" = "")),
      year = year,
      ## this recoding is mapped from: https://www2.census.gov/programs-surveys/bds/technical-documentation/label_empszes.csv
      employee_size_range_label = dplyr::case_when(
        employee_size_range == "001" ~ "All establishments",
        employee_size_range == "204" ~ "No paid employees",
        employee_size_range == "207" ~ "<10 employees",
        employee_size_range == "209" ~ "<20 employees",
        employee_size_range == "210" ~ "<5 employees",
        employee_size_range == "211" ~ "<4 employees",
        employee_size_range == "212" ~ "1-4 employees",
        employee_size_range == "213" ~ "1 employees",
        employee_size_range == "214" ~ "2 employees",
        employee_size_range == "215" ~ "3-4 employees",
        employee_size_range == "219" ~ "0-4 employees",
        employee_size_range == "220" ~ "5-9 employees",
        employee_size_range == "221" ~ "5-6 employees",
        employee_size_range == "222" ~ "7-9 employees",
        employee_size_range == "223" ~ "10-14 employees",
        employee_size_range == "230" ~ "10-19 employees",
        employee_size_range == "231" ~ "10-14 employees",
        employee_size_range == "232" ~ "15-19 employees",
        employee_size_range == "233" ~ "1-19 employees",
        employee_size_range == "235" ~ "20+ employees",
        employee_size_range == "240" ~ "20-99 employees",
        employee_size_range == "241" ~ "20-49 employees",
        employee_size_range == "242" ~ "50-99 employees",
        employee_size_range == "243" ~ "50+ employees",
        employee_size_range == "249" ~ "100-499 employees",
        employee_size_range == "250" ~ "20-499 employees",
        employee_size_range == "251" ~ "<100-249 employees",
        employee_size_range == "252" ~ "250-499 employees",
        employee_size_range == "253" ~ "500+ employees",
        employee_size_range == "254" ~ "500-999 employees",
        employee_size_range == "260" ~ "1000+ employees",
        employee_size_range == "261" ~ "1000-2499 employees",
        employee_size_range == "262" ~ "1000-1499 employees",
        employee_size_range == "263" ~ "1500-2499 employees",
        employee_size_range == "270" ~ "2500+ employees",
        employee_size_range == "271" ~ "2500-4999 employees",
        employee_size_range == "272" ~ "5000-9999 employees",
        employee_size_range == "273" ~ "5000+ employees",
        employee_size_range == "280" ~ "10000+ employees",
        employee_size_range == "281" ~ "10000-24999 employees",
        employee_size_range == "282" ~ "25000-49999 employees",
        employee_size_range == "283" ~ "50000-99999 employees",
        employee_size_range == "290" ~ "100000+ employees",
        employee_size_range == "298" ~ "Covered by administrative records",
        TRUE ~ NA_character_),
      employee_size_range_label = dplyr::case_when(
        stringr::str_extract(employee_size_range_label, "[0-9]{4}") %>% as.numeric >= 1000 ~ "1000+",
        TRUE ~ employee_size_range_label)) %>%
    dplyr::rename(employee_size_range_code = employee_size_range) %>%
    { if (geo == "county") {
        dplyr::select(.,
          year, state, county, employees, employers, annual_payroll,
          industry, employee_size_range_label, employee_size_range_code, naics_code)} 
      else if (geo == "zipcode") {
        dplyr::select(.,
          year, zip_code, employees, employers, annual_payroll,
          industry, employee_size_range_label, employee_size_range_code, naics_code) } }

  high_missingness = cbp %>%
    skimr::skim() %>%
    dplyr::filter(complete_rate < .9) %>%
    dplyr::pull(skim_variable)

  if (length(high_missingness) > 0) {
    warning(
      stringr::str_c("Variables with high missingness in County Business Patterns", ": ",
      base::paste(high_missingness, collapse = ", "))) } 

  return(cbp)
}

utils::globalVariables(
  c("EMP", "EMPSZES", "ESTAB", "NAICS2017_LABEL", "PAYANN", "annual_payroll",
    "employee_size_range", "employee_size_range_code", "employee_size_range_label",
    "employees", "employers", "industry", "values_code", "values_label", "naics_code"))
