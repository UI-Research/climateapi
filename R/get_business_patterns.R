#' @importFrom magrittr %>%

#' @title Get NAICS Codes for County Business Patterns
#'
#' @description A utility function to programmatically identify and select NAICS
#'     codes for use with `get_business_patterns()`. This is a wrapper around
#'     `censusapi::listCensusMetadata(name = "cbp")`.
#'
#' @param year The vintage year for NAICS codes. Data are available from 2008 through 2023.
#'     Years 1986-2007 use SIC or older NAICS classification systems that are not currently
#'     supported. Default is 2023.
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
#' get_naics_codes(year = 2023, digits = 2)
#'
#' # Get all 3-digit NAICS codes (default)
#' get_naics_codes(year = 2022)
#'
#' # Get 4-digit NAICS codes for a specific year
#' get_naics_codes(year = 2020, digits = 4)
#' }
get_naics_codes <- function(year = 2023, digits = 3) {
  if (year < 2008) { stop("Year must be 2008 or later. Years 1986-2007 use SIC or older NAICS classification systems that are not currently supported.") }
  if (year > 2023) { stop("Most recent year for data is 2023.") }
  if (!digits %in% 2:6) { stop("`digits` must be between 2 and 6.") }

  naics_metadata <- get_cbp_naics_metadata(year) |>
    dplyr::filter(nchar(values_code) == digits) |>
    dplyr::transmute(
      naics_code = values_code,
      naics_label = values_label,
      year = year) |>
    dplyr::distinct() |>
    dplyr::arrange(naics_code) |>
    tibble::as_tibble()

}

#' Get CBP NAICS code/label metadata, falling back to a bundled reference table for
#' 2008-2011 vintages where the Census API serves no code/label lookup at all
#'
#' @param year The CBP vintage year.
#'
#' @return A tibble with (at least) `values_code` and `values_label` columns, matching
#'   the schema `censusapi::listCensusMetadata(..., include_values = TRUE)` returns for
#'   vintages where a lookup table is available.
#' @noRd
get_cbp_naics_metadata = function(year) {
  naics_metadata_raw = censusapi::listCensusMetadata(
      name = "cbp",
      vintage = as.character(year),
      type = "variables",
      include_values = TRUE)

  if ("values_code" %in% colnames(naics_metadata_raw)) {
    naics_metadata_raw %>%
      dplyr::filter(!is.na(values_code)) %>%
      tibble::as_tibble()
  } else {
    ## years 2008-2011: the Census API returns unlabeled variable definitions only for
    ## this vintage (no values_code/values_label columns at all); fall back to Census's
    ## own official, bundled NAICS2007 code/label reference table
    readr::read_csv(
        system.file("extdata", "naics2007_codes.csv", package = "climateapi"),
        show_col_types = FALSE) %>%
      dplyr::rename(values_code = naics_code, values_label = naics_label) }
}

#' @title Obtain County Business Patterns (CBP) Estimates per County
#'
#' @param year The vintage of CBP data desired. Data are available from 2008-2023.
#'     Earlier years use different NAICS classification systems that are not currently supported.
#'     Default is 2023.
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
#' For `geo = "zipcode"`, Census's ZIP Code Business Patterns (ZBP) only publishes
#' `employees`/`annual_payroll` for `naics_code == "00"` (total, all sectors); the Census API
#' returns a literal `0` for every other NAICS code at the zip-code level, which reflects a
#' suppression convention, not a true absence of establishments. This function coerces those
#' values to `NA` and emits a one-time message explaining this.
#'
#' @return A tibble with data on employees, employers, and aggregate annual payrolls by
#'     industry and employer size. The geographic columns differ by `geo`:
#'     \describe{
#'         \item{year}{the year for which CBP data is pulled from}
#'         \item{state}{(geo = "county" only) A two-digit state identifier.}
#'         \item{county}{(geo = "county" only) A three-digit county identifier.}
#'         \item{zip_code}{(geo = "zipcode" only) A five-digit ZIP code.}
#'         \item{employees}{number of individual employees employed in that particular industry
#'              and establishment size combination. For geo = "zipcode", this is only published
#'              by Census for `naics_code == "00"` (total, all sectors); all other NAICS codes
#'              are NA -- see `@details`.}
#'         \item{employers}{number of establishments of each employment size}
#'         \item{annual_payroll}{total annual payroll expenditures measured in $1,000's of USD.
#'              For geo = "zipcode", this is only published by Census for `naics_code == "00"`
#'              (total, all sectors); all other NAICS codes are NA -- see `@details`.}
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
#'  year = 2023,
#'  naics_code_digits = 3)
#'
#' get_business_patterns(
#'  year = 2017,
#'  naics_codes = c(221111, 221112))
#' }

get_business_patterns = function(year = 2023, geo = "county", naics_code_digits = 2, naics_codes = NULL) {
  if (year < 2008) { stop("Year must be 2008 or later. Earlier years use different data structures not currently supported.") }
  if (year > 2023) { stop("Most recent year for data is 2023.") }
  if (! geo %in% c("county", "zipcode")) { stop("`geo` must be one of 'county' or 'zipcode'.") }
  if (! naics_code_digits %in% c(2, 3)) {
    stop("`naics_code_digits` must be one of c(2, 3). For more detailed codes, explicitly pass desired codes to the `naics_codes` parameter.") }

  # Determine NAICS classification system based on year
  # Census uses different NAICS vintages for different data years

  naics_version <- dplyr::case_when(
    year >= 2017 ~ "NAICS2017",
    year >= 2012 ~ "NAICS2012",
    year >= 2008 ~ "NAICS2007",
    TRUE ~ "NAICS2007"
  )
  ## the 2008-2011 CBP API vintage uses a "_TTL" (title) suffix for the NAICS label
  ## variable rather than the "_LABEL" suffix used from 2012 onward
  naics_label_var <- if (year %in% 2008:2011) {
    paste0(naics_version, "_TTL") } else {
    paste0(naics_version, "_LABEL") }

  ## single source of truth for NAICS code/label metadata, shared with get_naics_codes()
  naics_codes_metadata = get_cbp_naics_metadata(year)

  if (!is.null(naics_codes)) {
    ## sectors 92 and 95 are valid NAICS classification codes but CBP has no data for them
    ## at all; give an honest, distinct error here rather than the generic "not a valid
    ## NAICS code" message below
    requested_92_95 = naics_codes[stringr::str_starts(as.character(naics_codes), "92|95")]
    if (length(requested_92_95) > 0) {
      stop(stringr::str_c(
        "NAICS sector(s) ", stringr::str_c(requested_92_95, collapse = ", "),
        " (92/95) have no CBP data available -- these sectors are excluded from Census's ",
        "County Business Patterns series entirely (see `@details`).")) }

    naics_code_check = naics_codes_metadata %>%
      dplyr::filter(values_code %in% naics_codes) %>%
      nrow()

    if (naics_code_check < 1) {
      stop("The provided `naics_codes` do not appear to be valid NAICS codes. Refer to https://www.census.gov/naics/ for the relevant vintage's NAICS codes.") }

    if (naics_code_check < length(naics_codes)) {
      warning("Some, but not all, supplied `naics_codes` appear to be valid NAICS codes. Returning data only for valid codes.") }

    naics_code_digits = NULL }

  ## sectors 92 and 95 have no CBP data at all (see above); exclude them from the
  ## digit-based query, which has no user-supplied codes to individually validate/error on
  naics_codes_metadata_queryable = naics_codes_metadata %>%
    dplyr::filter(!stringr::str_starts(values_code, "92|95"))

  if (!is.null(naics_code_digits)) {
    naics_codes_to_query = naics_codes_metadata_queryable %>%
      dplyr::filter(
        nchar(values_code) == naics_code_digits,
        !is.na(values_code)) %>%
      dplyr::pull(values_code) } else {
    naics_codes_to_query = naics_codes_metadata_queryable %>%
      dplyr::filter(
        values_code %in% naics_codes,
        !is.na(values_code)) %>%
      dplyr::pull(values_code) }

  ## fetches CBP data for one NAICS code, nationwide in a single call. Some older CBP
  ## vintages (2008-2011) reject a single nationwide county-level query for exceeding the
  ## Census API's cell limit; when that specific error occurs, fall back to querying
  ## state-by-state instead (some high-level codes also error with "No data to return",
  ## which doesn't appear to be an error with our query -- there's no data for these codes
  ## on data.census.gov either -- so any other error is treated as "no data").
  fetch_cbp_data = function(naics_code) {
    api_args <- list(
      name = "cbp",
      vintage = year,
      vars = c("EMP", "YEAR", "ESTAB", "PAYANN", "EMPSZES", naics_label_var),
      region = paste0(geo, ":*"))
    api_args[[naics_version]] <- naics_code

    tryCatch(
      do.call(censusapi::getCensus, api_args) %>%
        dplyr::mutate(naics_code = naics_code),
      error = function(e) {
        if (geo == "county" && stringr::str_detect(conditionMessage(e), "cell limit")) {
          state_codes = tidycensus::fips_codes$state_code |> unique()

          purrr::map_dfr(
            state_codes,
            function(state_code) {
              state_api_args = api_args
              state_api_args$regionin = stringr::str_c("state:", state_code)
              tryCatch(
                do.call(censusapi::getCensus, state_api_args) %>%
                  dplyr::mutate(naics_code = naics_code),
                error = function(e2) tibble::tibble()) })
        } else { tibble::tibble() } }) }

  cbp = purrr::map_dfr(naics_codes_to_query, fetch_cbp_data) %>%
    # Rename the NAICS label column to a standard name
    dplyr::rename_with(~ "NAICS_LABEL", dplyr::matches("NAICS[0-9]+_(LABEL|TTL)")) %>%
    dplyr::mutate(
      # state,
      # county,
      employees = EMP,
      employers = ESTAB,
      annual_payroll = PAYANN,
      employee_size_range = EMPSZES,
      industry = NAICS_LABEL,
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

  ## Census's ZIP Code Business Patterns (ZBP) only publishes employees/annual_payroll for
  ## naics_code == "00" (total, all sectors); every other NAICS code returns a literal 0 at
  ## the zip-code level, which reflects a suppression convention, not a true absence of
  ## establishments -- coerce these to NA and disclose the convention once per call
  if (geo == "zipcode" && any(cbp$naics_code != "00", na.rm = TRUE)) {
    message(stringr::str_c(
      "For geo = 'zipcode', Census's ZIP Code Business Patterns (ZBP) only publishes ",
      "`employees`/`annual_payroll` for NAICS code '00' (total, all sectors); every other ",
      "NAICS code returns a literal 0 from the Census API for these fields, which is NOT a ",
      "true zero -- it reflects a suppression convention, not an absence of establishments. ",
      "These values have been coerced to NA."))

    cbp = cbp %>%
      dplyr::mutate(
        employees = dplyr::if_else(naics_code != "00", NA_real_, employees),
        annual_payroll = dplyr::if_else(naics_code != "00", NA_real_, annual_payroll)) }

  return(cbp %>% tibble::as_tibble())
}

utils::globalVariables(
  c("EMP", "EMPSZES", "ESTAB", "NAICS2017_LABEL", "NAICS2012_LABEL", "NAICS2007_LABEL",
    "NAICS_LABEL", "PAYANN", "annual_payroll",
    "employee_size_range", "employee_size_range_code", "employee_size_range_label",
    "employees", "employers", "industry", "values_code", "values_label", "naics_code",
    "naics_label"))
