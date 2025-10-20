#' Obtain County Business Patterns (CBP) Estimates per County
#'
#' @param year The vintage of CBP data desired. Data are available from 1986, though this function likely only supports more recent years (it it tested on 2022-vintage data only). Default is 2022.
#' @param naics_code_digits One of c(2, 3). Default is 2. NAICS codes range in specificity; 2-digit codes describe the highest groupings of industries, while six-digit codes are exceedingly detailed. There are 20 2-digit NAICS codes and 196 3-digit codes.
#' @param naics_codes A vector of NAICS codes to query. If NULL, the function will query all available codes with the specified number of digits. If not NULL, this argument overrides the `naics_code_digits` argument.
#' @return A tibble with data on county-level employees, employers, and aggregate annual payrolls by industry and employer size
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

get_business_patterns = function(year = 2022, naics_code_digits = 2, naics_codes = NULL) {
  if (year < 1986) { stop("Year must be 1986 or later.") }
  if (! naics_code_digits %in% c(2, 3)) {
    stop("`naics_code_digits` must be one of c(2, 3). For more detailed codes, explicitly pass desired codes to the `naics_codes` parameter.") }

  naics_codes_metadata = censusapi::listCensusMetadata(
    name = "cbp",
    vintage = "2022",
    type = "variables",
    include_values = TRUE) %>%
    dplyr::filter(!stringr::str_starts(values_code, "92|95")) ## BC: filter out codes 92 and 95 which do not appear to have data associated and don't appear on the census list of naics codes https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/naics-descriptions/naics2017.txt


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
          "ESTAB",
          "PAYANN",
          "EMPSZES",
          "NAICS2017_LABEL"),
        region = "county:*",
        NAICS2017 = .x) %>%
        mutate(naics_code = .x)},
      error = function(e) {
        message("Error in NAICS2017: ", .x)
        return(tibble::tibble())})) %>%
    dplyr::select(
      state,
      county,
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
    dplyr::select(year, state, county, employees, employers, annual_payroll, industry, employee_size_range_label, employee_size_range_code, naics_code)

  return(cbp)
}

utils::globalVariables(
  c("EMP", "EMPSZES", "ESTAB", "NAICS2017_LABEL", "PAYANN", "annual_payroll",
    "employee_size_range", "employee_size_range_code", "employee_size_range_label",
    "employees", "employers", "industry", "values_code"))
