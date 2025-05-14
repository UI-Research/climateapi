#' Obtain County Business Patterns (CBP) Estimates per County
#'
#' @return A tibble with data on county-level employees, employers, and aggregate annual payrolls by industry and employer size
#' @export
#'
#' @examples
#' \dontrun{
#' get_business_patterns()
#' }
get_business_patterns = function() {
  naics_codes = censusapi::listCensusMetadata(
    name = "cbp",
    vintage = "2022",
    type = "variables",
    include_values = TRUE)

  ## only selecting high-level (two-digit) NAICS codes
  ## though this could be easily modified to accept any/all NAICS codes
  ## (though the query will get progressively slower as we supply more codes,
  ## or so I would assume)
  naics_codes_to_query = naics_codes %>%
    dplyr::filter(nchar(values_code) == 2) %>%
    dplyr::pull(values_code)

  cbp = purrr::map_dfr(
    naics_codes_to_query,
    ## some high-level codes (92 and 94) error with a "No data to return"
    ## message, so we wrap this in a tryCatch. this doesn't appear to be an
    ## error with our query--there's no data for these codes on data.census.gov
    ## either
    ~ tryCatch({
      censusapi::getCensus(
        name = "cbp",
        vintage = 2022,
        vars = c(
          "EMP",
          "ESTAB",
          "PAYANN",
          "EMPSZES",
          "NAICS2017_LABEL"),
        region = "county:*",
        NAICS2017 = .x)},
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
      industry = NAICS2017_LABEL) %>%
    dplyr::mutate(
      industry = industry %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(c(" " = "_", ",|\\(|\\)|_for_all_sectors|and_" = "")),
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
    dplyr::select(state, county, employees, employers, annual_payroll, industry, employee_size_range_label, employee_size_range_code)

  return(cbp)
}

utils::globalVariables(
  c("EMP", "EMPSZES", "ESTAB", "NAICS2017_LABEL", "PAYANN", "annual_payroll",
    "employee_size_range", "employee_size_range_code", "employee_size_range_label",
    "employees", "employers", "industry", "values_code"))
