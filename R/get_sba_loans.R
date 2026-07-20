#' @importFrom magrittr %>%

#' @title Access SBA data on disaster loans
#' @description Retrieves Small Business Administration (SBA) disaster loan data
#'   for both home and business loans at the city and zip code level.
#'
#' @details Data are sourced from the SBA's disaster loan reports. See
#'   \url{https://www.sba.gov/funding-programs/disaster-assistance}.
#'
#'   The FY16 source workbook (`sba_disaster_loan_data_fy16.xlsx`) is anomalous: its Home
#'   and Business sheets have identical row counts and dollar totals, suggesting possible
#'   ~2x double-counting in the source file itself. This function selects the correct
#'   Home/Business sheet by name (avoiding the sheet-order swap present in this vintage),
#'   but the underlying FY16 data should be manually re-verified against
#'   \url{https://data.sba.gov} before being trusted.
#'
#' @returns A dataframe comprising city- and zip-level data on SBA loanmaking.
#'   Columns include:
#'   \describe{
#'     \item{fiscal_year}{The federal fiscal year of the loan.}
#'     \item{disaster_description}{Text description of the disaster (only populated for
#'        older, FY01-FY03 vintages; NA otherwise).}
#'     \item{disaster_number_fema}{FEMA disaster number associated with the loan.}
#'     \item{disaster_number_sba_physical}{SBA physical disaster declaration number.}
#'     \item{disaster_number_sba}{SBA disaster declaration number (distinct from
#'        `disaster_number_sba_physical`).}
#'     \item{disaster_number_sba_eidl}{SBA Economic Injury Disaster Loan (EIDL) declaration number.}
#'     \item{damaged_property_zip_code}{ZIP code of the damaged property.}
#'     \item{damaged_property_city_name}{City name of the damaged property.}
#'     \item{damaged_property_state_code}{Two-letter state abbreviation.}
#'     \item{verified_loss_total}{Total verified loss amount in dollars.}
#'     \item{verified_loss_real_estate}{Verified loss amount for real estate in dollars.}
#'     \item{verified_loss_content}{Verified loss amount for contents in dollars.}
#'     \item{approved_amount_total}{Total approved loan amount in dollars.}
#'     \item{approved_amount_real_estate}{Approved loan amount for real estate in dollars.}
#'     \item{approved_amount_content}{Approved loan amount for contents in dollars.}
#'     \item{approved_amount_eidl}{Approved EIDL amount in dollars (business loans only; NA for residential).}
#'     \item{loan_type}{Type of loan: "residential" or "business".}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' get_sba_loans()
#' }
get_sba_loans = function() {
  inpath = file.path(get_box_path(), "hazards", "sba", "disaster-loans")

  if (! file.exists(inpath)) {
    stop("The path to the data does not exist.") }

  ## selects the Home or Business sheet by name (not position): some vintages (e.g. FY16)
  ## have their Home/Business sheets in a swapped order relative to other years, which
  ## silently mislabeled loan_type for every FY16 row when sheets were selected by position
  find_sheet_index = function(file_path, loan_type_pattern) {
    sheet_names = readxl::excel_sheets(file_path)
    matches = which(stringr::str_detect(sheet_names, stringr::regex(stringr::str_c("^FY.*", loan_type_pattern), ignore_case = TRUE)))

    if (length(matches) != 1) {
      stop(stringr::str_c(
        "Expected exactly one '", loan_type_pattern, "' data sheet in ", basename(file_path),
        ", found ", length(matches), ". Sheet names: ", stringr::str_c(sheet_names, collapse = ", "))) }

    matches
  }

  read_loan_sheet = function(file_path, loan_type_pattern) {
    readxl::read_xlsx(file_path, sheet = find_sheet_index(file_path, loan_type_pattern), skip = 4) %>%
      janitor::clean_names() %>%
      dplyr::mutate(fiscal_year = stringr::str_extract(file_path, "fy[0-9]{2}") %>% stringr::str_replace("fy", "20"))
  }

  home_loans = list.files(inpath, full.names = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "xlsx")) %>%
    purrr::map(~ read_loan_sheet(.x, "Home")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      fema_disaster_number = dplyr::if_else(is.na(fema_disaster_number), fema_disaster, fema_disaster_number),
      sba_disaster_number = dplyr::if_else(is.na(sba_disaster_number), sba_disaster, sba_disaster_number),
      sba_eidl_declaration_number = dplyr::if_else(is.na(sba_eidl_declaration_number), sba_eidl_declaration, sba_eidl_declaration_number),
      damaged_property_zip_code = dplyr::if_else(is.na(damaged_property_zip_code), damaged_property_zip, damaged_property_zip_code),
      damaged_property_city_name = dplyr::if_else(is.na(damaged_property_city_name), damaged_property_city, damaged_property_city_name),
      ## coalesced in place (not into a separate shadow column) so the cleanup steps
      ## below -- which target this same column -- operate on the coalesced value
      damaged_property_state_code = dplyr::if_else(is.na(damaged_property_state_code), damaged_property_state, damaged_property_state_code),
      total_approved_loan_amount = dplyr::if_else(is.na(total_approved_loan_amount), total_approved, total_approved_loan_amount),
      approved_amount_real_estate = dplyr::if_else(is.na(approved_amount_real_estate), approved_amount_real, approved_amount_real_estate),
      total_verified_loss = dplyr::if_else(is.na(total_verified_loss), total_verified, total_verified_loss)) %>%
    dplyr::select(-dplyr::any_of(c(
      "fema_disaster", "sba_disaster", "sba_eidl_declaration", "damaged_property_zip", "damaged_property_city",
      "damaged_property_state", "total_approved", "damaged_property_county_parish_name", "total_verified",
      "approved_amount_real", "approved_amount_eidl"))) %>%
    dplyr::rename(
      disaster_number_fema = fema_disaster_number,
      disaster_number_sba = sba_disaster_number,
      disaster_number_sba_physical = sba_physical_declaration_number,
      disaster_number_sba_eidl = sba_eidl_declaration_number,
      verified_loss_total = total_verified_loss,
      approved_amount_total = total_approved_loan_amount)

  business_loans = list.files(inpath, full.names = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "xlsx")) %>%
    purrr::map(~ read_loan_sheet(.x, "Business")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      fema_disaster_number = dplyr::if_else(is.na(fema_disaster_number), fema_disaster, fema_disaster_number),
      sba_disaster_number = dplyr::if_else(is.na(sba_disaster_number), sba_disaster, sba_disaster_number),
      sba_eidl_declaration_number = dplyr::if_else(is.na(sba_eidl_declaration_number), sba_eidl_declaration, sba_eidl_declaration_number),
      damaged_property_zip_code = dplyr::if_else(is.na(damaged_property_zip_code), damaged_property_zip, damaged_property_zip_code),
      damaged_property_city_name = dplyr::if_else(is.na(damaged_property_city_name), damaged_property_city, damaged_property_city_name),
      damaged_property_state_code = dplyr::if_else(is.na(damaged_property_state_code), damaged_property_state, damaged_property_state_code),
      total_approved_loan_amount = dplyr::if_else(is.na(total_approved_loan_amount), total_approved, total_approved_loan_amount),
      approved_amount_real_estate = dplyr::if_else(is.na(approved_amount_real_estate), approved_amount_real, approved_amount_real_estate),
      total_verified_loss = dplyr::if_else(is.na(total_verified_loss), total_verified, total_verified_loss)) %>%
    dplyr::select(-dplyr::any_of(c(
      "fema_disaster", "sba_disaster", "sba_eidl_declaration", "damaged_property_zip", "damaged_property_city",
      "damaged_property_state", "total_approved", "damaged_property_county_parish_name", "total_verified",
      "approved_amount_real"))) %>%
    dplyr::rename(
      disaster_number_fema = fema_disaster_number,
      disaster_number_sba = sba_disaster_number,
      disaster_number_sba_physical = sba_physical_declaration_number,
      disaster_number_sba_eidl = sba_eidl_declaration_number,
      verified_loss_total = total_verified_loss,
      approved_amount_total = total_approved_loan_amount)

  result = dplyr::bind_rows(
      business_loans %>% dplyr::mutate(loan_type = "business"),
      home_loans %>% dplyr::mutate(loan_type = "residential")) %>%
    dplyr::mutate(
      ## state codes should be characters, not numbers (e.g., "AL"); as.numeric() on a
      ## real state abbreviation returns NA, so this only clears out numeric junk
      damaged_property_state_code = dplyr::if_else(
        !is.na(suppressWarnings(as.numeric(damaged_property_state_code))), NA, damaged_property_state_code),
      ## in the raw data, zip/city are sometimes transposed; as.numeric(city) should be
      ## NA if city is accurate, so this substitution is safe
      damaged_property_zip_code = dplyr::case_when(
        is.na(suppressWarnings(as.numeric(damaged_property_zip_code))) &
          !is.na(suppressWarnings(as.numeric(damaged_property_city_name))) ~
          as.character(suppressWarnings(as.numeric(damaged_property_city_name))),
        TRUE ~ damaged_property_zip_code),
      ## pad every purely-numeric zip <= 5 characters to 5 digits (not just Puerto Rico --
      ## MA/NJ/CT/ME/VT/NH/RI/VI have the identical leading-zero problem) and truncate
      ## 9-digit ZIP+4 codes to the 5-digit ZIP; anything still not numeric is set to NA
      damaged_property_zip_code = dplyr::if_else(
        !is.na(suppressWarnings(as.numeric(damaged_property_zip_code))),
        damaged_property_zip_code %>% stringr::str_sub(1, 5) %>% stringr::str_pad(width = 5, side = "left", pad = "0"),
        NA_character_),
      damaged_property_state_code = dplyr::case_when(
        ## we infer state codes from disaster codes, where state codes are prefixed on the disaster number
        is.na(damaged_property_state_code) & stringr::str_detect(disaster_number_sba, "^[A-Z]{2}-") ~ stringr::str_sub(disaster_number_sba, 1, 2),
        is.na(damaged_property_state_code) & stringr::str_detect(disaster_number_fema, "^[A-Z]{2}-") ~ stringr::str_sub(disaster_number_fema, 1, 2),
        ## another transposition issue; we only use the city name when it is two, uppercase characters
        is.na(damaged_property_state_code) & stringr::str_detect(damaged_property_city_name, "^[A-Z]{2}$") ~ damaged_property_city_name,
        TRUE ~ damaged_property_state_code),
      damaged_property_city_name = dplyr::case_when(
        ## yet another raw data transposition issue
        stringr::str_detect(damaged_property_city_name, "^[A-Z]{2}$") & !stringr::str_detect(disaster_number_sba, "-") ~ disaster_number_sba,
        TRUE ~ damaged_property_city_name)) %>%
    dplyr::filter(
      ## these are weird, meaningless records that are either embedded in the raw data
      ## or that are accidentally created as rows when data are read-in from file. EIDL-only
      ## loans legitimately have no physical declaration number (NA), so those must be kept.
      is.na(disaster_number_sba_physical) |
        !stringr::str_detect(disaster_number_sba_physical, "Business Data Only|United States Small Business|Home Data Only"))

  return(result)
}

utils::globalVariables(c(
  "fema_disaster_number", "sba_disaster_number", "sba_eidl_declaration_number",
  "damaged_property_zip_code", "damaged_property_city_name", "damaged_property_state_code",
  "total_approved_loan_amount", "approved_amount_real_estate", "total_verified_loss",
  "disaster_number_fema", "disaster_number_sba_physical", "disaster_number_sba", "disaster_number_sba_eidl",
  "verified_loss_total", "approved_amount_total", "approved_amount_eidl", "approved_amount_real",
  "damaged_property_city", "damaged_property_county_parish_name", "damaged_property_state",
  "damaged_property_zip", "fema_disaster", "sba_disaster", "sba_eidl_declaration",
  "sba_physical_declaration_number", "total_approved", "total_verified"))
