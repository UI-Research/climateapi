# Author: Will Curran-Groome

#' @importFrom magrittr %>%

#' @title Access SBA data on disaster loans

#' @returns A dataframe comprising city- and zip-level data on SBA loanmaking
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

  home_loans = list.files(inpath, full.names = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "xlsx")) %>%
    purrr::map(~
      readxl::read_xlsx(.x, sheet = 4, skip = 4) %>%
      janitor::clean_names() %>%
      dplyr::mutate(fiscal_year = stringr::str_extract(.x, "fy[0-9]{2}") %>% stringr::str_replace("fy", "20"))) %>%
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
    dplyr::select(-c(
      fema_disaster, sba_disaster, sba_eidl_declaration, damaged_property_zip, damaged_property_city,
      damaged_property_state, total_approved, damaged_property_county_parish_name, total_verified,
      approved_amount_real, approved_amount_eidl)) %>%
    dplyr::rename(
      disaster_number_fema = fema_disaster_number,
      disaster_number_sba_physical = sba_physical_declaration_number,
      disaster_number_sba_eidl = sba_eidl_declaration_number,
      verified_loss_total = total_verified_loss,
      approved_amount_total = total_approved_loan_amount)

  business_loans = list.files(inpath, full.names = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "xlsx")) %>%
    purrr::map(~
      readxl::read_xlsx(.x, sheet = 5, skip = 4) %>%
      janitor::clean_names() %>%
      dplyr::mutate(fiscal_year = stringr::str_extract(.x, "fy[0-9]{2}") %>% stringr::str_replace("fy", "20"))) %>%
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
    dplyr::select(-c(
      fema_disaster, sba_disaster, sba_eidl_declaration, damaged_property_zip, damaged_property_city,
      damaged_property_state, total_approved, damaged_property_county_parish_name, total_verified,
      approved_amount_real)) %>%
    dplyr::rename(
      disaster_number_fema = fema_disaster_number,
      disaster_number_sba_physical = sba_physical_declaration_number,
      disaster_number_sba_eidl = sba_eidl_declaration_number,
      verified_loss_total = total_verified_loss,
      approved_amount_total = total_approved_loan_amount)

  result = dplyr::bind_rows(
    business_loans %>% dplyr::mutate(loan_type = "business"),
    home_loans %>% dplyr::mutate(loan_type = "residential")) %>%
    ## these are weird, meaningless records that are either embedded in the raw data
    ## or that are accidentally created as rows when data are read-in from file
    dplyr::filter(!stringr::str_detect(disaster_number_sba_physical, "Business Data Only|United States Small Business"))

  return(result)
}

utils::globalVariables(c(
  "fema_disaster_number", "sba_disaster_number", "sba_eidl_declaration_number",
  "damaged_property_zip_code", "damaged_property_city_name", "damaged_property_state_code",
  "total_approved_loan_amount", "approved_amount_real_estate", "total_verified_loss",
  "disaster_number_fema", "disaster_number_sba_physical", "disaster_number_sba_eidl",
  "verified_loss_total", "approved_amount_total", "approved_amount_eidl", "approved_amount_real",
  "damaged_property_city", "damaged_property_county_parish_name", "damaged_property_state",
  "damaged_property_zip", "fema_disaster", "sba_disaster", "sba_eidl_declaration",
  "sba_physical_declaration_number", "total_approved", "total_verified"))
