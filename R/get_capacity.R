#' Get government unit-level expenses from the Census of Governments
#'
#' @param year A four-digit year. The default is 2022.
#'
#' @return A dataframe containing government unit-level expenses for the specified year.
#' @export

get_government_finances = function(year = 2022) {

  file_path_base = file.path(get_box_path(), "sociodemographics", "census-of-governments")

  year = stringr::str_sub(year, 1, 2)

  ## this contains expense descriptions that map to the expense codes that are
  ## included in the raw dataset
  item_crosswalk = readr::read_csv(
    file.path(file_path_base, "item_crosswalk.csv"))

  ## this contains the names and other metadata for each unit of government
  government_unit_crosswalk = readr::read_fwf(
    file = file.path(file_path_base, "Fin_PID_2022.txt"),
    col_positions = readr::fwf_widths(
      widths = c(2, 1, 3, 6, 64, 35, 5, 9, 2, 7, 2, 2, 2, 4, 2),
      col_names = c(
        "state_code", "government_type", "county_code", "unit_id", "unit_name",
        "county_name", "place_code", "population", "population_year",
        "enrollment", "enrollment_year", "function_code", "school_level_code",
        "fiscal_year_ending", "data_year"))) %>%
    dplyr::select(-c(government_type, data_year))

  ## each row corresponds to a finance item (generally, a class of expenditures)
  ## for a single government unit in a single year
  data = readr::read_fwf(
    file = file.path(file_path_base, "2022FinEstDAT_09202024modp_pu.txt"),
    col_positions = readr::fwf_widths(
      widths = c(2, 1, 3, 6, 3, 12, 4, 1),
      col_names = c(
        "state_code", "government_type", "county_code", "unit_id", "item", "amount_thousands",
        "year_data", "imputation_type"))) %>%
    dplyr::select(-c(state_code, county_code)) %>%
    dplyr::mutate(
      government_type = dplyr::case_when(
        government_type == 0 ~ "State",
        government_type == 1 ~ "County",
        government_type == 2 ~ "City",
        government_type == 3 ~ "Township",
        government_type == 4 ~ "Special District",
        government_type == 5 ~ "School District/Educational Service Agency"),
      imputation_type = dplyr::case_when(
        imputation_type == "A" ~ "Analyst correction",
        imputation_type == "I" ~ "Imputed",
        imputation_type == "M" ~ "Unknown",
        imputation_type == "N" ~ "Not applicable",
        imputation_type == "R" ~ "Reported",
        imputation_type == "S" ~ "Alternative source")) %>%
    dplyr::left_join(item_crosswalk, by = c("item" = "code")) %>%
    dplyr::select(-item,cexpense_description = description)

  data_by_unit = data %>%
    dplyr::group_by(unit_id, year_data, imputation_type) %>%
    dplyr::summarize(
        count = dplyr::n(),
        amount_thousands = sum(amount_thousands, na.rm = TRUE),
        dplyr::across(.cols = c(dplyr::matches("code"), dplyr::matches("name"), government_type), dplyr::first),
      .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = imputation_type,
      values_from = count,
      values_fill = 0,
      names_prefix = "imputation_") %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      data_quality = imputation_reported /
        (imputation_reported + imputation_imputed + imputation_analyst_correction + imputation_unknown + imputation_alternative_source)) %>%
    dplyr::select(-dplyr::matches("imputation")) %>%
    dplyr::left_join(government_unit_crosswalk, by = "unit_id") %>%
    dplyr::mutate(amount_per_capita = dplyr::if_else(
      is.na(enrollment),
      amount_thousands * 1000 / population,
      amount_thousands * 1000 / enrollment))

  return(data_by_unit)
}

utils::globalVariables(c(
  "amount_thousands", "data_year", "description", "enrollment", "government_type",
  "imputation_alternative_source", "imputation_analyst_correction", "imputation_imputed",
  "imputation_reported", "imputation_type", "imputation_unknown", "item", "population",
  "unit_id", "year_data"))
