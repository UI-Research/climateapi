#' Get county government revenues and expenditures
#'
#' @description Retrieves county government revenues and expenditures over time.
#'
#' @details Data are from the U.S. Census Bureau's Annual Survey of State and Local
#'   Government Finances and Census of Governments, compiled per: Pierson K., Hand M.,
#'   and Thompson F. (2015). The Government Finance Database: A Common Resource for
#'   Quantitative Research in Public Financial Analysis. PLoS ONE doi:
#'   10.1371/journal.pone.0130119. See \url{https://my.willamette.edu/site/mba/public-datasets}.
#' 
#'   This is a survey in most years--i.e., only a subset of the full population of governments
#'   comprises the sampling frame--but is a true census with relatively high response rates (90%%+)
#'   in years ending in 2 and 7.
#'
#'   Definitions of key constructs:
#'
#'   **Revenues**
#'   - General revenue: All revenue not arising from utilities, liquor stores, or social insurance.
#'   - Intergovernmental revenue: Transfers from federal, state, or other local governments.
#'   - Current charges: Fees collected for providing services.
#'
#'   **Expenditures**
#'   - General expenditure: All spending except utilities, liquor stores, or social insurance.
#'   - Capital outlay: Spending on construction and equipment.
#'
#' @return A dataframe containing county government-level revenues and expenditures by year.
#'   All monetary values are in thousands of dollars.
#'   \describe{
#'     \item{year}{Year of the financial data.}
#'     \item{GEOID}{Five-digit county FIPS code.}
#'     \item{county_name}{Name of the county.}
#'     \item{revenue_total}{Total county revenue.}
#'     \item{revenue_general}{General revenue (excludes utilities, liquor stores, social insurance).}
#'     \item{revenue_tax_total}{Total tax revenue.}
#'     \item{revenue_tax_property}{Property tax revenue.}
#'     \item{revenue_tax_sales}{Sales and gross receipts tax revenue.}
#'     \item{revenue_tax_income}{Income tax revenue.}
#'     \item{revenue_intergovernmental_total}{Total intergovernmental transfers received.}
#'     \item{revenue_intergovernmental_federal}{Federal intergovernmental transfers.}
#'     \item{revenue_intergovernmental_state}{State intergovernmental transfers.}
#'     \item{revenue_charges_total}{Total charges and miscellaneous general revenue.}
#'     \item{revenue_utility_total}{Utility revenue.}
#'     \item{expenditure_total}{Total county expenditures.}
#'     \item{expenditure_general}{General expenditures.}
#'     \item{expenditure_capital_outlay}{Capital outlay spending.}
#'     \item{expenditure_construction}{Construction spending.}
#'     \item{expenditure_salaries_wages}{Salaries and wages.}
#'     \item{expenditure_intergovernmental}{Intergovernmental transfers paid.}
#'     \item{expenditure_education_total}{Education spending.}
#'     \item{expenditure_public_safety}{Police protection spending.}
#'     \item{expenditure_health_hospitals}{Health spending.}
#'     \item{expenditure_highways}{Highway spending.}
#'     \item{expenditure_public_welfare}{Public welfare spending.}
#'     \item{expenditure_utility_total}{Utility expenditures.}
#'   }
#' @export

get_government_finances = function() {

  file_path_base = file.path(get_box_path(), "government-units", "government-finance-database", "gfd_2026_02_04.csv")
  if (!file.exists(file_path_base)) { stop("File does not exist at specified path.") }

  df1 = readr::read_csv(file_path_base) %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    ## Remove duplicate construct of total_ltd_out
    dplyr::select(-dplyr::any_of("total_long_term_debt_out"))

  df_renamed = df1 %>%
    ## Step 1: Expand common abbreviations across all columns
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ .x %>%
        stringr::str_replace_all(c(
          "^tot_" = "total_",
          "_tot_" = "_total_",
          "_tot$" = "_total",
          "_igr_" = "_intergovernmental_",
          "_igr$" = "_intergovernmental",
          "^igr_" = "intergovernmental_",
          "_ig_" = "_intergovernmental_",
          "_ig$" = "_intergovernmental",
          "^ig_" = "intergovernmental_",
          "_trans_" = "_transit_",
          "_educ_" = "_education_",
          "_educ$" = "_education",
          "^educ_" = "education_",
          "^elem_" = "elementary_",
          "_elem_" = "_elementary_",
          "_elec_" = "_electric_",
          "_chg_" = "_charges_",
          "_chg$" = "_charges",
          "_ret_" = "_retirement_",
          "_prop_" = "_property_",
          "_nec_" = "_not_elsewhere_classified_",
          "_nec$" = "_not_elsewhere_classified",
          "_ltd_" = "_long_term_debt_",
          "_ltd$" = "_long_term_debt",
          "_unemp_" = "_unemployment_",
          "^emp_" = "employee_",
          "_emp_" = "_employee_",
          "_ffc_" = "_full_faith_credit_",
          "_ffc$" = "_full_faith_credit",
          "_ng_" = "_nonguaranteed_",
          "_ng$" = "_nonguaranteed",
          "_util_" = "_utility_",
          "_util$" = "_utility",
          "_iss_" = "_issued_",
          "_iss$" = "_issued",
          "_fed_" = "_federal_",
          "_fed$" = "_federal",
          "^fed_" = "federal_",
          "_lic_" = "_license_",
          "_lic$" = "_license",
          "_misc_" = "_miscellaneous_",
          "_misc$" = "_miscellaneous",
          "_gen_" = "_general_",
          "_gen$" = "_general",
          "_cur_" = "_current_",
          "_cur$" = "_current",
          "_cap_" = "_capital_",
          "_cap$" = "_capital",
          "_out_" = "_outstanding_",
          "_out$" = "_outstanding",
          "welf_" = "welfare_",
          "__+" = "_"))) %>%
    ## Step 2: Prefix revenue columns
    dplyr::rename_with(
      .cols = dplyr::matches("_rev_|_rev$|^total_revenue"),
      .fn = ~ stringr::str_c("revenue_", .x) %>%
        stringr::str_remove_all("_rev_|_rev$") %>%
        stringr::str_replace("revenue_total_revenue", "revenue_total")) %>%
    ## Step 3: Prefix tax columns (subset of revenue)
    dplyr::rename_with(
      .cols = dplyr::matches("_tax$|_taxes$|^total_taxes"),
      .fn = ~ stringr::str_c("revenue_tax_", .x) %>%
        stringr::str_remove_all("_tax$|_taxes$") %>%
        stringr::str_replace("revenue_tax_total_taxes", "revenue_tax_total")) %>%
    ## Step 4: Prefix expenditure columns
    dplyr::rename_with(
      .cols = dplyr::matches("_exp_|_exp$|_expend$|^total_expend"),
      .fn = ~ stringr::str_c("expenditure_", .x) %>%
        stringr::str_remove_all("_exp_|_exp$|_expend$|_expend_") %>%
        stringr::str_replace("expenditure_total_expenditure", "expenditure_total")) %>%
    ## Step 5: Clean up any double underscores or trailing underscores
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ .x %>%
        stringr::str_replace_all("__+", "_") %>%
        stringr::str_remove("_$"))
  
  df_final = df_renamed %>%
    dplyr::transmute(
      year = year4,
      GEOID = stringr::str_c(fips_code_state, fips_county),
      county_name = name,
      ## Total revenues and major subtotals
      revenue_total,
      revenue_general = general_revenue,
      revenue_tax_total,
      revenue_tax_property,
      revenue_tax_sales = revenue_tax_total_sales_gr_rec,
      revenue_tax_income = revenue_tax_total_income,
      revenue_intergovernmental_total = total_intergovernmental_revenue,
      revenue_intergovernmental_federal = total_federal_intergovernmental_revenue,
      revenue_intergovernmental_state = total_state_intergovernmental_revenue,
      ## Total expenditures and major subtotals
      expenditure_total,
      expenditure_general = general_expenditure#,
      # expenditure_capital_outlay = expenditure_total_capital_outlay,
      # expenditure_construction = expenditure_total_construction,
      # expenditure_salaries_wages = expenditure_total_salaries_wages,
      # expenditure_intergovernmental = expenditure_total_intergovernmental,
      # expenditure_education_total = expenditure_total_education,
      # expenditure_public_safety = expenditure_total_police_protection,
      # expenditure_health_hospitals = expenditure_total_health,
      # expenditure_highways = expenditure_total_highways,
      # expenditure_public_welfare = expenditure_total_public_welfare,
      # expenditure_utility_total = expenditure_total_utility
    )

  return(df_final)
}

utils::globalVariables(c(
  "year4", "fips_code_state", "fips_county", "name",
  "revenue_total", "revenue_total_general", "revenue_tax_total", "revenue_tax_property",

  "revenue_tax_total_sales_gross_receipts", "revenue_tax_total_income",
 "revenue_total_intergovernmental", "revenue_total_federal_intergovernmental",
  "revenue_total_state_intergovernmental", "revenue_total_charges_miscellaneous_general",
  "revenue_total_utility",
  "expenditure_total", "expenditure_total_general", "expenditure_total_capital_outlay",
  "expenditure_total_construction", "expenditure_total_salaries_wages",
  "expenditure_total_intergovernmental", "expenditure_total_education",
  "expenditure_total_police_protection", "expenditure_total_health",
  "expenditure_total_highways", "expenditure_total_public_welfare",
  "expenditure_total_utility"))
