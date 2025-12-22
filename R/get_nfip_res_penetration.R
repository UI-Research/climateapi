#' @title Access residential National Flood Insurance Program (NFIP) penetration rates.
#' @param county_geoids A state name. NULL by default.
#' @param file_name The name (not the full path) of the Box file containing the raw data.
#'
#' @details
#' These data are from: https://www.fema.gov/openfema-data-page/nfip-residential-penetration-rates-v1.
#' Per FEMA: NFIP "take-up rates" or residential penetration rates are an estimate of the percentage
#' of total residential structures covered by an NFIP policy.
#' The NFIP residential penetration rate within an area, such as a county,
#' is the ratio of insured residential structures to total residential structures in that area.
#' Private flood insurance take-up rates are not available to FEMA and therefore,
#' are not included in these estimates.
#'
#' @returns A data frame comprising county-level data on Residential NFIP penetration rates.
#'  \describe{
#'    \item{state}{A state name}
#'    \item{county}{The county name.}
#'    \item{res_penetration_rate_sfha}{NFIP insured residential structures in the Special Flood Hazard Area (SFHA) of the county divided by all residential structures in the SFHA of the county.}
#'    \item{res_penetration_rate}{NFIP insured residential structures in the county divided by all residential structures in the county.}
#'    \item{res_contracts_in_force_sfha}{NFIP Insured Residential Policy Contracts in Force (CIF) in the SFHA of the county.}
#'    \item{res_contracts_in_force}{National Flood Insurance Program (NFIP) Insured Residential Policy Contracts in Force (CIF) in the county.}
#'    \item{total_res_structures_sfha}{	Total residential structures in the SFHA of the county.}
#'    \item{total_res_structures}{	Total residential structures by county.}
#'    \item{fips_code}{Five digit fips code.}
#'    \item{as_of_date}{	The date used to determine current Contracts in Force. When time is not specified in the field, it will default to T00:00:00.000Z.}
#'    \item{id}{	Unique ID assigned to the record that does not persist between data set refreshes.}
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#'
#' nfip_rates_us <- get_nfip_res_penetration() %>%
#' summarise(res_contracts_in_force_sfha = sum(res_contracts_in_force_sfha, na.rm = TRUE),
#'   res_contracts_in_force = sum(res_contracts_in_force, na.rm = TRUE),
#'   total_res_structures_sfha = sum(total_res_structures_sfha, na.rm = TRUE),
#'   total_res_structures = sum(total_res_structures, na.rm = TRUE)
#'   ) %>%
#'   mutate(res_penetration_rate_sfha = res_contracts_in_force_sfha/total_res_structures_sfha,
#'   res_penetration_rate = res_contracts_in_force / total_res_structures)


get_nfip_res_penetration <- function(
    states = NULL,
    file_name = "nfip_residential_penetration_rates_12_12_2025.csv"
) {

  inpath <- file.path(
    get_box_path(), "hazards", "fema", "national-flood-insurance-program",
    "raw", file_name
  )

  if (!file.exists(inpath)) {
    stop("The provided `file_name` is invalid.")
  }

  df1 <- read.csv(file = inpath) %>%
    janitor::clean_names()

  # Apply filter only when states is not NULL
  if (!is.null(states)) {
    df1 <- df1 %>%
      dplyr::filter(state %in% states)
  }

  return(df1)
}
