#' @title Get Disaster Dollar Database Data 
#' @details These data are sourced from: https://carnegieendowment.org/features/disaster-dollar-database. The data returned from this function are unchanged, though some columns have been renamed slightly for clarity and consistency.
#'
#' @param file_path The path (on Box) to the file containing the raw data.
#'
#' @returns A dataframe comprising disaster-level observations with financial assistance metrics from FEMA's Individual and Households Program (IHP), Public Assistance (PA), HUD's Community Development Block Grant Disaster Recovery (CDBG-DR), and SBA disaster loans.
#' @export
#'
#' @examples
#' \dontrun{
#' get_disaster_dollar_database()
#' }

get_disaster_dollar_database = function(
  file_path = file.path(get_box_path(), "hazards", "carnegie-endowment", "disaster_dollar_database_2025_11_19.csv")) {

  ## is the file path valid/accessible?
  if (!file.exists(file_path)) {
    stop(stringr::str_c(
      "The path to the dataset does not point to a valid file. ",
      "Please ensure there is a file located at this path: ", file_path, ".")) }

  df_raw = readr::read_csv(file_path) |>
    dplyr::select(
      year,
      event_name = event,
      incident_number,
      incident_start,
      incident_end,
      declaration_date,
      state_abbreviation = state,
      incident_type,
      ihp_applications_valid = valid_ihp_applications,
      ihp_applications_approved = eligible_ihp_applications,
      ihp_eligibility_rate,
      ihp_allocated_amount_total = ihp_total,
      ihp_allocated_amount_average = ihp_average_award,
      pa_allocated_amount_total = pa_total,
      pa_projects_count,
      cdbg_df_allocated_amount_total = cdbg_dr_allocation,
      sba_loan_amount_approved_total = sba_total_approved_loan_amount)

  message(stringr::str_c(
    "The unit of observation is: disaster event. ",
    "Each observation represents a single declared disaster event with associated financial assistance metrics from multiple federal programs."))

  return(df_raw)

}

utils::globalVariables(c(
  "year", "event", "incident_number", "incident_start", "incident_end",
  "declaration_date", "state", "incident_type", "valid_ihp_applications",
  "eligible_ihp_applications", "ihp_eligibility_rate", "ihp_total",
  "ihp_average_award", "pa_total", "pa_projects_count", "cdbg_dr_allocation",
  "sba_total_approved_loan_amount"))