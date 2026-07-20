#' Get Emergency Management Performance Grant (EMPG) data
#'
#' @description Retrieves Emergency Management Performance Grant (EMPG) award data
#'   from FEMA, which supports state and local emergency management agencies.
#'
#' @param file_path Path to the raw data. If NULL (default), reads the most recently
#'   cached file for this dataset from `get_openfema_cache_path()`.
#' @param api Logical indicating whether to use the OpenFEMA API to retrieve the data.
#'   Default is FALSE (read from `file_path`, or the local OpenFEMA cache if NULL).
#'
#' @details Data are from FEMA's OpenFEMA API. See
#'   \url{https://www.fema.gov/openfema-data-page/emergency-management-performance-grants-v2}.
#'
#' @return A data frame containing emergency management performance grant (EMPG) data.
#'   Columns include:
#'   \describe{
#'     \item{id}{Unique identifier for the grant record.}
#'     \item{reporting_period}{The reporting period associated with the record.}
#'     \item{state_name}{Full state name.}
#'     \item{state_code}{Two-digit state FIPS code.}
#'     \item{state_abbreviation}{Two-letter state abbreviation.}
#'     \item{legal_agency_name}{The name of the legal agency administering the grant.}
#'     \item{project_type}{The type of project funded.}
#'     \item{year_project_start}{Year the project started (derived from `project_start_date`,
#'        with corrections for a handful of records with typos in the raw data).}
#'     \item{project_start_date}{Date the project started.}
#'     \item{project_end_date}{Date the project ended.}
#'     \item{name_of_program}{The name of the EMPG program.}
#'     \item{funding_amount}{Funding amount in dollars.}
#'   }
#' @export

get_emergency_management_performance = function(
    file_path = NULL,
    api = FALSE) {

  if (isTRUE(api)) {
    df1 = rfema::open_fema(data_set = "emergencymanagementperformancegrants", ask_before_call = FALSE) %>%
      janitor::clean_names()
  } else if (is.null(file_path)) {

    df1 = arrow::read_parquet(find_openfema_cache_file("EmergencyManagementPerformanceGrants")) %>%
      janitor::clean_names()
  } else {

    if (!file.exists(file_path)) { stop("Please query the API (`api = TRUE`) or provide a valid `file_path`.")}

    df1 = readr::read_csv(file_path) %>%
      janitor::clean_names()
  }

  ## funding_amount is character in api mode, numeric in file mode
  df1 = df1 %>%
    dplyr::mutate(funding_amount = as.numeric(funding_amount))

  df2a = df1 %>%
    dplyr::rename(state_name = state) %>%
    dplyr::mutate(
      year_project_start = lubridate::year(project_start_date),
      year_project_start = dplyr::case_when(
        ## there are a handful of records with typos in their project start dates
        year_project_start < 100 ~ year_project_start + 2000,
        id == "d947023a-220c-4cbf-9297-a8e9f712b481" ~ 2014,
        id == "6becff59-bbbf-4cdd-b8ec-527aa5dab1e8" ~ 2017,
        id == "b6c42f5e-fe86-465b-8235-f016dd702029" ~ 2017,
        id == "3aff6c2e-798e-47e3-8a14-4f3d3e9f5ebf" ~ 2019,
        id == "ebc64764-a62e-4d98-8a73-bc14a7c78ee3" ~ 2021,
        id == "a1932015-40ff-4f3d-8a01-f4946da70444" ~ 2022,
        id == "c8917cc1-853b-4ac5-b3ef-0a607c6e6881" ~ 2022,
        TRUE ~ year_project_start))

  n_excluded_years = df2a %>% dplyr::filter(year_project_start <= 2012) %>% nrow()

  df2 = df2a %>%
    dplyr::filter(year_project_start > 2012) %>%
    dplyr::left_join(get_geography_metadata(geography_type = "state"))

warning(stringr::str_c(
"At the time of writing, there are virtually no records with `year_project_start` values in 2024 and 2025 ",
"although the dataset was reported as last being updated on August 24, 2025. ",
"Users should be cautious in interpreting these data as being complete for either year. ",
n_excluded_years, " records with `year_project_start` <= 2012 have been excluded, due to ",
"suspected year mislabeling in the source data for years this far outside the program's ",
"typical reporting window."))

    return(df2)
}

utils::globalVariables(c("year_project_start", "project_start_date", "funding_amount"))
