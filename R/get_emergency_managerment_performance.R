#' Get EMPG data
#'
#' @param file_path Path to the downloaded dataset on Box.
#' @param api Logical indicating whether to use the OpenFEMA API to retrieve the data. Default is TRUE.
#'
#' @return A data frame containing emergency management performance grant (EMPG) data.
#' @export

get_emergency_management_performance = function(
    file_path = file.path(
      get_box_path(), "hazards", "FEMA", "emergency-management-performance",
      "emergency_management_performance_grants_2025_06_29.csv"),
    api = TRUE) {

  if (isTRUE(api)) {
    df1 = rfema::open_fema(data_set = "emergencymanagementperformancegrants", ask_before_call = FALSE) %>%
      janitor::clean_names()
  } else {

    if (!file.exists(file_path)) { stop("Please query the API (`api = TRUE`) or provide a valid `file_path`.")}

    df1 = readr::read_csv(file_path) %>%
      janitor::clean_names()
  }

  df2 %>% dplyr::count(year_project_start)

  df2 = df1 %>%
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
        TRUE ~ year_project_start)) %>%
    dplyr::filter(year_project_start > 2012) %>%
    dplyr::left_join(get_geography_metadata(geography_type = "state"))

warning(stringr::str_c(
"At the time of writing, there are virtually no records with `year_project_start` values in 2024 and 2025 ",
"although the dataset was reported as last being updated on August 24, 2025. ",
"Users should be cautious in interpreting these data as being complete for either year.",
"Further, some records have erroneous project start years in the raw data; these have been ",
"addressed by the authors of the `climateapi` package in part, but some records are included ",
"for years 2004, 2010, 2011, and 2012, those these awards likely correspond to different years."))

    return(df2)
}

utils::globalVariables(c("year_project_start", "project_start_date"))
