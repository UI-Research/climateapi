#' @title Access temporal county-level SHELDUS hazard damage data
#'
#' @description Retrieves county-level hazard event data from the Spatial Hazard Events
#'   and Losses Database for the United States (SHELDUS), including property damage,
#'   crop damage, fatalities, and injuries.
#'
#' @param file_path The path to the raw SHELDUS data. If NULL (default), the most recently
#'   modified `direct_loss_aggregated_output_*.csv` file found under Box's `hazards/sheldus`
#'   directory is used, and a message discloses which file/vintage was selected.
#'
#' @details Data are from Arizona State University's SHELDUS database. Access requires
#'   a subscription. See \url{https://cemhs.asu.edu/sheldus}.
#'
#' @returns A dataframe comprising hazard x month x year x county observations of hazard events.
#'   Columns include:
#'   \describe{
#'     \item{unique_id}{Unique identifier for each observation.}
#'     \item{GEOID}{Five-digit county FIPS code.}
#'     \item{state_name}{Full state name (sentence case).}
#'     \item{county_name}{County name.}
#'     \item{year}{Year of the hazard event(s).}
#'     \item{month}{Month of the hazard event(s).}
#'     \item{hazard}{Type of hazard (e.g., "Flooding", "Hurricane/Tropical Storm").}
#'     \item{damage_property}{Property damage in inflation-adjusted dollars (base year varies
#'       by SHELDUS vintage; see the function's message for the base year used).}
#'     \item{damage_crop}{Crop damage in inflation-adjusted dollars (see `damage_property`).}
#'     \item{fatalities}{Number of fatalities.}
#'     \item{injuries}{Number of injuries.}
#'     \item{records}{Number of individual events aggregated into this observation.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' get_sheldus()
#' }

get_sheldus = function(file_path = NULL) {

  if (is.null(file_path)) {
    sheldus_directory = file.path(get_box_path(), "hazards", "sheldus")

    candidate_files = list.files(sheldus_directory, recursive = TRUE, full.names = TRUE) |>
      stringr::str_subset("_archive", negate = TRUE) |>
      stringr::str_subset("SHELDUS_[0-9.]+_.*YearMonthHazard.*/direct_loss_aggregated_output_.*\\.csv$")

    if (length(candidate_files) == 0) {
      stop(stringr::str_c(
        "No SHELDUS output file matching the expected naming pattern was found under: ",
        sheldus_directory, ". Provide an explicit `file_path` argument.")) }

    file_path = candidate_files[order(file.info(candidate_files)$mtime, decreasing = TRUE)][1]

    message(stringr::str_c("Using the most recently modified SHELDUS file found: ", file_path)) }

  ## is the file path valid/accessible?
  if (!file.exists(file_path)) {
    stop(stringr::str_c(
      "The path to the dataset does not point to a valid file. ",
      "Please ensure there is a file located at this path: ", file_path, ".")) }

  ## the data date back to ~1960, so there are some valid observations for counties that
  ## no longer exist as of 2010; we drop those counties
  ## counties that exist in 2010 or 2022
  benchmark_geographies = dplyr::bind_rows(
    tigris::counties(cb = TRUE, year = 2010) |>
      dplyr::transmute(GEOID = stringr::str_c(STATE, COUNTY)) |>
      sf::st_drop_geometry(),
    tigris::counties(cb = TRUE, year = 2022) |>
      dplyr::select(GEOID) |>
      sf::st_drop_geometry()) |>
    dplyr::pull(GEOID) |>
    unique()

  df1 = file_path |>
    readr::read_csv() |>
    janitor::clean_names()

  ## the raw dollar-denominated columns embed their inflation-adjustment base year in the
  ## column name (e.g. `property_dmg_adj_2023`); detect it dynamically rather than hardcoding
  ## a year that will go stale as new SHELDUS vintages are released
  detected_base_year = df1 |>
    colnames() |>
    stringr::str_subset("^property_dmg_adj_\\d{4}$") |>
    stringr::str_extract("\\d{4}")

  if (length(detected_base_year) != 1) {
    stop(stringr::str_c(
      "Could not detect a single `property_dmg_adj_<year>` column in the raw SHELDUS data; ",
      "check that the file matches the expected schema.")) }

  df2 = df1 |>
    dplyr::rename_with(~ stringr::str_replace(.x, "^(property|crop)_dmg_adj_\\d{4}$", "damage_\\1_adjusted")) |>
    dplyr::mutate(
      state_name = state_name |> stringr::str_to_sentence(),
      GEOID = stringr::str_remove_all(county_fips, "'")) |>
    dplyr::select(
      GEOID,
      state_name,
      county_name,
      year,
      month,
      hazard,
      damage_property = damage_property_adjusted,
      damage_crop = damage_crop_adjusted,
      dplyr::matches("injuries|fatalities"),
      records) |>
    dplyr::select(-dplyr::matches("per_capita|duration")) |>
    dplyr::arrange(GEOID, year, month) |>
    dplyr::filter(GEOID %in% benchmark_geographies)

  interpolate_columns = c("damage_property", "damage_crop", "fatalities", "injuries", "records")

  ## SHELDUS retains "historic county" records (county_name prefixed with "*") that share a
  ## GEOID with a currently-existing county at the same year x month x hazard grain, producing
  ## duplicate keys. Sort the historic-prefixed row after the current row within each group so
  ## that `dplyr::first()` keeps the current county's identity, while summing the historic row's
  ## values into the survivor. Groups without a collision pass through this step unchanged.
  df2b = df2 |>
    dplyr::mutate(is_historic_county = stringr::str_starts(county_name, stringr::fixed("*"))) |>
    dplyr::arrange(GEOID, year, month, hazard, is_historic_county) |>
    dplyr::summarize(
      .by = c("GEOID", "year", "month", "hazard"),
      dplyr::across(
        .cols = dplyr::all_of(interpolate_columns),
        .fns = ~ sum(.x, na.rm = TRUE)),
      dplyr::across(
        .cols = -dplyr::all_of(c(interpolate_columns, "is_historic_county")),
        .fns = dplyr::first))

  if (anyDuplicated(dplyr::select(df2b, GEOID, year, month, hazard)) != 0) {
    stop(stringr::str_c(
      "Duplicate GEOID x year x month x hazard keys remain after historic-county ",
      "deduplication; a new SHELDUS vintage may have introduced a new collision pattern.")) }

  ## connecticut counties are dated -- we need to crosswalk them to current county-
  ## equivalents (planning regions)
  ct_crosswalk_table = crosswalk::get_crosswalk(
      source_geography = "county",
      target_geography = "county",
      source_year = 2020,
      target_year = 2022,
      silent = TRUE)$crosswalks$step_1 |>
    dplyr::filter(state_fips == "09") |>
    dplyr::select(source_geoid, target_geoid, allocation_factor_source_to_target)

  crosswalked_ct_counties = df2b %>%
    dplyr::filter(state_name == "Connecticut") %>%
    dplyr::left_join(ct_crosswalk_table, by = c("GEOID" = "source_geoid"), relationship = "many-to-many") %>%
    dplyr::summarize(
      .by = c("target_geoid", "year", "month", "hazard"),
      dplyr::across(
        .cols = dplyr::all_of(interpolate_columns),
        .fns = ~ sum(.x * allocation_factor_source_to_target, na.rm = TRUE)),
      dplyr::across(
        .cols = -dplyr::all_of(c(interpolate_columns, "GEOID", "allocation_factor_source_to_target")),
        .fns = ~ dplyr::first(.x))) %>%
    dplyr::rename(GEOID = target_geoid)

  df3 = df2b %>%
    dplyr::filter(state_name != "Connecticut") %>%
    dplyr::bind_rows(crosswalked_ct_counties)

  df4 = df3 %>%
    dplyr::mutate(
      unique_id = uuid::UUIDgenerate(n = nrow(df3))) %>%
    dplyr::select(unique_id, dplyr::everything()) %>%
    dplyr::select(-allocation_factor)

  message(stringr::str_c(
    "The unit of observation is: county x year x month x hazard. ",
    "The `unique_id` field is a unique identifier for each observation. ",
    "Not all counties have observations for each month x year x hazard. ",
    "That is, only counties with a disaster event have an observation for a given month x year x hazard. ",
    "The `records` field reflects the number of events that were aggregated to ",
    "calculate the values reflected in the given observation. ",
    "All dollar-denominated values are in ", detected_base_year, " dollars."))

  return(df4)
}

utils::globalVariables(c(
  "state_name", "county_fips", "unique_id", "GEOID", "county_name", "year", "month",
  "damage_property", "damage_property_per_capita", "damage_crop", "damage_property_adjusted",
  "damage_crop_adjusted", "damage_crop_per_capita", "records", "benchmark_geographies",
  "is_historic_county", "state_fips", "source_geoid", "target_geoid",
  "allocation_factor_source_to_target", "STATE", "COUNTY", "."))
