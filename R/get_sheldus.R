#' @title Access temporal county-level SHELDUS hazard damage data.
#' @param file_path The path to the raw SHELDUS data.
#'
#' @returns A dataframe comprising hazard x month x year x county observations of hazard events.
#' @export
#'
#' @examples
#' \dontrun{
#' get_sheldus()
#' }

get_sheldus = function(
    file_path = file.path(
      get_box_path(),"hazards", "sheldus",
      "SHELDUS_23.0_12312023_AllCounties_CountyAggregate_YearMonthHazard_2023USD",
      "direct_loss_aggregated_output_24075.csv")) {

  ## is the file path valid/accessible?
  if (!file.exists(file_path)) {
    stop(stringr::str_c(
      "The path to the dataset does not point to a valid file. ",
      "Please ensure there is a file located at this path: ", file_path, ".")) }

  ## connecticut counties are dated -- we need to crosswalk them to current county-
  ## equivalents (planning regions)
  ct_county_crosswalk1 = readr::read_csv(file.path(get_box_path(), "crosswalks", "ctdata_2022_tractcrosswalk_connecticut.csv")) %>%
    janitor::clean_names() %>%
    dplyr::select(
      tract_fips_2020,
      tract_fips_2022,
      county_fips_2020,
      county_fips_2022 = ce_fips_2022)

  suppressWarnings({suppressMessages({
    ct_tract_populations = tidycensus::get_acs(
      year = 2021,
      geography = "tract",
      state = "CT",
      variable = "B01003_001",
      output = "wide") %>%
    dplyr::select(
      tract_fips_2020 = GEOID,
      population_2020 = B01003_001E)
  })})

  ct_county_crosswalk = ct_county_crosswalk1 %>%
    dplyr::left_join(ct_tract_populations) %>%
    dplyr::summarize(
      .by = c("county_fips_2020", "county_fips_2022"),
      population_2020 = sum(population_2020)) %>%
    dplyr::mutate(
      .by = "county_fips_2020",
      population_2020_total = sum(population_2020, na.rm = TRUE)) %>%
    dplyr:: mutate(allocation_factor = population_2020 / population_2020_total) %>%
    dplyr::select(-dplyr::matches("population"))

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

  df2 = df1 |>
    dplyr::mutate(
      state_name = state_name |> stringr::str_to_sentence(),
      GEOID = stringr::str_remove_all(county_fips, "'")) |>
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ stringr::str_replace_all(.x, c("dmg" = "damage", "adj" = "adjusted"))) |>
    dplyr::select(
      GEOID,
      state_name,
      county_name,
      year,
      month,
      hazard,
      ### confirmed in data documentation the per capita values are calculated using the most recent year inflation
      ### all values are in the latest year's data
      damage_property = property_damage_adjusted_2023,
      # damage_property_per_capita = property_damage_per_capita,
      damage_crop = crop_damage_adjusted_2023,
      # damage_crop_per_capita = crop_damage_per_capita,
      dplyr::matches("injuries|fatalities"),
      records) |>
    dplyr::select(-dplyr::matches("per_capita|duration")) |>
    dplyr::arrange(GEOID, year, month) |>
    dplyr::filter(GEOID %in% benchmark_geographies)

  interpolate_columns = c("damage_property", "damage_crop", "fatalities", "injuries", "records")

  crosswalked_ct_counties = df2 %>%
    dplyr::filter(state_name == "Connecticut") %>%
    dplyr::left_join(ct_county_crosswalk, by = c("GEOID" = "county_fips_2020"), relationship = "many-to-many") %>%
    dplyr::summarize(
      .by = c("county_fips_2022", "year", "month", "hazard"),
      dplyr::across(
        .cols = dplyr::all_of(interpolate_columns),
        .fns = ~ sum(.x * allocation_factor, na.rm = TRUE)),
      dplyr::across(
        .cols = -dplyr::all_of(interpolate_columns),
        .fns = ~ dplyr::first(.x))) %>%
    dplyr::select(-GEOID) %>%
    dplyr::rename(GEOID = county_fips_2022)

  df3 = df2 %>%
    dplyr::filter(state_name != "Connecticut") %>%
    dplyr::bind_rows(crosswalked_ct_counties)

  df4 = df3 %>%
    dplyr::mutate(
      unique_id = uuid::UUIDgenerate(n = nrow(df3))) %>%
    dplyr::select(unique_id, everything())

  message(stringr::str_c(
    "The unit of observation is: county x year x month x hazard. ",
    "The `unique_id` field is a unique identifier for each observation. ",
    "Not all counties have observations for each month x year x hazard. ",
    "That is, only counties with a disaster event have an observation for a given month x year x hazard. ",
    "The `records` field reflects the number of events that were aggregated to ",
    "calculate the values reflected in the given observation.",
    "All dollar-denominated values are in 2023 dollars.")) ## note this should be updated to reflect the programmatic input

  return(df4)
}

utils::globalVariables(c(
  "state_name", "county_fips", "unique_id", "GEOID", "county_name", "year", "month",
  "damage_property", "damage_property_per_capita", "damage_crop",
  "damage_crop_per_capita", "records", "benchmark_geographies",
  "STATE", "COUNTY", "."))
