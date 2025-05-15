## Author: Will Curran-Groome

#' @importFrom magrittr %>%
#'
#' @title Interpolate tract-level sociodemographic data to zoning polygons
#'
#' @param sociodemographic_tracts_sf (optional) A tract level spatial (sf) dataset resulting from urbnindicators. If NULL, this function is run behind the scenes for appropriate tracts.
#' @param zones_sf A spatial (sf) dataset defining zoning districts.
#' @param id_column The name of the column in zones_sf that identifies each unique combination of zoning regulations.
#' @param weights One of c("population", "housing"). The variable to be used as the weight in the interpolation.
#'
#' @return A spatial (sf) dataset comprising one observation for each level of id_column with interpolated values taken from sociodemographic_tracts_sf.
#' @export
interpolate_demographics = function(
    zones_sf,
    sociodemographic_tracts_sf = NULL,
    id_column,
    weights = "population") {

  ## main function body
  projection = 5070

  ## this takes a value greater than one if there are any duplicate observations
  duplicate_observations = zones_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::count(.data[[id_column]], sort = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::pull(n)

  ## transform zoning data to a standard projection
  ## group by the supplied summary column if there are duplicate observations
  ## summarize so that there's only a single observation per value of the
  ## grouping column, consolidating geometries accordingly
  zones0 = zones_sf %>%
    sf::st_transform(projection) %>%
    { if (duplicate_observations > 1)
      (dplyr::group_by(., .data[[id_column]]) %>% dplyr::summarize())
      else .}

  states_sf = tigris::states(
    cb = TRUE,
    resolution = "20m",
    year = 2023,
    progress_bar = FALSE,
    refresh = TRUE) %>%
    sf::st_transform(projection)

  zone_bbox = zones0 %>%
    sf::st_bbox() %>%
      sf::st_as_sfc()

  state_geoids = states_sf %>%
    sf::st_filter(zone_bbox) %>%
    .[["GEOID"]]

  options(tigris_use_cache = FALSE)
  relevant_tracts = state_geoids %>%
    purrr::map_dfr(
      ~ tigris::tracts(
        state = .x,
        county = NULL,
        cb = TRUE,
        year = 2023,
        progress_bar = FALSE,
        refresh = TRUE)) %>%
    sf::st_transform(projection) %>%
    sf::st_filter(zones0) %>%
    dplyr::mutate(
      state_geoid = stringr::str_sub(GEOID, 1, 2),
      county_geoid = stringr::str_sub(GEOID, 1, 5),
      tract_area_sqm = sf::st_area(.) %>% as.numeric())

  relevant_blocks = relevant_tracts %>%
    .[["state_geoid"]] %>%
    unique() %>%
    purrr::map_dfr(
      function(state_geoid) {
        county_geoids = relevant_tracts %>%
          dplyr::filter(state_geoid == !!state_geoid) %>%
          dplyr::pull(county_geoid) %>%
          unique() %>%
          stringr::str_sub(3, 5)

        result = tigris::blocks(
          state = state_geoid,
          county = county_geoids,
          year = 2022,
          progress_bar = FALSE,
          refresh = TRUE)

        }) %>%
    sf::st_transform(projection)

  ## in the case that sociodemographic data are not provided by the user,
  ## we query them
  if (is.null(sociodemographic_tracts_sf)) {
    sociodemographic_tracts_sf = urbnindicators::compile_acs_data(
      variables = NULL,
      years = c(2023),
      geography = "tract",
      states = state_geoids,
      counties = NULL,
      spatial = TRUE) %>%
      sf::st_transform(projection) %>%
      dplyr::filter(GEOID %in% relevant_tracts$GEOID)
  } else {
    sociodemographics0 = sociodemographic_tracts_sf %>%
      sf::st_transform(projection) %>%
      dplyr::filter(GEOID %in% relevant_tracts$GEOID)
  }

  interpolated_zone_counts = tidycensus::interpolate_pw(
    from = sociodemographics0 %>%
      dplyr::select(
        dplyr::where(is.numeric),
        -dplyr::matches("_percent$|_mean$|_median$|_M$|_cv$|density|area")),
    to = zones0,
    to_id = id_column,
    weights = relevant_blocks,
    weight_column = weights,
    crs = 5070,
    extensive = TRUE) ## calculating weighted sums

  interpolated_zone_noncounts = tidycensus::interpolate_pw(
    from = sociodemographics0 %>%
      dplyr::select(dplyr::matches("_percent$|_mean$|_median$|density$")),
    to = zones0,
    to_id = id_column,
    weights = relevant_blocks,
    weight_column = weights,
    crs = 5070,
    extensive = FALSE) ## calculating weighted means

  interpolated_zones = dplyr::left_join(
    interpolated_zone_counts,
    interpolated_zone_noncounts %>%
      sf::st_drop_geometry())

  return(interpolated_zones)
}

utils::globalVariables(c(".data"))
