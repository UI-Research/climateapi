# Author: Will Curran-Groome

#' @importFrom magrittr %>%

#' @title Estimate counts of hazard-impacted structures by structure type
#' @param geography The desired geography of the results. One of "tract" or "county".
#' @param boundaries A POLYGON or MULTIPOLYGON object, or an sf::st_bbox()-style bbox.
#' @returns A dataframe comprising estimated counts of each structure type, at the specified `geography`, for all such geographic units intersecting the `boundaries` object.
#' @export
#' @examples
#' \dontrun{
#' estimate_impacted_structures(
#'   geography = "tract",
#'   boundaries = tigris::states(cb = TRUE) %>% dplyr::filter(stringr::str_detect(NAME, "District")))
#'}
estimate_impacted_structures = function(
    boundaries,
    geography = "county") {

  if (! geography %in% c("county", "tract")) {
    stop("`geography` must be one of 'county' or 'tract'.") }

  if ( boundaries %>% sf::st_crs() %>% is.na(.) ) {
    stop("You must specify a spatial vector object with a defined CRS or a bbox from sf::st_bbox().")}

  projection = 5070

  boundaries = boundaries %>%
    sf::st_transform(projection) %>%
    dplyr::mutate(boundaries_area = sf::st_area(.) %>% as.numeric)
  states_sf = tigris::states(cb = TRUE, year = 2023) %>%
    sf::st_transform(projection)

  ## states for which we'll pull USA Structures data:
  ## those with more than a .01% of state land area overlapping with the specified
  ## `boundaries` object
  structure_data_states = states_sf %>%
    dplyr::mutate(state_area = sf::st_area(.) %>% as.numeric) %>%
    sf::st_intersection(boundaries) %>%
    dplyr::mutate(
      intersection_area = sf::st_area(.) %>% as.numeric,
      intersection_share_boundary_area = intersection_area / boundaries_area) %>%
    dplyr::filter(intersection_share_boundary_area > .01) %>%
    dplyr::pull(STUSPS) %>%
    unique()

  if ( (length(structure_data_states) < 1) | !(structure_data_states %in% tidycensus::fips_codes$state %>% unique())) {
    stop("The provided `boundaries` object does not appear to overlap with any of the
         states or territories for which building footprint data area available. Please
         ensure that the `boundaries` object is a valid `library(sf)` enabled vector
         object that is properly projected.") }

  box_path = file.path(
    path.expand("~"), "Box", "METRO Climate and Communities Practice Area",
    "github-repository", "built-environment", "housing-units", "usa-structures", "raw")

  df1 = purrr::map_dfr(
    structure_data_states,
    function(state_abbreviation) {
      state_name = tidycensus::fips_codes %>%
        dplyr::filter(state == !!state_abbreviation) %>%
        dplyr::pull(state_name) %>%
        unique()

      existing_files = list.files(box_path) %>%
        purrr::keep(~ stringr::str_detect(.x, state_name))

      if (length(existing_files) == 1) {
        structures1 = readr::read_csv(existing_files)
        warning("Data are being read from Box and reflect a cached version of these data.")
      } else {
        url = stringr::str_c(
          "https://fema-femadata.s3.amazonaws.com/Partners/ORNL/USA_Structures/",
          state_name, "/Deliverable20230526", state_abbreviation, ".zip")

        outpath = file.path(box_path, stringr::str_c(state_name, ".zip"))

        utils::download.file(
          url = url,
          destfile = outpath)

        utils::unzip(
          zip = outpath,
          exdir = box_path)

        structures1 = sf::st_read(
          file.path(
            box_path,
            stringr::str_c("Deliverable20230526", state_abbreviation),
            stringr::str_c(state_abbreviation, "_Structures.gdb"))) }

      structures2 = structures1 %>%
        janitor::clean_names() %>%
        sf::st_drop_geometry() %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_transform(projection) %>%
        sf::st_filter(boundaries) %>%
        dplyr::select(
          unique_id = build_id,
          occupancy_class = occ_cls,
          primary_occupancy = prim_occ,
          county_fips = fips)

      if (geography == "county") {
        structures3 = structures2 %>%
          sf::st_drop_geometry() %>%
          dplyr::group_by(county_fips, primary_occupancy) %>%
          dplyr::summarize(
            occupancy_class = dplyr::first(occupancy_class),
            count = n()) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(county_fips, dplyr::desc(count)) %>%
          dplyr::rename(GEOID = county_fips) }

      if (geography == "tract") {
        tracts_sf = tigris::tracts(cb = TRUE, year = 2023, state = state_abbreviation) %>%
          sf::st_transform(projection) %>%
          dplyr::select(GEOID)

        structures3 = structures2 %>%
          sf::st_join(tracts_sf) %>%
          sf::st_drop_geometry() %>%
          dplyr::group_by(GEOID, primary_occupancy) %>%
          dplyr::summarize(
            occupancy_class = dplyr::first(occupancy_class),
            count = n()) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(GEOID, dplyr::desc(count))}
      })

  return(df1)
}

utils::globalVariables(
  c("intersection_area", "state_area", "intersection_share_state_area", "STUSPS",
    "build_id", "occ_cls", "prim_occ", "fips", "primary_occupancy", "occupancy_class"))

