#' @importFrom magrittr %>%
#' @importFrom rlang :=

#' @title Subdivide a linestring into segments of a specified length
#'
#' @param line A linestring or simple feature collection thereof
#' @param max_length  The maximum length of each segment. Segments longer than this value will be subdivided; those that are below this threshold will be returned as-is.
#' @param crs The coordinate reference system to which the linestring should be transformed. Default is 5070.
#'
#' @return An `sf` object (simple feature collection) with geometry type LINESTRING. The returned object contains:
#' \describe{
#'   \item{row_id}{Integer. The row index from the original input linestring, allowing linkage back to the input data.}
#'   \item{...}{All original attributes from the input `line` object are preserved and joined back via `row_id`.}
#'   \item{geometry}{LINESTRING geometry. Each segment is at most `max_length` units long (in the CRS units). Segments shorter than `max_length` in the input are returned unchanged.}
#' }
#' The CRS of the output is set to the value specified by the `crs` parameter (default: EPSG:5070).
#' @export
subdivide_linestring = function(line, max_length, crs = 5070) {

  ## create a copy of the input dataset; we'll join back to this at the end to
  ## return all of the original attributes
  lines0 = line %>%
    dplyr::mutate(row_id = dplyr::row_number())

  lines1 = line %>%
    dplyr::transmute(
      row_id = dplyr::row_number(),
      length = as.numeric(sf::st_length(.)))

  lines2 = lines1 %>%
    dplyr::filter(length > max_length)

  lines3 = purrr::map_dfr(
    lines2$row_id,
    function(row_id) {
      lines2 %>%
        dplyr::filter(row_id == !!row_id) %>%
        sf::st_as_sfc() %>%
        sf::st_line_interpolate(
          dist = seq(
            from = 0,
            to = as.numeric(sf::st_length(.)),
            by = max_length)) %>%
        sf::st_as_sf() %>%
        dplyr::mutate(
          row_id = row_id,
          point_id = dplyr::row_number())})

  lines4 = purrr::map_dfr(
    lines3$row_id %>% unique(),
    function(row_id) {
      df = lines3 %>% dplyr::filter(row_id == !!row_id)
      point_ids = df %>% dplyr::pull(point_id)
      purrr::map_dfr(
        point_ids,
        function(point_id) {
          if (point_id < length(point_ids)) {
            result = df %>%
              dplyr::filter(point_id %in% c(!!point_id, !!point_id + 1)) %>%
              sf::st_coordinates() %>%
              sf::st_linestring() %>%
              sf::st_sfc() %>%
              sf::st_as_sf() %>%
              dplyr::mutate(row_id = row_id) } else {
                result = NULL } })})

  lines5 = dplyr::left_join(
    lines1 %>%
      sf::st_transform(crs) %>%
      dplyr::filter(!row_id %in% lines4$row_id) %>%
      dplyr::bind_rows(lines4 %>% sf::st_set_crs(crs)) %>%
      dplyr::select(-length),
    lines0 %>% sf::st_drop_geometry(),
    by = "row_id")
}

#' @title Convert polygons into their component linestrings
#'
#' @param .sf The spatial dataframe containing one or more polygons
#'
#' @return An `sf` object (simple feature collection) with geometry type LINESTRING. The returned object contains:
#' \describe{
#'   \item{polygon_id}{Integer. The row index of the originating polygon from the input `.sf` object, enabling linkage back to the source polygon.}
#'   \item{line_id}{Integer. A sequential identifier for each line segment within its originating polygon. Line segments are ordered according to the vertex sequence of the polygon boundary.}
#'   \item{...}{All original attributes from the input `.sf` object are preserved and joined back via `polygon_id`.}
#'   \item{geometry}{LINESTRING geometry. Each line segment represents one edge of the original polygon boundary.}
#' }
#' The CRS of the output matches the input `.sf` object (transformed to EPSG:5070 during processing).
#' @export
polygons_to_linestring = function(.sf) {
  sf1 = .sf %>%
    dplyr::mutate(polygon_id = dplyr::row_number())

  sf1 %>%
    sf::st_coordinates() %>% ## convert to a matrix of coordinates for each polygon vertex
    tibble::as_tibble() %>%
    dplyr::group_by(L3) %>% ## this identifies the originating polygon's index
    dplyr::mutate(order = dplyr::row_number()) %>% ## this is the order of the vertices in the polygon
    dplyr::bind_rows(., .) %>% ## this duplicates the rows, so we can create line segments
    dplyr::arrange(L3, order) %>% ## this sorts the rows by polygon and vertex order
    dplyr::group_by(L3) %>% ## this groups the rows by originating polygon
    ## this creates an id that matches coordinate pairs to ID line segments
    dplyr::mutate(line_id = dplyr::lag(order), line_id = dplyr::if_else(is.na(line_id), max(line_id, na.rm = TRUE), line_id)) %>%
    ## adding spatial coordinates (points)
    sf::st_as_sf(coords = c("X", "Y"), crs = 5070) %>%
    dplyr::group_by(L3, line_id) %>% ## grouping into vertex pairs per polygon to create lines
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>% ## and this takes us to linestrings, finally
    dplyr::left_join(sf1 %>% sf::st_drop_geometry(), by = c("L3" = "polygon_id")) %>%
    dplyr::rename(polygon_id = L3)
}

utils::globalVariables(c("point_id", "row_id", "line_id", "L3"))
