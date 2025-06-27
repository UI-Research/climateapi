#' #' Get NLCD Land Cover Data for a Specified Input Spatial Vector
#' #'
#' #' @param county_geoid A five-character string corresponding to the county GEOID
#' #' @param year One of c(2001, 2004, 2006, 2008, 2011, 2016, 2019, 2021, 2023). Default is 2022.
#' #' @param projection A projection accepted by `sf::st_transform()`. Default is 5070.
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' get_nlcd = function(county_geoids, sf = NULL, year = 2023, projection = 5070) {
#'
#'   year = as.numeric(year)
#'   geoid = as.character(county_geoids)
#'
#'   # if (! (year %in% c(2001, 2004, 2006, 2008, 2011, 2016, 2019, 2021))) {
#'   #   stop("`year` must be one of 2001, 2004, 2006, 2008, 2011, 2016, 2019, or 2021.") }
#'
#'   if (!(nchar(county_geoids) == 5)) {
#'     stop("`geoid` must be a five-character string.") }
#'
#'   suppressWarnings({suppressMessages({
#'     tracts_sf <- tigris::tracts(
#'       state = stringr::str_sub(county_geoids, 1, 2),
#'       county = stringr::str_sub(county_geoids, 3, 5),
#'       year = year,
#'       cb = TRUE,
#'       progress_bar = FALSE) %>%
#'       sf::st_transform(projection)
#'
#'     # query rasterized canopy data from NLCD for the target county
#'     nlcd_annual <- FedData::get_nlcd_annual(
#'       label = "annual_nlcd",
#'       template = tracts_sf,
#'       year = year,
#'       #product = "LndCov",
#'       force.redo = TRUE)
#'   })})
#'
#'   ?FedData::get_nlcd_annual
#'   NLCD_ANNUAL <-
#'     FedData::get_nlcd_annual(
#'       template = FedData::meve,
#'       label = "meve",
#'       year = 2020,
#'       product =
#'         c(
#'           "LndCov",
#'           "LndChg",
#'           "LndCnf",
#'           "FctImp",
#'           "ImpDsc",
#'           "SpcChg"
#'         )
#'     )
#'   # exact_extract summarizes raster values over polygonal areas.
#'   # in this case each raster cell's canopy coverage is extracted
#'   # and applied to the corresponding tract. because the match
#'   # described above is not 1:1 it takes the mean canopy coverage
#'   # of the rasters that intersect a given tract
#'   tracts_sf$nlcd_canopy_coverage_tract <- exactextractr::exact_extract(
#'     x = canopy,
#'     y = tracts_sf,
#'     fun = "mean")
#'
#'   canopy_tracts <- tracts_sf %>%
#'     dplyr::select(GEOID, nlcd_canopy_coverage_tract) %>%
#'     sf::st_drop_geometry() %>%
#'     tibble::as_tibble()
#'
#'   return(canopy_tracts)
#' }
#' county_geoids = "01001"
