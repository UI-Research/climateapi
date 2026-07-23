# Tests for get_nlcd_land_cover().
#
# The argument-validation tests short-circuit before any network access. The pipeline
# tests hit the live Annual NLCD service (via FedData::get_nlcd_annual()); they crop a
# tiny (~5 km^2) area so they run in a second or two, but are skipped off-network, on
# CRAN, or when FedData (>= 4.4.0) / exactextractr are unavailable.

# ---- argument validation (no network) ----------------------------------------------

test_that("get_nlcd_land_cover has the expected formals", {
  expect_true(is.function(get_nlcd_land_cover))
  expect_equal(
    names(formals(get_nlcd_land_cover)),
    c("county_geoids", "year", "geometries", "return_raw"))
})

test_that("get_nlcd_land_cover rejects years outside 1985-2024", {
  expect_error(get_nlcd_land_cover(county_geoids = "11001", year = 1984), "between 1985 and 2024")
  expect_error(get_nlcd_land_cover(county_geoids = "11001", year = 2025), "between 1985 and 2024")
  expect_error(get_nlcd_land_cover(county_geoids = "11001", year = NA), "between 1985 and 2024")
})

test_that("get_nlcd_land_cover requires a geometry source", {
  expect_error(get_nlcd_land_cover(), "either `county_geoids` or `geometries`")
})

test_that("get_nlcd_land_cover validates county_geoids length", {
  expect_error(get_nlcd_land_cover(county_geoids = "1100"), "five-character")
  expect_error(get_nlcd_land_cover(county_geoids = c("11001", "24")), "five-character")
})

test_that("get_nlcd_land_cover validates the geometries argument", {
  # not an sf object
  expect_error(
    get_nlcd_land_cover(geometries = data.frame(GEOID = "x")),
    "simple features")

  # sf without a defined CRS
  no_crs = sf::st_sf(
    GEOID = "x",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0))))))
  expect_error(get_nlcd_land_cover(geometries = no_crs), "coordinate reference system")

  # sf with a CRS but no GEOID column
  no_geoid = sf::st_sf(
    id = "x",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
      crs = 4326))
  expect_error(get_nlcd_land_cover(geometries = no_geoid), "GEOID")
})

# ---- full pipeline against the live Annual NLCD service -----------------------------

# a small (~5 km^2) two-part area over central Washington, DC, with GEOIDs. Supplying
# geometries directly avoids a tigris download and keeps the raster crop tiny.
dc_test_geometries = function(parts = 2) {
  bbox = sf::st_bbox(
    c(xmin = -77.05, ymin = 38.88, xmax = -77.00, ymax = 38.92),
    crs = sf::st_crs(4326))
  geographies = sf::st_as_sf(sf::st_make_grid(sf::st_as_sfc(bbox), n = c(parts, 1)))
  geographies$GEOID = paste0("AREA_", LETTERS[seq_len(nrow(geographies))])
  geographies
}

skip_unless_nlcd_available = function() {
  skip_on_cran()
  testthat::skip_if_offline()
  skip_if_not_installed("FedData")
  skip_if_not_installed("exactextractr")
  skip_if(
    utils::packageVersion("FedData") < "4.4.0",
    "Annual NLCD (1985-2024) requires FedData (>= 4.4.0).")
}

test_that("get_nlcd_land_cover returns tidy per-class area fractions (live NLCD service)", {
  skip_unless_nlcd_available()

  geographies = dc_test_geometries(parts = 2)

  result = tryCatch(
    get_nlcd_land_cover(geometries = geographies, year = c(2001, 2024)),
    error = function(e) skip(paste("Annual NLCD service unavailable:", conditionMessage(e))))

  # shape
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("GEOID", "year", "land_cover_code", "land_cover_class",
      "area_fraction", "area_sq_meters"))

  # both requested vintages came back for both geometries
  expect_setequal(unique(result$year), c(2001L, 2024L))
  expect_setequal(unique(result$GEOID), c("AREA_A", "AREA_B"))

  # types and value ranges
  expect_type(result$land_cover_code, "integer")
  expect_type(result$area_fraction, "double")
  expect_true(all(result$area_fraction > 0 & result$area_fraction <= 1))

  # every class code resolved to a name (the class-code join is complete)
  expect_false(any(is.na(result$land_cover_class)))

  # land cover is a partition of the geometry: fractions sum to ~1 within geometry-year
  totals = tapply(result$area_fraction, list(result$GEOID, result$year), sum)
  expect_true(all(abs(totals - 1) < 0.01))

  # area_sq_meters is the absolute, cross-geometry-comparable area of each record (a
  # single land cover class within a geometry-year), always in square meters
  expect_type(result$area_sq_meters, "double")
  expect_true(all(result$area_sq_meters > 0))

  # summing the per-record areas within a geometry-year recovers that geometry's area,
  # computed here independently in EPSG:5070 (the CRS the function works in)
  expected_area = stats::setNames(
    as.numeric(sf::st_area(sf::st_transform(geographies, 5070))),
    geographies$GEOID)
  recovered = tapply(result$area_sq_meters, list(result$GEOID, result$year), sum)
  for (g in rownames(recovered)) {
    expect_true(all(abs(recovered[g, ] / expected_area[[g]] - 1) < 0.01))
  }

  # within a geometry-year, each record's share of the summed area matches area_fraction
  group = interaction(result$GEOID, result$year, drop = TRUE)
  group_area = ave(result$area_sq_meters, group, FUN = sum)
  expect_equal(result$area_sq_meters / group_area, result$area_fraction, tolerance = 1e-6)

  # central DC should register some developed land cover -- an illustrative sanity check
  expect_true(any(stringr::str_detect(result$land_cover_class, "Developed")))
})

test_that("get_nlcd_land_cover attaches raw rasters when return_raw = TRUE (live NLCD service)", {
  skip_unless_nlcd_available()

  geographies = dc_test_geometries(parts = 1)

  result = tryCatch(
    get_nlcd_land_cover(geometries = geographies, year = 2024, return_raw = TRUE),
    error = function(e) skip(paste("Annual NLCD service unavailable:", conditionMessage(e))))

  raw = attr(result, "nlcd_raster")
  expect_false(is.null(raw))
  # FedData::get_nlcd_annual() returns one row per requested year, with a `rast`
  # list-column of SpatRasters
  expect_true(all(c("year", "rast") %in% names(raw)))
  expect_equal(nrow(raw), 1L)
  expect_s4_class(raw$rast[[1]], "SpatRaster")
})
