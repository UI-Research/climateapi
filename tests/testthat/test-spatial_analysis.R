# Tests for spatial_analysis.R functions

test_that("subdivide_linestring runs without error with valid inputs", {
  # Create a linestring in projected CRS (5070 uses meters)
  # Line from (0,0) to (1000,0) in meters

  coords <- matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)
  line <- sf::st_sfc(sf::st_linestring(coords), crs = 5070)
  line_sf <- sf::st_as_sf(data.frame(id = 1), geometry = line)

  # Test that function runs without error with valid inputs
  expect_no_error(subdivide_linestring(line_sf, max_length = 500))
})

test_that("subdivide_linestring applies crs parameter correctly", {
  # Create test data in projected CRS
  coords <- matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)
  line <- sf::st_sfc(sf::st_linestring(coords), crs = 5070)
  line_sf <- sf::st_as_sf(data.frame(id = 1), geometry = line)

  # Test with default crs (5070)
  result <- subdivide_linestring(line_sf, max_length = 500)
  expect_equal(sf::st_crs(result)$epsg, 5070)

  # Test with explicit different crs
  result2 <- subdivide_linestring(line_sf, max_length = 500, crs = 3857)
  expect_equal(sf::st_crs(result2)$epsg, 3857)
})

test_that("subdivide_linestring returns expected structure", {
  # Create test data - line of 1000 meters
  coords <- matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)
  line <- sf::st_sfc(sf::st_linestring(coords), crs = 5070)
  line_sf <- sf::st_as_sf(data.frame(attr1 = "test", attr2 = 123), geometry = line)

  result <- subdivide_linestring(line_sf, max_length = 500, crs = 5070)

  # Check that result is an sf object
  expect_s3_class(result, "sf")

  # Check that geometry type is LINESTRING
  expect_true(all(sf::st_geometry_type(result) == "LINESTRING"))

  # Check that original attributes are preserved
  expect_true("attr1" %in% names(result))
  expect_true("attr2" %in% names(result))

  # Check that row_id column exists
  expect_true("row_id" %in% names(result))
})

test_that("subdivide_linestring subdivides long lines", {
  # Create a line that's 1000 meters long
  coords <- matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)
  line <- sf::st_sfc(sf::st_linestring(coords), crs = 5070)
  line_sf <- sf::st_as_sf(data.frame(id = 1), geometry = line)

  # Subdivide into segments of max 400 meters - should create 3 segments
  result <- subdivide_linestring(line_sf, max_length = 400, crs = 5070)

  # Should have more than 1 row (line was subdivided)
  expect_gt(nrow(result), 1)
})

test_that("subdivide_linestring preserves short lines unchanged", {
  # Create a line that's only 100 meters long
  coords <- matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)
  line <- sf::st_sfc(sf::st_linestring(coords), crs = 5070)
  line_sf <- sf::st_as_sf(data.frame(id = 1), geometry = line)

  # With max_length = 500, line should not be subdivided
  result <- subdivide_linestring(line_sf, max_length = 500, crs = 5070)

  # Should still have 1 row
  expect_equal(nrow(result), 1)
})

test_that("polygons_to_linestring runs without error with valid inputs", {

  # Create a MULTIPOLYGON for testing (function uses L3 from st_coordinates which
  # requires MULTIPOLYGON geometry)
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  mpoly <- sf::st_sfc(sf::st_multipolygon(list(list(coords))), crs = 5070)
  poly_sf <- sf::st_as_sf(data.frame(id = 1), geometry = mpoly)

  # Test that function runs without error with valid inputs
  expect_no_error(polygons_to_linestring(poly_sf))
})

test_that("polygons_to_linestring returns expected structure", {
  # Create MULTIPOLYGON test data
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  mpoly <- sf::st_sfc(sf::st_multipolygon(list(list(coords))), crs = 5070)
  poly_sf <- sf::st_as_sf(data.frame(attr1 = "test"), geometry = mpoly)

  result <- polygons_to_linestring(poly_sf)

  # Check that result is an sf object
  expect_s3_class(result, "sf")

  # Check that geometry type is LINESTRING
  expect_true(all(sf::st_geometry_type(result) == "LINESTRING"))

  # Check that polygon_id column exists
  expect_true("polygon_id" %in% names(result))

  # Check that line_id column exists
  expect_true("line_id" %in% names(result))

  # Check that original attributes are preserved
  expect_true("attr1" %in% names(result))

  # A square polygon should produce 4 line segments (one per edge)
  expect_equal(nrow(result), 4)
})

test_that("polygons_to_linestring preserves original attributes", {
  # Create MULTIPOLYGON test data with multiple attributes
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  mpoly <- sf::st_sfc(sf::st_multipolygon(list(list(coords))), crs = 5070)
  poly_sf <- sf::st_as_sf(
    data.frame(
      name = "test_polygon",
      value = 42,
      category = "A"
    ),
    geometry = mpoly
  )

  result <- polygons_to_linestring(poly_sf)

  # Check all original attributes are present
  expect_true("name" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("category" %in% names(result))

  # Check that attribute values are correct
  expect_equal(unique(result$name), "test_polygon")
  expect_equal(unique(result$value), 42)
  expect_equal(unique(result$category), "A")
})
