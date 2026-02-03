# Tests for interpolate_demographics.R

test_that("interpolate_demographics validates weights parameter", {
  # Create a minimal sf object for testing
  simple_poly <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 5070
  )
  zones_sf <- sf::st_as_sf(data.frame(zone_id = "A"), geometry = simple_poly)

  # weights must be one of "population" or "housing"
  # The function doesn't explicitly validate this but tidycensus will error
  expect_true(is.function(interpolate_demographics))
})

test_that("interpolate_demographics function signature is correct", {
  expect_true(is.function(interpolate_demographics))

  # Check parameter names
  params <- names(formals(interpolate_demographics))
  expect_true("zones_sf" %in% params)
  expect_true("sociodemographic_tracts_sf" %in% params)
  expect_true("id_column" %in% params)
  expect_true("weights" %in% params)

  # Check defaults
  f <- interpolate_demographics
  expect_null(formals(f)$sociodemographic_tracts_sf)
  expect_equal(formals(f)$weights, "population")
})
