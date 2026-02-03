# Tests for get_structures.R

test_that("get_structures validates geography parameter", {
  expect_error(
    get_structures(
      boundaries = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326),
      geography = "invalid"
    ),
    "must be one of"
  )
})

test_that("get_structures validates boundaries CRS", {
  # Create boundary without CRS
  boundary_no_crs <- sf::st_sfc(sf::st_point(c(0, 0)))

  expect_error(
    get_structures(boundaries = boundary_no_crs),
    "must specify a spatial vector object"
  )
})

test_that("get_structures function signature is correct", {
  expect_true(is.function(get_structures))

  # Check parameter names
  params <- names(formals(get_structures))
  expect_true("boundaries" %in% params)
  expect_true("geography" %in% params)
  expect_true("keep_structures" %in% params)

  # Check defaults
  f <- get_structures
  expect_equal(formals(f)$geography, "county")
  expect_false(formals(f)$keep_structures)
})
