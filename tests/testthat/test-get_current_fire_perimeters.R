# Tests for get_current_fire_perimeters.R

test_that("get_current_fire_perimeters validates geography parameter", {
  # geography must be NULL
  expect_error(
    get_current_fire_perimeters(geography = "county"),
    "must be NULL"
  )
})

test_that("get_current_fire_perimeters validates api parameter", {
  # api must be TRUE
  expect_error(
    get_current_fire_perimeters(api = FALSE),
    "must be queried from the API"
  )
})

test_that("get_current_fire_perimeters validates bbox parameter", {
  # Invalid bbox should error (either with custom message or st_bbox error)
  expect_error(
    get_current_fire_perimeters(bbox = "invalid")
  )
})

test_that("get_current_fire_perimeters function signature is correct", {
  expect_true(is.function(get_current_fire_perimeters))

  # Check parameter names
  params <- names(formals(get_current_fire_perimeters))
  expect_true("geography" %in% params)
  expect_true("file_path" %in% params)
  expect_true("bbox" %in% params)
  expect_true("api" %in% params)

  # Check defaults
  f <- get_current_fire_perimeters
  expect_null(formals(f)$geography)
  expect_null(formals(f)$file_path)
  expect_null(formals(f)$bbox)
  expect_true(formals(f)$api)
})
