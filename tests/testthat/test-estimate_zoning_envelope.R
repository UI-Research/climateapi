# Tests for estimate_zoning_envelope.R

test_that("estimate_zoning_envelope validates required columns", {
  # Create test data missing required columns
  incomplete_df <- tibble::tibble(
    parcel_id = 1:3,
    parcel_area_sqft = c(1000, 2000, 3000)
  )

  expect_error(
    estimate_zoning_envelope(incomplete_df),
    "must contain the following columns"
  )
})

test_that("estimate_zoning_envelope validates parcel_dimensions consistency", {
  # If one parcel dimension is provided, both must be provided
  partial_df <- tibble::tibble(
    parcel_id = 1:3,
    parcel_area_sqft = c(1000, 2000, 3000),
    setback_front = 10,
    setback_rear = 10,
    setback_side = 10,
    parcel_area_sqft_minimum = 1000,
    units_per_parcel_maximum = 10,
    units_per_acre_maximum = NA,
    parcel_coverage_percent_maximum = 70,
    parcel_coverage_percent_maximum_building = 70,
    open_space_ratio_minimum = 0.2,
    floor_area_ratio_maximum = 2,
    height_stories_maximum = 3,
    height_feet_maximum = NA,
    parking_stalls_per_parcel_minimum = 1,
    parking_stalls_per_unit_minimum = 2,
    parcel_depth = c(50, 60, 70)  # Only depth provided, not width
  )

  expect_error(
    estimate_zoning_envelope(partial_df),
    "either both or neither"
  )
})

test_that("estimate_zoning_envelope function signature is correct", {
  expect_true(is.function(estimate_zoning_envelope))

  # Check parameter defaults
  f <- estimate_zoning_envelope
  expect_equal(formals(f)$development_size_maximum, 300)
  expect_equal(formals(f)$standard_unit_sqft_multifamily, 1000)
  expect_equal(formals(f)$standard_parking_stall_sqft, 325)
  expect_equal(formals(f)$parking_model, "singlestory")
})

test_that("estimate_zoning_envelope returns expected structure with valid input", {
  # Create valid test data
  valid_df <- tibble::tibble(
    parcel_id = 1:3,
    parcel_area_sqft = c(5000, 10000, 15000),
    setback_front = 10,
    setback_rear = 10,
    setback_side = 5,
    parcel_area_sqft_minimum = 1000,
    units_per_parcel_maximum = 10,
    units_per_acre_maximum = NA,
    parcel_coverage_percent_maximum = 70,
    parcel_coverage_percent_maximum_building = 70,
    open_space_ratio_minimum = 0.2,
    floor_area_ratio_maximum = 2,
    height_stories_maximum = 3,
    height_feet_maximum = NA,
    parking_stalls_per_parcel_minimum = 1,
    parking_stalls_per_unit_minimum = 1
  )

  result <- estimate_zoning_envelope(valid_df)

  # Check that result is a data frame
  expect_true(is.data.frame(result))

  # Check that maximum_development_capacity_zoned column exists
  expect_true("maximum_development_capacity_zoned" %in% names(result))

  # Check that all input rows are preserved
  expect_equal(nrow(result), 3)

  # Check that capacity values are non-negative
  expect_true(all(result$maximum_development_capacity_zoned >= 0))
})

test_that("estimate_zoning_envelope respects parking_model parameter", {
  valid_df <- tibble::tibble(
    parcel_id = 1,
    parcel_area_sqft = 10000,
    setback_front = 10,
    setback_rear = 10,
    setback_side = 5,
    parcel_area_sqft_minimum = 1000,
    units_per_parcel_maximum = 50,
    units_per_acre_maximum = NA,
    parcel_coverage_percent_maximum = 70,
    parcel_coverage_percent_maximum_building = 70,
    open_space_ratio_minimum = 0.2,
    floor_area_ratio_maximum = 5,
    height_stories_maximum = 5,
    height_feet_maximum = NA,
    parking_stalls_per_parcel_minimum = 1,
    parking_stalls_per_unit_minimum = 2
  )

  result_single <- estimate_zoning_envelope(valid_df, parking_model = "singlestory")
  result_multi <- estimate_zoning_envelope(valid_df, parking_model = "multistory")

  # Multistory parking should allow more units (parking distributed vertically)
  expect_true(result_multi$maximum_development_capacity_zoned >= result_single$maximum_development_capacity_zoned)
})
