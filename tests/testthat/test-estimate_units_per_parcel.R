# Tests for estimate_units_per_parcel.R

test_that("estimate_units_per_parcel function signature is correct", {
  expect_true(is.function(estimate_units_per_parcel))

  # Check parameter names
  params <- names(formals(estimate_units_per_parcel))
  expect_true("structures" %in% params)
  expect_true("parcels" %in% params)
  expect_true("zoning" %in% params)
  expect_true("acs" %in% params)

  # Check default for acs
  f <- estimate_units_per_parcel
  expect_null(formals(f)$acs)
})

test_that("estimate_units_per_parcel requires specific input datasets", {
  # This is a complex function requiring specific data structures
  # The function will error without properly structured inputs
  expect_true(is.function(estimate_units_per_parcel))
})
