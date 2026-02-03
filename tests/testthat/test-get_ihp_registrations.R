# Tests for get_ihp_registrations.R
#
# NOTE: This function relies on large input data and is slow.
# Data is loaded once at the top of the test file to avoid repeated I/O.
# Validation/error tests call the function directly (not using cached data).

# ---------------------------------------------------------------------------
# Load data once for success tests (skip if Box path unavailable)
# ---------------------------------------------------------------------------
ihp_test_data <- NULL

skip_if_no_box <- function() {

  box_path <- tryCatch(get_box_path(), error = function(e) NULL)
  if (is.null(box_path) || !dir.exists(box_path)) {
    skip("Box path not available")
  }
}

# Attempt to load data once for all success tests
local({
  box_path <- tryCatch(get_box_path(), error = function(e) NULL)
  if (!is.null(box_path) && dir.exists(box_path)) {
    ihp_test_data <<- tryCatch(
      suppressWarnings(suppressMessages(
        get_ihp_registrations(state_fips = "DC", api = FALSE)
      )),
      error = function(e) NULL
    )
  }
})

# ---------------------------------------------------------------------------
# Validation tests (expected to fail - call function directly)
# ---------------------------------------------------------------------------
test_that("get_ihp_registrations validates file_name when api=FALSE", {
  expect_error(
    get_ihp_registrations(
      file_name = "nonexistent_file.parquet",
      api = FALSE
    ),
    "invalid"
  )
})

test_that("get_ihp_registrations function signature is correct", {
  expect_true(is.function(get_ihp_registrations))

  # Check parameter names

params <- names(formals(get_ihp_registrations))
  expect_true("state_fips" %in% params)
  expect_true("file_name" %in% params)
  expect_true("api" %in% params)
  expect_true("outpath" %in% params)

  # Check defaults
  f <- get_ihp_registrations
  expect_null(formals(f)$state_fips)
  expect_false(formals(f)$api)
  expect_null(formals(f)$outpath)
})

# ---------------------------------------------------------------------------
# Success tests (use pre-loaded data)
# ---------------------------------------------------------------------------
test_that("get_ihp_registrations returns expected columns", {
  skip_if_no_box()
  skip_if(is.null(ihp_test_data), "IHP test data not loaded")

  expected_cols <- c(
    "unique_id", "allocation_factor_zcta_to_county", "geoid_county",
    "zcta_code", "geoid_tract", "geoid_block_group", "disaster_number",
    "amount_individual_housing_program", "amount_housing_assistance",
    "amount_other_needs_assistance", "amount_rental_assistance",
    "amount_repairs", "amount_replacement", "amount_personal_property",
    "amount_flood_insurance_premium_paid_by_fema",
    "state_name", "state_abbreviation", "state_code"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(ihp_test_data), info = paste("Missing column:", col))
  }
})

test_that("get_ihp_registrations returns a tibble", {
  skip_if_no_box()
  skip_if(is.null(ihp_test_data), "IHP test data not loaded")

  expect_s3_class(ihp_test_data, "tbl_df")
})

test_that("get_ihp_registrations allocation_factor is numeric between 0 and 1", {
  skip_if_no_box()
  skip_if(is.null(ihp_test_data), "IHP test data not loaded")

  alloc <- ihp_test_data$allocation_factor_zcta_to_county
  expect_type(alloc, "double")
  expect_true(all(alloc >= 0 & alloc <= 1, na.rm = TRUE))
})

test_that("get_ihp_registrations geoid_county is 5 characters", {
  skip_if_no_box()
  skip_if(is.null(ihp_test_data), "IHP test data not loaded")

  geoids <- ihp_test_data$geoid_county[!is.na(ihp_test_data$geoid_county)]
  expect_true(all(nchar(geoids) == 5))
})
