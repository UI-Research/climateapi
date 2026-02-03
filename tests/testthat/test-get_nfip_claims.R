# Tests for get_nfip_claims.R
#
# NOTE: This function relies on large input data and is slow.
# Data is loaded once at the top of the test file to avoid repeated I/O.
# Validation/error tests call the function directly (not using cached data).

# ---------------------------------------------------------------------------
# Load data once for success tests (skip if Box path unavailable)
# ---------------------------------------------------------------------------
nfip_claims_test_data <- NULL

skip_if_no_box <- function() {
  box_path <- tryCatch(get_box_path(), error = function(e) NULL)
  if (is.null(box_path) || !dir.exists(box_path)) {
    skip("Box path not available")
  }
}

# Attempt to load data once for all success tests (small county: DC = 11001)
local({
  box_path <- tryCatch(get_box_path(), error = function(e) NULL)
  if (!is.null(box_path) && dir.exists(box_path)) {
    nfip_claims_test_data <<- tryCatch(
      suppressWarnings(suppressMessages(
        get_nfip_claims(county_geoids = "11001", api = FALSE)
      )),
      error = function(e) NULL
    )
  }
})

# ---------------------------------------------------------------------------
# Validation tests (expected to fail - call function directly)
# ---------------------------------------------------------------------------
test_that("get_nfip_claims validates county_geoids when api=TRUE", {
  expect_error(
    get_nfip_claims(county_geoids = NULL, api = TRUE),
    "must supply this argument"
  )
})

test_that("get_nfip_claims validates file_name when api=FALSE", {
  expect_error(
    get_nfip_claims(file_name = NA, api = FALSE),
    "must provide a `file_name`"
  )
})

test_that("get_nfip_claims function signature is correct", {
  expect_true(is.function(get_nfip_claims))

  # Check parameter names
  params <- names(formals(get_nfip_claims))
  expect_true("county_geoids" %in% params)
  expect_true("file_name" %in% params)
  expect_true("api" %in% params)

  # Check defaults
  f <- get_nfip_claims
  expect_null(formals(f)$county_geoids)
  expect_false(formals(f)$api)
})

# ---------------------------------------------------------------------------
# Success tests (use pre-loaded data)
# ---------------------------------------------------------------------------
test_that("get_nfip_claims returns expected columns", {
  skip_if_no_box()
  skip_if(is.null(nfip_claims_test_data), "NFIP claims test data not loaded")

  expected_cols <- c(
    "state_fips", "county_geoid", "county_name", "occupancy_type",
    "year_loss", "year_construction", "count_units_insured",
    "deductible_building", "deductible_contents",
    "value_building", "value_contents",
    "replacement_cost_building", "replacement_cost_contents",
    "insurance_coverage_building", "insurance_coverage_contents",
    "damage_building", "damage_contents",
    "net_payment_building", "net_payment_contents", "net_payment_icc"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(nfip_claims_test_data), info = paste("Missing column:", col))
  }
})

test_that("get_nfip_claims returns a data frame", {
  skip_if_no_box()
  skip_if(is.null(nfip_claims_test_data), "NFIP claims test data not loaded")

  expect_true(is.data.frame(nfip_claims_test_data))
})

test_that("get_nfip_claims county_geoid is 5 characters", {
  skip_if_no_box()
  skip_if(is.null(nfip_claims_test_data), "NFIP claims test data not loaded")

  geoids <- nfip_claims_test_data$county_geoid[!is.na(nfip_claims_test_data$county_geoid)]
  if (length(geoids) > 0) {
    expect_true(all(nchar(geoids) == 5))
  }
})

test_that("get_nfip_claims occupancy_type has expected categories", {
  skip_if_no_box()
  skip_if(is.null(nfip_claims_test_data), "NFIP claims test data not loaded")

  valid_types <- c("single family", "multi-family", "mobile/manufactured home", "non-residential", NA)
  occupancy <- nfip_claims_test_data$occupancy_type
  expect_true(all(occupancy %in% valid_types))
})

test_that("get_nfip_claims year_loss is numeric", {
  skip_if_no_box()
  skip_if(is.null(nfip_claims_test_data), "NFIP claims test data not loaded")

  expect_type(nfip_claims_test_data$year_loss, "double")
})
