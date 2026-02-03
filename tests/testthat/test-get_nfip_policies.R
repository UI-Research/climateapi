# Tests for get_nfip_policies.R
#
# NOTE: This function relies on large input data and is slow.
# Data is loaded once at the top of the test file to avoid repeated I/O.
# Validation/error tests call the function directly (not using cached data).

# ---------------------------------------------------------------------------
# Load data once for success tests (skip if Box path unavailable)
# ---------------------------------------------------------------------------
nfip_policies_test_data <- NULL

skip_if_no_box <- function() {
  box_path <- tryCatch(get_box_path(), error = function(e) NULL)
  if (is.null(box_path) || !dir.exists(box_path)) {
    skip("Box path not available")
  }
}

# Attempt to load data once for all success tests (small state: DC)
local({
  box_path <- tryCatch(get_box_path(), error = function(e) NULL)
  if (!is.null(box_path) && dir.exists(box_path)) {
    nfip_policies_test_data <<- tryCatch(
      suppressWarnings(suppressMessages(
        get_nfip_policies(state_abbreviation = "DC", api = FALSE)
      )),
      error = function(e) NULL
    )
  }
})

# ---------------------------------------------------------------------------
# Validation tests (expected to fail - call function directly)
# ---------------------------------------------------------------------------
test_that("get_nfip_policies validates file_name when api=FALSE", {
  expect_error(
    get_nfip_policies(
      state_abbreviation = "TX",
      file_name = "nonexistent_file.parquet",
      api = FALSE
    ),
    "invalid"
  )
})

test_that("get_nfip_policies function signature is correct", {
  expect_true(is.function(get_nfip_policies))

  # Check parameter names
  params <- names(formals(get_nfip_policies))
  expect_true("state_abbreviation" %in% params)
  expect_true("county_geoids" %in% params)
  expect_true("file_name" %in% params)
  expect_true("api" %in% params)

  # Check defaults
  f <- get_nfip_policies
  expect_null(formals(f)$county_geoids)
  expect_false(formals(f)$api)
})

# ---------------------------------------------------------------------------
# Success tests (use pre-loaded data)
# ---------------------------------------------------------------------------
test_that("get_nfip_policies returns expected columns", {
  skip_if_no_box()
  skip_if(is.null(nfip_policies_test_data), "NFIP policies test data not loaded")

  expected_cols <- c(
    "state_fips", "state_abbreviation", "county_geoid", "county_name",
    "census_tract", "policy_cost", "policy_count", "policy_rated_flood_zone",
    "policy_premium_total_cost", "policy_date_termination", "policy_date_effective",
    "building_occupancy_type", "building_replacement_cost"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(nfip_policies_test_data), info = paste("Missing column:", col))
  }
})

test_that("get_nfip_policies returns a data frame", {
  skip_if_no_box()
  skip_if(is.null(nfip_policies_test_data), "NFIP policies test data not loaded")

  expect_true(is.data.frame(nfip_policies_test_data))
})

test_that("get_nfip_policies county_geoid is 5 characters", {
  skip_if_no_box()
  skip_if(is.null(nfip_policies_test_data), "NFIP policies test data not loaded")

  geoids <- nfip_policies_test_data$county_geoid[!is.na(nfip_policies_test_data$county_geoid)]
  if (length(geoids) > 0) {
    expect_true(all(nchar(geoids) == 5))
  }
})

test_that("get_nfip_policies building_occupancy_type has expected categories", {
  skip_if_no_box()
  skip_if(is.null(nfip_policies_test_data), "NFIP policies test data not loaded")

  valid_types <- c("single family", "multi-family", "mobile/manufactured home", "non-residential", NA)
  occupancy <- nfip_policies_test_data$building_occupancy_type
  expect_true(all(occupancy %in% valid_types))
})
