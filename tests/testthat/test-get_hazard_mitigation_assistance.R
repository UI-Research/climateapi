# Tests for get_hazard_mitigation_assistance.R

test_that("get_hazard_mitigation_assistance validates state_abbreviations", {
  expect_error(
    get_hazard_mitigation_assistance(state_abbreviations = "XX"),
    "Only the 50 states and DC"
  )
})

test_that("get_hazard_mitigation_assistance validates file paths", {
  expect_error(
    get_hazard_mitigation_assistance(
      file_path_old_grant_system = "/nonexistent/path.parquet",
      file_path_new_grant_system = "/also/nonexistent.parquet"
    ),
    "no file at the specified"
  )
})

test_that("get_hazard_mitigation_assistance function signature is correct", {
  expect_true(is.function(get_hazard_mitigation_assistance))

  # Check parameter names
  params <- names(formals(get_hazard_mitigation_assistance))
  expect_true("file_path_old_grant_system" %in% params)
  expect_true("file_path_new_grant_system" %in% params)
  expect_true("state_abbreviations" %in% params)

  # Check default for state_abbreviations is NULL (meaning all states)
  expect_null(formals(get_hazard_mitigation_assistance)$state_abbreviations)
})
