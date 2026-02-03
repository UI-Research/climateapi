# Tests for get_emergency_management_performance.R

test_that("get_emergency_management_performance function signature is correct", {
  expect_true(is.function(get_emergency_management_performance))

  # Check parameter names
  params <- names(formals(get_emergency_management_performance))
  expect_true("file_path" %in% params)
  expect_true("api" %in% params)

  # Check defaults
  f <- get_emergency_management_performance
  expect_true(formals(f)$api)
})

test_that("get_emergency_management_performance validates file_path when api=FALSE", {
  expect_error(
    get_emergency_management_performance(
      file_path = "/nonexistent/path/file.csv",
      api = FALSE
    ),
    "valid `file_path`"
  )
})
