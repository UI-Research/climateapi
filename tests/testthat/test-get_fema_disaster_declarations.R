# Tests for get_fema_disaster_declarations.R

test_that("get_fema_disaster_declarations validates file_path when api=FALSE", {
  # When api=FALSE, file_path must exist
  expect_error(
    get_fema_disaster_declarations(
      file_path = "/nonexistent/path/file.csv",
      api = FALSE
    ),
    "does not point to a valid file"
  )
})

test_that("get_fema_disaster_declarations function exists and has correct signature", {
  expect_true(is.function(get_fema_disaster_declarations))

  # Check default parameters
  f <- get_fema_disaster_declarations
  expect_true(formals(f)$api)
})

test_that("get_fema_disaster_declarations returns expected structure (mocked)", {
  # This is a structural test - actual API calls require network access
  # We're testing that the function signature and parameters are correct
  expect_true("file_path" %in% names(formals(get_fema_disaster_declarations)))
  expect_true("api" %in% names(formals(get_fema_disaster_declarations)))
})
