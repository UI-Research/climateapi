# Tests for get_sheldus.R

test_that("get_sheldus validates file_path", {
  expect_error(
    get_sheldus(file_path = "/nonexistent/path/file.csv"),
    "does not point to a valid file"
  )
})

test_that("get_sheldus function signature is correct", {
  expect_true(is.function(get_sheldus))

  # Check parameter name exists
  params <- names(formals(get_sheldus))
  expect_true("file_path" %in% params)
})
