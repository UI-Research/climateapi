# Tests for read_ipums_cached.R

test_that("read_ipums_cached validates filename parameter", {
  expect_error(
    read_ipums_cached(
      filename = 123,
      download_directory = tempdir(),
      extract_definition = NULL
    ),
    "must be a character string"
  )
})

test_that("read_ipums_cached validates download_directory parameter", {
  expect_error(
    read_ipums_cached(
      filename = "test",
      download_directory = 123,
      extract_definition = NULL
    ),
    "must be a character string"
  )
})

test_that("read_ipums_cached validates refresh parameter", {
  expect_error(
    read_ipums_cached(
      filename = "test",
      download_directory = tempdir(),
      extract_definition = NULL,
      refresh = "yes"
    ),
    "must be either"
  )
})

test_that("read_ipums_cached validates download_directory exists", {
  expect_error(
    read_ipums_cached(
      filename = "test",
      download_directory = "/nonexistent/path",
      extract_definition = NULL
    ),
    "does not exist"
  )
})

test_that("read_ipums_cached function signature is correct", {
  expect_true(is.function(read_ipums_cached))

  # Check parameter names
  params <- names(formals(read_ipums_cached))
  expect_true("filename" %in% params)
  expect_true("download_directory" %in% params)
  expect_true("extract_definition" %in% params)
  expect_true("refresh" %in% params)

  # Check default for refresh
  expect_false(formals(read_ipums_cached)$refresh)
})
