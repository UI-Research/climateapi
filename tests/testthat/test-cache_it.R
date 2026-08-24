# Tests for cache_it.R

test_that("cache_it validates input parameters", {
  # Test that either object or file_name must be provided
  expect_error(
    cache_it(file_name = NULL, path = tempdir(), read = FALSE),
    "Either 'object' or 'file_name' must be provided"
  )
})

test_that("cache_it handles path validation", {
  test_data <- tibble::tibble(x = 1:5, y = letters[1:5])

  # Test with non-existent path (non-interactive should error)
  expect_error(
    cache_it(test_data, path = "/nonexistent/path/12345", read = FALSE)
  )
})

test_that("cache_it creates correct filename format", {
  # Create temporary directory for testing
  temp_dir <- tempdir()

  # Create test data
  test_data <- tibble::tibble(x = 1:5, y = letters[1:5])

  # Cache the data
  result <- cache_it(test_data, file_name = "test_cache", path = temp_dir, read = FALSE)

  # Check that file was created with correct naming pattern
  files <- list.files(temp_dir, pattern = "^test_cache_\\d{4}_\\d{2}_\\d{2}\\.parquet$")
  expect_true(length(files) >= 1)

  # Clean up
  unlink(file.path(temp_dir, files))
})

test_that("cache_it returns the cached object", {
  temp_dir <- tempdir()
  test_data <- tibble::tibble(x = 1:5, y = letters[1:5])

  result <- cache_it(test_data, file_name = "test_return", path = temp_dir, read = FALSE)

  # Result should be the same as input
  expect_equal(result$x, test_data$x)
  expect_equal(result$y, test_data$y)

  # Clean up
  files <- list.files(temp_dir, pattern = "^test_return")
  unlink(file.path(temp_dir, files))
})

test_that("cache_it read parameter works correctly", {
  temp_dir <- tempdir()
  test_data <- tibble::tibble(x = 1:5, y = letters[1:5])

  # First, write the data
  cache_it(test_data, file_name = "test_read", path = temp_dir, read = FALSE)

  # Then read it back (should find the cached file)
  result <- cache_it(file_name = "test_read", path = temp_dir, read = TRUE)

  expect_equal(result$x, test_data$x)
  expect_equal(result$y, test_data$y)

  # Clean up
  files <- list.files(temp_dir, pattern = "^test_read")
  unlink(file.path(temp_dir, files))
})

test_that("cache_it handles specific file read", {
  temp_dir <- tempdir()
  test_data <- tibble::tibble(x = 1:5, y = letters[1:5])

  # Write the data first
  result1 <- cache_it(test_data, file_name = "test_specific", path = temp_dir, read = FALSE)

  # Get the created filename
  files <- list.files(temp_dir, pattern = "^test_specific_\\d{4}_\\d{2}_\\d{2}\\.parquet$")
  expect_true(length(files) >= 1)

  # Read specific file
  result2 <- cache_it(file_name = "test_specific", path = temp_dir, read = files[1])

  expect_equal(result2$x, test_data$x)

  # Clean up
  unlink(file.path(temp_dir, files))
})

test_that("cache_it errors when reading non-existent specific file", {
  temp_dir <- tempdir()

  expect_error(
    cache_it(file_name = "test", path = temp_dir, read = "nonexistent_file.parquet"),
    "does not exist"
  )
})

test_that("cache_it preserves the units of difftime columns", {
  temp_dir <- tempdir()

  for (difftime_units in c("days", "hours", "mins", "weeks", "secs")) {
    test_data <- tibble::tibble(
      duration = as.difftime(c(1, 2, NA), units = difftime_units))

    cache_it(test_data, file_name = "test_difftime", path = temp_dir, read = FALSE)
    result <- cache_it(file_name = "test_difftime", path = temp_dir, read = TRUE)

    expect_equal(units(result$duration), difftime_units)
    expect_equal(result$duration, test_data$duration)

    files <- list.files(temp_dir, pattern = "^test_difftime")
    unlink(file.path(temp_dir, files))
  }
})

test_that("cache_it preserves difftime units when reading a specific file", {
  temp_dir <- tempdir()
  test_data <- tibble::tibble(duration = as.difftime(c(1, 2), units = "days"))

  cache_it(test_data, file_name = "test_difftime_specific", path = temp_dir, read = FALSE)
  files <- list.files(
    temp_dir, pattern = "^test_difftime_specific_\\d{4}_\\d{2}_\\d{2}\\.parquet$")

  result <- cache_it(file_name = "test_difftime_specific", path = temp_dir, read = files[1])

  expect_equal(units(result$duration), "days")
  expect_equal(result$duration, test_data$duration)

  unlink(file.path(temp_dir, files))
})

test_that("cache_it does not leave a units attribute on returned columns", {
  temp_dir <- tempdir()
  test_data <- tibble::tibble(
    duration = as.difftime(c(1, 2), units = "days"),
    x = 1:2)

  cache_it(test_data, file_name = "test_difftime_attr", path = temp_dir, read = FALSE)
  result <- cache_it(file_name = "test_difftime_attr", path = temp_dir, read = TRUE)

  expect_null(attr(result$duration, "climateapi_difftime_units"))
  expect_equal(result$x, test_data$x)

  files <- list.files(temp_dir, pattern = "^test_difftime_attr")
  unlink(file.path(temp_dir, files))
})

test_that("cache_it leaves frames without difftime columns unchanged", {
  temp_dir <- tempdir()
  test_data <- tibble::tibble(x = 1:3, y = letters[1:3])

  cache_it(test_data, file_name = "test_no_difftime", path = temp_dir, read = FALSE)
  result <- cache_it(file_name = "test_no_difftime", path = temp_dir, read = TRUE)

  expect_equal(result, test_data)

  files <- list.files(temp_dir, pattern = "^test_no_difftime")
  unlink(file.path(temp_dir, files))
})
