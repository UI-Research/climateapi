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
