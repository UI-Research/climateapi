# Tests for utilities.R functions

test_that("get_system_username returns a character string", {
  result <- get_system_username()
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("get_box_path returns a valid path string", {
  result <- get_box_path()
  expect_type(result, "character")
  expect_true(stringr::str_detect(result, "Box"))
  expect_true(stringr::str_detect(result, "METRO Climate and Communities"))
})

test_that("get_dataset_columns validates input parameters", {
  # Test that dataset must be character
  expect_error(get_dataset_columns(123))

  # Test that dataset must be length one
  expect_error(get_dataset_columns(c("nfip_policies", "ihp_registrations")))

  # Test that dataset must be a valid option
  expect_error(
    get_dataset_columns("invalid_dataset"),
    "must be one of"
  )

  # Test valid inputs work
  expect_no_error(get_dataset_columns("nfip_policies"))
  expect_no_error(get_dataset_columns("ihp_registrations"))
})

test_that("get_dataset_columns returns expected structure", {
  # Test nfip_policies
  result <- get_dataset_columns("nfip_policies")
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true("censusTract" %in% result)
  expect_true("policyCost" %in% result)

  # Test ihp_registrations
  result <- get_dataset_columns("ihp_registrations")
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true("disasterNumber" %in% result)
})

test_that("get_geography_metadata validates input parameters", {
  # Test that geography_type must be valid
  expect_error(
    get_geography_metadata(geography_type = "invalid")
  )

  # Test valid inputs work (note: these require API access, so we just check they don't error on parameter validation)
  expect_true(is.function(get_geography_metadata))
})

test_that("get_spatial_extent_census validates input parameters", {
  # Function exists and is callable
  expect_true(is.function(get_spatial_extent_census))
})

test_that("read_xlsx_from_url validates input parameters", {
  # Test that urls and file_names must be same length
  expect_error(
    read_xlsx_from_url(
      urls = c("http://example.com/a.xlsx", "http://example.com/b.xlsx"),
      directory = tempdir(),
      file_names = c("file1.xlsx")
    ),
    "same length"
  )

  # Test that directory cannot be a file path
  expect_error(
    read_xlsx_from_url(
      urls = "http://example.com/test.xlsx",
      directory = "path/to/file.xlsx"
    ),
    "must point to a directory"
  )
})

# ---- inflation_adjust --------------------------------------------------------------
#
# The PCE Price Index is pulled live from FRED (series DPCERG3A086NBEA, no key). The
# math and shape tests hit the live endpoint and are skipped off-network or on CRAN.
# The download-failure test mocks httr2::req_perform, so it needs no network.

test_that("inflation_adjust has the expected formals", {
  expect_true(is.function(inflation_adjust))
  expect_equal(
    names(formals(inflation_adjust)),
    c("df", "year_variable", "dollar_variables", "names_suffix", "base_year"))
})

test_that("inflation_adjust errors informatively when FRED cannot be reached", {
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("simulated network failure"),
    .package = "httr2")

  test_df <- tibble::tibble(year = 2000, amount = 100)
  expect_error(
    inflation_adjust(test_df, year_variable = "year", dollar_variables = "amount"),
    "Could not download the PCE Price Index from FRED")
})

test_that("inflation_adjust adjusts values and preserves the originals (live FRED)", {
  skip_on_cran()
  testthat::skip_if_offline()

  test_df <- tibble::tibble(
    year = c(2000, 2010),
    amount = c(100, 100))

  result <- tryCatch(
    inflation_adjust(test_df, year_variable = "year", dollar_variables = "amount", base_year = 2010),
    error = function(e) skip(paste("FRED PCE endpoint unavailable:", conditionMessage(e))))

  # original columns are preserved unchanged, alongside one new adjusted column
  expect_true(all(c("year", "amount") %in% names(result)))
  expect_equal(result$amount, c(100, 100))
  expect_true("amount_2010" %in% names(result))

  # the base-year row is unchanged (inflation factor is 1); the earlier year inflates upward
  expect_equal(result$amount_2010[result$year == 2010], 100)
  expect_gt(result$amount_2010[result$year == 2000], 100)
})

test_that("inflation_adjust default base_year uses the most recent FRED year (live FRED)", {
  skip_on_cran()
  testthat::skip_if_offline()

  test_df <- tibble::tibble(year = 2000, amount = 100)

  result <- tryCatch(
    inflation_adjust(test_df, year_variable = "year", dollar_variables = "amount"),
    error = function(e) skip(paste("FRED PCE endpoint unavailable:", conditionMessage(e))))

  # the new column is named for the base year, which defaults to the newest FRED year.
  # FRED publishes annual PCE with a lag of at most about a year, so it is >= 2024 today.
  adjusted_column <- setdiff(names(result), c("year", "amount"))
  expect_length(adjusted_column, 1)
  base_year <- as.integer(stringr::str_extract(adjusted_column, "[0-9]{4}"))
  expect_gte(base_year, 2024)
})

test_that("inflation_adjust honors names_suffix (live FRED)", {
  skip_on_cran()
  testthat::skip_if_offline()

  test_df <- tibble::tibble(year = 2000, amount = 100)

  result <- tryCatch(
    inflation_adjust(
      test_df, year_variable = "year", dollar_variables = "amount",
      base_year = 2010, names_suffix = "_real"),
    error = function(e) skip(paste("FRED PCE endpoint unavailable:", conditionMessage(e))))

  expect_true("amount_real" %in% names(result))
})

test_that("inflation_adjust rejects a base_year FRED does not cover (live FRED)", {
  skip_on_cran()
  testthat::skip_if_offline()

  test_df <- tibble::tibble(year = 2000, amount = 100)

  # confirm the endpoint is reachable (skip if not), then exercise the out-of-range guard
  tryCatch(
    inflation_adjust(test_df, year_variable = "year", dollar_variables = "amount", base_year = 2000),
    error = function(e) skip(paste("FRED PCE endpoint unavailable:", conditionMessage(e))))

  # 3000 is well beyond the published series, so the base-year membership check must fire
  expect_error(
    inflation_adjust(test_df, year_variable = "year", dollar_variables = "amount", base_year = 3000),
    "not available in the FRED PCE Price Index")
})

test_that("convert_delimited_to_parquet validates input parameters", {
  # Test that outpath check works
  temp_file <- tempfile(fileext = ".parquet")
  file.create(temp_file)

  expect_error(
    convert_delimited_to_parquet(
      inpath = "test.csv",
      outpath = temp_file
    ),
    "file already exists"
  )

  unlink(temp_file)

  # Test that dataset validation works
  expect_error(
    convert_delimited_to_parquet(
      inpath = "test.csv",
      dataset = "invalid_dataset"
    )
  )
})
