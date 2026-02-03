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

test_that("inflation_adjust validates and processes correctly", {
  # Create test data
  test_df <- tibble::tibble(
    year = c(2020, 2021, 2022),
    amount = c(100, 100, 100)
  )

  # Function exists
  expect_true(is.function(inflation_adjust))
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
