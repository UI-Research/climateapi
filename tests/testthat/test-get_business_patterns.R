# Tests for get_business_patterns()

testthat::test_that("naics_code_digits errors clearly when not in c(2,3)", {
  testthat::expect_error({get_business_patterns(year = 2022, naics_code_digits = 4)})
})


testthat::test_that("year errors clearly when year is outside accceptable range", {
  testthat::expect_error({get_business_patterns(year = 2025, naics_code_digits = 3)})
  testthat::expect_error({get_business_patterns(year = 1970, naics_code_digits = 3)})
})

testthat::test_that("geo errors clearly when geo is not 'county' or 'zipcode'; does not error when in 'county' or 'zipcode' ", {
  testthat::expect_error({get_business_patterns(geo = "tract", naics_code_digits = 3)})
  testthat::expect_no_error({get_business_patterns(geo = "county", naics_code_digits = 2)})
})

testthat::test_that("employees has no negative values", {
  test <- get_business_patterns(geo = "zipcode", naics_code_digits = 2)

  # Ensure the column exists
  testthat::expect_true(
    "employees" %in% names(test),
    info = "`employees` column is missing in the returned data."
  )

  # Assert no negative values (ignore NAs)
  testthat::expect_true(
    all(test$employees[!is.na(test$employees)] >= 0),
    info = "Found negative values in `employees`."
  )
})

# Tests for get_naics_codes()

testthat::test_that("get_naics_codes exists and is a function", {
  testthat::expect_true(is.function(get_naics_codes))
})

testthat::test_that("get_naics_codes validates year parameter", {
  testthat::expect_error(get_naics_codes(year = 1985), "1986 or later")
  testthat::expect_error(get_naics_codes(year = 2030), "2023")
})

testthat::test_that("get_naics_codes validates digits parameter", {
  testthat::expect_error(get_naics_codes(year = 2022, digits = 1), "between 2 and 6")
  testthat::expect_error(get_naics_codes(year = 2022, digits = 7), "between 2 and 6")
})

testthat::test_that("get_naics_codes returns tibble with expected columns", {
  result <- get_naics_codes(year = 2022, digits = 2)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true("naics_code" %in% names(result))
  testthat::expect_true("naics_label" %in% names(result))
  testthat::expect_true("year" %in% names(result))
})
testthat::test_that("get_naics_codes filters by digit count correctly", {
  result_2 <- get_naics_codes(year = 2022, digits = 2)
  result_3 <- get_naics_codes(year = 2022, digits = 3)

  # All 2-digit codes should have exactly 2 characters
  testthat::expect_true(all(nchar(result_2$naics_code) == 2))

  # All 3-digit codes should have exactly 3 characters
  testthat::expect_true(all(nchar(result_3$naics_code) == 3))

  # 3-digit should have more codes than 2-digit
  testthat::expect_gt(nrow(result_3), nrow(result_2))
})

testthat::test_that("get_naics_codes default digits is 3", {
  result <- get_naics_codes(year = 2022)

  testthat::expect_true(all(nchar(result$naics_code) == 3))
})

testthat::test_that("get_naics_codes year column matches requested year", {
  result <- get_naics_codes(year = 2020)

  testthat::expect_true(all(result$year == 2020))
})
