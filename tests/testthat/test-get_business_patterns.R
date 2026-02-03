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
