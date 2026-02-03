# Tests for get_business_patterns.R

test_that("get_business_patterns validates year parameter", {
  # Year must be 1986 or later
  expect_error(
    get_business_patterns(year = 1985),
    "1986 or later"
  )
})

test_that("get_business_patterns validates naics_code_digits parameter", {
  # naics_code_digits must be 2 or 3
  expect_error(
    get_business_patterns(naics_code_digits = 4),
    "must be one of"
  )

  expect_error(
    get_business_patterns(naics_code_digits = 1),
    "must be one of"
  )
})

test_that("get_business_patterns accepts valid parameters", {
  # These should not error on parameter validation
  # (actual API calls would require network access)
  expect_true(is.function(get_business_patterns))

  # Check default parameter values are acceptable
  f <- get_business_patterns
  expect_equal(formals(f)$year, 2022)
  expect_equal(formals(f)$naics_code_digits, 2)
  expect_null(formals(f)$naics_codes)
})
