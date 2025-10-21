testthat::test_that("naics_code_digits errors clearly when not in c(2,3)", {
  testthat::expect_error({get_business_patterns(year = 2022, naics_code_digits = 3)})
})
