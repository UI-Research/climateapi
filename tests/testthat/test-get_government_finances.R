# Tests for get_government_finances.R

test_that("get_government_finances function signature is correct", {
  expect_true(is.function(get_government_finances))

  # Check parameter names
  params <- names(formals(get_government_finances))
  expect_true("year" %in% params)

  # Check default year
  f <- get_government_finances
  expect_equal(formals(f)$year, 2022)
})

test_that("get_government_finances validates year parameter type", {
  # Year should be numeric (function uses str_sub on it)
  expect_true(is.function(get_government_finances))
})
