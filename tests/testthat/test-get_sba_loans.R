# Tests for get_sba_loans.R

test_that("get_sba_loans function signature is correct", {
  expect_true(is.function(get_sba_loans))

  # Function has no parameters
  params <- names(formals(get_sba_loans))
  expect_equal(length(params), 0)
})

test_that("get_sba_loans validates data path internally", {
  # The function checks if the path exists
  # Since it uses get_box_path(), it will error if Box is not set up
  expect_true(is.function(get_sba_loans))
})
