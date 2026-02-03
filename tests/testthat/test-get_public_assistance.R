# Tests for get_public_assistance.R

test_that("get_public_assistance validates state_abbreviations", {
  expect_error(
    get_public_assistance(state_abbreviations = "XX"),
    "Only the 50 states and DC"
  )
})

test_that("get_public_assistance function signature is correct", {
  expect_true(is.function(get_public_assistance))

  # Check parameter names
  params <- names(formals(get_public_assistance))
  expect_true("file_path" %in% params)
  expect_true("state_abbreviations" %in% params)

  # Check default for state_abbreviations is NULL (meaning all states)
  expect_null(formals(get_public_assistance)$state_abbreviations)
})
