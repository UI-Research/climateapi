# Tests for get_preliminary_damage_assessments.R

test_that("get_preliminary_damage_assessments function signature is correct", {
  expect_true(is.function(get_preliminary_damage_assessments))

  # Check parameter names
  params <- names(formals(get_preliminary_damage_assessments))
  expect_true("file_path" %in% params)
  expect_true("directory_path" %in% params)
  expect_true("use_cache" %in% params)

  # Check defaults
  f <- get_preliminary_damage_assessments
  expect_true(formals(f)$use_cache)
})
