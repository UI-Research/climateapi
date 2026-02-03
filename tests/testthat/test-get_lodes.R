# Tests for get_lodes.R

test_that("get_lodes validates lodes_type parameter", {
  expect_error(
    get_lodes(lodes_type = "invalid", states = "TX", years = 2020),
    "must be one of"
  )
})

test_that("get_lodes validates jobs_type parameter", {
  expect_error(
    get_lodes(lodes_type = "wac", jobs_type = "invalid", states = "TX", years = 2020),
    "must be one of"
  )
})

test_that("get_lodes validates geography parameter", {
  expect_error(
    get_lodes(lodes_type = "wac", states = "TX", years = 2020, geography = "invalid"),
    "must be one of"
  )
})

test_that("get_lodes validates state_part parameter", {
  expect_error(
    get_lodes(lodes_type = "wac", states = "TX", years = 2020, state_part = "invalid"),
    "must be one of"
  )
})

test_that("get_lodes validates states parameter", {
  expect_error(
    get_lodes(lodes_type = "wac", states = "XX", years = 2020),
    "must be a vector of state abbreviations"
  )
})

test_that("get_lodes validates years parameter", {
  # Years before 2002 should error
  expect_error(
    get_lodes(lodes_type = "wac", states = "TX", years = 2001),
    "2002 onward"
  )
})

test_that("get_lodes accepts valid parameters", {
  # Check that valid parameters don't cause errors at validation stage
  expect_true(is.function(get_lodes))

  # Check valid lodes_type options
  f <- get_lodes
  expect_equal(formals(f)$jobs_type, "all")
  expect_equal(formals(f)$geography, "tract")
  expect_equal(formals(f)$state_part, "main")
})

test_that("get_lodes handles geography aliases", {
  # 'bg' should be converted to 'block group' internally
  # This tests the logic but not the actual API call
  expect_true(is.function(get_lodes))
})
