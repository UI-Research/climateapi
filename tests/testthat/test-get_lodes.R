
testthat::test_that("states clearly errors when invalid state abbreviation is supplied", {
  testthat::expect_error({get_lodes(lodes_type = "wac", year = 2022, states = "AB")})
  testthat::expect_error({get_lodes(lodes_type = "rac", year = 2022, states = "AB")})
  testthat::expect_error({get_lodes(lodes_type = "od", year = 2022, states = c("AL", "AM"))})
})

testthat::test_that("warning generated when missing state and year combination is supplied", {
  testthat::expect_warning(
    {get_lodes(lodes_type = "od", years = 2022, states = c("AK", "MN"), cache_directory = get_test_cache_directory())},
    "state-year combinations")
  ## pre-2015 years also trigger the federal-jobs reporting warning; capture both
  ## so neither escapes the expectation
  testthat::expect_warning(
    testthat::expect_warning(
      {get_lodes(lodes_type = "wac", years = 2009, states = c("DC", "MN"), cache_directory = get_test_cache_directory())},
      "state-year combinations"),
    "federal jobs")
})

testthat::test_that("error generated when invalid lodes_type is supplied", {
  testthat::expect_error({get_lodes(lodes_type = "dc", year = 2022, states = c("AK", "MN"))})
  testthat::expect_error({get_lodes(lodes_type = "mac", year = 2009, states = c("DC", "MN"))})
})

testthat::test_that("variables have no negative values", {
  ## states = "all" includes state-years missing from LODES (e.g., AK/MI in 2022),
  ## which triggers an expected availability warning
  test <- suppressWarnings(get_lodes(
    lodes_type = "wac", years = 2022, states = "all",
    cache_directory = get_test_cache_directory()))

  # Select numeric columns
  num_df <- dplyr::select(test, where(is.numeric))

  # Identify if any numeric columns have any negative values (ignoring NAs)
  neg_cols <- num_df %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ any(.x < 0, na.rm = TRUE))) %>%
    tidyr::pivot_longer(everything(), names_to = "col", values_to = "has_neg") %>%
    dplyr::filter(has_neg) %>%
    dplyr::pull(col)

  # Expect no negatives anywhere; if present, list the columns
  testthat::expect_true(
    length(neg_cols) == 0,
    info = if (length(neg_cols) > 0)
      sprintf("Negative values found in: %s", paste(neg_cols, collapse = ", "))
    else
      NULL
  )
})
