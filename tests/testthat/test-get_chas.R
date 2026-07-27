# Tests for get_chas_housing_affordability.R and the HUD API key helpers (register_hud_api_key / get_hud_api_key).
#
# These tests deliberately avoid the network (HUD API) and the Box mirror: they exercise
# argument validation and the branch-selection logic, all of which short-circuit before
# any API call, file read, or `directory_path` default evaluation.

test_that("get_chas_housing_affordability is a function with the expected arguments", {
  expect_true(is.function(get_chas_housing_affordability))
  expect_equal(
    names(formals(get_chas_housing_affordability)),
    c("geography", "end_year", "state_code", "entity_code", "api", "directory_path", "columns"))
})

test_that("get_chas_housing_affordability rejects invalid geographies", {
  expect_error(get_chas_housing_affordability("planet"), "geography")
  expect_error(get_chas_housing_affordability(c("county", "tract")), "geography")
})

test_that("get_chas_housing_affordability validates end_year", {
  expect_error(get_chas_housing_affordability("tract", end_year = "2021"), "single numeric year")
  expect_error(get_chas_housing_affordability("tract", end_year = c(2020, 2021)), "single numeric year")
})

test_that("get_chas_housing_affordability requires the API for non-tract geographies", {
  expect_error(get_chas_housing_affordability("county", api = FALSE), "only available via the API")
  expect_error(get_chas_housing_affordability("nation"), "only available via the API")
})

test_that("get_chas_housing_affordability does not offer tract data via the API", {
  expect_error(get_chas_housing_affordability("tract", api = TRUE), "not available via the API")
})

test_that("get_chas_codebook validates end_year", {
  expect_error(get_chas_codebook(end_year = "2021"), "single numeric year")
  expect_error(get_chas_codebook(end_year = c(2020, 2021)), "single numeric year")
})

test_that("get_chas_codebook returns an empty codebook when no dictionary is found", {
  ## an empty directory stands in for an unsynced Box mirror: the function should report
  ## the problem and return an empty codebook of the documented shape, not error, because
  ## get_chas_housing_affordability() relies on that to fall back to raw column names
  empty_directory <- withr::local_tempdir()

  expect_message(
    codebook <- get_chas_codebook(end_year = 2021, directory_path = empty_directory),
    "No CHAS data dictionary")

  expect_s3_class(codebook, "tbl_df")
  expect_equal(nrow(codebook), 0)
  expect_equal(
    names(codebook),
    c("column_name", "column_name_source", "chas_table", "column_type", "definition",
      "vintage"))
})

test_that("get_chas_codebook falls back to the most recent dictionary with a warning", {
  ## two empty files stand in for real dictionaries: which one to read is decided from the
  ## file names alone, so the choice can be checked without a real workbook. Reading the
  ## empty file fails immediately afterwards; that failure is ignored here.
  dictionary_directory <- withr::local_tempdir()
  file.create(file.path(
    dictionary_directory,
    c("2013thru2017-dictionary.xlsx", "2017thru2021-dictionary.xlsx")))

  ## the warnings raised for one request, ignoring the read failure that follows
  warnings_for = function(end_year) {
    warnings_seen = character()
    withCallingHandlers(
      try(
        get_chas_codebook(end_year = end_year, directory_path = dictionary_directory),
        silent = TRUE),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning") })
    warnings_seen }

  expect_match(
    warnings_for(2009), "using the most recent one available \\(2021\\)", all = FALSE)

  ## a period that is on disk is used as requested, without a warning
  expect_false(
    any(stringr::str_detect(warnings_for(2017), "most recent one available")))
})

test_that("get_hud_api_key errors when no key is set", {
  withr::local_envvar(HUD_API_KEY = "")
  expect_error(get_hud_api_key(), "No HUD API key")
})

test_that("register_hud_api_key validates its input", {
  expect_error(register_hud_api_key(123), "non-empty character")
  expect_error(register_hud_api_key(c("a", "b")), "non-empty character")
  expect_error(register_hud_api_key(NA_character_), "non-empty character")
  expect_error(register_hud_api_key(""), "non-empty character")
})

test_that("register_hud_api_key sets the session key (install = FALSE)", {
  withr::local_envvar(HUD_API_KEY = "")
  result = register_hud_api_key("test-key-123")
  expect_equal(result, "test-key-123")
  expect_equal(get_hud_api_key(), "test-key-123")
})
