# Tests for get_usgs_gage.R

test_that("get_usgs_gage validates counties parameter", {
  expect_error(get_usgs_gage(counties = 54097), "five-digit county FIPS")
  expect_error(get_usgs_gage(counties = character(0)), "five-digit county FIPS")
  expect_error(get_usgs_gage(counties = "WV"), "five-digit county FIPS")
  expect_error(get_usgs_gage(counties = c("54097", "3904")), "five-digit county FIPS")
})

test_that("get_usgs_gage validates measure and statistic parameters", {
  expect_error(get_usgs_gage(counties = "54097", measure = "00065"))
  expect_error(get_usgs_gage(counties = "54097", measure = "gage_height"))
  expect_error(get_usgs_gage(counties = "54097", statistic = "daily_median"))
})

test_that("get_usgs_gage validates date parameters", {
  expect_error(
    get_usgs_gage(counties = "54097", start_date = "01/01/2020"),
    "`start_date` must be")
  expect_error(
    get_usgs_gage(counties = "54097", end_date = "2020-1-1"),
    "`end_date` must be")
  expect_error(
    get_usgs_gage(counties = "54097", start_date = NULL),
    "`start_date` must be")
})

test_that("get_usgs_gage function signature is correct", {
  expect_true(is.function(get_usgs_gage))

  params <- names(formals(get_usgs_gage))
  expect_equal(
    params,
    c("counties", "measure", "statistic", "start_date", "end_date",
      "refresh_cache", "cache_dir"))

  f <- formals(get_usgs_gage)
  expect_equal(eval(f$measure), c("height", "discharge"))
  expect_equal(eval(f$statistic), c("daily_max", "daily_mean"))
  expect_equal(f$start_date, "")
  expect_equal(f$end_date, "")
  expect_false(f$refresh_cache)
})

test_that("get_usgs_gage returns the cached file without hitting the API", {
  cache_dir <- withr::local_tempdir()
  cached_history <- tibble::tibble(
    site_number = "03183500",
    gage_name = "Greenbrier River at Alderson, WV",
    county_geoid = "54063",
    county_name = "Monroe",
    state_abbreviation = "WV",
    latitude = 37.7,
    longitude = -80.6,
    drainage_area_sqmi = 1364,
    date = as.Date("2016-06-23"),
    value = 15.5,
    approval_status = "approved")

  ## the cache file name encodes the counties, measure, and statistic; a
  ## matching file must be returned as-is, with no network calls
  arrow::write_parquet(
    cached_history,
    file.path(cache_dir, "gage_history_54063_height_daily_mean.parquet"))

  result <- get_usgs_gage(
    counties = "54063", measure = "height", statistic = "daily_mean",
    cache_dir = cache_dir)
  expect_equal(result, cached_history)
})

test_that("get_usgs_gage returns daily means with expected structure", {
  skip_if_offline()

  ## one month of daily-mean discharge across one county's gages keeps the
  ## live pull small; Monroe County, WV (54063) contains the long-record
  ## Greenbrier River at Alderson gage (03183500)
  result <- tryCatch(
    suppressWarnings(suppressMessages(get_usgs_gage(
      counties = "54063",
      measure = "discharge",
      statistic = "daily_mean",
      start_date = "2020-01-01",
      end_date = "2020-01-31",
      cache_dir = withr::local_tempdir()))),
    error = function(e) NULL)
  skip_if(is.null(result) || nrow(result) == 0, "Live USGS API data not available")

  expect_equal(
    names(result),
    c("site_number", "gage_name", "county_geoid", "county_name",
      "state_abbreviation", "latitude", "longitude", "drainage_area_sqmi",
      "date", "value", "approval_status"))
  expect_true("03183500" %in% result$site_number)
  expect_true(all(result$county_geoid == "54063"))
  expect_true(all(result$state_abbreviation == "WV"))
  expect_true(all(result$date >= as.Date("2020-01-01") & result$date <= as.Date("2020-01-31")))
  expect_true(all(result$approval_status %in% c("approved", "provisional")))
  expect_true(all(!is.na(result$value)))
})

test_that("get_usgs_gage computes daily maxima from the continuous record", {
  skip_if_offline()

  ## a few days of the county's 15-minute records keeps the live pull small
  cache_dir <- withr::local_tempdir()
  result <- tryCatch(
    suppressWarnings(suppressMessages(get_usgs_gage(
      counties = "54063",
      measure = "height",
      statistic = "daily_max",
      start_date = "2020-01-01",
      end_date = "2020-01-05",
      cache_dir = cache_dir))),
    error = function(e) NULL)
  skip_if(is.null(result) || nrow(result) == 0, "Live USGS API data not available")

  expect_true(all(result$county_geoid == "54063"))
  ## one row per gage-day
  expect_equal(
    nrow(result),
    nrow(dplyr::distinct(result, site_number, date)))
  expect_true(all(result$approval_status %in% c("approved", "provisional")))

  ## the per-site parquet caches enable resumable pulls
  expect_gt(
    length(list.files(file.path(cache_dir, "by-site-height-daily-max"))), 0)
})
