# Tests for get_wildfire_burn_zones.R
#
# Data is loaded once at the top of the test file to avoid repeated I/O.
# The error-path test and the message test call the function directly, since
# they need to exercise the error/messaging behavior itself.

wfbz_data_path <- file.path(
  get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
  "wfbz_disasters_2000-2025.geojson")

skip_if_no_wfbz_data <- function() {
  skip_if_not(file.exists(wfbz_data_path), message = "Wildfire burn zones data file not available")
}

wfbz_test_data <- NULL

local({
  if (file.exists(wfbz_data_path)) {
    wfbz_test_data <<- tryCatch(
      suppressMessages(get_wildfire_burn_zones()),
      error = function(e) NULL)
  }
})

test_that("get_wildfire_burn_zones errors when file path does not exist", {
  expect_error(
    get_wildfire_burn_zones(file_path = "/nonexistent/path/to/file.geojson"),
    "does not point to a valid file"
  )
})

test_that("get_wildfire_burn_zones returns expected structure", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  expect_s3_class(result, "sf")

  expected_columns <- c(
    "wildfire_id", "id_fema", "year", "wildfire_name",
    "state_fips", "county_fips", "county_name",
    "area_sq_km", "wildfire_complex_binary",
    "date_start", "date_containment",
    "fatalities_total", "injuries_total",
    "structures_destroyed", "structures_threatened",
    "evacuation_total", "wui_type",
    "density_people_sq_km_wildfire_buffer", "geometry"
  )
  expect_true(all(expected_columns %in% names(result)))
})

test_that("get_wildfire_burn_zones has correct CRS", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  expect_equal(sf::st_crs(result)$epsg, 5070)
})

test_that("get_wildfire_burn_zones has one county per row", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  expect_false(any(stringr::str_detect(result$county_fips, "\\|"), na.rm = TRUE))
  expect_false(any(stringr::str_detect(result$county_name, "\\|"), na.rm = TRUE))
})

test_that("get_wildfire_burn_zones wildfire-level columns repeat identically across a multi-county wildfire's rows", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  multi_county_ids <- result$wildfire_id[duplicated(result$wildfire_id)] |> unique()
  # this test is only meaningful if the live data actually contains multi-county wildfires
  expect_true(length(multi_county_ids) > 0)

  wildfire_level_columns <- c(
    "area_sq_km", "fatalities_total", "injuries_total", "structures_destroyed",
    "structures_threatened", "evacuation_total", "wui_type",
    "density_people_sq_km_wildfire_buffer")

  n_distinct_by_wildfire <- result |>
    sf::st_drop_geometry() |>
    dplyr::filter(wildfire_id %in% multi_county_ids) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(wildfire_level_columns), dplyr::n_distinct),
      .by = wildfire_id)

  expect_true(all(dplyr::select(n_distinct_by_wildfire, -wildfire_id) == 1))

  n_distinct_geometry_by_wildfire <- result |>
    dplyr::filter(wildfire_id %in% multi_county_ids) |>
    dplyr::mutate(geometry_wkt = sf::st_as_text(geometry)) |>
    sf::st_drop_geometry() |>
    dplyr::summarize(n_distinct_geometry = dplyr::n_distinct(geometry_wkt), .by = wildfire_id)

  expect_true(all(n_distinct_geometry_by_wildfire$n_distinct_geometry == 1))

  # demonstrates the @returns-documented hazard directly: summing a wildfire-level column
  # across the county-expanded rows over-counts multi-county wildfires; de-duplicating on
  # wildfire_id first (as the docs instruct) avoids it
  naive_sum <- sum(result$area_sq_km, na.rm = TRUE)
  deduped_sum <- sum(result$area_sq_km[!duplicated(result$wildfire_id)], na.rm = TRUE)
  expect_gt(naive_sum, deduped_sum)
})

test_that("get_wildfire_burn_zones has valid FIPS codes", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  non_na_state_fips <- result$state_fips[!is.na(result$state_fips)]
  non_na_county_fips <- result$county_fips[!is.na(result$county_fips)]

  expect_true(all(stringr::str_length(non_na_state_fips) == 2))
  expect_true(all(stringr::str_length(non_na_county_fips) == 5))
  expect_true(all(result$state_fips == stringr::str_sub(result$county_fips, 1, 2), na.rm = TRUE))
})

test_that("get_wildfire_burn_zones county names use Census's canonical casing", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  non_na_names <- result$county_name[!is.na(result$county_name)]
  # county_name is joined from tidycensus::fips_codes (Census's own canonical casing) by
  # county_fips, rather than title-cased from the raw source string -- a naive
  # stringr::str_to_title() would mangle Mc-prefixed county names (e.g. "McKenzie" ->
  # "Mckenzie"), so we check against the real reference list instead of str_to_title()
  expect_true(all(non_na_names %in% tidycensus::fips_codes$county))
  expect_false(any(stringr::str_detect(non_na_names, "^Mc[a-z]")))
})

test_that("get_wildfire_burn_zones emits expected message", {
  skip_if_no_wfbz_data()

  expect_message(
    get_wildfire_burn_zones(),
    stringr::str_c(
    "Each observation represents a county affected by a wildfire burn zone disaster. ",
    "Wildfires spanning multiple counties appear as multiple rows. ",
    "Disasters are defined as wildfires that burned near a community and resulted in ",
    "at least one civilian fatality, one destroyed structure, or received federal disaster relief. ",
    "Geometries represent burn zone perimeters sourced from FIRED, MTBS, or NIFC datasets.")
  )
})

test_that("get_wildfire_burn_zones year column is integer", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  expect_type(result$year, "integer")
  expect_true(all(result$year >= 2000 & result$year <= 2025, na.rm = TRUE))
})

test_that("get_wildfire_burn_zones date columns are Date class", {
  skip_if_no_wfbz_data()
  skip_if(is.null(wfbz_test_data), "Wildfire burn zones test data not loaded")

  result <- wfbz_test_data

  expect_s3_class(result$date_start, "Date")
  expect_s3_class(result$date_containment, "Date")
})
