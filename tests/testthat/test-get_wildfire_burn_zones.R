test_that("get_wildfire_burn_zones errors when file path does not exist", {
  expect_error(
    get_wildfire_burn_zones(file_path = "/nonexistent/path/to/file.geojson"),
    "does not point to a valid file"
  )
})

test_that("get_wildfire_burn_zones returns expected structure", {
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

  result <- suppressMessages(get_wildfire_burn_zones())

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
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

  result <- suppressMessages(get_wildfire_burn_zones())

  expect_equal(sf::st_crs(result)$epsg, 5070)
})

test_that("get_wildfire_burn_zones has one county per row", {
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

  result <- suppressMessages(get_wildfire_burn_zones())

  expect_false(any(stringr::str_detect(result$county_fips, "\\|"), na.rm = TRUE))
  expect_false(any(stringr::str_detect(result$county_name, "\\|"), na.rm = TRUE))
})

test_that("get_wildfire_burn_zones has valid FIPS codes", {
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

  result <- suppressMessages(get_wildfire_burn_zones())

  non_na_state_fips <- result$state_fips[!is.na(result$state_fips)]
  non_na_county_fips <- result$county_fips[!is.na(result$county_fips)]

  expect_true(all(stringr::str_length(non_na_state_fips) == 2))
  expect_true(all(stringr::str_length(non_na_county_fips) == 5))
  expect_true(all(result$state_fips == stringr::str_sub(result$county_fips, 1, 2), na.rm = TRUE))
})

test_that("get_wildfire_burn_zones county names are title case", {
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

  result <- suppressMessages(get_wildfire_burn_zones())

  non_na_names <- result$county_name[!is.na(result$county_name)]
  expect_equal(non_na_names, stringr::str_to_title(non_na_names))
})

test_that("get_wildfire_burn_zones emits expected message", {
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

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
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

  result <- suppressMessages(get_wildfire_burn_zones())

  expect_type(result$year, "integer")
  expect_true(all(result$year >= 2000 & result$year <= 2025, na.rm = TRUE))
})

test_that("get_wildfire_burn_zones date columns are Date class", {
  skip_if_not(
    file.exists(file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")),
    message = "Wildfire burn zones data file not available"
  )

  result <- suppressMessages(get_wildfire_burn_zones())

  expect_s3_class(result$date_start, "Date")
  expect_s3_class(result$date_containment, "Date")
})
