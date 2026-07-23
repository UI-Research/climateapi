# Tests for get_chas.R and the HUD API key helpers (register_hud_api_key / get_hud_api_key).
#
# These tests deliberately avoid the network (HUD API) and the Box mirror: they exercise
# argument validation and the branch-selection logic, all of which short-circuit before
# any API call, file read, or `directory_path` default evaluation.

test_that("get_chas is a function with the expected arguments", {
  expect_true(is.function(get_chas))
  expect_equal(
    names(formals(get_chas)),
    c("geography", "end_year", "state_code", "entity_code", "api", "directory_path", "columns"))
})

test_that("get_chas rejects invalid geographies", {
  expect_error(get_chas("planet"), "geography")
  expect_error(get_chas(c("county", "tract")), "geography")
})

test_that("get_chas validates end_year", {
  expect_error(get_chas("tract", end_year = "2021"), "single numeric year")
  expect_error(get_chas("tract", end_year = c(2020, 2021)), "single numeric year")
})

test_that("get_chas requires the API for non-tract geographies", {
  expect_error(get_chas("county", api = FALSE), "only available via the API")
  expect_error(get_chas("nation"), "only available via the API")
})

test_that("get_chas does not offer tract data via the API", {
  expect_error(get_chas("tract", api = TRUE), "not available via the API")
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
