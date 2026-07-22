# Absolute path to a local raw-data cache used by get_lodes()/get_business_patterns()
# tests, so re-running devtools::check() doesn't re-download the same data every time.
#
# Returns NULL (no caching -- tests hit the live API directly, same as before this
# helper existed) unless CLIMATEAPI_TEST_CACHE_DIR is set to an absolute path (e.g. in
# .Renviron). A relative path would not work here: devtools::check() builds and checks
# the package from a temporary directory disconnected from this source tree, so tests
# running under check() can't find a repo-relative "../../data" -- only an absolute
# path set outside that sandbox (via an environment variable) survives the trip.
get_test_cache_directory <- function() {
  cache_directory <- Sys.getenv("CLIMATEAPI_TEST_CACHE_DIR", unset = NA)

  if (is.na(cache_directory) || !nzchar(cache_directory)) {
    return(NULL)
  }

  cache_directory
}
