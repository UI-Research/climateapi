#' @importFrom magrittr %>%

#' @title Acquire daily stream-gage readings from USGS gages
#'
#' @description Pulls daily stream-gage readings, over each gage's full period of
#'   record by default, for USGS gages in one or more counties via the
#'   dataRetrieval package (USGS Water Data OGC APIs; these replace the
#'   now-decommissioning NWIS web services). Two statistics are supported:
#'   \itemize{
#'     \item "daily_mean": the published daily-mean series. Available for a
#'       century-plus at many gages.
#'     \item "daily_max": the maximum reading each day, computed here from the
#'       continuous (15-minute) record, because USGS publishes no daily-maximum
#'       gage-height series. Continuous records only begin in the mid-1990s at
#'       the earliest. These pulls are slow (a minute or more per long-record
#'       gage), so each gage's aggregated result is cached to its own parquet
#'       file and the pull resumes wherever it left off.
#'   }
#'
#' @param counties Character vector of five-digit county FIPS codes (e.g.,
#'   "54097" for Upshur County, WV). Every stream gage in these counties with
#'   data for the requested measure and statistic is pulled.
#' @param measure One of "height" (gage height in feet, the default; USGS
#'   parameter code 00065) or "discharge" (streamflow in cubic feet per second;
#'   USGS parameter code 00060).
#' @param statistic One of "daily_max" (the default; computed from continuous
#'   readings) or "daily_mean" (the published daily-value series). See the
#'   description for the record-length and runtime trade-offs.
#' @param start_date,end_date Character "YYYY-MM-DD" bounds on the readings. The
#'   defaults ("" for both) request each gage's full period of record; either
#'   bound may be supplied alone.
#' @param refresh_cache When TRUE, ignore cached parquet files (including the
#'   per-site continuous-record caches) and pull fresh data. Defaults to FALSE.
#'   Note that without a refresh, previously cached gages are frozen at the time
#'   they were pulled.
#' @param cache_dir Directory for cached parquet files. Defaults to a
#'   session-specific temporary directory; supply a persistent directory (e.g.,
#'   `tools::R_user_dir("climateapi", which = "cache")`) to keep the cache
#'   across sessions, which is strongly recommended for
#'   `statistic = "daily_max"` pulls so they can resume.
#'
#' @details Site metadata (name, county, coordinates, drainage area) is attached
#'   to every reading. Data are from the USGS Water Data APIs; see
#'   \url{https://api.waterdata.usgs.gov/}. USGS asks that heavy users register
#'   a free API key (see `dataRetrieval::setAccess()` documentation); unkeyed
#'   access is rate-limited but sufficient for modest pulls.
#'
#' @returns A tibble with one row per gage-day. Columns include:
#'   \describe{
#'     \item{site_number}{USGS site number.}
#'     \item{gage_name}{USGS station name.}
#'     \item{county_geoid}{Five-digit county FIPS code.}
#'     \item{county_name}{County name.}
#'     \item{state_abbreviation}{Two-letter state abbreviation.}
#'     \item{latitude, longitude}{Gage coordinates (decimal degrees).}
#'     \item{drainage_area_sqmi}{Upstream drainage area, in square miles.}
#'     \item{date}{Calendar day. For `statistic = "daily_max"`, the day is
#'       defined in the gage's local (Eastern) time zone.}
#'     \item{value}{The reading, in feet ("height") or cubic feet per second
#'       ("discharge").}
#'     \item{approval_status}{"approved" or "provisional". For
#'       `statistic = "daily_max"`, a day is "provisional" if any reading that
#'       day is provisional.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' ## daily maximum gage heights in Upshur and Lewis Counties, WV
#' get_usgs_gage(
#'   counties = c("54097", "54041"),
#'   cache_dir = tools::R_user_dir("climateapi", which = "cache"))
#'
#' get_usgs_gage(
#'   counties = "54063",
#'   measure = "discharge",
#'   statistic = "daily_mean")
#' }
get_usgs_gage = function(
    counties,
    measure = c("height", "discharge"),
    statistic = c("daily_max", "daily_mean"),
    start_date = "",
    end_date = "",
    refresh_cache = FALSE,
    cache_dir = file.path(tempdir(), "usgs-gage-history")) {

  measure = match.arg(measure)
  statistic = match.arg(statistic)
  parameter_code = dplyr::if_else(measure == "height", "00065", "00060")

  if (missing(counties) || !is.character(counties) || length(counties) == 0 ||
      any(!stringr::str_detect(counties, "^[0-9]{5}$"))) {
    stop("`counties` must be a character vector of five-digit county FIPS codes (e.g., '54097').") }

  purrr::walk2(
    list(start_date, end_date), c("start_date", "end_date"),
    function(date_value, date_name) {
      if (!is.character(date_value) || length(date_value) != 1 ||
          (date_value != "" && !stringr::str_detect(date_value, "^\\d{4}-\\d{2}-\\d{2}$"))) {
        stop("`", date_name, "` must be '' or a 'YYYY-MM-DD' string.") } })

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_path = file.path(
    cache_dir,
    stringr::str_c(
      "gage_history_",
      stringr::str_c(sort(unique(counties)), collapse = "-"), "_",
      measure, "_", statistic,
      dplyr::if_else(start_date == "", "", stringr::str_c("_from", start_date)),
      dplyr::if_else(end_date == "", "", stringr::str_c("_to", end_date)),
      ".parquet"))

  if (file.exists(cache_path) && !refresh_cache) {
    message("Reading cached file: ", basename(cache_path))
    return(arrow::read_parquet(cache_path)) }

  ## the monitoring-locations endpoint filters by two-digit state and
  ## three-digit county FIPS codes, so the five-digit codes are split and the
  ## endpoint queried once per state. Filtering to site_type_code "ST"
  ## restricts results to stream gages (the endpoint also serves wells, lakes,
  ## etc.). County FIPS codes, coordinates, drainage area, and station names
  ## all come from this endpoint; drainage area lets readers normalize
  ## discharge across basins of different sizes
  county_geoids = sort(unique(counties))
  site_metadata = county_geoids %>%
    split(stringr::str_sub(., 1, 2)) %>%
    purrr::imap(
      ~ dataRetrieval::read_waterdata_monitoring_location(
        state_code = .y,
        county_code = stringr::str_sub(.x, 3, 5),
        site_type_code = "ST")) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(
      latitude = sf::st_coordinates(geometry)[, "Y"],
      longitude = sf::st_coordinates(geometry)[, "X"]) %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      monitoring_location_id,
      site_number = monitoring_location_number,
      gage_name = monitoring_location_name,
      county_geoid = stringr::str_c(
        stringr::str_pad(state_code, 2, pad = "0"),
        stringr::str_pad(county_code, 3, pad = "0")),
      drainage_area_sqmi = drainage_area,
      latitude,
      longitude) %>%
    ## a state-plus-county query can only match the requested counties, but the
    ## endpoint treats each filter as an independent OR-list, so a multi-state
    ## request (e.g. "54097" and "39041") could otherwise return unrequested
    ## state-county combinations
    dplyr::filter(county_geoid %in% county_geoids)

  ## of the counties' stream sites, keep those with a series of the requested
  ## measure and statistic. daily_mean uses the published daily "Mean" series;
  ## daily_max must be computed from the continuous ("Instantaneous") record.
  ## Each series' period-of-record start is retained: the continuous endpoint
  ## needs explicit time bounds (see below)
  site_inventory = dataRetrieval::read_waterdata_ts_meta(
      monitoring_location_id = site_metadata$monitoring_location_id,
      parameter_code = parameter_code,
      computation_identifier = dplyr::if_else(
        statistic == "daily_mean", "Mean", "Instantaneous"),
      computation_period_identifier = dplyr::if_else(
        statistic == "daily_mean", "Daily", "Points"),
      skipGeometry = TRUE) %>%
    tibble::as_tibble() %>%
    ## a site can have several series (e.g. sublocations); keep the earliest
    dplyr::summarize(
      record_begin_date = as.Date(min(begin, na.rm = TRUE)),
      .by = monitoring_location_id)

  site_metadata = site_metadata %>%
    dplyr::filter(monitoring_location_id %in% site_inventory$monitoring_location_id)

  if (nrow(site_metadata) == 0) {
    stop(
      "No USGS stream gages with ", statistic, " ", measure,
      " data found in counties: ", stringr::str_c(county_geoids, collapse = ", ")) }

  county_names = tigris::fips_codes %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      county_geoid = stringr::str_c(state_code, county_code),
      county_name = stringr::str_remove(county, " County$"),
      state_abbreviation = state)

  ## the OGC APIs express time bounds as a start/end interval; ".." leaves an
  ## end open, and NA requests each gage's full period of record
  time_bounds = if (start_date == "" && end_date == "") {
    NA_character_
  } else {
    c(
      dplyr::if_else(start_date == "", "..", start_date),
      dplyr::if_else(end_date == "", "..", end_date)) }

  daily_readings = if (statistic == "daily_mean") {
    ## published daily-mean values; read_waterdata_daily() batches large site
    ## lists into multiple requests internally
    message(
      "Pulling daily means for ", nrow(site_metadata),
      " gages; this can take several minutes.")

    dataRetrieval::read_waterdata_daily(
      monitoring_location_id = site_metadata$monitoring_location_id,
      parameter_code = parameter_code,
      statistic_id = "00003", ## daily mean
      time = time_bounds,
      skipGeometry = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::transmute(
        monitoring_location_id,
        date = time,
        value,
        approval_status = dplyr::if_else(
          approval_status == "Approved", "approved", "provisional"))
  } else {
    ## daily maxima computed from the continuous (15-minute) record, one site
    ## at a time. Each site's full continuous record is a multi-minute
    ## download, so the aggregated daily maxima are cached per site and the pull
    ## resumes wherever it left off (which also makes it safe to run several
    ## worker processes over disjoint site lists against the same cache)
    site_cache_dir = file.path(
      cache_dir, stringr::str_c("by-site-", measure, "-daily-max"))
    dir.create(site_cache_dir, showWarnings = FALSE, recursive = TRUE)
    site_cache_paths = file.path(
      site_cache_dir, stringr::str_c(site_metadata$site_number, ".parquet"))
    uncached_ids = site_metadata$monitoring_location_id[
      refresh_cache | !file.exists(site_cache_paths)]

    if (length(uncached_ids) > 0) {
      message(
        "Pulling full continuous records for ", length(uncached_ids),
        " gages (", nrow(site_metadata) - length(uncached_ids),
        " already cached). Expect a minute or more per long-record gage.") }

    ## unlike the daily endpoint, the continuous endpoint silently returns only
    ## the most recent year when no time interval is given, and rejects
    ## intervals much beyond three years with an HTTP 400 -- so each site's
    ## record is requested in two-year chunks anchored at its period-of-record
    ## start
    site_record_begins = site_inventory %>%
      dplyr::filter(monitoring_location_id %in% uncached_ids)

    for (site_id in uncached_ids) {
      site_start = dplyr::if_else(
        start_date == "",
        site_record_begins$record_begin_date[
          site_record_begins$monitoring_location_id == site_id],
        as.Date(start_date))
      site_end = dplyr::if_else(
        end_date == "", Sys.Date() + 1, as.Date(end_date))

      chunk_starts = seq(site_start, site_end, by = "2 years")
      chunk_ends = c(utils::tail(chunk_starts, -1), site_end)

      site_daily_maxima = tryCatch(
        purrr::map2(
          chunk_starts, chunk_ends,
          ~ dataRetrieval::read_waterdata_continuous(
            monitoring_location_id = site_id,
            parameter_code = parameter_code,
            time = c(as.character(.x), as.character(.y)))) %>%
          purrr::list_rbind() %>%
          tibble::as_tibble() %>%
          dplyr::filter(!is.na(value)) %>%
          ## readings arrive in UTC; the calendar day is defined in the gage's
          ## local (Eastern) time zone. A day is provisional if any reading
          ## that day is provisional
          dplyr::mutate(
            date = as.Date(lubridate::with_tz(time, "America/New_York"))) %>%
          dplyr::summarize(
            value = max(value),
            approval_status = dplyr::if_else(
              all(approval_status == "Approved"), "approved", "provisional"),
            .by = c(monitoring_location_id, date)),
        error = function(e) {
          warning(
            "Continuous-record request failed for site ",
            stringr::str_remove(site_id, "^USGS-"), ": ", conditionMessage(e))
          NULL })
      if (!is.null(site_daily_maxima)) {
        arrow::write_parquet(
          site_daily_maxima,
          file.path(
            site_cache_dir,
            stringr::str_c(stringr::str_remove(site_id, "^USGS-"), ".parquet"))) } }

    available_cache_paths = file.path(
      site_cache_dir, stringr::str_c(site_metadata$site_number, ".parquet"))
    available_cache_paths[file.exists(available_cache_paths)] %>%
      purrr::map(arrow::read_parquet) %>%
      purrr::list_rbind()
  }

  gage_history = daily_readings %>%
    dplyr::filter(!is.na(value)) %>%
    tidylog::left_join(
      site_metadata, by = "monitoring_location_id", relationship = "many-to-one") %>%
    tidylog::left_join(county_names, by = "county_geoid", relationship = "many-to-one") %>%
    dplyr::select(
      site_number, gage_name, county_geoid, county_name, state_abbreviation,
      latitude, longitude, drainage_area_sqmi, date, value, approval_status)

  arrow::write_parquet(gage_history, cache_path)

  return(gage_history)
}

utils::globalVariables(c(
  "monitoring_location_id", "monitoring_location_number",
  "monitoring_location_name", "site_type_code", "state_code", "county_code",
  "drainage_area", "county", "state", "geometry", "time",
  "begin", "record_begin_date", ".",
  "site_number", "gage_name", "county_geoid", "county_name",
  "state_abbreviation", "latitude", "longitude", "drainage_area_sqmi",
  "value", "approval_status", "date"))
