#' @title Get FEMA National Risk Index scores
#'
#' @description Downloads the complete FEMA National Risk Index (NRI) for every US
#'   census tract or county. The NRI characterizes a community's relative risk from
#'   18 natural hazards by combining expected annual loss, social vulnerability, and
#'   community resilience. All fields published by the NRI feature service are
#'   returned, with the source's abbreviated field codes expanded into descriptive
#'   snake_case column names (see Details).
#'
#' @param geography The geographic summary level of the results. One of "tract"
#'   (the default) or "county".
#' @param cache_path Optional path to a `.parquet` file used as a read-through
#'   cache. If supplied and the file already exists, data are read from it and no
#'   download occurs. If supplied and the file does not exist, freshly downloaded
#'   data are written there for reuse. If `NULL` (the default), data are downloaded
#'   fresh and not written to disk. Because the NRI is periodically revised (see
#'   Details), delete a stale cache file to force a refresh.
#'
#' @details Data are pulled from FEMA's National Risk Index ArcGIS feature services
#'   (one service per geography) and paginated 2,000 records at a time. After
#'   `janitor::clean_names()` normalization, the NRI's abbreviated field codes are
#'   expanded into readable snake_case names (for example, `WFIR_EALT` becomes
#'   `wildfire_estimated_annual_loss_total`, and `EAL_SPCTL` becomes
#'   `estimated_annual_loss_state_percentile`); the ArcGIS service artifacts
#'   (`OBJECTID`, `Shape__Area`, `Shape__Length`) and redundant source identifier
#'   columns are dropped, and a standardized `geoid` join key is prepended.
#'
#'   Interpreting the fields depends on the suffix:
#'   \itemize{
#'     \item `*_score`, `*_state_percentile`, and `*_national_percentile` columns are
#'       **values from 0 to 100**: `*_score` and `*_national_percentile` rank a
#'       community against all US communities of the same type (all tracts, or all
#'       counties), while `*_state_percentile` ranks it against communities in the
#'       same state. Because these are ranks rather than additive quantities, they
#'       **cannot be summed or averaged across geographies**; to describe risk for an
#'       area spanning several tracts, request `geography = "county"` (or higher)
#'       directly rather than aggregating tract scores.
#'     \item `*_rating` columns are categorical labels (e.g. "Relatively High").
#'     \item Value and loss columns (`*_estimated_annual_loss_*`, `*_exposure_*`,
#'       `*_exposed_area`, `*_event_count`, and the community totals `value_building`,
#'       `value_agriculture`, `population`, and `area_sq_mi`) are absolute quantities
#'       (dollars, counts, or areas) and *are* additive across geographies.
#'     \item Rate and ratio columns (`*_annual_frequency`, `*_historic_loss_ratio_*`,
#'       and `*_expected_annual_loss_rate_*`) are normalized rates rather than absolute
#'       quantities and, like the percentile columns, **cannot be summed across
#'       geographies**.
#'   }
#'
#'   Field definitions are documented in FEMA's NRI Technical Documentation and data
#'   dictionary at <https://hazards.fema.gov/nri/data-resources> (which uses the
#'   original abbreviated field codes). Hazard coverage reflects NRI v1.20 (December
#'   2025), in which inland flooding (the `inland_flood_*` columns) replaced the
#'   earlier riverine flooding hazard; the source version is carried in the
#'   `nri_version` column. If a future NRI release changes the schema, the underlying
#'   query surfaces the service error rather than silently truncating results.
#'
#' @returns A tibble with one row per census tract or county. Every field published
#'   by the NRI service is returned, with the source's abbreviated field codes expanded
#'   into descriptive snake_case names (see Details). Columns fall into these families:
#'   \describe{
#'     \item{Identifiers}{`geoid` (a standardized, zero-padded 11-digit tract or
#'       5-digit county FIPS join key, prepended by this function), the `state_name`
#'       and `county_name` labels, and the source version `nri_version`. The NRI's
#'       redundant identifier fields (`nri_id`, the individual state and county FIPS
#'       codes, and `tractfips`) are dropped in favor of `geoid`.}
#'     \item{Community totals}{`population`, `value_building` (building value, dollars),
#'       `value_agriculture` (agricultural value, dollars), and `area_sq_mi` (square
#'       miles).}
#'     \item{Composite index families}{`risk_*` (overall National Risk Index),
#'       `estimated_annual_loss_*` and `expected_annual_loss_rate_composite_*`
#'       (expected annual loss and its annualized rate), `social_vulnerability_index_*`
#'       (social vulnerability), `resilience_*` (community resilience), and
#'       `community_risk_factor_value`. Each family carries some combination of
#'       `_score`/`_state_percentile`/`_national_percentile` (percentile), `_rating`
#'       (rating), and `_value`/`_value_*` (absolute) columns.}
#'     \item{Per-hazard metrics}{One block per hazard, prefixed by the hazard name:
#'       `avalanche`, `coastal_flood`, `cold_wave`, `drought`, `earthquake`, `hail`,
#'       `heat_wave`, `hurricane`, `ice_storm`, `landslide`, `lightning`,
#'       `inland_flood`, `severe_wind`, `tornado`, `tsunami`, `volcano`,
#'       `wildfire`, and `winter_weather`. Within each block, suffixes denote the
#'       metric: `_event_count`/`_annual_frequency` (event count and annualized
#'       frequency), `_exposure_*` and `_exposed_area` (exposure), `_historic_loss_ratio_*`
#'       (historic loss ratio), `_estimated_annual_loss_*` (expected annual loss),
#'       `_expected_annual_loss_rate_*` (annualized loss rate), and `_risk_value`/
#'       `_risk_score`/`_risk_rating` (hazard risk value, score, and rating).}
#'   }
#'   Coastal or otherwise geographically limited hazards are `NA` for communities with
#'   no exposure.
#' @export
#' @examples
#' \dontrun{
#' # county-level scores, downloaded fresh
#' nri_counties <- get_national_risk_index(geography = "county")
#'
#' # tract-level scores, cached locally for reuse across sessions
#' nri_tracts <- get_national_risk_index(
#'   geography = "tract",
#'   cache_path = file.path(tempdir(), "nri_tracts.parquet"))
#' }
get_national_risk_index = function(
    geography = c("tract", "county"),
    cache_path = NULL) {

  geography = match.arg(geography)

  ## read-through cache: return the cached copy if the caller supplied a path to one
  if (!is.null(cache_path) && file.exists(cache_path)) {
    message("Reading cached National Risk Index data from: ", cache_path)
    return(arrow::read_parquet(cache_path))
  }

  service_name = if (geography == "tract") {
    "National_Risk_Index_Census_Tracts"
  } else {
    "National_Risk_Index_Counties"
  }
  service_url = stringr::str_c(
    "https://services.arcgis.com/XG15cJAlne2vxtgt/arcgis/rest/services/",
    service_name, "/FeatureServer/0/query")

  ## the FIPS field that uniquely identifies each row; already a zero-padded string
  ## in the source, but re-padded defensively when building the `geoid` key below
  geoid_field = if (geography == "tract") "TRACTFIPS" else "STCOFIPS"

  page_size = 2000

  fetch_page = function(offset) {
    response = httr2::request(service_url) |>
      httr2::req_url_query(
        where = "1=1",
        ## pull every field the service exposes rather than a curated subset
        outFields = "*",
        ## explicit ordering makes resultOffset pagination deterministic, so pages
        ## can't overlap or leave gaps if the service's default order shifts
        orderByFields = geoid_field,
        returnGeometry = "false",
        resultOffset = offset,
        resultRecordCount = page_size,
        f = "json") |>
      httr2::req_timeout(120) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform() |>
      ## simplifyDataFrame turns the features array into a data frame in one pass
      ## (missing values become NA), far faster than per-feature assembly for the
      ## full ~460-column schema
      httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = TRUE)

    ## ArcGIS reports query errors (e.g. a schema change) as an `error` object inside
    ## an HTTP 200 body, which req_perform() won't raise; surface it here so a failed
    ## query can't masquerade as "no more records"
    if (!is.null(response$error)) {
      stop(stringr::str_c(
        "NRI feature service returned an error for the ", geography, " query: ",
        response$error$message,
        ". The service schema may have changed; see https://hazards.fema.gov/nri/data-resources."))
    }

    features = response$features
    if (length(features) == 0 || is.null(features$attributes)) {
      return(tibble::tibble())
    }
    tibble::as_tibble(features$attributes)
  }

  pages = list()
  offset = 0
  repeat {
    page = fetch_page(offset)
    if (nrow(page) == 0) break
    pages = c(pages, list(page))
    ## advance by the number of rows actually returned rather than a hardcoded
    ## page_size, so results aren't skipped if the service caps a page below page_size
    offset = offset + nrow(page)
  }

  nri_raw = dplyr::bind_rows(pages)

  minimum_expected_records = if (geography == "tract") 80000 else 3000
  if (nrow(nri_raw) < minimum_expected_records) {
    warning(stringr::str_c(
      "NRI ", geography, " download returned only ", nrow(nri_raw),
      " records; the feature service may have changed or truncated results."))
  }

  geoid_width = if (geography == "tract") 11 else 5

  nri_df = nri_raw |>
    janitor::clean_names() |>
    ## drop ArcGIS service plumbing that isn't part of the NRI dataset
    dplyr::select(-dplyr::any_of(c("objectid", "shape_area", "shape_length"))) |>
    ## prepend a standardized, zero-padded join key consistent with the rest of the package
    dplyr::mutate(
      geoid = stringr::str_pad(
        as.character(.data[[stringr::str_to_lower(geoid_field)]]),
        width = geoid_width, side = "left", pad = "0")) |>
    dplyr::relocate("geoid") |>
    dplyr::rename(
      state_name = state,
      county_name = county,
      value_building = buildvalue,
      value_agriculture = agrivalue,
      area_sq_mi = area) |>
    dplyr::select(-dplyr::any_of(c("nri_id", "stateabbrv", "statefips", "countytype", "countyfips", "stcofips", "tract", "tractfips"))) |>
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ stringr::str_replace_all(.x, c(
        "crf" = "community_risk_factor",
        "ratng" = "rating",
        "wfir" = "wildfire",
        "trnd" = "tornado",
        "vlcn" = "volcano",
        "tsun" = "tsunami",
        "ltng" = "lightning",
        "ifld" = "inland_flood",
        "swnd" = "severe_wind",
        "hrcn" = "hurricane",
        "istm" = "ice_storm",
        "cwav" = "cold_wave",
        "hwav" = "heat_wave",
        "drgt" = "drought",
        "cfld" = "coastal_flood",
        "avln" = "avalanche",
        "erqk" = "earthquake",
        "eal_" = "estimated_annual_loss_",
        "ealt" = "estimated_annual_loss_total",
        "npctl" = "national_percentile",
        "^alr_" = "expected_annual_loss_rate",
        "evnts" = "event_count",
        "afreq" = "annual_frequency",
        "ealpe" = "estimated_annual_loss_population_equivalence",
        "eala" = "estimated_annual_loss_agriculture",
        "ealb" = "estimated_annual_loss_building",
        "ealp" = "estimated_annual_loss_population",
        "eals" = "expected_annual_loss_score",
        "ealr" = "expected_annual_loss_rating",
        "alrb" = "expected_annual_loss_rate_building",
        "alrp" = "expected_annual_loss_rate_population",
        "alra" = "expected_annual_loss_rate_agriculture",
        "_alr_" = "_expected_annual_loss_rate_",
        "resl_" = "resilience_",
        "sovi_" = "social_vulnerability_index_",
        "_valt" = "_value_total",
        "_valb" = "_value_building",
        ## more-specific pattern first: str_replace_all applies these in order, so
        ## "_valpe" must precede "_valp" or "_valp" strips "valp" and leaves a stray "e"
        "_valpe" = "_value_population_equivalence",
        "_valp" = "_value_population",
        "_vala" = "_value_agriculture",
        "_spctl" = "_state_percentile",
        "wntw" = "winter_weather",
        "expb" = "exposure_building",
        "expa" = "exposure_agriculture",
        "exppe" = "exposure_population_equivalence",
        "expp" = "exposure_population",
        "riskv" = "risk_value",
        "riskr" = "risk_rating",
        "risks" = "risk_score",
        "nri_ver" = "nri_version",
        "hlrb" = "historic_loss_ratio_building",
        "hlrp" = "historic_loss_ratio_population",
        "hlra" = "historic_loss_ratio_agriculture",
        "hlrr" = "historic_loss_ratio_total_rating",
        "expt" = "exposure_total",
        "exp_area" = "exposed_area",
        "lnds" = "landslide",
        "ratevalb" = "rate_composite_building",
        "ratevalp" = "rate_composite_population",
        "ratevala" = "rate_composite_agriculture",
        "ratenational_percentile" = "rate_composite_national_percentile",
        "ratevra_national_percentile" = "rate_composite_vulnerability_resilience_national_percentile")))

  if (!is.null(cache_path)) {
    arrow::write_parquet(nri_df, sink = cache_path)
  }

  return(nri_df)
}

utils::globalVariables(c(
  "buildvalue", "agrivalue", "area", "nri_id", "stateabbrv", "statefips", "countytype",
  "countyfips", "stcofips", "tract", "tractfips"))
