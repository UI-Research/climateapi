# Acquire daily stream-gage readings from USGS gages

Pulls daily stream-gage readings, over each gage's full period of record
by default, for USGS gages in one or more counties via the dataRetrieval
package (USGS Water Data OGC APIs; these replace the now-decommissioning
NWIS web services). Two statistics are supported:

- "daily_mean": the published daily-mean series. Available for a
  century-plus at many gages.

- "daily_max": the maximum reading each day, computed here from the
  continuous (15-minute) record, because USGS publishes no daily-maximum
  gage-height series. Continuous records only begin in the mid-1990s at
  the earliest. These pulls are slow (a minute or more per long-record
  gage), so each gage's aggregated result is cached to its own parquet
  file and the pull resumes wherever it left off.

## Usage

``` r
get_usgs_gage(
  counties,
  measure = c("height", "discharge"),
  statistic = c("daily_max", "daily_mean"),
  start_date = "",
  end_date = "",
  refresh_cache = FALSE,
  cache_dir = file.path(tempdir(), "usgs-gage-history")
)
```

## Arguments

- counties:

  Character vector of five-digit county FIPS codes (e.g., "54097" for
  Upshur County, WV). Every stream gage in these counties with data for
  the requested measure and statistic is pulled.

- measure:

  One of "height" (gage height in feet, the default; USGS parameter
  code 00065) or "discharge" (streamflow in cubic feet per second; USGS
  parameter code 00060).

- statistic:

  One of "daily_max" (the default; computed from continuous readings) or
  "daily_mean" (the published daily-value series). See the description
  for the record-length and runtime trade-offs.

- start_date, end_date:

  Character "YYYY-MM-DD" bounds on the readings. The defaults ("" for
  both) request each gage's full period of record; either bound may be
  supplied alone.

- refresh_cache:

  When TRUE, ignore cached parquet files (including the per-site
  continuous-record caches) and pull fresh data. Defaults to FALSE. Note
  that without a refresh, previously cached gages are frozen at the time
  they were pulled.

- cache_dir:

  Directory for cached parquet files. Defaults to a session-specific
  temporary directory; supply a persistent directory (e.g.,
  `tools::R_user_dir("climateapi", which = "cache")`) to keep the cache
  across sessions, which is strongly recommended for
  `statistic = "daily_max"` pulls so they can resume.

## Value

A tibble with one row per gage-day. Columns include:

- site_number:

  USGS site number.

- gage_name:

  USGS station name.

- county_geoid:

  Five-digit county FIPS code.

- county_name:

  County name.

- state_abbreviation:

  Two-letter state abbreviation.

- latitude, longitude:

  Gage coordinates (decimal degrees).

- drainage_area_sqmi:

  Upstream drainage area, in square miles.

- date:

  Calendar day. For `statistic = "daily_max"`, the day is defined in the
  gage's local (Eastern) time zone.

- value:

  The reading, in feet ("height") or cubic feet per second
  ("discharge").

- approval_status:

  "approved" or "provisional". For `statistic = "daily_max"`, a day is
  "provisional" if any reading that day is provisional.

## Details

Site metadata (name, county, coordinates, drainage area) is attached to
every reading. Data are from the USGS Water Data APIs; see
<https://api.waterdata.usgs.gov/>. USGS asks that heavy users register a
free API key (see
[`dataRetrieval::setAccess()`](https://rdrr.io/pkg/dataRetrieval/man/setAccess.html)
documentation); unkeyed access is rate-limited but sufficient for modest
pulls.

## Examples

``` r
if (FALSE) { # \dontrun{
## daily maximum gage heights in Upshur and Lewis Counties, WV
get_usgs_gage(
  counties = c("54097", "54041"),
  cache_dir = tools::R_user_dir("climateapi", which = "cache"))

get_usgs_gage(
  counties = "54063",
  measure = "discharge",
  statistic = "daily_mean")
} # }
```
