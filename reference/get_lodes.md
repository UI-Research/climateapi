# Get LEHD Origin-Destination Employment Statistics (LODES) data Returned data are from LODES Version 8, which is enumerated in 2020-vintage geometries.

Get LEHD Origin-Destination Employment Statistics (LODES) data Returned
data are from LODES Version 8, which is enumerated in 2020-vintage
geometries.

## Usage

``` r
get_lodes(
  lodes_type,
  jobs_type = "all",
  states,
  years,
  geography = "tract",
  state_part = "main"
)
```

## Arguments

- lodes_type:

  One of c("rac", "wac", "od"). "rac" = Residence Area Characteristics,
  where jobs are associated with employees' residences. "wac" =
  Workplace Area Characteristics, where jobs are associated with
  employees' workplaces. "od" = Origin-Destination data, where jobs are
  associated with both workers' residences and their workplaces.

- jobs_type:

  One of c("all", "primary"). Default is "all", which includes multiple
  jobs for workers with multiple jobs. "primary" includes only the
  highest-paying job per worker.

- states:

  A vector of state abbreviations.

- years:

  A vector of years.

- geography:

  One of c("block", "block group", "tract", "county", "state"). Default
  is "tract".

- state_part:

  One of c("main", "aux"). Default is "main", which includes only
  workers who reside inside the state where they work. "aux" returns
  only workers who work in the specified state but live outside of that
  state.

## Value

A tibble with one record per geography per year per job type. Attributes
include total jobs and jobs by worker earnings, industry, and
demographics; the origin-destination results have more limited
demographics compared to the "wac" and "rac" results.
