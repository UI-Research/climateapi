# Read IPUMS data leveraging a local cache

This script wraps a standard ipumsr::read_ipums\*() query workflow,
addressing two common challenges: (1) the default workflow downloads
arbitrarily named raw data files that are sequentially numbered and
dependent on the total number of extracts submitted by a given user; and
(2) the default workflow does not provide an inbuilt capacity to check
for a local version of the query before re-submitting to the API.

This script addresses these challenges by taking a user-supplied
filename and file directory, checking if there is an existing file at
that path, and otherwise downloading the extract (again user-specified)
to the given filepath.

## Usage

``` r
read_ipums_cached(
  filename,
  download_directory,
  extract_definition,
  refresh = FALSE
)
```

## Arguments

- filename:

  The name of the file (not the full file path).

- download_directory:

  A path specifying where to download the data.

- extract_definition:

  A `define_extract_micro()` or `define_extract_agg()` object.

- refresh:

  If true, execute the API query, even if data are already stored
  locally. Defaults to FALSE.

## Value

A dataframe corresponding to the supplied `extract_definition`

## Examples

``` r
if (FALSE) { # \dontrun{
read_ipums_cached(
  filename = "acs_insurance_race_2022_1yr_repweights",
  download_directory = file.path("data"),
  extract_definition = ipumsr::define_extract_micro(
    collection = "usa",
    description = "2022 ACS 1-year sample with replicate weights - insurance and race",
    samples = c("us2022a"),
    variables = list(
      "HCOVANY",
      ipumsr::var_spec("RACE", case_selections = c("1", "2")))),
  refresh = FALSE)
} # }
```
