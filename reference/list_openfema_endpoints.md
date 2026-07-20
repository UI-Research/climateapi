# List available OpenFEMA dataset endpoints

Queries the OpenFEMA metadata API and returns a tibble of all available
datasets with their names, versions, record counts, and download URLs by
format.

## Usage

``` r
list_openfema_endpoints()
```

## Value

A tibble with columns:

- name:

  The API endpoint name (e.g., "DisasterDeclarationsSummaries").

- title:

  Human-readable dataset title.

- version:

  API version number.

- record_count:

  Number of records in the dataset.

- formats:

  Comma-separated list of available download formats.

- url_parquet:

  Download URL for parquet format (NA if unavailable).

- url_csv:

  Download URL for CSV format (NA if unavailable).

## Examples

``` r
if (FALSE) { # \dontrun{
endpoints <- list_openfema_endpoints()
endpoints |> dplyr::filter(stringr::str_detect(name, "Nfip"))
} # }
```
