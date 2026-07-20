# Download full OpenFEMA datasets

Downloads full data files for OpenFEMA API endpoints. Prefers parquet
format, falling back to CSV. Uses the OpenFEMA metadata API to
dynamically resolve download URLs.

## Usage

``` r
download_openfema_datasets(
  endpoints = NULL,
  download_directory = ".",
  format_preference = c("parquet", "csv"),
  overwrite = FALSE
)
```

## Arguments

- endpoints:

  A character vector of dataset endpoint names (e.g.,
  `"DisasterDeclarationsSummaries"`). Use
  [`list_openfema_endpoints()`](https://ui-research.github.io/climateapi/reference/list_openfema_endpoints.md)
  to see available names. Default `NULL` downloads all endpoints.

- download_directory:

  Directory path where files will be saved. Created if it does not
  exist. Defaults to `"."`.

- format_preference:

  Character vector specifying format preference order. The first
  available format is used. Defaults to `c("parquet", "csv")`.

- overwrite:

  Logical. If `FALSE` (default), skips files that already exist in
  `download_directory`.

## Value

A tibble summarizing the results with columns: `name`, `format`,
`file_path`, `status` (one of "downloaded", "skipped", "failed",
"no_format").

## Examples

``` r
if (FALSE) { # \dontrun{
# Download a single small dataset
download_openfema_datasets(
  endpoints = "DisasterDeclarationsSummaries",
  download_directory = "data/openfema")

# Download all datasets
download_openfema_datasets(download_directory = "data/openfema")

# See what's available first
list_openfema_endpoints()
} # }
```
