# Get the path to the local OpenFEMA dataset cache

Get the path to the local OpenFEMA dataset cache

## Usage

``` r
get_openfema_cache_path()
```

## Value

A character string containing the full path to the local cache of
OpenFEMA datasets, populated via
[`download_openfema_datasets()`](https://ui-research.github.io/climateapi/reference/download_openfema_datasets.md).
This cache lives at the root of the user's Box folder
(`data-cache/openfema`), not under the C&C practice area subfolder
returned by
[`get_box_path()`](https://ui-research.github.io/climateapi/reference/get_box_path.md).

## Examples

``` r
if (FALSE) { # \dontrun{
get_openfema_cache_path()
} # }
```
