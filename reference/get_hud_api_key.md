# Retrieve the stored HUD API key

Returns the HUD API key stored in the `HUD_API_KEY` environment
variable, erroring with setup guidance if none is found. Used internally
by
[`get_chas_housing_affordability()`](https://ui-research.github.io/climateapi/reference/get_chas_housing_affordability.md)'s
API path.

## Usage

``` r
get_hud_api_key()
```

## Value

The HUD API key, as a character string.

## Examples

``` r
if (FALSE) { # \dontrun{
get_hud_api_key()
} # }
```
