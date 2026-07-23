# Register a HUD API key

Stores a HUD API key so that
[`get_chas_housing_affordability()`](https://ui-research.github.io/climateapi/reference/get_chas_housing_affordability.md)
can authenticate against HUD's CHAS API. By default the key is set for
the current R session only; set `install = TRUE` to also persist it to
your user-level `.Renviron` so that it is available in future sessions.

## Usage

``` r
register_hud_api_key(key, install = FALSE)
```

## Arguments

- key:

  A HUD API key: a single, non-empty character string. Obtain one by
  registering at <https://www.huduser.gov/portal/dataset/fmr-api.html>.

- install:

  Logical. If `TRUE`, write the key to your user-level `.Renviron`
  (replacing any existing `HUD_API_KEY` entry) so it persists across
  sessions. Defaults to `FALSE`, which sets the key for the current
  session only.

## Value

Invisibly returns `key`. Called for the side effect of setting the
`HUD_API_KEY` environment variable (and, when `install = TRUE`, writing
it to `.Renviron`).

## Examples

``` r
if (FALSE) { # \dontrun{
register_hud_api_key("your-hud-api-key")
register_hud_api_key("your-hud-api-key", install = TRUE)
} # }
```
