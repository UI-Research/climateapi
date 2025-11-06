# Get major disaster declarations by county

Get major disaster declarations by county

## Usage

``` r
get_fema_disaster_declarations(
  file_path = file.path(get_box_path(), "hazards", "fema", "disaster-declarations",
    "raw", "fema_disaster_declarations_county_2024_10_25.csv"),
  api = TRUE
)
```

## Arguments

- file_path:

  The path (on Box) to the file containing the raw data.

- api:

  If TRUE (default), access data from the API. Else, read locally from
  `file_path`.

## Value

A dataframe comprising Major Disaster Declarations by month by year by
county. Tribal declarations are stored as an attribute of the primary
dataframe called `tribal_declarations`.

## Examples

``` r
if (FALSE) { # \dontrun{
get_fema_disaster_declarations(api = TRUE)
} # }
```
