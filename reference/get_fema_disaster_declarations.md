# Get major disaster declarations by county

Retrieves FEMA Major Disaster Declarations at the county level,
aggregated by year and month. Tribal declarations are stored separately
as an attribute.

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
county. Tribal declarations are stored as an attribute
(`tribal_declarations`). Columns include:

- unique_id:

  Unique identifier for each observation.

- GEOID:

  Five-digit county FIPS code.

- year_declared:

  Year the disaster was declared.

- month_declared:

  Month the disaster was declared (1-12).

- declaration_title:

  Title(s) of the disaster declaration(s).

- incidents_all:

  Total count of disaster declarations in the county-month.

- incidents_natural_hazard:

  Count of natural hazard declarations.

- incidents\_\*:

  Additional columns for other incident types, each of which reflects
  the count of the given incident type.

## Details

Data are from FEMA's OpenFEMA API. See
<https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2>.
Statewide declarations are expanded to all counties in the state.

## Examples

``` r
if (FALSE) { # \dontrun{
get_fema_disaster_declarations(api = TRUE)
} # }
```
