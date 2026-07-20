# Get major disaster declarations by county

Retrieves FEMA Major Disaster Declarations at the county level,
aggregated by year and month. Tribal declarations are stored separately
as an attribute.

## Usage

``` r
get_fema_disaster_declarations(file_path = NULL, api = FALSE)
```

## Arguments

- file_path:

  The path to the raw data. If NULL (default), reads the most recently
  cached file for this dataset from
  [`get_openfema_cache_path()`](https://ui-research.github.io/climateapi/reference/get_openfema_cache_path.md).

- api:

  If TRUE, access data from the API. Else (default), read from
  `file_path` (or the local OpenFEMA cache, if `file_path` is NULL).

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

Connecticut's pre-2022 counties (FIPS 09001-09015) are crosswalked onto
the 2022-vintage planning regions (09110-09190) using a binary
any-overlap assumption: a planning region is treated as having received
a declaration if ANY overlapping pre-2022 county received it, even if
only part of that county's area falls within the region. This is not a
proportional/population-weighted allocation (unlike the crosswalks this
package uses for dollar-valued FEMA datasets), since a declaration is a
binary event, not a divisible amount.

## Examples

``` r
if (FALSE) { # \dontrun{
get_fema_disaster_declarations(api = TRUE)
} # }
```
