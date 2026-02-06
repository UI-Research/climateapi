# Get NAICS Codes for County Business Patterns

A utility function to programmatically identify and select NAICS codes
for use with
[`get_business_patterns()`](https://ui-research.github.io/climateapi/reference/get_business_patterns.md).
This is a wrapper around `censusapi::listCensusMetadata(name = "cbp")`.

## Usage

``` r
get_naics_codes(year = 2023, digits = 3)
```

## Arguments

- year:

  The vintage year for NAICS codes. Data are available from 1986
  through 2023. Default is 2023.

- digits:

  The number of digits for desired NAICS codes. Must be between 2 and 6.
  Default is 3. Two-digit codes represent broad industry sectors (20
  codes), while six-digit codes represent detailed industries.

## Value

A tibble with the following columns:

- naics_code:

  The NAICS code (character)

- naics_label:

  The descriptive label for the NAICS code

- year:

  The vintage year of the NAICS codes

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all 2-digit NAICS codes
get_naics_codes(year = 2023, digits = 2)

# Get all 3-digit NAICS codes (default)
get_naics_codes(year = 2022)

# Get 4-digit NAICS codes for a specific year
get_naics_codes(year = 2020, digits = 4)
} # }
```
