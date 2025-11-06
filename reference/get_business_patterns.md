# Obtain County Business Patterns (CBP) Estimates per County

Obtain County Business Patterns (CBP) Estimates per County

## Usage

``` r
get_business_patterns(year = 2022, naics_code_digits = 2, naics_codes = NULL)
```

## Arguments

- year:

  The vintage of CBP data desired. Data are available from 1986, though
  this function likely only supports more recent years (it it tested on
  2022-vintage data only). Default is 2022.

- naics_code_digits:

  One of c(2, 3). Default is 2. NAICS codes range in specificity;
  2-digit codes describe the highest groupings of industries, while
  six-digit codes are exceedingly detailed. There are 20 2-digit NAICS
  codes and 196 3-digit codes.

- naics_codes:

  A vector of NAICS codes to query. If NULL, the function will query all
  available codes with the specified number of digits. If not NULL, this
  argument overrides the `naics_code_digits` argument.

## Value

A tibble with data on county-level employees, employers, and aggregate
annual payrolls by industry and employer size

## Examples

``` r
if (FALSE) { # \dontrun{
get_business_patterns(
 year = 2022,
 naics_code_digits = 3)

get_business_patterns(
 year = 2017,
 naics_codes = c(221111, 221112))
} # }
```
