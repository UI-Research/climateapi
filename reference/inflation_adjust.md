# Inflation adjust dollar values using annual PCE Index

The Personal Consumption Expenditures Price Index (PCE Index) is from
the Federal Reserve Bank of St. Louis's FRED tool.

## Usage

``` r
inflation_adjust(
  df,
  year_variable,
  dollar_variables,
  names_suffix = NULL,
  base_year = 2024
)
```

## Arguments

- df:

  The dataframe with values to inflation-adjust

- year_variable:

  The name of the column denoting the year that existing
  dollar-denominated figures are based in.

- dollar_variables:

  The variables to inflation-adjust.

- names_suffix:

  A suffix to add to the names of the inflation-adjusted variables. If
  NULL, defaults to "\_\<base_year\>". If "", columns are renamed in
  place.

- base_year:

  The year to use as the base for inflation adjustment. If NULL,
  defaults to the most recent year in the PCE index data.

## Value

A dataframe with inflation-adjusted values

## Examples

``` r
if (FALSE) { # \dontrun{
df = tibble::tribble(
  ~ year, ~ amount,
  1990, 1,
  1991, 1,
  1992, 1)

df |>
  inflation_adjust(
    year_variable = year,
    dollar_variables = amount,
    names_suffix = "inflation_adjusted")
} # }
```
