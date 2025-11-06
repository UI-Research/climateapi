# Access temporal county-level SHELDUS hazard damage data.

Access temporal county-level SHELDUS hazard damage data.

## Usage

``` r
get_sheldus(
  file_path = file.path(get_box_path(), "hazards", "sheldus",
    "SHELDUS_23.0_12312023_AllCounties_CountyAggregate_YearMonthHazard_2023USD",
    "direct_loss_aggregated_output_24075.csv")
)
```

## Arguments

- file_path:

  The path to the raw SHELDUS data.

## Value

A dataframe comprising hazard x month x year x county observations of
hazard events.

## Examples

``` r
if (FALSE) { # \dontrun{
get_sheldus()
} # }
```
