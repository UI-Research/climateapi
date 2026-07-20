# Access SBA data on disaster loans

Retrieves Small Business Administration (SBA) disaster loan data for
both home and business loans at the city and zip code level.

## Usage

``` r
get_sba_loans()
```

## Value

A dataframe comprising city- and zip-level data on SBA loanmaking.
Columns include:

- fiscal_year:

  The federal fiscal year of the loan.

- disaster_description:

  Text description of the disaster (only populated for older, FY01-FY03
  vintages; NA otherwise).

- disaster_number_fema:

  FEMA disaster number associated with the loan.

- disaster_number_sba_physical:

  SBA physical disaster declaration number.

- disaster_number_sba:

  SBA disaster declaration number (distinct from
  `disaster_number_sba_physical`).

- disaster_number_sba_eidl:

  SBA Economic Injury Disaster Loan (EIDL) declaration number.

- damaged_property_zip_code:

  ZIP code of the damaged property.

- damaged_property_city_name:

  City name of the damaged property.

- damaged_property_state_code:

  Two-letter state abbreviation.

- verified_loss_total:

  Total verified loss amount in dollars.

- verified_loss_real_estate:

  Verified loss amount for real estate in dollars.

- verified_loss_content:

  Verified loss amount for contents in dollars.

- approved_amount_total:

  Total approved loan amount in dollars.

- approved_amount_real_estate:

  Approved loan amount for real estate in dollars.

- approved_amount_content:

  Approved loan amount for contents in dollars.

- approved_amount_eidl:

  Approved EIDL amount in dollars (business loans only; NA for
  residential).

- loan_type:

  Type of loan: "residential" or "business".

## Details

Data are sourced from the SBA's disaster loan reports. See
<https://www.sba.gov/funding-programs/disaster-assistance>.

The FY16 source workbook (`sba_disaster_loan_data_fy16.xlsx`) is
anomalous: its Home and Business sheets have identical row counts and
dollar totals, suggesting possible ~2x double-counting in the source
file itself. This function selects the correct Home/Business sheet by
name (avoiding the sheet-order swap present in this vintage), but the
underlying FY16 data should be manually re-verified against
<https://data.sba.gov> before being trusted.

## Examples

``` r
if (FALSE) { # \dontrun{
get_sba_loans()
} # }
```
