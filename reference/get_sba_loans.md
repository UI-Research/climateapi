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

- disaster_number_fema:

  FEMA disaster number associated with the loan.

- disaster_number_sba_physical:

  SBA physical disaster declaration number.

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

- approved_amount_total:

  Total approved loan amount in dollars.

- approved_amount_real_estate:

  Approved loan amount for real estate in dollars.

- loan_type:

  Type of loan: "residential" or "business".

## Details

Data are sourced from the SBA's disaster loan reports. See
<https://www.sba.gov/funding-programs/disaster-assistance>.

## Examples

``` r
if (FALSE) { # \dontrun{
get_sba_loans()
} # }
```
