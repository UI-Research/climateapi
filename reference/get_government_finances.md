# Get county government revenues and expenditures

Retrieves county government revenues and expenditures over time.

## Usage

``` r
get_government_finances()
```

## Value

A dataframe containing county government-level revenues and expenditures
by year. All monetary values are in thousands of dollars.

- year:

  Year of the financial data.

- GEOID:

  Five-digit county FIPS code.

- county_name:

  Name of the county.

- revenue_total:

  Total county revenue.

- revenue_general:

  General revenue (excludes utilities, liquor stores, social insurance).

- revenue_tax_total:

  Total tax revenue.

- revenue_tax_property:

  Property tax revenue.

- revenue_tax_sales:

  Sales and gross receipts tax revenue.

- revenue_tax_income:

  Income tax revenue.

- revenue_intergovernmental_total:

  Total intergovernmental transfers received.

- revenue_intergovernmental_federal:

  Federal intergovernmental transfers.

- revenue_intergovernmental_state:

  State intergovernmental transfers.

- revenue_charges_total:

  Total charges and miscellaneous general revenue.

- revenue_utility_total:

  Utility revenue.

- expenditure_total:

  Total county expenditures.

- expenditure_general:

  General expenditures.

- expenditure_capital_outlay:

  Capital outlay spending.

- expenditure_construction:

  Construction spending.

- expenditure_salaries_wages:

  Salaries and wages.

- expenditure_intergovernmental:

  Intergovernmental transfers paid.

- expenditure_education_total:

  Education spending.

- expenditure_public_safety:

  Police protection spending.

- expenditure_health_hospitals:

  Health spending.

- expenditure_highways:

  Highway spending.

- expenditure_public_welfare:

  Public welfare spending.

- expenditure_utility_total:

  Utility expenditures.

## Details

Data are from the U.S. Census Bureau's Annual Survey of State and Local
Government Finances and Census of Governments, compiled per: Pierson K.,
Hand M., and Thompson F. (2015). The Government Finance Database: A
Common Resource for Quantitative Research in Public Financial Analysis.
PLoS ONE doi: 10.1371/journal.pone.0130119. See
<https://my.willamette.edu/site/mba/public-datasets>.

This is a survey in most years–i.e., only a subset of the full
population of governments comprises the sampling frame–but is a true
census with relatively high response rates (90%%+) in years ending in 2
and 7.

Definitions of key constructs:

**Revenues**

- General revenue: All revenue not arising from utilities, liquor
  stores, or social insurance.

- Intergovernmental revenue: Transfers from federal, state, or other
  local governments.

- Current charges: Fees collected for providing services.

**Expenditures**

- General expenditure: All spending except utilities, liquor stores, or
  social insurance.

- Capital outlay: Spending on construction and equipment.
