# Obtain County Business Patterns (CBP) Estimates per County

Obtain County Business Patterns (CBP) Estimates per County

## Usage

``` r
get_business_patterns(
  year = 2023,
  geo = "county",
  naics_code_digits = 2,
  naics_codes = NULL
)
```

## Arguments

- year:

  The vintage of CBP data desired. Data are available from 2008-2023.
  Earlier years use different NAICS classification systems that are not
  currently supported. Default is 2023.

- geo:

  The level of geography of CBP data desired. Either "county" or
  "zipcode". Zipcode level data only The ZIP Code Business Patterns
  (ZBP) dataset includes the number of establishments, employment during
  the week of March 12th, first quarter and annual payroll for NAICS 00
  (total for all sectors). Additionally, the number of establishments
  (but not employment or payroll) are available by employment size of
  the establishment for 2- through 6-digit NAICS.

- naics_code_digits:

  One of c(2, 3). Default is 2. NAICS codes range in specificity;
  2-digit codes describe the highest groupings of industries, while
  six-digit codes are exceedingly detailed. There are 20 2-digit NAICS
  codes and 196 3-digit codes. If more specific codes are desired, leave
  this argument as NULL and supply the desired codes as the argument to
  `naics_codes`.

- naics_codes:

  A vector of NAICS codes to query. If NULL, the function will query all
  available codes with the specified number of digits. If not NULL, this
  argument overrides the `naics_code_digits` argument.

## Value

A tibble with data on county-level employees, employers, and aggregate
annual payrolls by industry and employer size

- year:

  the year for which CBP data is pulled from

- state:

  A two-digit state identifier.

- county:

  A three-digit county identifier.

- employees:

  number of individual employees employed in that particular industry
  and establishment size combination

- employers:

  number of establishments of each employment size

- annual_payroll:

  total annual payroll expenditures measured in \$1,000's of USD

- industry:

  industry classification according to North American Industry
  Classification System. Refer to details for additional information

- employee_size_range_label:

  range for the employment size of establishments included in each given
  grouping

- employee_size_range_code:

  three-digit code used to categorize employment sizes

- naics_code:

  two to six-digit code used by the NAICS to categorize and
  sub-categorize industries

## Details

County Business Patterns (CBP) is an annual series that provides
subnational economic data for establishments with paid employees by
industry and employment size. This series includes the number of
establishments, employment during the week of March 12, first quarter
payroll, and annual payroll. Industry classification of business
establishments in CBP is according to the North American Industry
Classification System (NAICS) https://www.census.gov/naics/

CBP data are useful for studying economic activity of small areas.
Federal agencies use the data to determine employee concentrations and
trends by industry. State and local government offices use the data to
assess business changes, develop fiscal policies, and plan future
policies and programs. CBP data are used to benchmark public and private
sector statistical series, surveys, and databases between economic
census years.

While similar to LEHD Origin-Destination Employment Statistics (LODES)
data in it's coverage of employment statistics, CBP differs mainly due
to its broader geographies (county vs. tract) and focus on framing the
statistics at an establishment/company level rather than at the
individual/job level found in LODES data. CBP also does not offer
information on locations of the jobs in relation to where the employee
actually resides.

The series excludes data on self-employed individuals, employees of
private households, railroad employees, agricultural production
employees, and most government employees. A certain amount of
undercoverage occurs in the universe, as the Census Bureau does not
create a multi-unit company structure in the Business Register for very
small employers (less than 10 employees) identified in the Economic
Census.

CBP covers most NAICS industries excluding Crop and Animal Production
(NAICS 111,112); Rail Transportation (NAICS 482); Postal Service (NAICS
491); Pension, Health, Welfare, and Other Insurance Funds (NAICS 525110,
525120, 525190); Trusts, Estates, and Agency Accounts (NAICS 525920);
Offices of Notaries (NAICS 541120); Private Households (NAICS 814); and
Public Administration (NAICS 92)

## Examples

``` r
if (FALSE) { # \dontrun{
get_business_patterns(
 year = 2023,
 naics_code_digits = 3)

get_business_patterns(
 year = 2017,
 naics_codes = c(221111, 221112))
} # }
```
