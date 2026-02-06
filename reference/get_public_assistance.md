# Get FEMA Public Assistance (PA) funding

Retrieves FEMA Public Assistance (PA) project funding data, crosswalked
to the county level for geographic analysis.

## Usage

``` r
get_public_assistance(
  file_path = file.path(get_box_path(), "hazards", "fema", "public-assistance", "raw",
    "PublicAssistanceFundedProjectsDetailsV2_2025_09_26.parquet"),
  state_abbreviations = NULL
)
```

## Arguments

- file_path:

  The file path to the raw data contained in a .parquet file.

- state_abbreviations:

  A character vector of state abbreviations. NULL by default, which
  returns records for all 51 states. Only the 51 states are supported at
  this time.

## Value

A dataframe of project-level funding requests and awards, along with
variables that can be aggregated to the county level.

- id:

  A unique identifier for each project in the raw data. This field is
  not unique in the returned data due to crosswalking statewide projects
  to the county level. Refer to the details for additional information

- state_fips:

  A two-digit state identifier.

- state_name:

  The name of the state.

- state_abbreviation:

  The two-character USPS abbreviation for the state.

- county_fips:

  A five-digit county identifier.

- county_name:

  The name of the county.

- declaration_year:

  The year when the authorizing disaster declaration was made.

- disaster_number:

  The FEMA-created disaster number. This is unique at the disaster-state
  level; for example, Hurricane Helene has multiple disaster numbers
  associated with it, one per state that received an associated disaster
  declaration.

- incident_type:

  The type of disaster, e.g., "Hurricane".

- project_status:

  The current status of the funded PA project, e.g., "Active".

- damage_category_code:

  A letter code identifying the category of damages/what funds may be
  used for.

- damage_category_description:

  A descriptive characteristization of the damage category.

- pa_federal_funding_obligated:

  Obligated federal funding at the project `id` level.

- pa_federal_funding_obligated_split:

  Obligated federal attributed to the `id`-by-county level. Refer to the
  details for additional information.

## Details

Data are from FEMA's OpenFEMA API. See
<https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v2>.

These data have been crosswalked so that estimates can be aggregated at
the county level. This is necessary (for county-level estimates) because
many projects are statewide projects and do not have county-level
observations in the data.

Analysts thus have two options for working with these data: (1)
De-select the variables suffixed with `_split` and then run
`distinct(df)`. This will provide unique observations for projects;
projects can be either county-level or statewide. These data can be
aggregated to the state level but cannot be comprehensively aggregated
to the county level. (2) Group the data at the county level and
summarize to produce county-level characterizations of PA projects and
funding, using the `_split`-suffixed variables to calculate funding
totals.

The attribution of statewide projects to the county level occurs by
proportionally attributing project costs based on county-level
populations. For example, in a fictional state with two counties, one of
population 10 and one of population 90, 10% of a statewide project's
funding would be attributed to the first county and 90% of the project's
funding to second county. Roughly 62 percent of the total PA funding
returned by this function is for county-specific projects, and the
remaining 38 percent is for statewide projects (as of 2025).

## Examples

``` r
if (FALSE) { # \dontrun{
  get_public_assistance(state_abbreviations = "NJ")
} # }
```
