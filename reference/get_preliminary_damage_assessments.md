# Get Data from Preliminary Damage Assessments Submitted to FEMA for Disaster Declarations

Retrieves data extracted from PDF preliminary damage assessment (PDA)
reports submitted to FEMA for disaster declarations.

## Usage

``` r
get_preliminary_damage_assessments(
  file_path = file.path(get_box_path(), "hazards", "urban",
    "preliminary-damage-assessments", "pda_data.csv"),
  directory_path = file.path(get_box_path(), "hazards", "urban",
    "preliminary-damage-assessments"),
  use_cache = TRUE
)
```

## Arguments

- file_path:

  The file path to the cached dataset, or if there is no cache, the path
  at which to cache the resulting data.

- directory_path:

  The path to the directory where PDA PDFs are stored. Use
  `scrape_pda_pdfs` to generate these files.

- use_cache:

  Boolean. Read the existing dataset stored at `file_path`? If FALSE,
  data will be generated anew. Else, if a file exists at `file_path`,
  this file will be returned.

## Value

A dataframe of preliminary damage assessment reports. Columns include:

- path:

  The local file path to the source PDA PDF.

- disaster_number:

  FEMA disaster number.

- event_type:

  Type of decision: "approved", "denial", "appeal_approved", or
  "appeal_denial".

- event_title:

  Title/description of the disaster event.

- event_date_determined:

  Date the PDA determination was made.

- event_native_flag:

  1 if tribal request, 0 otherwise.

- pa_requested:

  1 if Public Assistance was requested, 0 otherwise.

- pa_preemptive_declaration:

  1 if the joint PDA requirement was waived due to the severity of the
  event, 0 otherwise.

- pa_primary_impact:

  The primary type of impact described for Public Assistance purposes.

- pa_cost_estimate_total:

  Estimated total Public Assistance cost.

- pa_per_capita_impact_statewide:

  Statewide (or territory/commonwealth) per capita impact amount.

- pa_per_capita_impact_indicator_statewide:

  Numeric ratio of the statewide per capita impact to the applicable
  threshold – a decimal ratio (e.g. 1.5, 1.89), not a "Met"/"Not Met"
  categorical indicator despite the field's FEMA-assigned name.

- pa_per_capita_impact_countywide:

  Raw text of countywide per capita impact ratios (may list multiple
  values across affected counties for a multi-county event).

- pa_per_capita_impact_indicator_countywide:

  Truncated text of the countywide per capita impact indicator.

- pa_per_capita_impact_countywide_max:

  Maximum countywide per capita impact ratio parsed from
  `pa_per_capita_impact_countywide`.

- pa_per_capita_impact_countywide_min:

  Minimum countywide per capita impact ratio parsed from
  `pa_per_capita_impact_countywide`.

- ia_requested:

  1 if Individual Assistance was requested, 0 otherwise.

- ia_residences_impacted:

  Total residences impacted.

- ia_residences_destroyed:

  Number of residences destroyed.

- ia_residences_major_damage:

  Number of residences with major damage.

- ia_residences_minor_damage:

  Number of residences with minor damage.

- ia_residences_affected:

  Number of residences affected (lowest damage category).

- ia_residences_insured_total_percent:

  Percentage of impacted residences with any insurance coverage.

- ia_residences_insured_flood_percent:

  Percentage of impacted residences with flood insurance coverage.

- ia_households_poverty_percent:

  Percentage of households in poverty (or low income, depending on
  report vintage).

- ia_households_owner_percent:

  Percentage of households that are owner-occupied.

- ia_population_other_government_assistance_percent:

  Percentage of the population receiving other government assistance
  (e.g. SSI, SNAP).

- ia_pre_disaster_unemployment_percent:

  Pre-disaster unemployment rate.

- ia_65plus_percent:

  Percentage of the population age 65 and older.

- ia_18below_percent:

  Percentage of the population age 18 and under.

- ia_disability_percent:

  Percentage of the population with a disability.

- ia_ihp_cost_to_capacity_ratio:

  Individuals and Households Program (IHP) Cost to Capacity (ICC) ratio.

- ia_cost_estimate_total:

  Estimated total Individual Assistance cost.

- text:

  The cleaned text extracted from the PDA PDF used to derive the fields
  above.

## Details

Data are extracted from PDF reports hosted at
<https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports>.
Owing to the unstructured nature of the source documents, some fields
may be incorrect in the data returned by the function, though
significant quality checks have been implemented in an effort to produce
a high-quality dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
get_preliminary_damage_assessments()
} # }
```
