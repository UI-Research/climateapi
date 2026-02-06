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

A dataframe of preliminary damage assessment reports. Key columns
include:

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

- ia_cost_estimate_total:

  Estimated total Individual Assistance cost.

- pa_requested:

  1 if Public Assistance was requested, 0 otherwise.

- pa_cost_estimate_total:

  Estimated total Public Assistance cost.

- pa_per_capita_impact_statewide:

  Statewide per capita impact amount.

- pa_per_capita_impact_indicator_statewide:

  Met/Not Met indicator for statewide threshold.

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
