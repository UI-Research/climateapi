# Get Data from Preliminary Damage Assessments Submitted to FEMA for Disaster Declarations

These data reflect extracted attributes from PDF preliminary damage
assessments hosted on FEMA's website at:
https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports.
Owing to the unstructured nature of the source documents, some fields
may be incorrect in the data returned by the function, though
significant quality checks have been implemented in an effort to produce
a high-quality dataset.

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

A dataframe of preliminary damage assessment reports.

## Examples

``` r
if (FALSE) { # \dontrun{
get_preliminary_damage_assessments()
} # }
```
