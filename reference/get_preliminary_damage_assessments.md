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

  The path to the directory where PDA PDFs are stored. These files are
  not fetched by this function; run
  [`scrape_pda_pdfs()`](https://ui-research.github.io/climateapi/reference/scrape_pda_pdfs.md)
  to download them and to refresh the archive as FEMA publishes new
  reports.

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
  "appeal_denial". The denial classes are read from FEMA's filename
  convention and the report title. "appeal_approved" is read from the
  report body instead, because an approved appeal is titled and named
  exactly like a first-instance approval and carries a disaster number;
  what identifies it is a narrative of a denied request that was
  subsequently appealed. Both halves of that narrative are required, so
  ordinary approvals that merely describe the appeals process are not
  misclassified.

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

  FEMA's statutory statewide per capita *threshold* in dollars for the
  relevant year (observed range 1.24–1.94), not a ratio and not a
  "Met"/"Not Met" categorical despite the field's FEMA-assigned name.
  Compare it against `pa_per_capita_impact_statewide`, which is the
  estimated per capita impact in the same units; the ratio of the two is
  what indicates whether the threshold was met.

- pa_per_capita_impact_countywide:

  Raw text of countywide per capita impact ratios (may list multiple
  values across affected counties for a multi-county event).

- pa_per_capita_impact_indicator_countywide:

  FEMA's statutory countywide per capita threshold in dollars (observed
  range 3.11–4.60), on the same basis as the statewide indicator above.

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

Before the data are returned – whether newly generated or read from the
cache – they are checked for the ways that parsing an unstructured PDF
fails silently: values that are not finite numbers, negative counts,
percentages outside 0-100, cost estimates small enough to be a label's
footnote number rather than a total, demographic shares of exactly zero
(the signature of a blank field whose footnote number was read as the
value), statutory per capita thresholds outside their published range,
damage categories summing to more than the stated total of impacted
residences, values recorded for a program the report says was not
requested, values far above the rest of their column, malformed or
missing disaster numbers, implausible determination dates, and columns
that are almost entirely empty among the reports that should state them.
Anything found is raised as a single
[`warning()`](https://rdrr.io/r/base/warning.html) naming example source
reports; the values themselves are returned as parsed, so each can be
checked against its PDF. The share of each field that is missing among
the reports that requested the program and were approved is reported
with [`message()`](https://rdrr.io/r/base/message.html), since whether a
given rate is a problem is a judgment rather than a rule.

## Examples

``` r
if (FALSE) { # \dontrun{
get_preliminary_damage_assessments()
} # }
```
