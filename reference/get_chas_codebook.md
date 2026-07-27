# Obtain a codebook for HUD CHAS variables

Reads the CHAS data dictionary that ships alongside the data and returns
it as a tibble: one row per variable, giving the descriptive column name
that
[`get_chas_housing_affordability()`](https://ui-research.github.io/climateapi/reference/get_chas_housing_affordability.md)
assigns, the source's original variable code, and a plain-English
definition. Use it to find the variables you want before pulling data,
and to interpret the columns you get back.

## Usage

``` r
get_chas_codebook(
  end_year = 2021,
  directory_path = file.path(climateapi::get_box_path(), "built-environment", "hud",
    "comprehensive-housing-affordability-strategies")
)
```

## Arguments

- end_year:

  The last year of the five-year ACS period, from 2009 to 2021. Defaults
  to 2021 (the most recent period with a published data dictionary). If
  no dictionary for this period is on disk, the most recent one that is
  available is used instead and a warning reports the substitution.

- directory_path:

  The directory containing the CHAS data dictionaries. Defaults to the
  CHAS folder under the C&C Box path.

## Value

A tibble with one row per CHAS estimate variable and the following
columns:

- `column_name`:

  The descriptive snake_case name assigned by
  [`get_chas_housing_affordability()`](https://ui-research.github.io/climateapi/reference/get_chas_housing_affordability.md).

- `column_name_source`:

  The source's original variable code, e.g. `"T8_est4"`.

- `chas_table`:

  The CHAS table the variable belongs to, e.g. `"T8"`.

- `column_type`:

  Whether the variable is a `"Total"`, a `"Subtotal"`, or a `"Detail"`
  line. Totals and subtotals are sums of the lines nested beneath them,
  so adding across types double counts households.

- `definition`:

  The variable's full definition as a single sentence.

- `vintage`:

  The last year of the five-year period whose dictionary these
  definitions were read from. This equals `end_year` unless no
  dictionary for `end_year` was found.

If no usable data dictionary is found, a message is emitted and a
zero-row tibble with these columns is returned.

## Details

CHAS variables are organized into numbered tables, each of which
cross-tabulates households by a different combination of characteristics
(tenure, income relative to HUD Area Median Family Income, cost burden,
race and ethnicity, household type, and so on). Each variable's
definition is published as a hierarchy of up to five nested clauses,
which are collapsed here into a single sentence (`definition`) and a
single snake_case name (`column_name`).

Because those clauses repeat the same long phrases in nearly every
variable, the name uses the abbreviations below. `definition` always
keeps the source's full wording, so use it whenever a name is unclear.

- `lte`, `gt`:

  "less than or equal to" and "greater than", as in
  `income_gt_30_lte_50_hamfi` for household income above 30% and at most
  50% of HAMFI.

- `hh`:

  household, as in `hh_type` and `hh_size`.

- `unit_problems`:

  the source's "4 housing unit problems": cost burden above 30% of
  income, more than one person per room, and incomplete kitchen or
  plumbing facilities.

- `severe_unit_problems`:

  the source's "4 severe housing unit problems", a stricter version of
  the same four conditions (cost burden above 50% of income, more than
  1.5 persons per room). This is a distinct measure from
  `unit_problems`, so the two are never collapsed into one name.

- `facilities`:

  "complete kitchen and plumbing facilities"; `lacks_facilities` is the
  unit lacking them.

- `ppr`:

  persons per room, the source's overcrowding measure, as in `gt_1_ppr`.

- `nocalc`:

  cost burden not computed, which the source reports for households with
  zero or negative income.

- `1ormore`, `none`:

  "has one or more of" and "has none of", used for the housing-problem,
  age, and disability breakdowns.

- `nh`, `pi`, `aian`:

  non-Hispanic, Pacific Islander, and American Indian or Alaska Native.

`column_name` is unique within a CHAS table but not across tables –
every table has its own "owner occupied" total, for example – so filter
to one `chas_table` when a unique key is needed.
[`get_chas_housing_affordability()`](https://ui-research.github.io/climateapi/reference/get_chas_housing_affordability.md)
handles this by only renaming columns whose descriptive name is
unambiguous among the columns it returns.

Only estimate variables are described. The source's margin-of-error
variables (`_moe` rather than `_est`) share their estimate's definition
and so would not have a distinct name; they are omitted here and keep
their original names in the data.

HUD publishes one dictionary per five-year release, and the variable
codes change between releases as tables are added or revised. The
`vintage` column records which release the returned definitions come
from. When the requested `end_year` has no dictionary on disk, the most
recent available release is used as a stand-in; its definitions are
usually still correct, but codes present in one release and not the
other will be missing or mismatched, so check `vintage` when the warning
appears.

## Examples

``` r
if (FALSE) { # \dontrun{
codebook = get_chas_codebook(end_year = 2021)

## find the cost-burden subtotals in table 8 (tenure by income by cost burden)
codebook |>
  dplyr::filter(chas_table == "T8", column_type == "Subtotal") |>
  dplyr::filter(stringr::str_detect(definition, "cost burden"))
} # }
```
