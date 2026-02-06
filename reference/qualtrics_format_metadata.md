# Prep Qualtrics metadata

Prep Qualtrics metadata

## Usage

``` r
qualtrics_format_metadata(metadata, sections = c(), text_replace = "zzzzz")
```

## Arguments

- metadata:

  A dataframe containing unprocessed metadata from the Qualtrics API

- sections:

  A named vector specifying the last question number in each survey
  section

- text_replace:

  A named character vector of regex patterns to replace in the metadata

## Value

A tibble containing formatted Qualtrics survey metadata with the
following columns:

- question_number:

  Integer. The sequential position of the question in the survey
  (1-indexed).

- question_name:

  Character. The internal Qualtrics question identifier (e.g., "Q1",
  "Q2_1").

- text_main:

  Character. The primary question text, with any patterns specified in
  `text_replace` substituted.

- text_sub:

  Character. The sub-question or response option text, with any patterns
  specified in `text_replace` substituted.

- survey_section:

  Character. The name of the survey section to which the question
  belongs, as defined by the `sections` parameter. Filled upward from
  section boundaries.
