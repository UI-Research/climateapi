# Fill in missing and non-missing values across interrelated survey questions

Fill in missing and non-missing values across interrelated survey
questions

## Usage

``` r
qualtrics_define_missing(
  df,
  question_code_include,
  question_code_omit = NULL,
  default_values = list("No", 0, as.Date(0)),
  predicate_question = NULL,
  predicate_question_negative_value = NULL
)
```

## Arguments

- df:

  A dataframe of survey responses

- question_code_include:

  A regex that matches the columns to include in the missing non-missing
  value imputation

- question_code_omit:

  A regex that matches the columns to omit from the missing non-missing
  value imputation

- default_values:

  A list of length three, specifying the default, non-missing values to
  be used for character, numeric, and Date columns, respectively

- predicate_question:

  Optional. The name of a single column that controls whether columns
  selected with `question_code_include`

- predicate_question_negative_value:

  If `predicate_question` is specified, provide the value that indicates
  a negative response to the predicate question. For responses where the
  predicate question has this value, this value will be imputed to the
  specified columns

## Value

A tibble containing only the columns selected by `question_code_include`
(excluding those matching `question_code_omit`), with missing values
handled according to the following logic:

- Without predicate_question:

  If all selected columns are NA for a row, values remain NA. If any
  selected column has a non-NA value, NA values in other selected
  columns are replaced with the appropriate default value from
  `default_values` based on column type.

- With predicate_question:

  If the predicate question is NA, all selected columns are set to NA.
  If the predicate question equals `predicate_question_negative_value `,
  all selected columns are set to the appropriate default value.
  Otherwise, original values are preserved.

Column types and their default value mappings: character uses
`default_values[[1]]`, numeric uses `default_values[[2]]`, and
Date/POSIXct uses `default_values[[3]]`.
