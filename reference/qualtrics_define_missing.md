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

The inputted `df` object with missing/non-missing values applied to
specified columns
