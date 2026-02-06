# Access Qualtrics metadata

Access Qualtrics metadata

## Usage

``` r
qualtrics_get_metadata(
  metadata,
  question_name = NULL,
  survey_section = NULL,
  return_values = "text_sub"
)
```

## Arguments

- metadata:

  The dataframe containing the Qualtrics metadata

- question_name:

  A regex pattern to match the question name(s)

- survey_section:

  A regex pattern to match the survey section(s)

- return_values:

  The name of the column (character) to be returned

## Value

A character vector containing the values from the column specified by
`return_values` (default: "text_sub"), filtered to rows matching either
the `question_name` or `survey_section` pattern. The length of the
vector corresponds to the number of matching rows in the metadata.
Returns an empty character vector if no matches are found.
