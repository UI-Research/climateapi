# Plot responses to Qualtrics survey questions

Plot responses to Qualtrics survey questions

## Usage

``` r
qualtrics_plot_question(
  df,
  metadata,
  question_code_include,
  question_code_omit = "zzzzz",
  question_type,
  title = "",
  subtitle = NULL,
  subtitle_replace = c(`\\[Field.*\\]` = "your community",
    `Your best estimate is fine\\.` = ""),
  text_remove = "please describe|please specify",
  text_replace = c(a = "a"),
  omit_other = TRUE
)
```

## Arguments

- df:

  A dataframe of survey responses

- metadata:

  A dataframe of Qualtrics metadata

- question_code_include:

  A regex that matches the question codes to include in the plot

- question_code_omit:

  A regex that matches the question codes to omit from the plot

- question_type:

  one of c("continuous", "checkbox_single", "checkbox_multi",
  "checkbox_factor")

- title:

  Plot title

- subtitle:

  Plot subtitle

- subtitle_replace:

  A named character vector of regex patterns to replace in the subtitle

- text_remove:

  A regex pattern to select response options to exclude from the plot

- text_replace:

  A named character vector of regex patterns to replace in the response
  text

- omit_other:

  Logical; whether to omit the "Other" response option. Default is TRUE.

## Value

A ggplot object
