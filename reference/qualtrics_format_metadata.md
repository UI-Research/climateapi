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

A dataframe of formatted metadata
