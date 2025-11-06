# Use an LLM to Convert Table Text to a Dataframe

It is common to encounter valuable tabular data that is stored in a file
type that does not codify tabular data as such, e.g., a table in a PDF
or .docx file. This function uses a user-specified LLM (from OpenAI or
Anthropic) to convert the text of a table into a dataframe. Note that
users must have an API key with credits for the specified LLM. For a
typical full-page PDF table, the LLM costs are roughly \$.02-.05 USD per
page.

## Usage

``` r
convert_table_text_to_dataframe(
  text,
  column_types,
  llm_company_name = "openai",
  preprocess = TRUE,
  read_warning = FALSE,
  short_document = FALSE,
  required = FALSE
)
```

## Arguments

- text:

  The table-as-text input. Each page of input should be its own vector
  or list item, as is the default when using
  [`pdftools::pdf_text()`](https://docs.ropensci.org/pdftools//reference/pdftools.html).
  Non-table text in the input `text` should be minimized.

- column_types:

  The column types of the output dataframe. This is a `type_object`
  object from the `ellmer` package. Including descriptions of what each
  column represents improves accuracy.

- llm_company_name:

  One of c("openai", "anthropic"). Default is "openai".

- preprocess:

  Should the text be preprocessed before it is passed to the LLM?
  Default is `TRUE`. This removes unneeded spaces, line breaks, and page
  numbers.

- read_warning:

  Did the user read the function documentation?

- short_document:

  Boolean; default is FALSE. If TRUE, it is assumed that the document is
  short enough that it can be processed in a single API call. If FALSE
  and the inputted `text` is a single item, the function throws an
  error. Note that multi-page documents should be broken into multi-item
  vectors/lists before being passed to `text`.

- required:

  Boolean; default is FALSE. If TRUE, the LLM will be instructed to
  return values for all columns. If FALSE, `NULL` values are allowed.
  Generally, NULL values should be allowed unless you are certain that
  every value in the inputted text-table has a non-NULL value.

## Value

A list of dataframes, with each item corresponding to one page of the
inputted text. The dataframes have the same column names and types as
specified in `column_types`. Use `purrr::bind_rows()` to consolidate
results into a single dataframe, if needed.

## Examples

``` r
if (FALSE) { # \dontrun{
column_types = type_object(
 col1 = type_string("Zip code"),
 col2 = type_integer("Number of buildings"))

convert_table_text_to_dataframe(
 text = example_text,
 column_types = column_types,
 preprocess = TRUE,
 read_warning = TRUE,
 llm_company_name = "openai",
 short_document = TRUE,
 required = FALSE)
} # }
```
