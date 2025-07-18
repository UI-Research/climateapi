% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_table_text_to_dataframe.R
\name{convert_table_text_to_dataframe}
\alias{convert_table_text_to_dataframe}
\title{Use an LLM to Convert Table Text to a Dataframe}
\usage{
convert_table_text_to_dataframe(
  text,
  column_types,
  llm_company_name = "openai",
  preprocess = TRUE,
  read_warning = FALSE,
  short_document = FALSE,
  required = FALSE
)
}
\arguments{
\item{text}{The table-as-text input. Each page of input should be its own vector or list item, as is the default when using \code{pdftools::pdf_text()}. Non-table text in the input \code{text} should be minimized.}

\item{column_types}{The column types of the output dataframe. This is a \code{type_object} object from the \code{ellmer} package. Including descriptions of what each column represents improves accuracy.}

\item{llm_company_name}{One of c("openai", "anthropic"). Default is "openai".}

\item{preprocess}{Should the text be preprocessed before it is passed to the LLM? Default is \code{TRUE}. This removes unneeded spaces, line breaks, and page numbers.}

\item{read_warning}{Did the user read the function documentation?}

\item{short_document}{Boolean; default is FALSE. If TRUE, it is assumed that the document is short enough that it can be processed in a single API call. If FALSE and the inputted \code{text} is a single item, the function throws an error. Note that multi-page documents should be broken into multi-item vectors/lists before being passed to \code{text}.}

\item{required}{Boolean; default is FALSE. If TRUE, the LLM will be instructed to return values for all columns. If FALSE, \code{NULL} values are allowed. Generally, NULL values should be allowed unless you are certain that every value in the inputted text-table has a non-NULL value.}
}
\value{
A list of dataframes, with each item corresponding to one page of the inputted text. The dataframes have the same column names and types as specified in \code{column_types}. Use \code{purrr::bind_rows()} to consolidate results into a single dataframe, if needed.
}
\description{
It is common to encounter valuable tabular data that is stored in a file type that does not codify tabular data as such, e.g., a table in a PDF or .docx file. This function uses a user-specified LLM (from OpenAI or Anthropic) to convert the text of a table into a dataframe. Note that users must have an API key with credits for the specified LLM. For a typical full-page PDF table, the LLM costs are roughly $.02-.05 USD per page.
}
\examples{
\dontrun{
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
}
}
