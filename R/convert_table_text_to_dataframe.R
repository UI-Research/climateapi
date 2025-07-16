#' Use an LLM to Convert Table Text to a Dataframe
#'
#' @description It is common to encounter valuable tabular data that is stored in a file type that does not codify tabular data as such, e.g., a table in a PDF or .docx file. This function uses a user-specified LLM (from OpenAI or Anthropic) to convert the text of a table into a dataframe. Note that users must have an API key with credits for the specified LLM. For a typical full-page PDF table, the LLM costs are roughly $.02-.05 USD per page.
#' @param text The table-as-text input. Each page of input should be its own vector or list item, as is the default when using `pdftools::pdf_text()`. Non-table text in the input `text` should be minimized.
#' @param column_types The column types of the output dataframe. This is a `type_object` object from the `ellmer` package. Including descriptions of what each column represents improves accuracy.
#' @param preprocess Should the text be preprocessed before it is passed to the LLM? Default is `TRUE`. This removes unneeded spaces, line breaks, and page numbers.
#' @param read_warning Did the user read the function documentation?
#' @param llm_company_name One of c("openai", "anthropic"). Default is "openai".
#' @param short_document Boolean; default is FALSE. If TRUE, it is assumed that the document is short enough that it can be processed in a single API call. If FALSE and the inputted `text` is a single item, the function throws an error. Note that multi-page documents should be broken into multi-item vectors/lists before being passed to `text`.
#' @param required Boolean; default is FALSE. If TRUE, the LLM will be instructed to return values for all columns. If FALSE, `NULL` values are allowed. Generally, NULL values should be allowed unless you are certain that every value in the inputted text-table has a non-NULL value.
#'
#' @return A list of dataframes, with each item corresponding to one page of the inputted text. The dataframes have the same column names and types as specified in `column_types`. Use `purrr::bind_rows()` to consolidate results into a single dataframe, if needed.
#' @export
#' @examples
#' \dontrun{
#' column_types = type_object(
#'  col1 = type_string("Zip code"),
#'  col2 = type_integer("Number of buildings"))
#'
#' convert_table_text_to_dataframe(
#'  text = example_text,
#'  column_types = column_types,
#'  preprocess = TRUE,
#'  read_warning = TRUE,
#'  llm_company_name = "openai",
#'  short_document = TRUE,
#'  required = FALSE)
#' }

convert_table_text_to_dataframe = function(
    text, column_types, llm_company_name = "openai", preprocess = TRUE,
    read_warning = FALSE, short_document = FALSE, required = FALSE) {

  if (!llm_company_name %in% c("openai", "anthropic")) {
    stop("Only `openai` and `anthropic` are supported `llm_company_name` values at this time.")}

  key_name = stringr::str_c(llm_company_name %>% stringr::str_to_upper(), "_API_KEY")
  key_check = Sys.getenv(key_name)

  if (nchar(key_check) == 0) {
    stop(stringr::str_c(
      "\nEnsure you have an appropriate API key registered in your .renviron file.
The name of the key should be", key_name, "\n")) }

  if (read_warning == FALSE) {
    stop("Read the function documentation before proceeding.") }

  if (length(text) == 1 & short_document == FALSE) {
    stop("\nThe supplied `text` has a length of 1. To avoid API timeouts and to improve cost efficiecy,
it is better to supply a vector (or list) where each item is a single page of the original document,
if applicable. If the original document is a single page or otherwise short, set `short_document` = TRUE.\n")
  }

  if (!is.list(text)) { text = as.list(text) }

  if (preprocess == TRUE) {
    text = text %>%
      stringr::str_replace_all(" {2,100}", " ") %>%
      stringr::str_replace_all("\\\n", " ") %>%
      stringr::str_remove_all("Page [0-9]{1,2} of [0-9]{1,2}") }

  ## override to allow NULL values
  if (required == FALSE) {
    column_types@properties = column_types@properties %>%
      purrr::map(
        function(x) {
          x@required = FALSE
          return (x) }) }

  if (llm_company_name == "openai") {
    chat = ellmer::chat_openai(model = "gpt-4.1-mini")
  } else if (llm_company_name == "anthropic") {
    chat = ellmer::chat_anthropic("claude-3-5-sonnet-20241022") }

  prompt_base = "This is the text of a table. Convert all of table data into the specified object. Here is the text: \n"

  suppressMessages({
    result = purrr::map(
      text,
      ~ tryCatch(
        chat$chat_structured(
          stringr::str_c(prompt_base, .x),
          echo = "text",
          type = ellmer::type_array(
            items = column_types)),
        error = function(e) { data.frame() } )) })

  warning(
    "These results are AI generated and should be thoroughly reviewed for accuracy and completeness.")

  return(result)
}
