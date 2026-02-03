# Tests for convert_table_text_to_dataframe.R

test_that("convert_table_text_to_dataframe validates llm_company_name", {
  expect_error(
    convert_table_text_to_dataframe(
      text = "test",
      column_types = NULL,
      llm_company_name = "invalid",
      read_warning = TRUE,
      short_document = TRUE
    ),
    "Only `openai` and `anthropic`"
  )
})

test_that("convert_table_text_to_dataframe requires read_warning acknowledgment", {
  expect_error(
    convert_table_text_to_dataframe(
      text = "test",
      column_types = NULL,
      read_warning = FALSE
    ),
    "Read the function documentation"
  )
})

test_that("convert_table_text_to_dataframe validates text length", {
  # Single-item text without short_document=TRUE should error
  expect_error(
    convert_table_text_to_dataframe(
      text = "single page text",
      column_types = NULL,
      read_warning = TRUE,
      short_document = FALSE
    ),
    "length of 1"
  )
})

test_that("convert_table_text_to_dataframe function signature is correct", {
  expect_true(is.function(convert_table_text_to_dataframe))

  # Check parameter names exist
  params <- names(formals(convert_table_text_to_dataframe))
  expect_true("text" %in% params)
  expect_true("column_types" %in% params)
  expect_true("llm_company_name" %in% params)
  expect_true("preprocess" %in% params)
  expect_true("read_warning" %in% params)
  expect_true("short_document" %in% params)
  expect_true("required" %in% params)

  # Check defaults
  f <- convert_table_text_to_dataframe
  expect_equal(formals(f)$llm_company_name, "openai")
  expect_true(formals(f)$preprocess)
  expect_false(formals(f)$read_warning)
  expect_false(formals(f)$short_document)
  expect_false(formals(f)$required)
})
