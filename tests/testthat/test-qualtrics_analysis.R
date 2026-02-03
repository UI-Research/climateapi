# Tests for qualtrics_analysis.R functions

test_that("qualtrics_format_metadata validates input parameters", {
  # Create mock metadata
  mock_metadata <- data.frame(
    qname = c("Q1", "Q2", "Q3"),
    main = c("Question 1 text", "Question 2 text", "Question 3 text"),
    sub = c("Sub 1", "Sub 2", "Sub 3"),
    stringsAsFactors = FALSE
  )

  # Test that function runs without error with valid inputs
  expect_no_error(qualtrics_format_metadata(mock_metadata))

  # Test with sections parameter
  sections <- c("Section A" = 2, "Section B" = 3)
  expect_no_error(qualtrics_format_metadata(mock_metadata, sections = sections))
})

test_that("qualtrics_format_metadata returns expected structure", {
  mock_metadata <- data.frame(
    qname = c("Q1", "Q2", "Q3", "Q4"),
    main = c("Main 1", "Main 2", "Main 3", "Main 4"),
    sub = c("Sub 1", "Sub 2", "Sub 3", "Sub 4"),
    stringsAsFactors = FALSE
  )

  result <- qualtrics_format_metadata(mock_metadata)

  # Check that result is a data frame/tibble
  expect_true(is.data.frame(result))

  # Check expected columns exist
  expect_true("question_number" %in% names(result))
  expect_true("question_name" %in% names(result))
  expect_true("text_main" %in% names(result))
  expect_true("text_sub" %in% names(result))

  # Check row count matches input
expect_equal(nrow(result), 4)

  # Check question_number is sequential
  expect_equal(result$question_number, 1:4)
})

test_that("qualtrics_format_metadata handles sections correctly", {
  mock_metadata <- data.frame(
    qname = c("Q1", "Q2", "Q3", "Q4"),
    main = c("Main 1", "Main 2", "Main 3", "Main 4"),
    sub = c("Sub 1", "Sub 2", "Sub 3", "Sub 4"),
    stringsAsFactors = FALSE
  )

  sections <- c("Section A" = 2, "Section B" = 4)
  result <- qualtrics_format_metadata(mock_metadata, sections = sections)

  # Check survey_section column exists
  expect_true("survey_section" %in% names(result))

  # Check sections are filled correctly (upward fill)
  expect_equal(result$survey_section[1:2], c("Section A", "Section A"))
  expect_equal(result$survey_section[3:4], c("Section B", "Section B"))
})

test_that("qualtrics_get_metadata validates input parameters", {
  mock_metadata <- data.frame(
    question_number = 1:3,
    question_name = c("Q1", "Q2", "Q3"),
    text_main = c("Main 1", "Main 2", "Main 3"),
    text_sub = c("Sub 1", "Sub 2", "Sub 3"),
    survey_section = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )

  # Test that function errors when neither question_name nor survey_section provided
  expect_error(
    qualtrics_get_metadata(mock_metadata),
    "One of `survey_section` and `question_name` must be supplied"
  )

  # Test that function runs with question_name
  expect_no_error(qualtrics_get_metadata(mock_metadata, question_name = "Q1"))

  # Test that function runs with survey_section
  expect_no_error(qualtrics_get_metadata(mock_metadata, survey_section = "A"))
})

test_that("qualtrics_get_metadata returns correct values", {
  mock_metadata <- data.frame(
    question_number = 1:3,
    question_name = c("Q1", "Q2", "Q3"),
    text_main = c("Main 1", "Main 2", "Main 3"),
    text_sub = c("Sub 1", "Sub 2", "Sub 3"),
    survey_section = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )

  # Test filtering by question_name
  result <- qualtrics_get_metadata(mock_metadata, question_name = "Q1")
  expect_equal(result, "Sub 1")

  # Test filtering by survey_section
  result <- qualtrics_get_metadata(mock_metadata, survey_section = "A")
  expect_equal(length(result), 2)

  # Test custom return_values
  result <- qualtrics_get_metadata(mock_metadata, question_name = "Q2", return_values = "text_main")
  expect_equal(result, "Main 2")
})

test_that("qualtrics_define_missing validates input parameters", {
  mock_df <- data.frame(
    Q1_a = c("Yes", NA, "Yes"),
    Q1_b = c(NA, "No", "Yes"),
    Q2 = c("Yes", "No", NA),
    stringsAsFactors = FALSE
  )

  # Test that default_values must be a list of length 3
  expect_error(
    qualtrics_define_missing(mock_df, question_code_include = "Q1", default_values = list("No")),
    "`default_values` must be a list of length 3"
  )

  expect_error(
    qualtrics_define_missing(mock_df, question_code_include = "Q1", default_values = "No"),
    "`default_values` must be a list of length 3"
  )

  # Test that function runs with valid inputs
  expect_no_error(
    qualtrics_define_missing(mock_df, question_code_include = "Q1")
  )
})

test_that("qualtrics_define_missing returns expected structure", {
  mock_df <- data.frame(
    Q1_a = c("Yes", NA, "Yes"),
    Q1_b = c(NA, "No", "Yes"),
    Q2 = c("Yes", "No", NA),
    stringsAsFactors = FALSE
  )

  result <- qualtrics_define_missing(mock_df, question_code_include = "Q1")

  # Check that result is a data frame
  expect_true(is.data.frame(result))

  # Check that only Q1 columns are returned (not Q2)
  expect_true("Q1_a" %in% names(result))
  expect_true("Q1_b" %in% names(result))
  expect_false("Q2" %in% names(result))
})

test_that("qualtrics_define_missing handles predicate question validation", {
  mock_df <- data.frame(
    predicate = c("Yes", "No", NA),
    Q1_a = c("A", NA, "C"),
    Q1_b = c(NA, "B", "D"),
    stringsAsFactors = FALSE
  )

  # Test that predicate_question must exist in df
  expect_error(
    qualtrics_define_missing(
      mock_df,
      question_code_include = "Q1",
      predicate_question = "nonexistent"
    ),
    "Predicate question not found"
  )

  # Test that predicate_question_negative_value must be provided with predicate_question
  expect_error(
    qualtrics_define_missing(
      mock_df,
      question_code_include = "Q1",
      predicate_question = "predicate"
    ),
    "negative value must also be provided"
  )
})
