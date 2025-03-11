## Author: Will Curran-Groome

#' @importFrom magrittr %>%

#' @title Prep Qualtrics metadata
#'
#' @param metadata A dataframe containing unprocessed metadata from the Qualtrics API
#' @param sections A named vector specifying the last question number in each survey section
#' @param text_replace A named character vector of regex patterns to replace in the metadata
#'
#' @return A dataframe of formatted metadata
#' @export
qualtrics_format_metadata = function(metadata, sections = c(), text_replace = "zzzzz") {

  metadata = metadata %>%
    dplyr::transmute(
      question_number = dplyr::row_number(),
      question_name = qname,
      text_main = main,
      text_sub = sub,
      dplyr::across(
        .cols = dplyr::matches("text"),
        .fns = ~ stringr::str_replace_all(.x, text_replace))) %>%
    dplyr::left_join(
      tibble::tibble(
        question_number = sections %>% as.numeric,
        survey_section = names(sections)),
      by = c("question_number")) %>%
    tidyr::fill(survey_section, .direction = "up")

  return(metadata)
}

#' @title Access Qualtrics metadata
#'
#' @param metadata The dataframe containing the Qualtrics metadata
#' @param question_name A regex pattern to match the question name(s)
#' @param survey_section A regex pattern to match the survey section(s)
#' @param return_values The name of the column (character) to be returned
#'
#' @return A character vector of the requested metadata
#' @export
qualtrics_get_metadata = function(
    metadata,
    question_name = NULL,
    survey_section = NULL,
    return_values = "text_sub") {

  if (is.null(survey_section) & is.null(question_name)) {
    stop("One of `survey_section` and `question_name` must be supplied.") }

  if (!is.null(question_name)) {
    result = metadata %>%
      dplyr::filter(stringr::str_detect(question_name, !!question_name)) %>%
      dplyr::pull(return_values) } else {
    result = metadata %>%
      dplyr::filter(stringr::str_detect(survey_section, !!survey_section)) %>%
      dplyr::pull(return_values) }

  return(result) }

#' @title Plot responses to Qualtrics survey questions
#'
#' @param df A dataframe of survey responses
#' @param metadata A dataframe of Qualtrics metadata
#' @param question_code_include A regex that matches the question codes to include in the plot
#' @param question_code_omit A regex that matches the question codes to omit from the plot
#' @param question_type one of c("checkbox_single", "checkbox_multi", "checkbox_factor")
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param subtitle_replace A named character vector of regex patterns to replace in the subtitle
#' @param text_remove A regex pattern to select response options to exclude from the plot
#' @param text_replace A named character vector of regex patterns to replace in the response text
#' @param omit_other Logical; whether to omit the "Other" response option. Default is TRUE.
#'
#' @return A ggplot object
#' @export
qualtrics_plot_question = function(
    df,
    metadata,
    question_code_include,
    question_code_omit = "zzzzz",
    question_type,
    title = "",
    subtitle = NULL,
    subtitle_replace = c("\\[Field.*\\]" = "your community", "Your best estimate is fine\\." = ""),
    text_remove = "please describe|please specify",
    text_replace = c("a" = "a"),
    omit_other = TRUE) {

  urbnthemes::set_urbn_defaults(style = "print")
  #
  # #### TEST
  # df = tibble::tribble(
  #   ~QID15_TEXT,
  #   NA,
  #   NA,
  #   15000,
  #   12500,
  #   6000,
  #   326,
  #   9000,
  #   1000,
  #   8500,
  #   9000)
  #
  # question_code_include = "QID15_TEXT"
  # question_type = "continuous"
  # metadata = tibble::tribble(
  #   ~question_number, ~question_name,                                                                    ~text_main, ~text_sub,
  #   33,   "QID15_TEXT", "How many people live in [Field-community_name]? Your best estimate is fine.",        NA)
  #
  # subtitle = NULL

  if (is.null(subtitle)) {
    subtitle = qualtrics_get_metadata(
      metadata,
      question_name = question_code_include,
      return_values = "text_main") %>%
      .[1] %>%
      stringr::str_replace_all(subtitle_replace) %>%
      stringr::str_wrap(100)}

  ## Continuous-response questions
  if (question_type == "continuous") {
    plot = df %>%
      dplyr::mutate(variable = as.numeric(.data[[question_code_include]])) %>%
      ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = variable)) +
        urbnthemes::theme_urbn_print() +
        ggplot2::scale_x_continuous(labels = scales::comma_format()) +
        ggplot2::labs(x = "", title = title, subtitle = subtitle) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  ## Checkbox-type questions
  if (stringr::str_detect(question_type, "checkbox")) {
    basic_data = df %>%
      dplyr::select(response_id, c(dplyr::matches(question_code_include), -dplyr::matches(question_code_omit))) %>%
      tidyr::pivot_longer(-response_id) %>%
      dplyr::filter(!is.na(value))

    if (question_type == "checkbox_factor") {
      plot = basic_data %>%
        dplyr::left_join(metadata, by = c("name" = "question_name")) %>%
        dplyr::count(text_sub, value) %>%
        dplyr::mutate(text_sub = stringr::str_replace_all(text_sub, text_replace) %>% stringr::str_wrap(60)) %>%
        dplyr::group_by(text_sub) %>%
        dplyr::mutate(order = sum(n)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(
          ggplot2::aes(
            y = stats::reorder(text_sub, order),
            x = n,
            fill = value),
          position = "stack") +
        urbnthemes::theme_urbn_print() +
        ggplot2::labs(
          x = "",
          y = "",
          title = title,
          subtitle = subtitle) }

    if (question_type %in% c("checkbox_single", "checkbox_multi")) {
      reformatted_data = basic_data %>%
        tidyr::pivot_wider(names_from = value) %>%
        janitor::clean_names() %>%
        dplyr::left_join(
          metadata %>%
            dplyr::select(question_name, text_sub) %>%
            dplyr::mutate(
              text_sub = text_sub %>% stringr::str_remove_all(" - .*")), by = c("name" = "question_name")) %>%
        dplyr::filter(!stringr::str_detect(text_sub, text_remove)) %>%
        dplyr::group_by(name) %>%
        dplyr::summarize(
          text_sub = dplyr::first(text_sub, na_rm = TRUE),
          dplyr::across(.cols = -c(response_id, text_sub), .fns = ~ sum(!is.na(.x), na.rm = T))) %>%
        dplyr::filter(!is.na(name)) %>%
        tidyr::pivot_longer(cols = -c(name, text_sub), names_to = "type", values_to = "count") %>%
        dplyr::mutate(
          text_sub = stringr::str_replace_all(text_sub, text_replace) %>% stringr::str_wrap(60),
          type = type %>% stringr::str_replace_all("_", " ") %>% stringr::str_to_sentence())

      plot = reformatted_data %>%
        dplyr::group_by(text_sub) %>%
        dplyr::mutate(order = sum(count)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(
          ggplot2::aes(y = stats::reorder(text_sub, order), x = count),
          position = "stack",
          fill = urbnthemes::palette_urbn_main[1] %>% as.character()) +
        urbnthemes::theme_urbn_print() +
        ggplot2::labs(
          x = "# responses",
          y = "",
          title = title,
          subtitle = subtitle)

      if (question_type == "checkbox_multi") {
        plot = plot +
          ggplot2::geom_col(
            ggplot2::aes(
              y = stats::reorder(text_sub, order),
              x = count,
              fill = type),
            position = "stack") }}
    }

  return(plot)
}

utils::globalVariables(
  c("qname", "main", "survey_section", "response_id", "value", "text_sub",
    "name", "question_name", "type", "variable"))
