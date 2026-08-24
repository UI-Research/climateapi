#' Derive collision-free local filenames for a set of PDA report URLs
#'
#' File names can have tricky characters, including escaped/special characters
#' derived from non-ASCI text. This handles that complexity.
#'
#' @param urls A character vector of PDF URLs.
#'
#' @return A tibble with one row per distinct URL, containing `url` and
#'   `destination_file` (a basename, not a full path).
#' @noRd
resolve_pdf_destinations = function(urls) {

  destinations1 = tibble::tibble(url = unique(urls)) %>%
    dplyr::mutate(
      base_name = url %>%
        ## a query string or fragment left in place produces an illegal Windows
        ## filename ("report.pdf?v=2" -> "report?v=2.pdf") that cannot be written
        stringr::str_remove("[?#].*$") %>%
        stringr::str_extract("[^/]+$") %>%
        curl::curl_unescape() %>%
        stringr::str_remove(stringr::regex("\\.pdf$", ignore_case = TRUE)) %>%
        stringr::str_replace_all("[<>:\"/\\\\|?*]", "_") %>%
        stringr::str_remove_all("[[:cntrl:]]") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("\\s+", "_")) %>%
    dplyr::add_count(base_name, name = "base_name_count") %>%
    dplyr::mutate(
      needs_hash = base_name_count > 1 | is.na(base_name) | base_name == "",
      destination_file = dplyr::if_else(
        needs_hash,
        stringr::str_c(
          dplyr::coalesce(base_name, "pda-report"), "-",
          purrr::map_chr(url, ~ stringr::str_sub(rlang::hash(.x), 1, 8)),
          ".pdf"),
        stringr::str_c(base_name, ".pdf")))

  if (any(destinations1$needs_hash)) {
    is_empty_name = is.na(destinations1$base_name) | destinations1$base_name == ""
    collided = destinations1$needs_hash & !is_empty_name

    message(
      stringr::str_c(
        sum(destinations1$needs_hash),
        " report URL(s) were given a URL hash so that every report is retained: ",
        sum(collided),
        " reduced to a filename already claimed by a different URL, and ",
        sum(is_empty_name),
        " reduced to no filename at all. Affected: ",
        stringr::str_c(
          destinations1$destination_file[destinations1$needs_hash],
          collapse = ", "))) }

  destinations1 %>% dplyr::select(url, destination_file)
}

#' The browser identity FEMA's site requires
#'
#' @return A length-one character `User-Agent` string.
#' @noRd
browser_user_agent = function() {
  stringr::str_c(
    "Mozilla/5.0 (Linux; Android 11; SAMSUNG SM-G973U) AppleWebKit/537.36 ",
    "(KHTML, like Gecko) SamsungBrowser/14.2 Chrome/87.0.4280.141 Mobile ",
    "Safari/537.36")
}

#' Download a PDF from a URL and verify
#'
#' A download is kept only if the resulting file begins with the `%PDF` magic
#' number and exceeds `minimum_bytes`. Anything else -- an HTML error page served
#' with a 200 status, a truncated transfer -- is deleted.
#'
#' @param url URL to the pdf.
#' @param destfile The full local path the PDF should be written to, as
#'   resolved by `resolve_pdf_destinations()`.
#' @param minimum_bytes Files smaller than this are treated as failed
#'   downloads. The smallest genuine report currently cached is roughly 21 KB.
#'
#' @return One of `"cached"`, `"downloaded"`, or `"failed"`.
#' @noRd
save_pdf = function(url, destfile, minimum_bytes = 2048) {

  if (file.exists(destfile)) { return("cached") }

  headers = c(
    "User-Agent" = browser_user_agent(),
    "Accept" = "application/pdf",
    "Accept-Language" = "en-US",
    "Connection" = "keep-alive")

  ## Some listing links are percent-encoded twice: FEMA publishes
  ## "...Luise%25C3%25B1oIndians.pdf", where "%25" is an encoded "%", so the
  ## request asks for a file literally named "Luise%C3%B1o..." and returns 404.
  ## Undoing one layer of encoding recovers it. 
  candidate_urls = unique(c(url, stringr::str_replace_all(url, "%25", "%")))
  downloaded = FALSE
  for (candidate_url in candidate_urls) {
    downloaded = tryCatch({
      utils::download.file(
        url = candidate_url,
        destfile = destfile,
        headers = headers,
        mode = "wb")
      TRUE},
      error = function(e) FALSE,
      warning = function(w) NA)

    if (isTRUE(downloaded)) { break } }

  is_pdf = FALSE
  if (file.exists(destfile) && file.size(destfile) >= minimum_bytes) {
    connection = file(destfile, "rb")
    file_header = tryCatch(
      rawToChar(readBin(connection, "raw", 4)),
      finally = close(connection))
    is_pdf = identical(file_header, "%PDF") }

  if (!is_pdf) {
    ## remove the partial or wrong-content file so this URL is retried next run
    if (file.exists(destfile)) { file.remove(destfile) }
    warning(
      stringr::str_c("Could not download a valid PDF from: ", url),
      call. = FALSE)
    return("failed") }

  "downloaded"
}

#' @title Download Preliminary Damage Assessment (PDA) Reports to Disk
#'
#' @description Downloads every PDA report that FEMA publishes into a local
#'   directory, so that `get_preliminary_damage_assessments()` has a complete and
#'   current set of source documents to parse. Run this before regenerating the
#'   dataset; `get_preliminary_damage_assessments()` parses whatever is already on
#'   disk and never fetches anything itself.
#'
#' @details Walks every page of FEMA's PDA report listing at
#' https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports
#' and downloads any report not
#' already present in `cache_directory`. 
#'
#' A page that errors is retried, and exhausting the retries raises an error
#' rather than ending the walk. 
#'
#' @param cache_directory The folder where scraped PDFs are written.
#' @param max_pages A guard against an unbounded walk if the listing ever stops
#'   returning empty pages. 
#' @param attempts_per_page How many times to try a listing page before treating
#'   it as a failure.
#' @param pages Which listing pages to read, as a numeric vector. The default
#'   `NULL` walks the whole listing until a page returns no links, which is the
#'   only setting that guarantees complete coverage. FEMA lists newest first, so
#'   `pages = 0:2` is enough to pick up recently published reports and is much
#'   faster. Two caveats when this is set: cached files that are no longer listed
#'   cannot be detected, because the full listing was never read; and filename
#'   collisions are resolved only against the pages requested, so a name could be
#'   assigned that a page outside the range also claims.
#' @param delay_seconds Seconds to pause between listing pages, to avoid
#'   hammering FEMA's site. This is what dominates the running time of a full
#'   walk -- roughly 70 pages -- not the downloads, which are skipped for reports
#'   already in `cache_directory`.
#' @param quiet Suppress progress messages? The walk covers roughly 70 pages with
#'   a pause between each and is otherwise silent for several minutes, so progress
#'   is reported by default. Warnings about failed downloads and about cached
#'   files no longer listed are always raised, regardless of this setting.
#'
#' @return Invisibly, a tibble with one row per report found on the site,
#'   containing `url`, `destination_file`, and `status` (`"cached"`,
#'   `"downloaded"`, or `"failed"`). Called for its side effect: PDFs are written
#'   to `cache_directory`.
#' @export
#'
#' @examples
#' \dontrun{
#' ## refresh the local archive, then rebuild the dataset from it
#' scrape_pda_pdfs()
#' get_preliminary_damage_assessments(use_cache = FALSE)
#' }
scrape_pda_pdfs = function(
    cache_directory = file.path(climateapi::get_box_path(), "hazards", "urban", "preliminary-damage-assessments", "pdfs"),
    pages = NULL,
    max_pages = 200,
    attempts_per_page = 5,
    delay_seconds = 2,
    quiet = FALSE) {

  ## progress reporting only; warnings are raised unconditionally elsewhere
  notify = function(...) { if (!quiet) { message(stringr::str_c(...)) } }

  base_url = "https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports?page="

  ## restored on exit so the function does not leave the user's session altered
  original_timeout = getOption("timeout")
  on.exit(options(timeout = original_timeout), add = TRUE)
  options(timeout = 200)

  listing_headers = httr::add_headers(
    "User-Agent" = browser_user_agent(),
    "Accept" = "text/html",
    "Accept-Language" = "en-US")

  walk_whole_listing = is.null(pages)
  page_queue = if (walk_whole_listing) NULL else as.numeric(pages)

  if (!walk_whole_listing) {
    notify(
      "Reading only page(s) ", stringr::str_c(sort(unique(page_queue)), collapse = ", "),
      ". Withdrawn-report detection is skipped and filename collisions are ",
      "resolved only across these pages.") }

  urls1 = character(0)
  page_number = if (walk_whole_listing) 0 else page_queue[1]
  page_index = 1
  pages_read = 0

  repeat {
    page_urls = NULL
    notify("Reading listing page ", page_number, ".")

    for (attempt in seq_len(attempts_per_page)) {
      page_urls = tryCatch({
        response = httr::GET(stringr::str_c(base_url, page_number), listing_headers)

        if (httr::status_code(response) != 200) { stop("non-200 response") }

        httr::content(response, as = "text", encoding = "UTF-8") %>%
          rvest::read_html() %>%
          rvest::html_elements("a") %>%
          rvest::html_attr("href") %>%
          purrr::discard(~ is.na(.x)) %>%
          purrr::keep(~ stringr::str_detect(.x, stringr::regex("\\.pdf$", ignore_case = TRUE)))},
        error = function(e) NULL)

      if (!is.null(page_urls)) { break }
      Sys.sleep(min(120, delay_seconds * 2 ^ attempt))
    }

    if (is.null(page_urls)) {
      stop(
        stringr::str_c(
          "Could not read listing page ", page_number, " after ",
          attempts_per_page, " attempts, most likely because FEMA is rate-",
          "limiting the walk. Stopping rather than continuing, because ",
          "treating a failed page as the end of the listing would silently ",
          "omit every report beyond it.\n",
          "Reports from pages already read were not downloaded. To resume from ",
          "where this stopped, wait a few minutes and call:\n",
          "  scrape_pda_pdfs(pages = ", page_number, ":",
          page_number + 49, ")\n",
          "or raise delay_seconds / attempts_per_page for a slower, more ",
          "patient walk."),
        call. = FALSE) }

    if (length(page_urls) == 0 && walk_whole_listing) { break }

    urls1 = c(urls1, page_urls)
    pages_read = pages_read + 1

    if (walk_whole_listing) {
      page_number = page_number + 1
      if (page_number >= max_pages) {
        stop(
          stringr::str_c(
            "Reached max_pages (", max_pages, ") without finding an empty ",
            "listing page. Coverage may be incomplete; raise max_pages or check ",
            "whether the listing URL still paginates as expected."),
          call. = FALSE) }
    } else {
      page_index = page_index + 1
      if (page_index > length(page_queue)) { break }
      page_number = page_queue[page_index] }

    Sys.sleep(delay_seconds)
  }

  ## listing hrefs are site-relative, but tolerate an absolute URL in case the
  ## markup changes
  urls2 = dplyr::if_else(
    stringr::str_detect(urls1, "^https?://"),
    urls1,
    stringr::str_c("https://www.fema.gov", urls1)) %>%
    unique()

  destinations = resolve_pdf_destinations(urls2)

  notify(
    "Found ", nrow(destinations), " report(s) across ", pages_read,
    " listing page(s).")

  destinations2 = destinations %>%
    dplyr::mutate(
      status = purrr::map2_chr(
        url,
        file.path(cache_directory, destination_file),
        ~ save_pdf(url = .x, destfile = .y)))

  notify(
    "Downloaded ", sum(destinations2$status == "downloaded"), "; already ",
    "cached ", sum(destinations2$status == "cached"), "; failed ",
    sum(destinations2$status == "failed"), ".")

  if (any(destinations2$status == "failed")) {
    warning(
      stringr::str_c(
        sum(destinations2$status == "failed"),
        " report(s) could not be downloaded and are absent from the cache. ",
        "They were not written to disk, so re-running will retry them. ",
        "Do not treat the archive as complete until this count is zero."),
      call. = FALSE) }

  ## Files present locally but not listed on the site. 
  orphans = setdiff(
    list.files(cache_directory, recursive = TRUE, pattern = "(?i)pdf$"),
    destinations2$destination_file)

  if (length(orphans) > 0) {
    warning(
      stringr::str_c(
        length(orphans), " cached file(s) are no longer listed on FEMA's site ",
        "and may have been withdrawn or renamed. They are still on disk and ",
        "will still be parsed into the dataset. Review before publishing: ",
        stringr::str_c(orphans, collapse = ", ")),
      call. = FALSE) }

  invisible(destinations2)
}

#' Extract the value a report prints between two field labels
#'
#' @param text The report text to search.
#' @param term1 The label the value follows.
#' @param term2 The label the value runs up to.
#'
#' @return The text between `term1` and `term2`, excluding both terms.
#' @noRd
extract_value = function(text, term1, term2) {

  ## Colons are optional because most reports render these fields as
  ## "<label><footnote digit> <value>" with no colon at all, while some print
  ## one. 
  optional_colon = function(term) stringr::str_replace_all(term, ":", ":?")

  ## Each term is wrapped in its own group before the two are joined, because
  ## several call sites pass an alternation ("poverty households:|low income
  ## households:").
  term1_grouped = stringr::str_c("(?:", optional_colon(term1), ")")
  term2_grouped = stringr::str_c("(?:", optional_colon(term2), ")")

  stringr::str_extract(
    text,
    stringr::str_c(term1_grouped, ".*?", term2_grouped)) %>%
    stringr::str_remove(stringr::str_c("^", term1_grouped)) %>%
    stringr::str_remove(stringr::str_c(term2_grouped, "$")) %>%
    stringr::str_squish()
}

#' The dash characters the reports use interchangeably
#'
#' @return A length-one character regular expression character class.
#' @noRd
dash_class = function() { "[-\\u2010-\\u2015]" }

#' Match a field label together with the dash that separates it from its value
#'
#' @param label The label text, without its separator.
#' @return A regular expression matching the label and its separator.
#' @noRd
dash_label = function(label) {
  stringr::str_c(label, "\\s*", dash_class(), "?\\s*")
}

#' Match a program's "Not requested" wording
#'
#' @param program The program name as the reports print it.
#' @return A regular expression.
#' @noRd
not_requested_pattern = function(program) {
  stringr::str_c(program, "\\s*", dash_class(), "?\\s*(N|n)ot\\s*(R|r)equested")
}

#' Drop a label's superscript footnote marker from an extracted value
#'
#' `pdftools` renders the superscript footnote numbers that FEMA attaches to
#' several field labels inline with the text, so an extracted value arrives as
#' "7 2.29" rather than "2.29". 
#'
#' @param value A character vector of extracted values.
#' @return `value` with a leading footnote marker removed where present.
#' @noRd
remove_footnote_marker = function(value) {
  dplyr::if_else(
    stringr::str_detect(value, "^[0-9]{1,2}\\s+\\S"),
    stringr::str_remove(value, "^[0-9]{1,2}\\s+"),
    value)
}

#' Take the first whitespace-separated token of an extracted value
#'
#' An extracted span often carries the beginning of the next field after the
#' value itself, so the value is the first token. Used wherever a single figure
#' is wanted from a span, so that all of them are taken the same way.
#'
#' @param value A character vector of extracted values.
#' @return A character vector of first tokens.
#' @noRd
first_token = function(value) {
  stringr::str_split(value, " ") %>% purrr::map_chr(~ .x[1])
}

#' Version of the PDA parsing logic
#'
#' Written into every generated dataset as `parser_version` and checked when a
#' cached dataset is read. Increment it whenever a change alters the values this
#' code produces.
#'
#' @return A length-one character version string.
#' @noRd
pda_parser_version = function() { "2.7.0" }

#' Warn when a cached dataset was written by different parsing logic
#'
#' @param file_path Path to the cached CSV.
#' @return Invisibly `TRUE` if the versions match, `FALSE` otherwise.
#' @noRd
check_cache_parser_version = function(file_path) {

  cached_version = tryCatch(
    {header = readr::read_csv(file_path, n_max = 1, show_col_types = FALSE)
     if ("parser_version" %in% names(header)) as.character(header$parser_version[1]) else NA_character_},
    error = function(e) NA_character_)

  if (identical(cached_version, pda_parser_version())) { return(invisible(TRUE)) }

  warning(
    stringr::str_c(
      "The cached dataset at ", file_path, " was written by parser version ",
      dplyr::if_else(is.na(cached_version), "(unrecorded)", cached_version),
      ", but this code is version ", pda_parser_version(),
      ". Its values may not be reproducible from the current code. Regenerate ",
      "with use_cache = FALSE before relying on these data."),
    call. = FALSE)

  invisible(FALSE)
}

#' Set impossible percentage values to NA
#'
#' Columns named `*_percent` record shares of a population and cannot fall
#' outside 0-100. A handful of reports yield values far outside that range; 
#' These are set to `NA` rather than published, and the number of such occurrences is reported.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @return `pda_df` with out-of-range percentages replaced by `NA`.
#' @noRd
drop_impossible_percentages = function(pda_df) {

  percent_columns = names(pda_df) %>%
    purrr::keep(~ stringr::str_detect(.x, "percent$")) %>%
    purrr::keep(~ is.numeric(pda_df[[.x]]))

  if (length(percent_columns) == 0) { return(pda_df) }

  pda_df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(percent_columns),
        .fns = ~ dplyr::if_else(.x < 0 | .x > 100, NA_real_, .x)))
}

#' Columns that are not parsed field values
#'
#' Identity and metadata columns, exempt from the whitespace, punctuation, and
#' numeric-coercion steps.
#'
#' @return A character vector of column names.
#' @noRd
non_extracted_columns = function() {
  c("path", "disaster_number", "disaster_number_filename", "event_type",
    "event_title", "event_native_flag", "text")
}

#' Standardize fields from PDA texts
#' @param path The path to the PDF file (local)
#' @return A dataframe with each of the standard PDA fields as a column (plus some other PDA metadata)
#' @noRd
extract_pda_attributes = function(path) {

  text0 = path %>%
    pdftools::pdf_text() %>%
    stringr::str_c(collapse = " ")

  text1 = text0 %>%
    stringr::str_replace_all("\\\n", " ") %>%
    stringr::str_remove_all("\\(|\\)|\\u2022")

  text_event_name = text0 %>%
    stringr::str_split("\\\n") %>%
    unlist() %>%
    .[1:3] %>%
    stringr::str_remove("On.*") %>%
    stringr::str_c(collapse = " ") %>%
    stringr::str_replace_all("\\\n", " ")

  filename_lower = tolower(basename(path))

  ## The outcome is read from FEMA's filename convention
  ## ("PDAReport_AppealDenial-KY.pdf", "PDAReport_Denial-GA.pdf", otherwise an
  ## approval), falling back to the report title. 

  ## An approved appeal is the exception: it is identified from the report body,
  ## because such a report is titled and named exactly like a first-instance
  ## approval and carries a disaster number. The
  ## signal is the word "appealed", which occurs only within a narrative of an
  ## actual appeal ("Governor Hogan appealed the denial") and never as
  ## boilerplate. 
  event_type = dplyr::case_when(
    stringr::str_detect(filename_lower, "appeal[-_ ]?denial")              ~ "appeal_denial",
    stringr::str_detect(text_event_name, "Denial of Appeal|Appeal Denied") ~ "appeal_denial",
    stringr::str_detect(filename_lower, "denial|denied")                   ~ "denial",
    stringr::str_detect(text_event_name, "Denial|Denied")                  ~ "denial",
    stringr::str_detect(text1, stringr::regex("appealed", ignore_case = TRUE)) ~ "appeal_approved",
    TRUE                                                                   ~ "approved")

  ## A report using the tribal layout names its per capita figures without a
  ## "Statewide"/"Territory" qualifier. Deriving this from the document rather
  ## than from title keywords alone catches the reports that use the tribal
  ## wording without saying so in their title.

  uses_tribal_layout =
    stringr::str_detect(text1, stringr::regex("per capita impact", ignore_case = TRUE)) &
    !stringr::str_detect(
      text1, "(Statewide|Territory|Commonwealth|District) per capita impact")

  is_denial = event_type %in% c("denial", "appeal_denial")

  ## an event so severe that no joint PDA was conducted; recorded in the text so
  ## that `pa_preemptive_declaration` can read it back below
  text_pda_preempted = dplyr::if_else(
    !is_denial & stringr::str_detect(text1, "requirement for a joint PDA may be waived"),
    "requirement for a joint PDA may be waived",
    "")

  ## The summary section holds the field values--we drop the boilerplate.
  boilerplate_pattern = dplyr::if_else(
    is_denial,
    "The (P|p)reliminary (D|d)amage (A|a)ssessment PDA process is a mechanism.*|The preliminary damage assessment PDA process.*",
    "The Preliminary Damage Assessment PDA process is a mechanism.*|The preliminary damage assessment PDA process.*")

  text_primary = text1 %>%
    stringr::str_extract("Summary of Damage Assessment.*") %>%
    stringr::str_remove(boilerplate_pattern) %>%
    stringr::str_squish() %>%
    stringr::str_remove_all("\\uf0b7") %>%
    stringr::str_replace_all("(\\:[0-9]|\\: [0-9] )", ":")

  text = stringr::str_c(text_event_name, text_pda_preempted, text_primary, sep = " ")

  ## The disaster number is read from the report body first and from the
  ## filename only where the body states none.
  read_disaster_number = function(x) {
    x %>%
      stringr::str_extract(
        stringr::regex("FEMA[-_ ]?([0-9]{4})[-_ ]?DR", ignore_case = TRUE)) %>%
      stringr::str_extract("[0-9]{4}") }

  disaster_number_from_text = read_disaster_number(text0)
  disaster_number_from_filename = read_disaster_number(basename(path))

  result = tibble::tibble(
      path = path,
      disaster_number = dplyr::coalesce(
        disaster_number_from_text,
        disaster_number_from_filename),
      ## retained so that correct_duplicate_disaster_numbers(), which sees the
      ## whole dataset, can fall back to the filename where a report's printed
      ## number is a typo that collides with another disaster
      disaster_number_filename = disaster_number_from_filename,
      event_type = event_type,
      event_title = text_event_name,
      ## Keywords in the title, combined with the layout test above rather than
      ## relied on alone. The keywords are word-bounded so that, for example,
      ## "Nation" does not match "National".
      ##
      ## "Village", "Traditional Council" and "IRA Council" are included because
      ## an Alaska Native village may name itself with none of the other words --
      ## "Newtok Village" carries none -- and a request from one that also
      ## reports in the state layout would otherwise be read as a state's.
      event_native_flag = dplyr::if_else(
        stringr::str_detect(
          event_title,
          stringr::regex(
            stringr::str_c(
              "\\b(Native|Tribe|Tribes|Tribal|Indians|Nation|Band|Pueblo|",
              "Rancheria|Reservation|Villages?)\\b|",
              "\\b(Traditional|IRA) Council\\b"),
            ignore_case = TRUE)) |
          uses_tribal_layout,
        1, 0),
      ia_requested = dplyr::if_else(
        stringr::str_detect(text, not_requested_pattern("Individual Assistance")),
        0, 1),
      ia_residences_impacted = text %>% extract_value(term1 = "Residences Impacted:", term2 = dash_label("Destroyed")),
      ia_residences_destroyed = text %>% extract_value(term1 = dash_label("Destroyed"), term2 = dash_label("Major Damage")),
      ia_residences_major_damage = text %>% extract_value(term1 = dash_label("Major Damage"), term2 = dash_label("Minor Damage")),
      ia_residences_minor_damage = text %>% extract_value(term1 = dash_label("Minor Damage"), term2 = dash_label("Affected")),
      ia_residences_affected = text %>% extract_value(term1 = dash_label("Affected"), term2 = "Percentage of insured residences:"),
      ia_residences_insured_total_percent = text %>% extract_value(
        term1 = "Percentage of insured residences:",
        term2 = "Percentage of low income households|Percentage of poverty households|Percentage of elderly households|Flood"),
      ia_residences_insured_flood_percent = text %>%
        stringr::str_extract("[0-9]{1,3}(\\.[0-9]{1,2})?\\s*\\%\\s*Flood") %>%
        stringr::str_remove("Flood") %>%
        stringr::str_remove("\\%") %>%
        stringr::str_squish(),
      ia_households_poverty_percent = text %>% extract_value(term1 = "Percentage of poverty households:|Percentage of low income households:", term2 = "Percentage of ownership households:|Percentage of elderly households:"),
      ia_households_owner_percent = text %>% extract_value(
        term1 = "Percentage of ownership households:",
        term2 = "Population receiving other government|Pre-Disaster Unemployment|Total Individual Assistance cost estimate|Disability:"),
      ia_population_other_government_assistance_percent = text %>% extract_value(
        term1 = "Population receiving other government\\s+assistance such as SSI and SNAP:",
        term2 = "Pre-Disaster Unemployment|Age 65 and older:|Total Individual Assistance cost estimate"),
      ia_pre_disaster_unemployment_percent = text %>% extract_value(term1 = "Pre-Disaster Unemployment", term2 = "Age 65 and older:"),
      ia_65plus_percent = text %>% extract_value(term1 = "Age 65 and older:", term2 = "Age 18 and under:"),
      ia_18below_percent = text %>% extract_value(term1 = "Age 18 and under:", term2 = "Disability:"),
      ia_disability_percent = text %>% extract_value(term1 = "Disability:", term2 = "IHP Cost to Capacity ICC Ratio"),
      ia_ihp_cost_to_capacity_ratio = text %>% extract_value(term1 = "IHP Cost to Capacity ICC Ratio:", term2 = "Total Individual Assistance cost estimate"),
      ia_cost_estimate_total = text %>% extract_value(term1 = "Total Individual Assistance cost estimate", term2 = "Primary Impact"),
      pa_requested = dplyr::if_else(
        stringr::str_detect(text, not_requested_pattern("Public Assistance")),
        0, 1),
      pa_preemptive_declaration = dplyr::if_else(stringr::str_detect(text, "requirement for a joint PDA may be waived"), 1, 0),
      pa_primary_impact = text %>% extract_value(term1 = "Primary Impact", term2 = "Total Public Assistance cost estimate:"),
      ## Tribal reports label this "Per capita impact:" rather than "Statewide
      ## per capita impact:".
      pa_cost_estimate_total = text %>% extract_value(
        term1 = "Total Public Assistance cost estimate:",
        term2 = "(Statewide|Territory|Commonwealth|District) per capita impact:|[Pp]er capita impact:"),
      pa_per_capita_impact_statewide = text %>% extract_value(term1 = "(Statewide|Territory|Commonwealth) per capita impact", term2 = "(Statewide|Territory|Commonwealth|District) per capita impact indicator"),
      pa_per_capita_impact_indicator_statewide = text %>% extract_value(term1 = "(Statewide|Territory|Commonwealth) per capita impact indicator", term2 = "(Countywide per capita impact|\\$[0-9]{1}\\.[0-9]{1,2} [0-9]{1})"),
      pa_per_capita_impact_countywide = text %>% extract_value(term1 = "Countywide per capita impact", term2 = "Countywide per capita impact indicator"),
      pa_per_capita_impact_indicator_countywide = text %>%
        extract_value(
          term1 = "Countywide per capita impact indicator:",
          term2 = "The [Pp]reliminary [Dd]amage|Footnote|$"),
      text = text1) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.character) & -dplyr::any_of(non_extracted_columns()),
        .fns = remove_footnote_marker),
      ia_cost_estimate_total = stringr::str_remove(ia_cost_estimate_total, "Public Assistance"),
      pa_per_capita_impact_indicator_statewide = first_token(pa_per_capita_impact_indicator_statewide),
      ## in the case of American Samoa, this is the last value
      pa_per_capita_impact_indicator_statewide = dplyr::if_else(
        nchar(pa_per_capita_impact_indicator_statewide) < 3,
        text %>%
          extract_value(term1 = "Statewide per capita impact indicator", term2 = "$") %>%
          first_token(),
        pa_per_capita_impact_indicator_statewide),
      pa_per_capita_impact_indicator_countywide = pa_per_capita_impact_indicator_countywide %>%
        stringr::str_remove("\\s+[0-9]{1,2}\\s*$") %>%
        stringr::str_trim(),
      pa_per_capita_impact_indicator_countywide = dplyr::if_else(
        stringr::str_detect(
          pa_per_capita_impact_indicator_countywide,
          stringr::str_c("^", dash_class())),
        NA_character_,
        stringr::str_extract(
          pa_per_capita_impact_indicator_countywide, "[0-9]+\\.?[0-9]*")),
      dplyr::across(
        .cols = -dplyr::all_of(non_extracted_columns()),
        .fns = ~ stringr::str_remove_all(.x, "\\%|\\:|\\$|\\,") %>% stringr::str_squish()))

  ## Tribes have differently structured PDA report fields: the per capita labels
  ## carry no "Statewide" qualifier, so the patterns above match nothing and the
  ## three fields are re-read here.
  ##
  ## These read the local `text` -- the report title and its summary section --
  ## rather than the `text` column.
  if (result$event_native_flag == 1) {
    tribal_cost_estimate = extract_value(
      text,
      term1 = "Total Public Assistance cost estimate:",
      term2 = "[Pp]er capita impact:")
    tribal_impact = extract_value(
      text,
      term1 = "[Pp]er capita impact:",
      term2 = "[Pp]er capita impact indicator:")
    tribal_indicator = extract_value(
      text,
      term1 = "[Pp]er capita impact indicator:",
      term2 = "Countywide per capita impact|The [Pp]reliminary|$")
    result = result %>%
      dplyr::mutate(
        pa_cost_estimate_total = remove_footnote_marker(tribal_cost_estimate),
        pa_per_capita_impact_statewide = remove_footnote_marker(tribal_impact),
        pa_per_capita_impact_indicator_statewide = remove_footnote_marker(tribal_indicator)) }

  months = c(
    "January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December") %>%
    stringr::str_c(collapse = "|")
  date_match_string = stringr::str_c("Denied (on |)(", months, ") [0-9]{1,2},? [0-9]{4}")
  first_date_match_string = stringr::str_c("(", months, ") [0-9]{1,2},? [0-9]{4}")

  ## A value beginning with FEMA's dash placeholder is a field the report left
  ## blank, and the whole value is discarded rather than having the dash
  ## stripped off the front. Discarding the whole
  ## value is safe because no real value ever follows the placeholder: every
  ## dash-led value is either empty or a one- or two-digit
  ## footnote marker.
  clean_extracted_value = function(value) {
    value1 = stringr::str_squish(value)
    value2 = dplyr::if_else(
      stringr::str_detect(value1, stringr::str_c("^", dash_class(), "(\\s|$)")),
      NA_character_,
      value1)
    value3 = stringr::str_remove_all(value2, "\\$|\\:|\\,")
    dplyr::if_else(stringr::str_detect(value3, "^N.A$"), NA_character_, value3) }

  summarise_countywide = function(matches, summary_function) {
    purrr::map_dbl(
      matches,
      ~ if (length(.x) == 0 || all(is.na(.x))) {
          NA_real_
        } else {
          summary_function(as.numeric(.x), na.rm = TRUE) }) }

  result2 = result %>%
    dplyr::mutate(
      dplyr::across(-dplyr::all_of(non_extracted_columns()), clean_extracted_value),
      pa_per_capita_impact_countywide_1 = pa_per_capita_impact_countywide %>%
        stringr::str_extract_all("[0-9]{1,4}\\.[0-9]{1,3}"),
      pa_per_capita_impact_countywide_max =
        summarise_countywide(pa_per_capita_impact_countywide_1, max),
      pa_per_capita_impact_countywide_min =
        summarise_countywide(pa_per_capita_impact_countywide_1, min),
      ## The determination date is taken from the report title where it states
      ## one, then from an explicit "Denied on <date>" statement, and finally
      ## from the first date printed in the document. That last source carries
      ## most of the coverage: well over half of reports name no month anywhere
      ## in their title, and for those the date at the head of the document is
      ## the determination date.
      event_date_determined = event_title %>% date_string_to_date,
      event_date_determined = dplyr::if_else(
        is.na(event_date_determined),
        stringr::str_extract(text, date_match_string) %>% stringr::str_remove("Denied (on |)") %>% date_string_to_date,
        event_date_determined),
      event_date_determined = dplyr::if_else(
        is.na(event_date_determined),
        stringr::str_extract(text, first_date_match_string) %>% date_string_to_date,
        event_date_determined),
      dplyr::across(
        .cols = -dplyr::all_of(c(
          non_extracted_columns(),
          "event_date_determined", "pa_per_capita_impact_countywide", "pa_primary_impact")),
        .fns = ~ first_token(.x) %>% as.numeric)) %>%
    dplyr::select(-pa_per_capita_impact_countywide_1) %>%
    dplyr::select(disaster_number, dplyr::matches("^event"), dplyr::matches("^pa"), dplyr::everything())

  return(result2)
}

#' Columns holding a dollar total, and a floor that is greater than possible footnote numbering
#'
#' This catches potential footmarks that have been initially captured as meaningful field values
#'
#' @return A named list with `columns` and `floor`.
#' @noRd
pda_dollar_columns = function() {
  list(
    columns = c("pa_cost_estimate_total", "ia_cost_estimate_total"),
    floor = 50)
}

#' Plausible ranges for FEMA's statutory per capita thresholds
#'
#' These two fields are not estimates but published statutory dollar
#' thresholds, so their values are known in advance and a value outside the
#' range is a mis-extraction rather than an unusual disaster.
#'
#' @return A named list of two-element numeric vectors.
#' @noRd
pda_indicator_ranges = function() {
  list(
    pa_per_capita_impact_indicator_statewide = c(1.00, 2.50),
    pa_per_capita_impact_indicator_countywide = c(3.00, 5.00))
}

#' Columns holding a measured value rather than a flag or an identifier
#'
#' Counts, dollars, percentages, and ratios. The 0/1 flags and the identity
#' columns are excluded because neither the quality checks nor the requested-flag
#' tie-break describe them.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @return A character vector of column names.
#' @noRd
program_measure_columns = function(pda_df) {
  flag_columns = c(
    "event_native_flag", "ia_requested", "pa_requested",
    "pa_preemptive_declaration")

  names(pda_df) %>%
    purrr::discard(~ .x %in% c(non_extracted_columns(), flag_columns, "parser_version")) %>%
    purrr::keep(~ is.numeric(pda_df[[.x]]))
}

#' Whether a report states any measurement for one assistance program
#'
#' Used both to settle a report that contradicts itself and to check for values
#' recorded against a program the report says was not requested, so that the two
#' ask exactly the same question of the data.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @param prefix A column-name prefix identifying the program (`"^ia_"` or
#'   `"^pa_"`).
#' @return A logical vector with one element per row of `pda_df`, `FALSE`
#'   throughout where the program has no measurement columns at all.
#' @noRd
states_any_program_value = function(pda_df, prefix) {
  columns = program_measure_columns(pda_df) %>%
    purrr::keep(~ stringr::str_detect(.x, prefix)) %>%
    purrr::discard(~ .x %in% names(pda_indicator_ranges()))

  if (length(columns) == 0) { return(rep(FALSE, nrow(pda_df))) }

  ## A value of exactly zero is treated as stating nothing: a report whose
  ## request did not cover the program sometimes prints "$0" or "0" against a
  ## field (FEMA-DR-WI "Lack of Snow" prints an ICC ratio of 0 and a $0 total
  ## estimate among all-dash placeholders), and a report that measured a
  ## program never reports every figure as zero.
  columns %>%
    purrr::map(~ !is.na(pda_df[[.x]]) & pda_df[[.x]] != 0) %>%
    purrr::reduce(`|`)
}

#' Settle whether a program was requested when the report contradicts itself
#'
#' `ia_requested` and `pa_requested` are read from the summary section, which
#' states "Individual Assistance - Not requested" or the equivalent. A handful of
#' reports say that and then print a full set of values for the program anyway. 
#' Where that happens the report's opening narrative
#' decides it: every report begins with a sentence of the form "The Governor
#' requested a declaration for Public Assistance for 17 counties and Hazard
#' Mitigation for the entire Commonwealth", and it is written independently of
#' the summary table. 
#'
#' A summary reading "not requested in appeal" is treated as not requested. An
#' appealed disaster is finally decided on the appeal, so the appeal's scope is
#' the one that matters.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @return `pda_df` with the two flags settled and a `requested_from_narrative`
#'   column naming any program whose flag the narrative changed.
#' @noRd
resolve_requested_flags = function(pda_df) {

  required = c("text", "ia_requested", "pa_requested")
  if (!all(required %in% names(pda_df))) { return(pda_df) }

  resolved = pda_df %>%
    dplyr::mutate(
      ## Every sentence about the request, taken from the narrative that precedes
      ## the summary tables, or from the whole report where that header does not
      ## appear. Matching whole sentences rather than one fixed
      ## opening phrase covers the reports that write "The Governor requested
      ## Public Assistance program for five areas and Hazard Mitigation
      ## statewide", which names no "declaration for" at all.
      request_sentence = stringr::str_extract(
          text, "^.*?(?=Summary of Damage Assessment)") %>%
        dplyr::coalesce(text) %>%
        dplyr::coalesce("") %>%
        stringr::str_extract_all("[^.]*\\brequest(ed|ing|s)?\\b[^.]*\\.") %>%
        purrr::map_chr(stringr::str_c, collapse = " ") %>%
        dplyr::na_if(""),
      ## A request for any component program of Individual Assistance counts as
      ## a request for Individual Assistance.
      narrative_requests_ia = stringr::str_detect(
        dplyr::coalesce(request_sentence, ""),
        stringr::regex(
          stringr::str_c(
            "Individual Assistance|Individuals and Households|",
            "Crisis Counseling|Disaster Unemployment Assistance|\\bDUA\\b|",
            "Disaster Legal Services|Disaster Case Management"),
          ignore_case = TRUE)),
      narrative_requests_pa = stringr::str_detect(
        dplyr::coalesce(request_sentence, ""),
        stringr::regex("Public Assistance", ignore_case = TRUE)),
      ## A narrative that names no recognized program was not read successfully,
      ## and is not evidence either way. Small Business Administration loans
      ## belong to the SBA rather than to FEMA's programs, but a request can
      ## consist of nothing else, so naming one is a successfully read narrative
      ## that requests neither Individual nor Public Assistance.
      narrative_is_readable = stringr::str_detect(
        dplyr::coalesce(request_sentence, ""),
        stringr::regex(
          stringr::str_c(
            "Individual Assistance|Individuals and Households|",
            "Public Assistance|Hazard Mitigation|",
            "Crisis Counseling|Disaster Unemployment Assistance|\\bDUA\\b|",
            "Disaster Legal Services|Disaster Case Management|",
            "Small Business Administration|\\bSBA\\b|",
            "Economic Injury (Disaster )?Loan"),
          ignore_case = TRUE)),
      ia_from_narrative =
        !is.na(ia_requested) & ia_requested == 0 &
        states_any_program_value(pda_df, "^ia_") & narrative_requests_ia,
      pa_from_narrative =
        !is.na(pa_requested) & pa_requested == 0 &
        states_any_program_value(pda_df, "^pa_") & narrative_requests_pa,
      ia_not_from_narrative =
        !is.na(ia_requested) & ia_requested == 1 & narrative_is_readable &
        !states_any_program_value(pda_df, "^ia_") & !narrative_requests_ia,
      pa_not_from_narrative =
        !is.na(pa_requested) & pa_requested == 1 & narrative_is_readable &
        !states_any_program_value(pda_df, "^pa_") & !narrative_requests_pa,
      ia_requested = dplyr::case_when(
        ia_from_narrative ~ 1,
        ia_not_from_narrative ~ 0,
        TRUE ~ ia_requested),
      pa_requested = dplyr::case_when(
        pa_from_narrative ~ 1,
        pa_not_from_narrative ~ 0,
        TRUE ~ pa_requested),
      ia_settled = ia_from_narrative | ia_not_from_narrative,
      pa_settled = pa_from_narrative | pa_not_from_narrative,
      requested_from_narrative = dplyr::case_when(
        ia_settled & pa_settled ~ "ia; pa",
        ia_settled ~ "ia",
        pa_settled ~ "pa",
        TRUE ~ NA_character_))

  resolved %>%
    dplyr::select(
      -request_sentence,
      -narrative_requests_ia, -narrative_requests_pa,
      -narrative_is_readable,
      -ia_from_narrative, -pa_from_narrative,
      -ia_not_from_narrative, -pa_not_from_narrative,
      -ia_settled, -pa_settled)
}

#' Run quality checks over the assembled PDA dataset
#'
#' Every field in this dataset is read out of an unstructured PDF by pattern
#' matching, so a change in how FEMA lays out a report does not raise an error:
#' it silently produces a wrong value or an empty column. These checks look for
#' the shapes that failure takes -- a value that cannot be what the field
#' measures, a footnote marker standing in for a dollar total, a column that
#' matched nothing at all -- and record them, so that a broken pattern is
#' visible when the dataset is built rather than after it has been published.
#'
#' Values are left as parsed so that a problem can be
#' investigated against the source report, rather than being replaced by `NA`
#' and losing the evidence. Each record's problems are written to a `warnings`
#' column (`NA` where none were found); dataset-wide problems, which belong to
#' no single record, are attached as the `"dataset_issues"` attribute. Nothing
#' is printed here -- `get_preliminary_damage_assessments()` reports the counts
#' in a single consolidated warning.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @return `pda_df` with a `warnings` column added, carrying the
#'   `"dataset_issues"` attribute.
#' @noRd
check_pda_quality = function(pda_df) {

  ## a value this many times the median of a column's non-zero values is
  ## reported as an outlier.
  outlier_multiple = 100

  ## a column emptier than this among the reports that should state it is
  ## treated as a broken extraction pattern rather than as sparse reporting.
  ## Set high deliberately: several fields are genuinely absent from 80-90% of
  ## reports because older report layouts do not contain them at all.
  empty_column_threshold = 0.95

  ## one character vector of problems per record
  row_issues = rep(list(character(0)), nrow(pda_df))

  dataset_issues = character(0)
  note_dataset = function(...) {
    dataset_issues <<- c(dataset_issues, stringr::str_c(...)) }

  ## appends a problem description to each flagged record. `texts` is either
  ## one string for every record or a vector aligned to the rows of `pda_df`,
  ## so a description can quote the record's own offending value.
  flag_rows = function(is_problem, texts) {
    rows = which(dplyr::coalesce(is_problem, FALSE))
    if (length(rows) == 0) { return(invisible(NULL)) }
    if (length(texts) == 1) { texts = rep(texts, nrow(pda_df)) }
    for (row in rows) {
      row_issues[[row]] <<- c(row_issues[[row]], texts[row]) } }

  has = function(column) { column %in% names(pda_df) && is.numeric(pda_df[[column]]) }

  measure_columns = program_measure_columns(pda_df)

  percent_columns = measure_columns %>% purrr::keep(~ stringr::str_detect(.x, "percent$"))

  ## values that can be compared against a threshold at all
  is_usable = function(x) { !is.na(x) & is.finite(x) }

  ## 1. Values that are not finite numbers.
  purrr::walk(measure_columns, function(column) {
    x = pda_df[[column]]
    flag_rows(
      !is.na(x) & !is.finite(x),
      stringr::str_c(column, " is Inf or NaN rather than a number")) })

  ## 2. Negative values.
  purrr::walk(measure_columns, function(column) {
    x = pda_df[[column]]
    flag_rows(
      is_usable(x) & x < 0,
      stringr::str_c(
        column, " is negative (", x, "); this field cannot hold a value ",
        "below zero")) })

  ## 3. Percentages outside 0-100. drop_impossible_percentages() sets these to
  ## NA when the dataset is generated, so anything found here came from a
  ## cached file written before that step existed.
  purrr::walk(percent_columns, function(column) {
    x = pda_df[[column]]
    flag_rows(
      is_usable(x) & (x < 0 | x > 100),
      stringr::str_c(
        column, " (", x, ") falls outside 0-100; the text extracted was not ",
        "the intended figure")) })

  ## 4. Dollar totals small enough to be a footnote marker rather than a cost.
  dollars = pda_dollar_columns()
  purrr::walk(dollars$columns, function(column) {
    if (!has(column)) { return(invisible(NULL)) }
    x = pda_df[[column]]
    flag_rows(
      is_usable(x) & x > 0 & x < dollars$floor,
      stringr::str_c(
        column, " (", x, ") is under $", dollars$floor, ", almost certainly ",
        "the label's footnote number rather than a cost estimate")) })

  ## 5. Statutory per capita thresholds outside their published range, which
  ## most often means the statewide and countywide values were swapped.
  ##
  ## The statewide threshold is exempt. `impute_statewide_indicator()` replaces
  ## an out-of-range value there with the one the rest of its fiscal year states,
  ## so by the time the data is returned the problem has been corrected and a
  ## warning would describe a value the caller never sees. The countywide
  ## threshold is still checked, because nothing corrects it.
  checked_ranges = pda_indicator_ranges() %>%
    purrr::discard_at("pa_per_capita_impact_indicator_statewide")

  purrr::iwalk(checked_ranges, function(range, column) {
    if (!has(column)) { return(invisible(NULL)) }
    x = pda_df[[column]]
    flag_rows(
      is_usable(x) & (x < range[1] | x > range[2]),
      stringr::str_c(
        column, " (", x, ") falls outside the published range of ", range[1],
        "-", range[2], " dollars; this field is a statutory threshold, so a ",
        "value outside that range is a mis-extraction -- commonly the other ",
        "geography's threshold")) })

  ## 6. Damage categories that do not add up. FEMA reports the four severity
  ## categories as a breakdown of the impacted total, so their sum cannot
  ## exceed it. A 5% tolerance absorbs reports that round each category
  ## separately.
  damage_components = c(
    "ia_residences_destroyed", "ia_residences_major_damage",
    "ia_residences_minor_damage", "ia_residences_affected")

  if (has("ia_residences_impacted") && all(purrr::map_lgl(damage_components, has))) {
    component_sum = damage_components %>%
      purrr::map(~ dplyr::coalesce(pda_df[[.x]], 0)) %>%
      purrr::reduce(`+`)

    ## only where every component was extracted; a partial sum is legitimately
    ## smaller than the total and says nothing about correctness
    is_complete = damage_components %>%
      purrr::map(~ !is.na(pda_df[[.x]])) %>%
      purrr::reduce(`&`)

    exceeds_total =
      is_complete &
      !is.na(pda_df$ia_residences_impacted) &
      is.finite(component_sum) &
      component_sum > pda_df$ia_residences_impacted * 1.05

    flag_rows(
      exceeds_total,
      stringr::str_c(
        "the four damage categories sum to more than the stated total of ",
        "impacted residences; at least one of the five figures was read from ",
        "the wrong field")) }

  ## 7. Individual and Public Assistance values recorded for a program the
  ## report states was not requested.
  purrr::iwalk(
    list(ia_requested = "^ia_", pa_requested = "^pa_"),
    function(prefix, flag) {
      if (!has(flag)) { return(invisible(NULL)) }

      flag_rows(
        !is.na(pda_df[[flag]]) & pda_df[[flag]] == 0 &
          states_any_program_value(pda_df, prefix),
        stringr::str_c(
          "the report carries values for a program that ", flag, " records ",
          "as not requested; either the flag or the values are wrong")) })

  ## 8. Demographic shares of exactly zero. No populated jurisdiction has no
  ## residents over 65, no children, nobody in poverty, or no unemployment, so
  ## a zero here is not a credible measurement.
  ## Insurance coverage shares are excluded because a report can genuinely
  ## state that none of the impacted residences carried flood insurance.
  zero_impossible_columns = c(
    "ia_households_poverty_percent", "ia_households_owner_percent",
    "ia_population_other_government_assistance_percent",
    "ia_pre_disaster_unemployment_percent", "ia_65plus_percent",
    "ia_18below_percent", "ia_disability_percent") %>%
    purrr::keep(has)

  purrr::walk(zero_impossible_columns, function(column) {
    x = pda_df[[column]]
    flag_rows(
      !is.na(x) & x == 0,
      stringr::str_c(
        column, " is exactly zero, which is not a credible value for this ",
        "measure; either the field was blank and a footnote number was read ",
        "in its place, or the report prints a zero that is itself wrong")) })

  ## 9. Single values far above the rest of their column.
  purrr::walk(measure_columns, function(column) {
    x = pda_df[[column]]
    usable_values = x[is_usable(x) & x > 0]
    if (length(usable_values) < 30) { return(invisible(NULL)) }
    flag_rows(
      is_usable(x) & x > stats::median(usable_values) * outlier_multiple,
      stringr::str_c(
        column, " (", x, ") exceeds ", outlier_multiple, " times the ",
        "column's median of ", stats::median(usable_values), "; check it ",
        "against the source report")) })

  ## 10. Disaster numbers that are not four digits.
  if ("disaster_number" %in% names(pda_df)) {
    numbers = as.character(pda_df$disaster_number)
    flag_rows(
      !is.na(numbers) & !stringr::str_detect(numbers, "^[0-9]{4}$"),
      stringr::str_c("disaster number ", numbers, " is not four digits"))

    if ("event_type" %in% names(pda_df)) {
      flag_rows(
        stringr::str_detect(pda_df$event_type, "approv") & is.na(numbers),
        stringr::str_c(
          "the report has no disaster number, though an approved request is ",
          "always assigned one")) }

    ## a disaster number mapping to more than one approved report means at
    ## least one of them carries the wrong number
    if ("event_type" %in% names(pda_df)) {
      approved_numbers = numbers[
        stringr::str_detect(pda_df$event_type, "approv") & !is.na(numbers)]
      shared = unique(approved_numbers[duplicated(approved_numbers)])
      flag_rows(
        stringr::str_detect(pda_df$event_type, "approv") &
          !is.na(numbers) & numbers %in% shared,
        stringr::str_c(
          "disaster number ", numbers, " maps to more than one approved PDA ",
          "report and is likely incorrect on at least one of them")) } }

  ## 11. Determination dates outside the period the archive covers. FEMA's
  ## earliest published report is from 2007, and a date in the future is a
  ## misread year.
  if ("event_date_determined" %in% names(pda_df)) {
    dates = suppressWarnings(as.Date(pda_df$event_date_determined))
    flag_rows(
      !is.na(dates) & (dates < as.Date("2000-01-01") | dates > Sys.Date() + 1),
      stringr::str_c(
        "the determination date (", as.character(dates), ") falls outside ",
        "2000 to today, so its year was misread")) }

  ## 12. The same source PDF parsed into more than one row.
  if ("path" %in% names(pda_df)) {
    flag_rows(
      pda_df$path %in% unique(pda_df$path[duplicated(pda_df$path)]),
      "this source PDF parsed into more than one row") }

  ## 13. A report that disagreed with itself about whether a program was
  ## requested, settled from its opening narrative by
  ## `resolve_requested_flags()`.
  if ("requested_from_narrative" %in% names(pda_df)) {
    flag_rows(
      !is.na(pda_df$requested_from_narrative),
      stringr::str_c(
        "the report disagrees with itself about whether ",
        pda_df$requested_from_narrative, " was requested; the flag was ",
        "settled from the opening narrative")) }

  ## 14. Columns that are empty, or nearly so, among the reports that should
  ## state them. This belongs to the dataset, not to any one record.
  relevant_rows = function(prefix) {
    flag = stringr::str_c(stringr::str_remove(prefix, "\\^"), "requested")
    if (!has(flag) || !"event_type" %in% names(pda_df)) { return(rep(TRUE, nrow(pda_df))) }
    !is.na(pda_df[[flag]]) &
      pda_df[[flag]] == 1 &
      stringr::str_detect(pda_df$event_type, "approv") }

  missingness = c("^ia_", "^pa_") %>%
    purrr::map_dfr(function(prefix) {
      rows = relevant_rows(prefix)
      if (sum(rows) == 0) { return(tibble::tibble()) }
      measure_columns %>%
        purrr::keep(~ stringr::str_detect(.x, prefix)) %>%
        purrr::map_dfr(~ tibble::tibble(
          column = .x,
          n_relevant = sum(rows),
          share_missing = mean(is.na(pda_df[[.x]][rows])))) })

  if (nrow(missingness) > 0) {
    empty_columns = missingness %>%
      dplyr::filter(share_missing >= empty_column_threshold)

    if (nrow(empty_columns) > 0) {
      note_dataset(
        nrow(empty_columns), " column(s) are at least ",
        round(empty_column_threshold * 100), "% missing among the reports that ",
        "requested the program and were approved, which usually means the ",
        "pattern that extracts them no longer matches the documents: ",
        stringr::str_c(
          empty_columns$column, " (",
          round(empty_columns$share_missing * 100, 1), "% of ",
          empty_columns$n_relevant, ")",
          collapse = "; "),
        ".") } }

  checked = pda_df %>%
    dplyr::mutate(warnings = purrr::map_chr(
      row_issues,
      ~ if (length(.x) == 0) { NA_character_ } else {
        stringr::str_c(.x, collapse = "; ") }))
  attr(checked, "dataset_issues") = dataset_issues
  checked
}

#' Correct disaster numbers shared by multiple, genuinely different PDA reports
#'
#' A handful of PDAs carry a typo'd or absent disaster number, which surfaces as
#' two different reports sharing one `disaster_number`.
#'
#' @param pda_df A dataframe of extracted PDA records (must contain `text` and
#'   `disaster_number`).
#' @return `pda_df` with corrected `disaster_number` values (coerced to character).
#' @noRd
correct_duplicate_disaster_numbers = function(pda_df) {

  corrected = pda_df %>%
    ## coerce so the if_else() below is type-stable regardless of how a cached CSV
    ## parsed the column (readr may guess double; the extracted value is character)
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("disaster_number", "disaster_number_filename")),
        as.character)) %>%
    dplyr::add_count(disaster_number, name = "disaster_number_count") %>%
    dplyr::mutate(
      disaster_number_from_text = stringr::str_extract(text, "FEMA-[0-9]{4}") %>%
        stringr::str_remove("FEMA-"),
      disaster_number = dplyr::if_else(
        disaster_number_count > 1 &
          !is.na(disaster_number) &
          !is.na(disaster_number_from_text),
        disaster_number_from_text,
        disaster_number)) %>%
    dplyr::select(-disaster_number_count, -disaster_number_from_text)

  ## The number printed in a report body is usually authoritative, but it is not
  ## always: one report (FEMA-4599-DR, Oregon) prints Kentucky's FEMA-4595-DR,
  ## evidently copied from another document, which lands two unrelated disasters
  ## on one number. Where a duplicated number disagrees with the number in that
  ## report's own filename, and the filename's number is claimed by no other
  ## report, the filename is preferred.
  if (!"disaster_number_filename" %in% names(corrected)) { return(corrected) }

  claimed = corrected$disaster_number[!is.na(corrected$disaster_number)]

  corrected %>%
    dplyr::add_count(disaster_number, name = "disaster_number_count") %>%
    dplyr::mutate(
      disaster_number = dplyr::if_else(
        disaster_number_count > 1 &
          !is.na(disaster_number_filename) &
          disaster_number_filename != disaster_number &
          !disaster_number_filename %in% claimed,
        disaster_number_filename,
        disaster_number)) %>%
    dplyr::select(-disaster_number_count)
}

#' A regular expression matching any state, territory, or District name
#'
#' @return A length-one character regular expression.
#' @noRd
state_name_pattern = function() {
  names_longest_first = tidycensus::fips_codes %>%
    dplyr::distinct(state_name) %>%
    dplyr::arrange(dplyr::desc(stringr::str_length(state_name))) %>%
    dplyr::pull(state_name) %>%
    stringr::str_c(collapse = "|")

  stringr::str_c("\\b(?:", names_longest_first, ")\\b")
}

#' Keyword patterns defining the shared set of hazard categories
#'
#' PDA titles and FEMA's own denial records both describe the hazard in free
#' text, and they do not use the same words for the same thing ("Severe Storms
#' and Flooding" against "Flood, Severe Storm(s)"). Each pattern below maps the
#' wordings observed in both sources onto one category name, so that a hazard
#' set derived from a PDA can be compared against one derived from a FEMA
#' record. The category names are the values written into the `hazards` and
#' `denial_hazards` columns.
#'
#' @return A named character vector: names are category labels, values are
#'   case-insensitive regular expressions.
#' @noRd
hazard_category_patterns = function() {
  c(
    "flooding" = "flood|flash flood|high water",
    "severe storm" = "severe storm|thunderstorm|straight-line wind|straight line wind|high wind|\\bwinds?\\b|hail|rainstorm|severe weather",
    "tornado" = "tornado",
    "hurricane" = "hurricane|typhoon",
    ## FEMA abbreviates a named tropical system to "TS Cristobal" in its own
    ## records, which no spelled-out pattern reaches
    "tropical storm" = "tropical storm|tropical depression|tropical cyclone|\\bts\\b",
    "coastal storm" = "coastal storm|storm surge|nor'?easter",
    "winter storm" = "winter storm|winter weather|snow|ice storm|blizzard|freez|extreme cold",
    "wildfire" = "wildfire|\\bfires?\\b",
    "landslide" = "landslide|mudslide|mudflow|debris flow",
    "earthquake" = "earthquake|seismic",
    "tsunami" = "tsunami",
    "volcanic eruption" = "volcan|lava",
    "drought" = "drought",
    "extreme heat" = "extreme heat|heat wave",
    "dam or levee failure" = "dam failure|levee failure|dam or levee",
    "pandemic" = "covid|coronavirus|pandemic|epidemic")
}

#' Assign hazard categories to free-text event descriptions
#'
#' @param descriptions A character vector of event titles or incident-type text.
#' @return A character vector the same length as `descriptions`, each element a
#'   semicolon-separated set of matched category names, or `NA` where no
#'   keyword matched.
#' @noRd
extract_hazard_categories = function(descriptions) {
  patterns = hazard_category_patterns()

  purrr::map_chr(
    stringr::str_to_lower(dplyr::coalesce(descriptions, "")),
    function(description) {
      categories_detected = names(patterns)[stringr::str_detect(description, patterns)]
      if (length(categories_detected) == 0) {
        NA_character_
      } else {
        stringr::str_c(categories_detected, collapse = "; ") } })
}

#' Hazard categories that name the same event in different words
#'
#' @return A named character vector mapping a category to its group.
#' @noRd
hazard_equivalent_categories = function() {
  c("tropical storm" = "tropical or coastal storm",
    "coastal storm" = "tropical or coastal storm")
}

#' Count the hazard categories two descriptions share
#'
#' @param categories_one,categories_two Character vectors of semicolon-separated
#'   category sets, as produced by `extract_hazard_categories()`.
#' @return An integer vector of shared-category counts, `NA` where either side
#'   has no assigned category.
#' @noRd
shared_hazard_category_count = function(categories_one, categories_two) {
  equivalents = hazard_equivalent_categories()
  ## categories that name the same event are compared under one name, so the
  ## count is of distinct events described in common rather than of words shared
  as_groups = function(categories) {
    unique(dplyr::coalesce(unname(equivalents[categories]), categories)) }

  purrr::map2_int(
    stringr::str_split(categories_one, "; "),
    stringr::str_split(categories_two, "; "),
    function(set_one, set_two) {
      if (any(is.na(set_one)) || any(is.na(set_two))) { return(NA_integer_) }
      length(intersect(as_groups(set_one), as_groups(set_two))) })
}

#' Add the state name, cleaned event title, and hazard categories to PDA records
#'
#' The state is the first state, territory, or District name appearing anywhere
#' in the report text; FEMA does not print it as a labelled field. The event
#' title as extracted is the report's heading.
#' The hazard categories are then read from that cleaned description.
#'
#' @param pda_df A dataframe of extracted PDA records (must contain `text` and
#'   `event_title`).
#' @return `pda_df` with `state_name` and `hazards` added and `event_title`
#'   cleaned.
#' @noRd
add_pda_derived_columns = function(pda_df) {
  state_pattern = state_name_pattern()
  dashes = "-\\u2010-\\u2015\\u2212"

  pda_df %>%
    dplyr::mutate(
      state_name = dplyr::coalesce(
        stringr::str_extract(event_title, state_pattern),
        stringr::str_extract(text, state_pattern)),
      event_title = event_title %>%
        stringr::str_remove_all(stringr::regex(
          "Preliminary Damage Assessments?( Reports?)?", ignore_case = TRUE)) %>%
        stringr::str_remove_all(stringr::regex("\\bPDA Reports?\\b", ignore_case = TRUE)) %>%
        stringr::str_remove_all(stringr::str_c("\\((", state_pattern, ")\\)")) %>%
        stringr::str_remove("^[^A-Za-z0-9]+") %>%
        stringr::str_remove(stringr::regex(
          "^\\s*(the\\s+)?(State|Commonwealth|Territory|District)\\s+of\\s+", ignore_case = TRUE)) %>%
        stringr::str_remove(stringr::str_c(
          "^\\s*(", state_pattern, ")",
          "(?!\\s+(County|City|Parish|Borough|Township)\\b)",
          "\\s*([", dashes, ",]|\\b)")) %>%
        stringr::str_remove(stringr::str_c("[", dashes, ",]\\s*(", state_pattern, ")\\s*$")) %>%
        ## the declaration number and the decision and its date are recorded in
        ## their own columns (`disaster_number`, `event_type`,
        ## `event_date_determined`), so the copies the heading carries are removed
        stringr::str_remove_all(stringr::regex(
          "FEMA[-\\s]?[0-9]{4}[-\\s]*DR", ignore_case = TRUE)) %>%
        stringr::str_remove_all(stringr::regex(
          "(Denial|Denied|Declared|Approved)\\s*(of Appeal|on)?\\s*(January|February|March|April|May|June|July|August|September|October|November|December)?\\s*[0-9]{0,2},?\\s*[0-9]{0,4}",
          ignore_case = TRUE)) %>%
        ## collapse the separators left behind by the removals above
        stringr::str_replace_all(stringr::str_c("(\\s*[", dashes, "]\\s*){2,}"), " - ") %>%
        stringr::str_remove("^[^A-Za-z0-9]+") %>%
        stringr::str_remove(stringr::str_c("[\\s", dashes, ",;:.]+$")) %>%
        stringr::str_squish(),
      hazards = extract_hazard_categories(event_title))
}

#' Hand-checked links between denial PDA reports and FEMA denial records
#'
#' A denied request has no disaster number, so the automatic passes match on the
#' decision date. Where two of a state's requests were decided on the same day
#' and FEMA's names for them carry nothing that ties either to its report, no
#' rule can separate them, and there are few enough such cases to settle by
#' hand.
#'
#' @return A tibble of `pda_file`, `denial_id`, and `note`.
#' @noRd
manual_pda_denial_links = function() {
  tibble::tribble(
    ~pda_file, ~denial_id, ~note,

    "PDAReportDenialOST.pdf",
    "South Dakota | 2018-09-18 | OST severe storm 07/27/2018",
    "Both South Dakota requests were turned down on 2018-09-18. FEMA abbreviates the Oglala Sioux Tribe to 'OST' in the incident name; the report is the Oglala Sioux Tribe's.",

    "PDAReportDenialCRST.pdf",
    "South Dakota | 2018-09-18 | SD - Severe Storms 07/04/2018",
    "The other of the two same-day South Dakota denials, left for the Cheyenne River Sioux Tribe once the Oglala Sioux Tribe claims the denial naming it.")
}

#' Fill in the statutory statewide per capita indicator where a report omits it
#'
#' The indicator is a single national dollar figure, published once for each
#' federal fiscal year, and every report of that year states the same value: it
#' does not vary by state, by territory, or by the size of the request.
#' A stated value outside the published range is treated as missing and replaced,
#' since it is not the threshold but the countywide figure printed in the
#' statewide row.
#'
#' The fiscal year is taken from the footnote the report prints about itself
#' ("Statewide Per Capita Impact Indicator for FY20"), falling back to the fiscal
#' year of the determination date. Those
#' fills are marked in `pa_per_capita_impact_indicator_statewide_source` so
#' this function's quality checks can tell them apart; the column is internal,
#' dropped before the data are returned.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @return `pda_df` with missing statewide indicators filled where they can be,
#'   and a `pa_per_capita_impact_indicator_statewide_source` column recording
#'   which values were read from a report and which were filled in.
#' @noRd
impute_statewide_indicator = function(pda_df) {

  plausible_range = pda_indicator_ranges()$pa_per_capita_impact_indicator_statewide

  prepared = pda_df %>%
    dplyr::mutate(
      fiscal_year_stated = stringr::str_extract(
          text,
          stringr::regex(
            "Statewide Per Capita Impact Indicator for (FY|Fiscal Year)\\s?[0-9]{2,4}",
            ignore_case = TRUE)) %>%
        stringr::str_extract("[0-9]{2,4}$") %>%
        as.numeric(),
      ## the footnote abbreviates the year ("FY20"), and one report spells it out
      fiscal_year_stated = dplyr::if_else(
        fiscal_year_stated < 100, fiscal_year_stated + 2000, fiscal_year_stated),
      ## a federal fiscal year begins on 1 October and is named for the calendar
      ## year it ends in, so a report from November 2019 belongs to FY2020
      fiscal_year_determined =
        lubridate::year(event_date_determined) +
        dplyr::if_else(lubridate::month(event_date_determined) >= 10, 1, 0),
      fiscal_year = dplyr::coalesce(fiscal_year_stated, fiscal_year_determined))

  indicator_by_fiscal_year = prepared %>%
    dplyr::filter(
      !is.na(fiscal_year),
      !is.na(pa_per_capita_impact_indicator_statewide),
      dplyr::between(
        pa_per_capita_impact_indicator_statewide,
        plausible_range[1], plausible_range[2])) %>%
    dplyr::count(
      fiscal_year, pa_per_capita_impact_indicator_statewide,
      name = "reports_stating_value") %>%
    ## sorted before the pick so that a year whose two commonest values are
    ## equally common resolves the same way on every run
    dplyr::arrange(
      fiscal_year,
      dplyr::desc(reports_stating_value),
      pa_per_capita_impact_indicator_statewide) %>%
    dplyr::slice_head(n = 1, by = fiscal_year) %>%
    dplyr::select(
      fiscal_year,
      indicator_for_fiscal_year = pa_per_capita_impact_indicator_statewide)

  imputed = prepared %>%
    dplyr::left_join(
      indicator_by_fiscal_year, by = "fiscal_year", relationship = "many-to-one") %>%
    dplyr::mutate(
      stated_out_of_range =
        !is.na(pa_per_capita_impact_indicator_statewide) &
        !dplyr::between(
          pa_per_capita_impact_indicator_statewide,
          plausible_range[1], plausible_range[2]),
      can_impute =
        (is.na(pa_per_capita_impact_indicator_statewide) | stated_out_of_range) &
        !is.na(indicator_for_fiscal_year),
      pa_per_capita_impact_indicator_statewide_source = dplyr::case_when(
        can_impute & stated_out_of_range ~
          "imputed, replacing a stated value outside the published range",
        can_impute & lubridate::month(event_date_determined) %in% 10:12 ~
          "imputed, and may be one fiscal year too recent",
        can_impute ~ "imputed",
        !is.na(pa_per_capita_impact_indicator_statewide) ~ "reported",
        TRUE ~ NA_character_),
      pa_per_capita_impact_indicator_statewide = dplyr::if_else(
        can_impute,
        indicator_for_fiscal_year,
        pa_per_capita_impact_indicator_statewide)) %>%
    dplyr::select(
      -fiscal_year_stated, -fiscal_year_determined, -fiscal_year,
      -indicator_for_fiscal_year, -can_impute, -stated_out_of_range)

  ## Not reported to the user directly:
  ## `pa_per_capita_impact_indicator_statewide_source` records which values
  ## were filled in and why.
  imputed
}

#' Match denied PDA reports to the FEMA denials they describe
#'
#' A denied request never receives a disaster number, so a denial PDA cannot be
#' joined to FEMA's `DeclarationDenials` record by key. What it does carry is
#' the date the decision was made, which is the denial's `request_status_date`,
#' so the primary match is an equality join on state and that date. Where one
#' date matches several of a state's denials -- FEMA can decide two requests
#' on the same day -- the candidates are narrowed by hazard-category agreement,
#' then by elimination (a denial claimed by one PDA cannot back another), and
#' anything still ambiguous is left unmatched rather than guessed at. A PDA with
#' no same-day denial gets one further chance: the nearest denial within
#' seven days either side, and only when the hazard categories agree.
#'
#' Two further passes then run over the reports the state-keyed passes could not
#' reach at all. A tribal report names the tribe rather than a state, so there is
#' no state to match on; for those the state is dropped from the key and the date
#' used alone, but only where exactly one unclaimed denial in the country
#' shares that date. Tested against the 168 denials the state-keyed pass does
#' resolve, that rule fires on 133 of them and picks the correct denial in all
#' 133, and declines on the remaining 35 rather than guessing. What it cannot
#' settle is two of a state's requests decided on the same day, which is what
#' `manual_pda_denial_links()` is for.
#'
#' @param denied_pdas Denied PDA records, with `state_name`, `decision`,
#'   `event_date_determined`, and `hazards`.
#' @param denials FEMA denial records, with `state_name`, `decision`,
#'   `request_status_date`, `denial_id`, and `denial_hazards`.
#' @return `denied_pdas` with one row each, plus `denial_id` (`NA` where no
#'   match was made) and `match_quality`.
#' @noRd
match_denied_pdas_to_denials = function(denied_pdas, denials) {

  denied_pdas1 = denied_pdas %>% dplyr::mutate(pda_id = dplyr::row_number())
  ## the reduced copy is what the state-keyed join in step 1 is allowed to see;
  ## the full `denials` is kept because the date-only pass below compares event
  ## titles and so needs `declaration_title` as well
  denials_keyed = denials %>%
    dplyr::select(state_name, decision, request_status_date, denial_id, denial_hazards)

  ## step 1: exact match on the denial decision date. The relationship is
  ## many-to-many only because two same-state denials can share a decision
  ## date; those ties are resolved in the steps that follow.
  candidates1 = denied_pdas1 %>%
    dplyr::left_join(
      denials_keyed,
      by = dplyr::join_by(state_name, decision, event_date_determined == request_status_date),
      relationship = "many-to-many") %>%
    dplyr::mutate(
      shared_hazard_count = shared_hazard_category_count(hazards, denial_hazards),
      hazards_agree = shared_hazard_count > 0,
      match_quality = dplyr::if_else(
        is.na(denial_id),
        NA_character_,
        "exact: the PDA determination date is the denial decision date"))

  ## step 2: where a PDA matched several same-day denials, drop the candidates
  ## sharing no hazard category with the PDA, then keep only the candidate(s)
  ## sharing the most categories (ties survive to step 3). PDAs whose candidates
  ## were all eliminated are re-attached as unmatched rows.
  candidates2 = candidates1 %>%
    dplyr::add_count(pda_id, name = "candidate_count") %>%
    dplyr::filter(candidate_count == 1 | dplyr::coalesce(hazards_agree, FALSE)) %>%
    dplyr::filter(
      dplyr::coalesce(shared_hazard_count, 0L) == max(dplyr::coalesce(shared_hazard_count, 0L)),
      .by = pda_id) %>%
    dplyr::bind_rows(dplyr::anti_join(denied_pdas1, ., by = "pda_id"))

  ## step 3: resolve the remaining ties by elimination. A PDA with exactly one
  ## candidate claims that denial, which
  ## can in turn leave another PDA with a single candidate. Repeat until nothing
  ## changes. Two PDAs whose only candidate is the same denial both keep it
  ## here; step 4 addresses this.
  candidates3 = local({
    remaining = candidates2 %>%
      dplyr::filter(!is.na(denial_id)) %>%
      dplyr::select(-candidate_count)

    repeat {
      counted = remaining %>% dplyr::add_count(pda_id, name = "candidates_per_pda")
      claimed_denial_ids = counted %>%
        dplyr::filter(candidates_per_pda == 1) %>%
        dplyr::pull(denial_id)
      pruned = counted %>%
        dplyr::filter(candidates_per_pda == 1 | !denial_id %in% claimed_denial_ids) %>%
        dplyr::select(-candidates_per_pda)
      if (nrow(pruned) == nrow(remaining)) { break }
      remaining = pruned }

    dplyr::bind_rows(remaining, dplyr::anti_join(denied_pdas1, remaining, by = "pda_id")) })

  ## step 4: enforce cardinality. A match survives only where the PDA has one
  ## candidate, the hazards are not known to disagree, and the denial is not
  ## also claimed by another PDA.
  resolved = candidates3 %>%
    dplyr::add_count(pda_id, name = "candidate_count_resolved") %>%
    dplyr::add_count(denial_id, name = "pdas_per_denial") %>%
    dplyr::mutate(
      match_ok =
        !is.na(match_quality) &
        candidate_count_resolved == 1 &
        dplyr::coalesce(hazards_agree, TRUE) &
        pdas_per_denial == 1)

  ## A report left unmatched here -- its candidate denial ambiguous, or
  ## describing a different hazard -- is counted in the unmatched total the
  ## single consolidated warning reports.
  matched_exactly = resolved %>%
    dplyr::mutate(
      denial_id = dplyr::if_else(match_ok, denial_id, NA_character_),
      match_quality = dplyr::if_else(match_ok, match_quality, NA_character_)) %>%
    dplyr::slice_head(n = 1, by = pda_id)

  ## step 5: fallback for the PDAs with no same-day denial -- the nearest
  ## denial of the same hazard type within a week, in either direction. 
  ## A tie between two equally near denials
  ## leaves two candidates, which the uniqueness test then refuses.
  claimed_exactly = stats::na.omit(matched_exactly$denial_id)

  matched_fuzzily = matched_exactly %>%
    dplyr::filter(is.na(match_quality)) %>%
    dplyr::select(dplyr::all_of(names(denied_pdas1))) %>%
    dplyr::inner_join(
      denials %>% dplyr::filter(!denial_id %in% claimed_exactly),
      by = dplyr::join_by(state_name, decision),
      relationship = "many-to-many") %>%
    dplyr::mutate(
      days_from_denial = as.numeric(event_date_determined - request_status_date),
      shared_hazard_count = shared_hazard_category_count(hazards, denial_hazards)) %>%
    dplyr::filter(
      abs(days_from_denial) <= 7,
      dplyr::coalesce(shared_hazard_count > 0, FALSE)) %>%
    dplyr::filter(
      abs(days_from_denial) == min(abs(days_from_denial)),
      .by = pda_id) %>%
    dplyr::add_count(pda_id, name = "candidate_count") %>%
    dplyr::filter(candidate_count == 1) %>%
    dplyr::transmute(
      pda_id,
      denial_id,
      match_quality = stringr::str_c(
        "approximate: no denial shares the PDA determination date, so the ",
        "nearest denial of the same hazard type was used, recorded by FEMA ",
        abs(days_from_denial), " day(s) ",
        dplyr::if_else(days_from_denial > 0, "earlier", "later")))

  resolved_by_state = matched_exactly %>%
    dplyr::rows_update(matched_fuzzily, by = "pda_id", unmatched = "ignore") %>%
    dplyr::arrange(pda_id) %>%
    dplyr::select(dplyr::all_of(names(denied_pdas1)), denial_id, match_quality)

  ## step 6: the date alone, without the state. This reaches only the reports the
  ## state-keyed passes never had a candidate for. The denial must be the only unclaimed one in
  ## the country on that date, and must either agree on hazard or share a word
  ## with the report's title, so a same-day denial from an unrelated request
  ## cannot be picked up on the date alone.
  had_state_candidate = candidates1 %>%
    dplyr::filter(!is.na(denial_id)) %>%
    dplyr::pull(pda_id) %>%
    unique()

  eligible_for_date_only = resolved_by_state %>%
    dplyr::filter(is.na(match_quality), !pda_id %in% had_state_candidate)

  unclaimed = denials %>%
    dplyr::filter(!denial_id %in% stats::na.omit(resolved_by_state$denial_id))

  matched_by_date = eligible_for_date_only %>%
    dplyr::select(-denial_id, -match_quality) %>%
    dplyr::left_join(
      unclaimed %>%
        dplyr::select(request_status_date, denial_id, declaration_title, denial_hazards),
      by = dplyr::join_by(event_date_determined == request_status_date),
      relationship = "many-to-many") %>%
    dplyr::add_count(pda_id, name = "candidate_count") %>%
    dplyr::mutate(
      shared_hazard_count = shared_hazard_category_count(hazards, denial_hazards),
      shares_a_word = shared_title_word_count(event_title, declaration_title) > 0,
      match_ok =
        !is.na(denial_id) &
        candidate_count == 1 &
        (dplyr::coalesce(shared_hazard_count > 0, FALSE) | dplyr::coalesce(shares_a_word, FALSE)),
      denial_id = dplyr::if_else(match_ok, denial_id, NA_character_),
      match_quality = dplyr::if_else(
        match_ok,
        "exact: the only denial in the country sharing the PDA determination date, matched without the state key because the report names none",
        NA_character_)) %>%
    dplyr::slice_head(n = 1, by = pda_id) %>%
    dplyr::select(dplyr::all_of(names(resolved_by_state)))

  resolved_by_date = resolved_by_state %>%
    dplyr::rows_update(matched_by_date, by = "pda_id", unmatched = "ignore")

  ## step 7: the hand-checked links, for what no rule can separate
  resolved_by_date %>%
    apply_manual_pda_denial_links(denials) %>%
    dplyr::select(-pda_id)
}

#' Count the words two event descriptions share
#'
#' Used only to confirm that a candidate denial is not unrelated to the report,
#' so the comparison is deliberately crude: both sides are reduced to lowercase
#' words, with punctuation, FEMA's region codes, and the words that appear in
#' every tribal name ("tribe", "band", "nation") removed, since those would make
#' any two tribal records look alike.
#'
#' @param descriptions_one,descriptions_two Character vectors of event titles.
#' @return An integer vector of shared-word counts, `NA` where either side is
#'   empty.
#' @noRd
shared_title_word_count = function(descriptions_one, descriptions_two) {
  to_words = function(x) {
    x %>%
      stringr::str_to_lower() %>%
      stringr::str_remove_all("\\br(egion)?\\s?[0-9]{1,2}\\b") %>%
      stringr::str_replace_all("[^a-z0-9 ]", " ") %>%
      stringr::str_remove_all(stringr::str_c(
        "\\b(the|of|and|a|an|tribe|tribes|tribal|band|nation|indians|indian|",
        "village|native|community|pueblo|rancheria|reservation)\\b")) %>%
      stringr::str_squish() %>%
      stringr::str_split(" ") }

  purrr::map2_int(
    to_words(descriptions_one),
    to_words(descriptions_two),
    function(words_one, words_two) {
      words_one = setdiff(words_one, "")
      words_two = setdiff(words_two, "")
      if (length(words_one) == 0 || length(words_two) == 0) { return(NA_integer_) }
      length(intersect(words_one, words_two)) })
}

#' Apply the hand-checked report-to-denial links
#'
#' Consulted only for reports still unmatched, so a hand-written row can fill a
#' gap but never displace a match the data itself supports.
#' 
#' @param denied_pdas Denied PDA records carrying `denial_id` and
#'   `match_quality` from the automatic passes.
#' @param denials FEMA denial records.
#' @return `denied_pdas` with the hand-checked links applied.
#' @noRd
apply_manual_pda_denial_links = function(denied_pdas, denials) {

  links = manual_pda_denial_links()
  if (nrow(links) == 0) { return(denied_pdas) }

  checked = links %>%
    dplyr::mutate(
      report_exists = pda_file %in% basename(denied_pdas$path),
      denial_exists = denial_id %in% denials$denial_id,
      denial_claimed =
        denial_id %in% stats::na.omit(denied_pdas$denial_id),
      report_already_matched = pda_file %in% basename(
        denied_pdas$path[!is.na(denied_pdas$match_quality)]),
      usable = report_exists & denial_exists & !denial_claimed & !report_already_matched)

  unusable = checked %>% dplyr::filter(!usable, !report_already_matched)
  if (nrow(unusable) > 0) {
    warning(
      stringr::str_c(
        nrow(unusable), " hand-checked PDA-to-denial link(s) could not be ",
        "applied and should be reviewed: ",
        stringr::str_c(
          unusable$pda_file, " -> ", unusable$denial_id, " (",
          dplyr::case_when(
            !unusable$report_exists ~ "no such report in the archive",
            !unusable$denial_exists ~ "FEMA no longer publishes this denial",
            unusable$denial_claimed ~ "another report already matched this denial",
            TRUE ~ "unknown reason"),
          ")",
          collapse = "; ")),
      call. = FALSE) }

  applied = checked %>%
    dplyr::filter(usable) %>%
    dplyr::select(pda_file, linked_denial_id = denial_id)

  if (nrow(applied) == 0) { return(denied_pdas) }

  denied_pdas %>%
    dplyr::mutate(pda_file = basename(path)) %>%
    dplyr::left_join(applied, by = "pda_file", relationship = "many-to-one") %>%
    dplyr::mutate(
      match_quality = dplyr::if_else(
        is.na(match_quality) & !is.na(linked_denial_id),
        "manual: linked by hand, because two of the state's requests were decided on the same day and FEMA's record names neither in a way a rule could tie to its report",
        match_quality),
      denial_id = dplyr::coalesce(denial_id, linked_denial_id)) %>%
    dplyr::select(-pda_file, -linked_denial_id)
}

#' Add the measures derived from a PDA report alone
#'
#' The decision, the combined cost estimate, and the per capita threshold ratio
#' are read from the report and nothing else, so they belong to every PDA record
#' whether or not it is later joined to FEMA's declaration and denial records.
#' They are computed here, outside `join_pda_outcomes()`, so that the
#' `join_outcomes = FALSE` path returns them too.

#' @param pda_df A dataframe of extracted PDA records.
#' @return `pda_df` with `decision`, `cost_estimate_ia_pa_total`, and
#'   `pa_threshold_ratio` added.
#' @noRd
add_pda_summary_measures = function(pda_df) {
  pda_df %>%
    dplyr::mutate(
      decision = dplyr::case_when(
        stringr::str_detect(event_type, "denial") ~ "Denied",
        stringr::str_detect(event_type, "approv") ~ "Approved"),
      ## a single estimate of the damage across both assistance programs; zero
      ## where neither program reported a cost
      cost_estimate_ia_pa_total = rowSums(
        cbind(pa_cost_estimate_total, ia_cost_estimate_total), na.rm = TRUE),
      pa_threshold_ratio =
        pa_per_capita_impact_statewide / pa_per_capita_impact_indicator_statewide)
}

#' Join PDA attributes onto FEMA's own record of what was declared and denied
#'
#' The PDA reports are text extracted from PDFs and are not a complete or
#' authoritative list of requests: a report can be missing, and a report that is
#' present can name a state or a disaster number the text was misread from.
#' FEMA's `DisasterDeclarationsSummaries` (granted major disaster declarations)
#' and `DeclarationDenials` (turned-down requests) are authoritative, so those
#' two records together define the universe of rows returned here, and the PDA
#' attributes are attached to them. A request with no usable PDA report keeps
#' its row, with the PDA columns empty.
#'
#' Approvals and denials are matched differently because they carry different
#' keys: an approved request shares a disaster number with the declarations
#' record, whereas a denied request has no number at all and must be matched on
#' state, decision date, and hazard type -- see
#' `match_denied_pdas_to_denials()`.
#'
#' @param pda_df A dataframe of extracted PDA records, after
#'   `add_pda_derived_columns()`.
#' @return One row per authoritative FEMA record, with the PDA columns joined on.
#' @noRd
join_pda_outcomes = function(pda_df) {

  declarations = rfema::open_fema(
      data_set = "DisasterDeclarationsSummaries",
      ## major disaster declarations only; a PDA precedes a major disaster
      ## request, not an emergency declaration or a fire management grant
      filters = list(declarationType = "=DR"),
      ask_before_call = FALSE) %>%
    janitor::clean_names() %>%
    ## the source returns one row per designated area, usually a county, so a
    ## single declaration appears many times
    dplyr::distinct(fema_declaration_string, .keep_all = TRUE) %>%
    dplyr::transmute(
      disaster_number = as.character(disaster_number),
      state,
      declaration_date = lubridate::as_date(declaration_date),
      declaration_title,
      decision = "Approved",
      ## FEMA's record of the programs the declaration turned on. `fema_`-named
      ## here so the final rename pass leaves them untouched.
      fema_ihp_declared = as.logical(ih_program_declared),
      fema_ia_declared = as.logical(ia_program_declared),
      fema_pa_declared = as.logical(pa_program_declared),
      fema_hm_declared = as.logical(hm_program_declared)) %>%
    ## the declarations record abbreviates the state; the PDA text spells it out
    dplyr::left_join(
      tidycensus::fips_codes %>% dplyr::distinct(state_name, state),
      by = "state",
      relationship = "many-to-one") %>%
    dplyr::select(-state)

  denials = rfema::open_fema(data_set = "DeclarationDenials", ask_before_call = FALSE) %>%
    janitor::clean_names() %>%
    dplyr::filter(
      ## FEMA has published this status as both "denial" and "Turndown"; both
      ## name the same outcome, a request the President declined
      stringr::str_to_lower(current_request_status) %in% c("denial", "turndown"),
      declaration_request_type == "Major Disaster") %>%
    dplyr::transmute(
      state_name = stringr::str_trim(state),
      declaration_request_date = lubridate::as_date(declaration_request_date),
      request_status_date = lubridate::as_date(request_status_date),
      declaration_title = stringr::str_trim(incident_name),
      requested_incident_types = stringr::str_trim(requested_incident_types),
      decision = "Denied",
      ## FEMA's record of the programs the request asked for. Requests for the
      ## Individual Assistance umbrella are recorded under `ihProgramRequested`
      ## (the Individuals and Households Program replaced the older Individual
      ## Assistance program in 2002; `iaProgramRequested` is FALSE throughout
      ## the years the PDA archive covers).
      fema_ihp_requested = as.logical(ih_program_requested),
      fema_ia_requested = as.logical(ia_program_requested),
      fema_pa_requested = as.logical(pa_program_requested),
      fema_hm_requested = as.logical(hm_program_requested)) %>%
    dplyr::mutate(
      denial_hazards = extract_hazard_categories(stringr::str_c(
        dplyr::coalesce(declaration_title, ""),
        dplyr::coalesce(requested_incident_types, ""),
        sep = " ")),
      ## the record has no disaster number, so this identifies a denial for the
      ## match and makes a denial claimed by two PDAs detectable
      denial_id = stringr::str_c(
        state_name, request_status_date,
        dplyr::coalesce(declaration_title, "unnamed"),
        sep = " | ")) %>%
    ## FEMA publishes a small number of denials twice, as rows identical in
    ## every field (one West Virginia request as of this writing). Keeping both
    ## would return two universe rows for one request and attach the same PDA
    ## report to each of them.
    dplyr::distinct(denial_id, .keep_all = TRUE)

  ## `decision`, `cost_estimate_ia_pa_total` and `pa_threshold_ratio` arrive
  ## already computed, from `add_pda_summary_measures()`, so that the
  ## `join_outcomes = FALSE` path carries them too
  pdas = pda_df %>%
    dplyr::mutate(disaster_number = as.character(disaster_number)) %>%
    ## a report whose decision could not be read cannot be matched to an
    ## authoritative record
    dplyr::filter(!is.na(decision))

  ## an approved request may have more than one report (an original and an
  ## appeal) but only one declaration, so the most recent report is kept and the
  ## fact that others exist is recorded in the match description
  pdas_approved_all = pdas %>%
    dplyr::filter(decision == "Approved", !is.na(disaster_number))

  pdas_approved = pdas_approved_all %>%
    dplyr::arrange(dplyr::desc(event_date_determined)) %>%
    dplyr::add_count(disaster_number, name = "report_count") %>%
    dplyr::slice_head(n = 1, by = disaster_number) %>%
    dplyr::mutate(
      match_quality = dplyr::if_else(
        report_count > 1,
        stringr::str_c(
          "exact: joined on the disaster number, but ", report_count,
          " PDA reports exist for this declaration and the most recent was kept"),
        "exact: joined on the disaster number")) %>%
    dplyr::select(-report_count)

  ## reports deliberately set aside by the dedup above, so the message below can
  ## keep them apart from reports that genuinely failed to match
  superseded_paths = setdiff(pdas_approved_all$path, pdas_approved$path)

  ## reports whose state could not be read -- a tribal report names the tribe,
  ## not a state -- are kept: the state-keyed passes cannot reach them, but the
  ## date-only pass and the hand-checked links can
  pdas_denied = pdas %>%
    dplyr::filter(decision == "Denied") %>%
    match_denied_pdas_to_denials(denials)

  joined_approved = declarations %>%
    tidylog::left_join(
      ## FEMA's own state name is authoritative, so the report's text-derived
      ## copy is dropped rather than returned alongside it
      pdas_approved %>% dplyr::select(-state_name),
      ## the disaster number is unique nationally, so it alone identifies the
      ## declaration. The state is deliberately not part of the key: a tribal
      ## report names no state, and one whose state was misread would otherwise
      ## be dropped rather than matched.
      by = c("disaster_number", "decision"),
      relationship = "many-to-one")

  joined_denied = denials %>%
    tidylog::left_join(
      pdas_denied %>%
        dplyr::filter(!is.na(denial_id)) %>%
        dplyr::select(-state_name, -decision, -disaster_number),
      by = "denial_id",
      relationship = "many-to-one") %>%
    dplyr::select(-denial_id) %>%
    ## FEMA's own date the request was turned down: the counterpart of
    ## `declaration_date` for a denied request, and the field every denial match
    ## is keyed on. 
    dplyr::rename(denial_date = request_status_date)

  outcomes = dplyr::bind_rows(joined_approved, joined_denied) %>%
    dplyr::mutate(pda_matched = !is.na(path))

  ## Counted by source file rather than by subtracting matched rows from the
  ## report total. A declaration with both an original and an appeal report keeps
  ## only the most recent, and the one set aside is represented in the data by
  ## its sibling.
  matched_paths = stats::na.omit(outcomes$path)

  ## A report set aside by the dedup counts as represented only if the
  ## declaration it describes was itself matched -- by its sibling report.
  superseded_represented = pdas_approved_all$path[
    pdas_approved_all$path %in% superseded_paths &
      pdas_approved_all$disaster_number %in% outcomes$disaster_number[outcomes$pda_matched]]

  ## Reports set aside as an original superseded by its appeal are not match
  ## failures -- the declaration they belong to is present, described by its
  ## most recent report -- so they are excluded from the unmatched count that
  ## `get_preliminary_damage_assessments()` reports.
  unmatched_reports = setdiff(pda_df$path, c(matched_paths, superseded_represented))

  ## Every returned column is labelled by where its value came from: `fema_`
  ## for FEMA's own declaration and denial records, `pda_` for values read out
  ## of the PDF reports. The two decision dates are one fact -- the day FEMA
  ## settled the request -- recorded in different source datasets, so they are
  ## combined into a single column.
  joined = outcomes %>%
    dplyr::select(-dplyr::any_of("disaster_number_filename")) %>%
    dplyr::mutate(
      fema_decision_date = dplyr::coalesce(declaration_date, denial_date),
      fema_decision_year = lubridate::year(fema_decision_date)) %>%
    dplyr::select(-declaration_date, -denial_date) %>%
    dplyr::rename(
      fema_disaster_number = disaster_number,
      fema_state_name = state_name,
      fema_decision = decision,
      fema_declaration_request_date = declaration_request_date,
      fema_declaration_title = declaration_title,
      fema_requested_incident_types = requested_incident_types,
      fema_hazards = denial_hazards,
      pda_match_quality = match_quality) %>%
    ## output names for the report-side columns; `declaration_title` and
    ## `decision` are free again, their FEMA-side owners renamed just above
    dplyr::rename(
      tribal_flag = event_native_flag,
      declaration_title = event_title,
      decision = event_type,
      date_determined = event_date_determined) %>%
    dplyr::select(
      -dplyr::any_of("pa_per_capita_impact_indicator_statewide_source")) %>%
    dplyr::rename_with(
      ~ stringr::str_c("pda_", .x),
      .cols = !dplyr::starts_with(c("fema_", "pda_"))) %>%
    dplyr::select(
      fema_disaster_number, fema_state_name, fema_decision, fema_decision_date,
      fema_decision_year, fema_declaration_request_date, fema_declaration_title,
      fema_requested_incident_types, fema_hazards,
      fema_ihp_declared, fema_ia_declared, fema_pa_declared, fema_hm_declared,
      fema_ihp_requested, fema_ia_requested, fema_pa_requested,
      fema_hm_requested, pda_matched,
      pda_match_quality, dplyr::any_of("pda_warnings"), dplyr::everything())

  ## the count feeds the single consolidated warning
  ## `get_preliminary_damage_assessments()` emits
  attr(joined, "n_unmatched") = length(unmatched_reports)
  joined
}

#' Get FEMA Preliminary Damage Assessments Report Data
#'
#' @description Retrieves data extracted from PDF preliminary damage assessment (PDA)
#'   reports published by FEMA as part of the disaster declaration request process.
#'
#' @details Data are extracted from PDF reports hosted at
#'   \url{https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports}.
#'   Owing to the unstructured nature of the source documents, some fields may be incorrect
#'   in the data returned by the function, though significant quality checks have been
#'   implemented in an effort to produce a high-quality dataset.
#' 
#'   With `join_outcomes = TRUE` (the default), the PDA data are attached to
#'   FEMA's own structured records of which declaration requests were declared and
#'   denied. FEMA's
#'   `DisasterDeclarationsSummaries` (granted major disaster declarations) and
#'   `DeclarationDenials` (turned-down major disaster requests) datasets are
#'   authoritative, so every such record is returned, with the PDA columns empty
#'   where no report could be matched to it. An approved request is matched on
#'   its disaster number. A denied request never receives a disaster number, so
#'   it is matched on state, on the decision date the report prints, and on
#'   agreement between the hazards the two records describe, with anything
#'   ambiguous left unmatched; `match_quality` records
#'   which of these applied. Set `join_outcomes = FALSE` for the reports alone.
#'
#'   Data quality is reported through a single consolidated warning: how many
#'   records carry values that may be incorrect -- each such record's specific
#'   issue(s) are written to the `pda_warnings` column -- and how many PDA
#'   reports could not be matched to an authoritative FEMA declaration or
#'   denial and are therefore absent from the returned data.
#'
#' @param file_path The file path to the cached dataset, or if there is no cache, the path
#'   at which to cache the resulting data.
#' @param directory_path The path to the directory where PDA PDFs are stored.
#'   These files are not fetched by this function; run [scrape_pda_pdfs()] to
#'   download them and to refresh the archive as FEMA publishes new reports.
#' @param use_cache Boolean. Read the existing dataset stored at `file_path`? If FALSE,
#'   data will be generated anew. Else, if a file exists at `file_path`, this file will be returned.
#' @param join_outcomes Boolean. Return FEMA's authoritative record of every
#'   granted and denied major disaster request, with the PDA attributes joined
#'   onto it? If FALSE, one row per PDA report is
#'   returned instead, without the declaration and denial columns.
#'
#' @return A dataframe with one row per FEMA declaration or denial record
#'   (or, when `join_outcomes = FALSE`, one row per PDA report). Several fields
#'   have no labelled equivalent in the source documents and are derived from
#'   the report text; the logic used is given with each.
#'
#'   When `join_outcomes = TRUE`, every column name carries a prefix saying
#'   where its value came from: `fema_` for FEMA's own declaration and denial
#'   records, `pda_` for values read out of the PDF reports. The column names
#'   listed below are the prefixed ones. When `join_outcomes = FALSE` the
#'   columns are returned unprefixed (so `pda_hazards` below is `hazards`, and
#'   `pda_decision` is `decision`), and
#'   the columns describing the FEMA record or the match to it --
#'   `fema_decision_date`, `fema_decision_year`, `fema_declaration_request_date`,
#'   `fema_declaration_title`, `fema_requested_incident_types`, `fema_hazards`,
#'   the eight `fema_*_declared`/`fema_*_requested` program fields,
#'   `pda_matched`, and `pda_match_quality` -- are absent.
#'   Columns include:
#'   \describe{
#'     \item{fema_disaster_number}{FEMA disaster number. NA for a denied request, which is never assigned one.}
#'     \item{fema_state_name}{The requesting state, territory, or the District of Columbia.
#'        For a joined record this is FEMA's own value; on the PDA side it is the first
#'        state or territory name appearing anywhere in the report text, since FEMA does
#'        not print it as a labelled field.}
#'     \item{fema_decision}{"Approved" or "Denied".}
#'     \item{fema_decision_date}{The date FEMA settled the request: the date the major
#'        disaster declaration was granted for an approved request, and FEMA's own
#'        `request_status_date` -- the date the request was turned down -- for a denied one.}
#'     \item{fema_decision_year}{The calendar year of `fema_decision_date`.}
#'     \item{fema_declaration_request_date}{Date the state filed the request (denied records only).}
#'     \item{fema_declaration_title}{FEMA's name for the event.}
#'     \item{fema_requested_incident_types}{FEMA's own classification of the requested incident
#'        types (denied records only).}
#'     \item{fema_hazards}{Semicolon-separated hazard categories read from FEMA's
#'        `fema_declaration_title` and `fema_requested_incident_types`, on the same category set
#'        as `pda_hazards` so the two can be compared (denied records only).}
#'     \item{fema_ihp_declared, fema_ia_declared, fema_pa_declared, fema_hm_declared}{FEMA's
#'        record of whether the declaration turned on the Individuals and Households Program,
#'        the (pre-2002) Individual Assistance program, Public Assistance, and Hazard
#'        Mitigation (approved records only; NA for denied ones). A program can be declared
#'        without appearing in the PDA -- added by a later request -- and requested without
#'        being declared, so these legitimately diverge from the `pda_*_requested` flags.}
#'     \item{fema_ihp_requested, fema_ia_requested, fema_pa_requested, fema_hm_requested}{FEMA's
#'        record of the programs the denied request asked for (denied records only; NA for
#'        approved ones). Requests for the Individual Assistance umbrella are recorded under
#'        `fema_ihp_requested`; `fema_ia_requested` names the pre-2002 Individual Assistance
#'        program and is FALSE throughout the years the PDA archive covers. `fema_ihp_requested`
#'        is narrower than `pda_ia_requested`: a request only for a component program such as
#'        Disaster Unemployment Assistance sets the `pda_` flag but not FEMA's.}
#'     \item{pda_matched}{TRUE where a PDA report was matched to this FEMA record.}
#'     \item{pda_warnings}{Semicolon-separated descriptions of the data quality problems
#'        found on this record -- a value that cannot be what its field measures, a
#'        footnote number standing in for a dollar total, damage categories that do not
#'        add up, a report contradicting itself about whether a program was requested,
#'        and the like. NA where no problem was found. Values are returned as parsed, so
#'        each problem can be checked against its source report. Named `warnings` when
#'        `join_outcomes = FALSE`.}
#'     \item{pda_match_quality}{How the match was made, or NA where none was. "exact" covers an
#'        approved request joined on its disaster number; a denied request whose determination
#'        date is the denial decision date within the same state; and a denied request whose
#'        report names no state -- a tribal report names the tribe instead -- matched on the
#'        determination date alone, where exactly one denial in the country shares that date
#'        and it agrees with the report on hazard or shares a word with its title.
#'        "approximate" is a denied request matched instead to the nearest denial of the same
#'        hazard type within seven days either side of the date the report prints, and says how
#'        many days apart the two records are. "manual" is a link established by hand,
#'        for the case no rule can settle: two of a state's requests decided on the same day,
#'        with FEMA naming neither in a way that ties it to its report.}
#'     \item{pda_path}{The local file path to the source PDA PDF.}
#'     \item{pda_decision}{Type of decision the report announces: "approved", "denial",
#'        "appeal_approved", or "appeal_denial". Finer-grained than `fema_decision`, which
#'        does not distinguish an original decision from one made on appeal.}
#'     \item{pda_declaration_title}{The report's description of the event.}
#'     \item{pda_date_determined}{Date the PDA determination was made.}
#'     \item{pda_tribal_flag}{1 where the request came from a tribal government, 0 otherwise.
#'        Set where the report title contains any of "Native", "Tribe", "Tribes", "Tribal",
#'        "Indians", "Nation", "Band", "Pueblo", "Rancheria", "Reservation", "Village",
#'        "Traditional Council", or "IRA Council". This drives the extraction logic for many fields
#'        because tribal records are formatted differently than state/territorial records.}
#'     \item{pda_hazards}{Semicolon-separated hazard categories ("flooding; landslide") read from
#'        `pda_declaration_title`. NA where no keyword matched.}
#'     \item{pda_pa_requested}{1 where Public Assistance was requested, 0 where the report states
#'        "Public Assistance - Not requested". Where a report says the program was not
#'        requested but reports values for it anyway, or states no values while its narrative
#'        does not name the program, the flag is settled from the report's opening narrative
#'        -- see `requested_from_narrative`.}
#'     \item{pda_pa_preemptive_declaration}{1 where the report states that the "requirement for a
#'        joint PDA may be waived", meaning the event was severe enough that FEMA proceeded
#'        without conducting a joint preliminary damage assessment first; 0 otherwise.}
#'     \item{pda_pa_primary_impact}{The primary type of impact described for Public Assistance
#'        purposes.}
#'     \item{pda_pa_cost_estimate_total}{Estimated total Public Assistance cost.}
#'     \item{pda_pa_per_capita_impact_statewide}{Statewide (or territory/commonwealth) per capita
#'        impact amount.}
#'     \item{pda_pa_per_capita_impact_indicator_statewide}{FEMA's statutory statewide per capita
#'        *threshold* in dollars for the relevant year (observed range 1.24--1.94). A report
#'        that states no value, or states one outside the published range, is given the value
#'        the other reports of its fiscal year state; the indicator is uniform within a fiscal
#'        year, so this fill is very low risk.}
#'     \item{pda_pa_per_capita_impact_countywide}{Raw text of countywide per capita impact ratios (may list
#'        multiple values across affected counties for a multi-county event).}
#'     \item{pda_pa_per_capita_impact_indicator_countywide}{FEMA's statutory countywide per capita
#'        threshold in dollars, on the same basis as the statewide indicator above.}
#'     \item{pda_pa_per_capita_impact_countywide_max}{Maximum countywide per capita impact ratio parsed
#'        from `pa_per_capita_impact_countywide`.}
#'     \item{pda_pa_per_capita_impact_countywide_min}{Minimum countywide per capita impact ratio parsed
#'        from `pa_per_capita_impact_countywide`.}
#'     \item{pda_pa_threshold_ratio}{`pa_per_capita_impact_statewide` divided by
#'        `pa_per_capita_impact_indicator_statewide`: the estimated per capita damage expressed as
#'        a multiple of the statutory threshold, so a value above 1 means the threshold was
#'        exceeded.}
#'     \item{pda_ia_requested}{1 where Individual Assistance was requested, 0 where the report states
#'        "Individual Assistance - Not requested". Where a report states no Individual
#'        Assistance values and its opening narrative does not name the program among those
#'        requested, the flag is set to 0 -- see `requested_from_narrative`. Individual
#'        Assistance is read broadly: a request only for one of its component programs
#'        (Crisis Counseling, Disaster Unemployment Assistance, Disaster Legal Services,
#'        Disaster Case Management) counts, so this flag can be 1 where FEMA's own
#'        `fema_ihp_requested` is FALSE.}
#'     \item{pda_requested_from_narrative}{Names the program whose requested flag was settled from
#'        the report's opening narrative ("ia", "pa", or "ia; pa"), NA otherwise. The narrative is
#'        consulted only where the report disagrees with itself: the summary says a program
#'        was not requested while the report prints values for it, or the report prints no
#'        values while the narrative does not name the program. A narrative naming any
#'        component program of Individual Assistance counts as requesting Individual
#'        Assistance.}
#'     \item{pda_ia_residences_impacted}{Total residences impacted.}
#'     \item{pda_ia_residences_destroyed}{Number of residences destroyed.}
#'     \item{pda_ia_residences_major_damage}{Number of residences with major damage.}
#'     \item{pda_ia_residences_minor_damage}{Number of residences with minor damage.}
#'     \item{pda_ia_residences_affected}{Number of residences affected (lowest damage category).}
#'     \item{pda_ia_residences_insured_total_percent}{Percentage of impacted residences with any insurance coverage.}
#'     \item{pda_ia_residences_insured_flood_percent}{Percentage of impacted residences with flood insurance coverage.}
#'     \item{pda_ia_households_poverty_percent}{Percentage of households in poverty (or low income,
#'        depending on report vintage).}
#'     \item{pda_ia_households_owner_percent}{Percentage of households that are owner-occupied.}
#'     \item{pda_ia_population_other_government_assistance_percent}{Percentage of the population receiving
#'        other government assistance (e.g. SSI, SNAP).}
#'     \item{pda_ia_pre_disaster_unemployment_percent}{Pre-disaster unemployment rate.}
#'     \item{pda_ia_65plus_percent}{Percentage of the population age 65 and older.}
#'     \item{pda_ia_18below_percent}{Percentage of the population age 18 and under.}
#'     \item{pda_ia_disability_percent}{Percentage of the population with a disability.}
#'     \item{pda_ia_ihp_cost_to_capacity_ratio}{Individuals and Households Program (IHP) Cost to Capacity (ICC) ratio.}
#'     \item{pda_ia_cost_estimate_total}{Estimated total Individual Assistance cost.}
#'     \item{pda_cost_estimate_ia_pa_total}{`pa_cost_estimate_total` plus `ia_cost_estimate_total`,
#'        treating a missing program cost as zero; 0 where neither program reported one.}
#'     \item{pda_text}{The cleaned text extracted from the PDA PDF used to derive the fields above.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' get_preliminary_damage_assessments()
#' }
get_preliminary_damage_assessments = function(
    file_path = file.path(get_box_path(), "hazards", "urban", "preliminary-damage-assessments", "pda_data.csv"),
    directory_path = file.path(get_box_path(), "hazards", "urban", "preliminary-damage-assessments"),
    use_cache = TRUE,
    join_outcomes = TRUE) {

  finalize = function(pda_df) {
    pda_df1 = pda_df %>%
      add_pda_derived_columns() %>%
      impute_statewide_indicator() %>%
      ## derived from the report alone, so they belong on both paths; this must
      ## follow the imputation, which supplies the threshold ratio's denominator
      add_pda_summary_measures()
    if (join_outcomes == FALSE) {
      ## the report-only path carries the same output names as the joined
      ## path's pda_-prefixed equivalents. The derived Approved/Denied
      ## `decision` -- computed for the join's benefit -- gives its name up to
      ## the report's own decision type.
      return(
        pda_df1 %>%
          dplyr::select(-dplyr::any_of(c(
            "decision", "pa_per_capita_impact_indicator_statewide_source"))) %>%
          dplyr::rename(
            tribal_flag = event_native_flag,
            declaration_title = event_title,
            decision = event_type,
            date_determined = event_date_determined)) }
    join_pda_outcomes(pda_df1) }

  ## The one place this function talks to the user about data quality: a single
  ## warning covering the per-record problems (detailed in the warnings column)
  ## and, when the FEMA records were joined, the reports no authoritative
  ## record could be matched to.
  report_quality = function(result, dataset_issues) {
    warnings_column = if ("pda_warnings" %in% names(result)) {
      "pda_warnings" } else { "warnings" }
    n_flagged = sum(!is.na(result[[warnings_column]]))
    n_unmatched = attr(result, "n_unmatched")

    parts = character(0)
    if (n_flagged > 0) {
      parts = c(parts, stringr::str_c(
        n_flagged, " record(s) have values parsed from their PDA reports ",
        "that may be incorrect, due either to the parsing or to errors in ",
        "the source document; the `", warnings_column, "` column describes ",
        "each record's issue(s).")) }
    if (!is.null(n_unmatched) && n_unmatched > 0) {
      parts = c(parts, stringr::str_c(
        n_unmatched, " PDA report(s) could not be matched to an ",
        "authoritative FEMA declaration or denial; unmatched reports are ",
        "not included in the returned data.")) }
    if (length(dataset_issues) > 0) {
      parts = c(parts, stringr::str_c(
        "Dataset-wide: ", stringr::str_c(dataset_issues, collapse = " "))) }

    if (length(parts) > 0) {
      warning(stringr::str_c(parts, collapse = " "), call. = FALSE) }

    attr(result, "n_unmatched") = NULL
    result }

  if (!file.exists(file_path) | use_cache == FALSE) {
    if (!is.null(directory_path)) {
      file_paths = list.files(directory_path, recursive = TRUE, full.names = TRUE) %>%
        purrr::keep(~ stringr::str_detect(.x, stringr::regex("pdf$", ignore_case = TRUE)))

      safe_extract = purrr::possibly(purrr::quietly(extract_pda_attributes), otherwise = NULL)
      extraction_results = purrr::map(file_paths, safe_extract)

      failed_files = file_paths[purrr::map_lgl(extraction_results, is.null)]
      if (length(failed_files) > 0) {
        message(stringr::str_c(
          length(failed_files), "/", length(file_paths),
          " files could not be parsed and were skipped: ",
          stringr::str_c(basename(failed_files), collapse = ", "))) }

      successful_results = extraction_results %>% purrr::compact()
      n_with_warnings = successful_results %>% purrr::map_lgl(~ length(.x$warnings) > 0) %>% sum()
      if (n_with_warnings > 0) {
        message(stringr::str_c(
          n_with_warnings, "/", length(file_paths),
          " files produced a parsing warning (extraction still completed for these files).")) }

      pda_df1 = successful_results %>% purrr::map_dfr(~ .x$result)

      pda_df2 = pda_df1 %>%
        correct_duplicate_disaster_numbers() %>%
        drop_impossible_percentages() %>%
        dplyr::mutate(parser_version = pda_parser_version())

      pda_df3 = resolve_requested_flags(pda_df2)
      pda_df4 = check_pda_quality(pda_df3)

      readr::write_csv(pda_df2, file_path)

      return(report_quality(
        finalize(pda_df4), attr(pda_df4, "dataset_issues")))
    } }

  if (use_cache == TRUE && file.exists(file_path)) {
    message("Reading cached preliminary damage assessment data from disk.")
    check_cache_parser_version(file_path)

    pda_df = readr::read_csv(file_path, show_col_types = FALSE) %>%
      correct_duplicate_disaster_numbers() %>%
      resolve_requested_flags() %>%
      check_pda_quality()

    return(report_quality(
      finalize(pda_df), attr(pda_df, "dataset_issues")))
  }

  stop(
    stringr::str_c(
      "Unable to generate preliminary damage assessment data. No cached dataset ",
      "exists at `file_path` (", file_path, ") and no `directory_path` of PDA ",
      "PDFs was given to parse. Run scrape_pda_pdfs() to populate a directory, ",
      "or point `file_path` at an existing cache."),
    call. = FALSE)
}

utils::globalVariables(c(
  "date_match_string", "disaster_number_count", "event_date_determined",
  "event_native_flag", "event_title", "disaster_number",
  "disaster_number_from_text", "approved_count", "event_type", "text",
  "first_date_match_string", "disaster_number_filename", "parser_version",
  "uses_tribal_layout", "filename_lower", "ia_cost_estimate_total",
  "ia_residences_insured_total_percent", "pa_per_capita_impact_countywide",
  "pa_per_capita_impact_countywide_1",
  "pa_per_capita_impact_indicator_countywide",
  "pa_per_capita_impact_indicator_statewide", "pa_primary_impact",
  "base_name", "base_name_count", "needs_hash", "destination_file", "status",
  "share_missing", "state_name", "hazards", "denial_hazards", "denial_id",
  "request_status_date", "declaration_request_date", "declaration_title",
  "requested_incident_types", "current_request_status",
  "declaration_request_type", "incident_name", "fema_declaration_string",
  "declaration_date", "decision", "state", "path", "pda_id",
  "fema_disaster_number", "fema_state_name", "fema_decision",
  "fema_decision_date", "fema_decision_year", "fema_declaration_request_date",
  "fema_declaration_title", "fema_requested_incident_types", "fema_hazards",
  "pda_match_quality",
  "candidate_count", "candidates_per_pda", "candidate_count_resolved",
  "pdas_per_denial", "match_ok", "match_quality", "hazards_agree",
  "shared_hazard_count", "days_from_denial", "report_count", "pda_matched",
  "pa_cost_estimate_total", "cost_estimate_ia_pa_total",
  "pa_threshold_ratio", "denial_date", "request_sentence", "warnings",
  "ih_program_declared", "ia_program_declared", "pa_program_declared",
  "hm_program_declared", "ih_program_requested", "ia_program_requested",
  "pa_program_requested", "hm_program_requested",
  "fema_ihp_declared", "fema_ia_declared", "fema_pa_declared",
  "fema_hm_declared", "fema_ihp_requested", "fema_ia_requested",
  "fema_pa_requested", "fema_hm_requested",
  "narrative_requests_ia",
  "narrative_requests_pa", "ia_from_narrative", "pa_from_narrative",
  "requested_from_narrative", "fiscal_year", "fiscal_year_stated",
  "fiscal_year_determined", "indicator_for_fiscal_year",
  "reports_stating_value", "can_impute", "pda_file", "report_exists",
  "denial_exists", "denial_claimed", "report_already_matched", "usable",
  "linked_denial_id", "shares_a_word"))
