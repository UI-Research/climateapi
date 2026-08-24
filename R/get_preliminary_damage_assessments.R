#' Derive collision-free local filenames for a set of PDA report URLs
#'
#' The last path segment of each URL is percent-decoded and stripped of
#' characters that cannot appear in a Windows filename, which produces a
#' readable local name. That transformation is lossy, so two different URLs can
#' in principle reduce to the same name; where that happens, a short hash of the
#' full URL is appended to each of the colliding names so that both reports are
#' kept rather than one silently overwriting or being skipped past the other.
#'
#' The suffix is a hash of the URL rather than a running `_1`, `_2` counter for
#' two reasons. First, it depends only on the URL, so a given report always
#' resolves to the same filename and the `file.exists()` check in `save_pdf()`
#' still recognizes it as already downloaded on a later run -- a counter
#' assigned in encounter order would change between runs and re-download the
#' whole archive. Second, FEMA's own filenames already use a `_0`/`_1` suffix to
#' distinguish their duplicates (72 of the currently cached files do), so a
#' counter would be indistinguishable from the source naming.
#'
#' Collisions are resolved across the complete set of URLs passed in, so
#' `scrape_pda_pdfs()` must collect every listing page before calling this;
#' resolving against a partial set could assign a name that a later page's URL
#' also claims.
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
        ## drop any query string or fragment, which would otherwise survive into
        ## the filename ("report.pdf?v=2" -> "report?v=2.pdf", which is not a
        ## legal Windows filename and fails to download at all)
        stringr::str_remove("[?#].*$") %>%
        stringr::str_extract("[^/]+$") %>%
        curl::curl_unescape() %>%
        ## anchored so only a trailing extension is removed, not every
        ## occurrence of ".pdf" in the name
        stringr::str_remove(stringr::regex("\\.pdf$", ignore_case = TRUE)) %>%
        stringr::str_replace_all("[<>:\"/\\\\|?*]", "_") %>%
        stringr::str_remove_all("[[:cntrl:]]") %>%
        stringr::str_squish() %>%
        ## decoded "%20" would otherwise leave literal spaces in the path; no
        ## currently cached file contains one, so this does not rename anything
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
    message(
      stringr::str_c(
        sum(destinations1$needs_hash),
        " report URL(s) reduced to a filename already claimed by a different ",
        "URL; a URL hash was appended so that every report is retained. ",
        "Affected: ",
        stringr::str_c(
          destinations1$destination_file[destinations1$needs_hash],
          collapse = ", "))) }

  destinations1 %>% dplyr::select(url, destination_file)
}

#' Download a PDF from a URL and verify that a PDF is what arrived
#'
#' A download is kept only if the resulting file begins with the `%PDF` magic
#' number and exceeds `minimum_bytes`. Anything else -- an HTML error page served
#' with a 200 status, a truncated transfer -- is deleted rather than left on
#' disk. This matters because the cache check is the file's existence: a bad file
#' that stayed would never be retried on a later run, turning one transient
#' failure into a permanent hole in the archive that looks no different from a
#' complete one. Deleting it means the next run downloads it again.
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

  ## otherwise the FEMA domain blocks our requests
  headers = c(
    "User-Agent" = "Mozilla/5.0 (Linux; Android 11; SAMSUNG SM-G973U) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/14.2 Chrome/87.0.4280.141 Mobile Safari/537.36",
    "Accept" = "application/pdf",
    "Accept-Language" = "en-US",
    "Connection" = "keep-alive")

  ## Some listing links are percent-encoded twice: FEMA publishes
  ## "...Luise%25C3%25B1oIndians.pdf", where "%25" is an encoded "%", so the
  ## request asks for a file literally named "Luise%C3%B1o..." and returns 404.
  ## The real filename contains "n" with a tilde, correctly encoded as "%C3%B1".
  ## Undoing one layer of encoding recovers it. Tried only after the URL as
  ## published fails, so a filename that genuinely contains a percent sign is
  ## still fetched correctly on the first attempt.
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
      warning = function(w) FALSE)

    if (downloaded) { break } }

  is_pdf = FALSE
  if (file.exists(destfile) && file.size(destfile) >= minimum_bytes) {
    connection = file(destfile, "rb")
    ## closed immediately rather than via on.exit(): the file.remove() below runs
    ## before function exit, and Windows refuses to delete a file that still has
    ## an open handle, which would leave the bad download in place
    file_header = tryCatch(
      rawToChar(readBin(connection, "raw", 4)),
      finally = close(connection))
    is_pdf = identical(file_header, "%PDF") }

  if (!downloaded || !is_pdf) {
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
#' @description Downloads every PDA report PDF that FEMA publishes into a local
#'   directory, so that `get_preliminary_damage_assessments()` has a complete and
#'   current set of source documents to parse. Run this before regenerating the
#'   dataset; `get_preliminary_damage_assessments()` parses whatever is already on
#'   disk and never fetches anything itself.
#'
#' @details Walks every page of FEMA's PDA report listing at
#' https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports,
#' advancing until a page returns no PDF links, and downloads any report not
#' already present in `cache_directory`. Because the full listing is traversed on
#' every run, the set of files on disk after a successful run is the complete set
#' of reports FEMA publishes -- coverage does not depend on the caller working
#' out which page numbers hold new reports.
#'
#' A page that errors is retried, and exhausting the retries raises an error
#' rather than ending the walk. This matters because a transient failure and a
#' genuinely empty final page are otherwise indistinguishable, and treating the
#' former as the end of the listing would silently truncate the archive.
#'
#' The listing is server-rendered, so the PDF links are present in the HTML that
#' `httr::GET()` returns and no headless browser is needed. An earlier version
#' used `rvest::read_html_live()`, which requires `chromote`; that dependency is
#' not declared by this package and is frequently absent, which made the
#' function fail outright rather than merely run slowly.
#'
#' Note that FEMA's bot protection rejects requests that do not carry a
#' browser-like `User-Agent`, and rejects them from some networks regardless, so
#' a failure here is not necessarily a change to the listing.
#'
#' @param cache_directory The folder where scraped PDFs are written.
#' @param max_pages A guard against an unbounded walk if the listing ever stops
#'   returning empty pages. Raises an error if reached.
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

  ## the same browser-like header the PDF downloads use; without it FEMA
  ## returns 403 rather than the listing
  listing_headers = httr::add_headers(
    "User-Agent" = "Mozilla/5.0 (Linux; Android 11; SAMSUNG SM-G973U) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/14.2 Chrome/87.0.4280.141 Mobile Safari/537.36",
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
          ## anchored to the extension: an unanchored "pdf" match also picks up
          ## links such as "/help-with-pdf-files", which are not reports
          purrr::keep(~ stringr::str_detect(.x, stringr::regex("\\.pdf$", ignore_case = TRUE)))},
        error = function(e) NULL)

      if (!is.null(page_urls)) { break }
      ## exponential rather than linear: FEMA rate-limits a sustained walk and
      ## the cooldown outlasts a few short pauses
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
          max(page_number + 49, page_number), ")\n",
          "or raise delay_seconds / attempts_per_page for a slower, more ",
          "patient walk."),
        call. = FALSE) }

    ## an empty page that loaded successfully is the end of the listing; only
    ## meaningful when walking, since an explicit page list may legitimately
    ## include a page beyond the end
    if (length(page_urls) == 0 && walk_whole_listing) { break }

    urls1 = c(urls1, page_urls)

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
    "Found ", nrow(destinations), " report(s) across ", page_number,
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

  ## Files present locally but no longer listed on the site. FEMA has not been
  ## observed to withdraw a report (as of the 2026-07-30 archive, all 1,356
  ## cached files were still listed), so this is a monitor rather than a known
  ## problem: a withdrawn report would otherwise keep flowing into the dataset
  ## with nothing to indicate it is no longer published.
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

#' Helper function to extract values from the PDAs
#' @param text The inputted string of text
#' @param term1 Where to begin matching
#' @param term2 Where to finish matching
#'
#' @return All the text between term1 and term2, but not including either of the terms themselves
#' @noRd
extract_value = function(text, term1, term2) {

  ## Colons are optional. The reports render these fields as
  ## "<label><footnote digit> <value>" with no colon anywhere -- verified across
  ## all 1,356 cached reports, 2007-2025 -- but the patterns were written as if a
  ## colon were present, so several of them matched nothing at all and their
  ## columns were empty for every row. Making the colon optional matches the real
  ## documents without having to know which labels ever carried one.
  optional_colon = function(term) stringr::str_replace_all(term, ":", ":?")

  term1_grouped = stringr::str_c("(?:", optional_colon(term1), ")")
  term2_grouped = stringr::str_c("(?:", optional_colon(term2), ")")

  ## Both terms are wrapped before being joined. Several call sites pass an
  ## alternation (e.g. "poverty households:|low income households:"), and
  ## unwrapped that composed to "A|B.*C" -- alternation binds loosest, so the
  ## pattern matched a bare label with no value at all rather than either label
  ## followed by a value.
  ##
  ## The span is matched lazily so the value ends at the FIRST occurrence of
  ## term2; a greedy ".*" ran to the LAST occurrence, swallowing every
  ## intervening field whenever a report repeated a label.
  stringr::str_extract(
    text,
    stringr::str_c(term1_grouped, ".*?", term2_grouped)) %>%
    ## anchored, so a term that also occurs inside the value cannot be removed
    ## from the middle of it
    stringr::str_remove(stringr::str_c("^", term1_grouped)) %>%
    stringr::str_remove(stringr::str_c(term2_grouped, "$")) %>%
    stringr::str_squish() %>%
    stringr::str_trim()
}

#' Drop a label's superscript footnote marker from an extracted value
#'
#' `pdftools` renders the superscript footnote numbers that FEMA attaches to
#' several field labels inline with the text, so an extracted value arrives as
#' "7 2.29" rather than "2.29". The marker is removed only when something
#' follows it, so a field whose value genuinely is a small integer -- "Major
#' Damage - 1" -- keeps that value rather than having it stripped as a marker.
#'
#' This runs after extraction rather than inside `extract_value()` because a
#' leading digit is only identifiable as a footnote once the full value span is
#' in hand: consuming an optional digit during matching would eat the real value
#' in the single-integer case.
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

#' Version of the PDA parsing logic
#'
#' Written into every generated dataset as `parser_version` and checked when a
#' cached dataset is read. Increment it whenever a change alters the values this
#' code produces.
#'
#' This exists because the shipped cache was found to have been written by a
#' version of this code that no longer exists in the repository: it held
#' comma-free titles and a fully populated `event_date_determined` that the
#' committed parser could not reproduce, and reading the cache concealed that for
#' as long as nobody regenerated the data. A dataset that cannot be traced to the
#' code that made it cannot be relied on for publication.
#'
#' @return A length-one character version string.
#' @noRd
pda_parser_version = function() { "2.1.0" }

#' Warn when a cached dataset was written by different parsing logic
#'
#' Reads only the header row, so this costs nothing on a large cache.
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
#' outside 0-100. A handful of reports yield values far outside that range
#' (79,186 and 12,778 were observed), which means the extracted text was not the
#' intended figure. These are set to `NA` rather than published, and the count is
#' reported so the loss is visible rather than silent.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @return `pda_df` with out-of-range percentages replaced by `NA`.
#' @noRd
drop_impossible_percentages = function(pda_df) {

  percent_columns = names(pda_df) %>%
    purrr::keep(~ stringr::str_detect(.x, "percent$")) %>%
    purrr::keep(~ is.numeric(pda_df[[.x]]))

  if (length(percent_columns) == 0) { return(pda_df) }

  n_dropped = percent_columns %>%
    purrr::map_dbl(~ sum(pda_df[[.x]] < 0 | pda_df[[.x]] > 100, na.rm = TRUE)) %>%
    sum()

  if (n_dropped > 0) {
    message(
      stringr::str_c(
        n_dropped, " percentage value(s) fell outside 0-100 and were set to NA; ",
        "the text extracted for those fields was not the intended figure.")) }

  pda_df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(percent_columns),
        .fns = ~ dplyr::if_else(.x < 0 | .x > 100, NA_real_, .x)))
}

#' Columns that are not parsed field values
#'
#' Identity and metadata columns, exempt from the whitespace, punctuation, and
#' numeric-coercion steps that every extracted value passes through. Defined once
#' rather than repeated at each cleanup step: the list was previously written out
#' three times, and a column missing from any one of them was silently coerced --
#' stripping ":" from `path` corrupted Windows drive letters, and adding
#' `disaster_number_filename` without updating all three turned it into a double.
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

  ## Outcome is read from FEMA's own filename convention first
  ## ("PDAReport_AppealDenial-KY.pdf", "PDAReport_Denial-GA.pdf", and otherwise
  ## an approval), falling back to the report title. The previous rule scanned
  ## the entire document body and reclassified any report containing the word
  ## "denial" or "denied" anywhere -- including in boilerplate describing the
  ## appeals process -- as an approved appeal. It also keyed on the literal
  ## "Denial Denied", which is an artefact of two title fields running together
  ## rather than a phrase FEMA writes deliberately.
  ##
  ## Only the title is consulted in the fallback, never the whole body, so
  ## process boilerplate cannot change the classification.
  filename_lower = tolower(basename(path))

  ## Both specific tests run before either general one. A report can be filed
  ## under a generic "Denial" filename while its title states "Denial of Appeal",
  ## and checking the filename's general case first misclassified 24 appeal
  ## denials as first-instance denials.
  ## An approved appeal is identified from the report body, not the title: these
  ## reports are titled and named exactly like first-instance approvals (e.g.
  ## PDAReport_FEMA4583DR-MD.pdf) and carry a disaster number, because the appeal
  ## succeeded. The signal is the word "appealed", which across all 1,356 reports
  ## occurs in 115 of them and always within a narrative of an actual appeal
  ## ("Governor Hogan appealed the denial"); 114 of those use the exact phrase
  ## "appealed the denial". No report contains "appeal" without "appealed", so
  ## there is no appeals-process boilerplate for this to misfire on.
  ##
  ## Matching on "denied" or "denial" instead, as an earlier version did, is what
  ## caused misclassification: those words appear in genuine first-instance
  ## approvals for unrelated reasons, such as a partial denial that the governor
  ## then amended rather than appealed (pda_report_fema_dr_4099_pa.pdf).
  ##
  ## The denial classes are settled first, so a report that was itself denied on
  ## appeal cannot reach this branch.
  event_type = dplyr::case_when(
    stringr::str_detect(filename_lower, "appeal[-_ ]?denial")              ~ "appeal_denial",
    stringr::str_detect(text_event_name, "Denial of Appeal|Appeal Denied") ~ "appeal_denial",
    stringr::str_detect(filename_lower, "denial|denied")                   ~ "denial",
    stringr::str_detect(text_event_name, "Denial|Denied")                  ~ "denial",
    stringr::str_detect(text1, stringr::regex("appealed", ignore_case = TRUE)) ~ "appeal_approved",
    TRUE                                                                   ~ "approved")

  ## A report using the tribal layout names its per-capita figures without a
  ## "Statewide"/"Territory" qualifier. This is derived from the document rather
  ## than from title keywords alone, because keyword matching missed 59 reports
  ## that use the tribal wording, and the flag gates a re-extraction branch that
  ## silently changes several Public Assistance values when it is wrong.
  uses_tribal_layout =
    stringr::str_detect(text1, "Per capita impact") &
    !stringr::str_detect(text1, "(Statewide|Territory|Commonwealth|District) per capita impact")

  text_pda_preempted = ""

  if (event_type %in% c("approved", "appeal_approved")) {
    ## the event was so severe that no pda was conducted
    if (stringr::str_detect(text1, "requirement for a joint PDA may be waived")) {
      text_pda_preempted = "requirement for a joint PDA may be waived" }

    ## the main attributes are stored here
    text_primary = text1 %>%
      stringr::str_extract("Summary of Damage Assessment.*") %>%
      stringr::str_remove("The Preliminary Damage Assessment PDA process is a mechanism.*|The preliminary damage assessment PDA process.*") %>%
      stringr::str_squish() %>%
      stringr::str_remove_all("\\uf0b7") %>%
      stringr::str_replace_all("(\\:[0-9]|\\: [0-9] )", ":") }

  if (event_type %in% c("denial", "appeal_denial")) {
    ## the main attributes are stored here
    text_primary = text1 %>%
      stringr::str_extract("Summary of Damage Assessment.*") %>%
      stringr::str_remove("The (P|p)reliminary (D|d)amage (A|a)ssessment PDA process is a mechanism.*|The preliminary damage assessment PDA process.*") %>%
      stringr::str_squish() %>%
      stringr::str_remove_all("\\uf0b7") %>%
      stringr::str_replace_all("(\\:[0-9]|\\: [0-9] )", ":") }

  text = stringr::str_c(text_event_name, text_pda_preempted, text_primary, sep = " ")

  ## the disaster number is parsed from the PDF text (the FEMA-XXXX-DR pattern) as the
  ## primary strategy; filename parsing is only a fallback when that pattern is absent,
  ## since some filenames contain unrelated 4-digit sequences (e.g. embedded dates) that
  ## collide with real disaster numbers from other files
  ## Separators around the number vary across report vintages ("FEMA-4857-DR",
  ## "FEMA 4857 DR", "FEMA4857DR"), so they are matched permissively rather than
  ## requiring hyphens.
  disaster_number_from_text = text0 %>%
    stringr::str_extract(stringr::regex("FEMA[-_ ]?([0-9]{4})[-_ ]?DR", ignore_case = TRUE)) %>%
    stringr::str_extract("[0-9]{4}")

  ## Filename fallback, used only when the report body states no disaster number.
  ##
  ## The number must be anchored to a literal "FEMA...DR" in the *basename*. The
  ## previous fallback took the first four consecutive digits anywhere in the
  ## full path, which fabricated numbers two ways. Dates embedded in filenames
  ## became disaster numbers -- "PDAReportAppealDenial-PA_090903.pdf" yielded
  ## 0909 and "PDAReportDenialNJ_2019.pdf" yielded 2019, which is itself a real
  ## disaster number -- affecting 13 reports, all of them denials, which FEMA
  ## never assigns a number to at all. And because it scanned the whole path
  ## rather than the filename, any four-digit run in a parent directory would
  ## have been picked up for every report beneath it.
  ##
  ## Where neither the text nor the filename names a disaster, the value stays
  ## NA. That is the correct answer for a denied request: no declaration was
  ## made, so no number exists to record.
  disaster_number_from_filename = basename(path) %>%
    stringr::str_extract(stringr::regex("FEMA[-_ ]?([0-9]{4})[-_ ]?DR", ignore_case = TRUE)) %>%
    stringr::str_extract("[0-9]{4}")

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
      ## Word-bounded so "Nation" no longer matches "National", and combined
      ## with the layout test above rather than relying on title keywords alone.
      ## "Cooperative" and the hardcoded disaster number 4844 are dropped: the
      ## former is not tribal-specific and the latter was a patch for a single
      ## report that the layout test now covers.
      event_native_flag = dplyr::if_else(
        stringr::str_detect(
          event_title,
          stringr::regex(
            "\\b(Native|Tribe|Tribes|Tribal|Indians|Nation|Band|Pueblo|Rancheria|Reservation)\\b",
            ignore_case = TRUE)) |
          uses_tribal_layout,
        1, 0),
      ## The dash between the program name and "Not requested" varies across
      ## reports: 222 use an en-dash and 451 a plain hyphen. Matching only the
      ## en-dash, as this did, left those 451 reports flagged as having requested
      ## Individual Assistance when they state the opposite -- a third of the
      ## dataset. Because the flag defaults to 1, every unmatched wording
      ## silently becomes "requested", so the pattern accepts any dash character
      ## (or none) and any surrounding whitespace.
      ia_requested = dplyr::if_else(
        stringr::str_detect(
          text,
          "Individual Assistance\\s*[-\\u2010-\\u2015]?\\s*(N|n)ot\\s*(R|r)equested"),
        0, 1),
      ia_residences_impacted = text %>% extract_value(term1 = "Residences Impacted:", term2 = "Destroyed -"),
      ia_residences_destroyed = text %>% extract_value(term1 = "Destroyed -", term2 = "Major Damage -"),
      ia_residences_major_damage = text %>% extract_value(term1 = "Major Damage -", term2 = "Minor Damage -"),
      ia_residences_minor_damage = text %>% extract_value(term1 = "Minor Damage -", term2 = "Affected -"),
      ia_residences_affected = text %>% extract_value(term1 = "Affected -", term2 = "Percentage of insured residences:"),
      ## term2 lists the labels actually observed to follow this one across the
      ## 1,356 cached reports, most frequent first: low income (799), poverty
      ## (436), Flood (99), elderly (13). It previously named only "Flood", which
      ## follows in 99 reports, so 450 of the 472 reports carrying a real value
      ## here extracted nothing.
      ia_residences_insured_total_percent = text %>% extract_value(
        term1 = "Percentage of insured residences:",
        term2 = "Percentage of low income households|Percentage of poverty households|Percentage of elderly households|Flood"),
      ia_residences_insured_flood_percent = text %>% stringr::str_extract("[0-9]{1,2}\\.[0-9]\\%( Flood|Flood)") %>% stringr::str_remove("Flood") %>% stringr::str_squish(),
      ia_households_poverty_percent = text %>% extract_value(term1 = "Percentage of poverty households:|Percentage of low income households:", term2 = "Percentage of ownership households:|Percentage of elderly households:"),
      ## observed successors: Pre-Disaster Unemployment (521), Total Individual
      ## Assistance cost estimate (152), Disability (4). The single successor
      ## named previously ("Population receiving...") in practice follows this
      ## label rarely, leaving 185 of the 259 real values unextracted.
      ia_households_owner_percent = text %>% extract_value(
        term1 = "Percentage of ownership households:",
        term2 = "Population receiving other government|Pre-Disaster Unemployment|Total Individual Assistance cost estimate|Disability:"),
      ## the label wraps across a line in the source, so the words between
      ## "government" and "SNAP" may be separated by arbitrary whitespace
      ia_population_other_government_assistance_percent = text %>% extract_value(
        term1 = "Population receiving other government\\s+assistance such as SSI and SNAP:",
        term2 = "Pre-Disaster Unemployment|Age 65 and older:|Total Individual Assistance cost estimate"),
      ia_pre_disaster_unemployment_percent = text %>% extract_value(term1 = "Pre-Disaster Unemployment", term2 = "Age 65 and older:"),
      ia_65plus_percent = text %>% extract_value(term1 = "Age 65 and older:", term2 = "Age 18 and under:"),
      ia_18below_percent = text %>% extract_value(term1 = "Age 18 and under:", term2 = "Disability:"),
      ## the parentheses in "(ICC)" are removed from `text1` upstream, so a
      ## pattern requiring them could never match and this column was empty for
      ## every one of the 1,356 cached reports
      ia_disability_percent = text %>% extract_value(term1 = "Disability:", term2 = "IHP Cost to Capacity ICC Ratio"),
      ia_ihp_cost_to_capacity_ratio = text %>% extract_value(term1 = "IHP Cost to Capacity ICC Ratio:", term2 = "Total Individual Assistance cost estimate"),
      ia_cost_estimate_total = text %>% extract_value(term1 = "Total Individual Assistance cost estimate", term2 = "Primary Impact"),
      ## same dash-agnostic form as ia_requested above, so the two flags are
      ## derived identically rather than by two differently-permissive patterns
      pa_requested = dplyr::if_else(
        stringr::str_detect(
          text,
          "Public Assistance\\s*[-\\u2010-\\u2015]?\\s*(N|n)ot\\s*(R|r)equested"),
        0, 1),
      pa_preemptive_declaration = dplyr::if_else(stringr::str_detect(text, "requirement for a joint PDA may be waived"), 1, 0),
      pa_primary_impact = text %>% extract_value(term1 = "Primary Impact", term2 = "Total Public Assistance cost estimate:"),
      ## Tribal reports label this "Per capita impact:" rather than "Statewide
      ## per capita impact:". A tribal branch further down re-extracts these, but
      ## it only fires on event_native_flag, which is keyword-matched from the
      ## report title and misses 59 reports that use the tribal wording. Adding
      ## the bare form here makes extraction independent of that flag. The
      ## capitalised "Per" cannot match inside "Statewide per capita impact:",
      ## so state reports are unaffected.
      pa_cost_estimate_total = text %>% extract_value(
        term1 = "Total Public Assistance cost estimate:",
        term2 = "(Statewide|Territory|Commonwealth|District) per capita impact:|Per capita impact:"),
      pa_per_capita_impact_statewide = text %>% extract_value(term1 = "(Statewide|Territory|Commonwealth) per capita impact", term2 = "(Statewide|Territory|Commonwealth|District) per capita impact indicator"),
      pa_per_capita_impact_indicator_statewide = text %>% extract_value(term1 = "(Statewide|Territory|Commonwealth) per capita impact indicator", term2 = "(Countywide per capita impact|\\$[0-9]{1}\\.[0-9]{1,2} [0-9]{1})"),
      pa_per_capita_impact_countywide = text %>% extract_value(term1 = "Countywide per capita impact", term2 = "Countywide per capita impact indicator"),
      pa_per_capita_impact_indicator_countywide = text %>% extract_value(term1 = "Countywide per capita impact indicator:", term2 = "$"),
      text = text1) %>%
    dplyr::mutate(
      ## Runs first, before any field-specific cleanup: several of the steps
      ## below take the first whitespace-separated token as the value, which is
      ## the footnote marker rather than the number whenever one is present.
      dplyr::across(
        .cols = dplyr::where(is.character) & -c(path, disaster_number, event_type, event_title, text),
        .fns = remove_footnote_marker),
      ## the previous "take everything up to the last space" step is gone: it
      ## existed to trim the oversized span the old term2 ("Flood") produced, and
      ## against a correctly-delimited value such as "42%" it matches nothing and
      ## returns NA
      ia_cost_estimate_total = stringr::str_remove(ia_cost_estimate_total, "Public Assistance"),
      pa_per_capita_impact_indicator_statewide = stringr::str_split(pa_per_capita_impact_indicator_statewide, " ") %>% purrr::map_chr(~ .[1]),
      ## in the case of Samoa, this is the last value
      pa_per_capita_impact_indicator_statewide = dplyr::if_else(
        nchar(pa_per_capita_impact_indicator_statewide) < 3,
        text %>% extract_value(term1 = "Statewide per capita impact indicator", term2 = "$") %>% stringr::str_split(" ") %>% purrr::map_chr(~ .[1]),
        pa_per_capita_impact_indicator_statewide),
      ## the first number in the span, rather than a blind five-character cut:
      ## the truncation happened to suit values of the form "N.NN" and silently
      ## corrupted anything longer or shorter
      pa_per_capita_impact_indicator_countywide = stringr::str_extract(
        pa_per_capita_impact_indicator_countywide, "[0-9]+\\.?[0-9]*"),
      ## scoped away from path/disaster_number/event_type/event_title/text (which should
      ## not have %, :, $, or , stripped -- doing so on `path` corrupted the colon in
      ## Windows drive letters, e.g. "C:/...")
      dplyr::across(
        .cols = -dplyr::all_of(non_extracted_columns()),
        .fns = ~ stringr::str_remove_all(.x, "\\%|\\:|\\$|\\,") %>% stringr::str_trim() %>% stringr::str_squish()))

  ## tribes have differently structured PDA report fields
  if (result$event_native_flag == 1) {
    result = result %>%
      dplyr::mutate(
        pa_cost_estimate_total = text %>% extract_value(term1 = "Total Public Assistance cost estimate", term2 = "Per capita impact"),
        pa_per_capita_impact_statewide = text %>% extract_value(term1 = "Per capita impact", term2 = "Per capita impact indicator"),
        pa_per_capita_impact_indicator_statewide = text %>% extract_value(term1 = "Per capita impact indicator", term2 = "$") %>% stringr::str_remove("^8 ")) }

  months = c(
    "January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December") %>%
    stringr::str_c(collapse = "|")
  date_match_string = stringr::str_c("Denied (on |)(", months, ") [0-9]{1,2},? [0-9]{4}")
  first_date_match_string = stringr::str_c("(", months, ") [0-9]{1,2},? [0-9]{4}")
  ## columns that should not have their raw values reformatted/stripped by the
  ## cleanup steps below (e.g. stripping ":" from `path` corrupted Windows drive
  ## letters like "C:/...", and 0/1,356 cached rows then failed to match a file on disk)

  result2 = result %>%
    dplyr::mutate(
      dplyr::across(-dplyr::all_of(non_extracted_columns()), ~ stringr::str_squish(.x) %>% stringr::str_trim()),
      ## A value that begins with FEMA's dash placeholder is a field the report
      ## left blank, and the whole value is discarded rather than having the
      ## dash stripped off the front.
      ##
      ## Stripping it, as this previously did, did not leave an empty value. FEMA
      ## prints an unreported field as a dash and sets the *next* label's
      ## superscript footnote number beside it, which `pdftools` renders inline:
      ## "Age 65 and older: -   0   Age 18 and under:". Removing the dash left
      ## the footnote number, which the numeric coercion below then took as the
      ## measurement. That put 258 impossible zeros into the demographic share
      ## columns -- a 0% pre-disaster unemployment rate, no residents over 65 --
      ## and turned eight cost estimates into $2. Because a blank field is
      ## indistinguishable from a real one once the dash is gone, the error was
      ## invisible in the finished dataset.
      ##
      ## Discarding the whole value is safe because no real value ever follows
      ## the placeholder: across all 1,381 cached reports and every extracted
      ## field, 5,358 values begin with a dash and not one of them contains a
      ## decimal or a number of three or more digits. Each is either empty or a
      ## one- or two-digit footnote marker.
      ##
      ## Any dash character is accepted, matching how `ia_requested` and
      ## `pa_requested` are derived, because the reports use an en-dash and a
      ## plain hyphen interchangeably.
      dplyr::across(
        -dplyr::all_of(non_extracted_columns()),
        ~ dplyr::if_else(
          stringr::str_detect(.x, "^[-\\u2010-\\u2015](\\s|$)"), NA_character_, .x)),
      dplyr::across(-dplyr::all_of(non_extracted_columns()), ~ stringr::str_remove_all(.x, "\\$|\\:|\\,")),
      dplyr::across(-dplyr::all_of(non_extracted_columns()), ~ dplyr::if_else(stringr::str_detect(.x, "^N.A$"), NA_character_, .x)),
      pa_per_capita_impact_countywide_1 = pa_per_capita_impact_countywide %>%
        stringr::str_extract_all("[0-9]{1,4}\\.[0-9]{1,3}"),
      ## guard on length==0 or all-NA rather than is.na(.x): pa_per_capita_impact_countywide_1
      ## is a list-column from str_extract_all(), where a no-match row is character(0) (not
      ## NA -- is.na() on that list silently returns FALSE) and an NA *input* row is a
      ## length-1 NA_character_ (not character(0)); either previously slipped through to
      ## max()/min() on an effectively-empty vector, producing -Inf/Inf instead of NA
      pa_per_capita_impact_countywide_max = pa_per_capita_impact_countywide_1 %>%
        purrr::map_dbl(~ if (length(.x) == 0 || all(is.na(.x))) { NA_real_ } else { .x %>% as.numeric() %>% max(na.rm = TRUE) }),
      pa_per_capita_impact_countywide_min = pa_per_capita_impact_countywide_1 %>%
        purrr::map_dbl(~ if (length(.x) == 0 || all(is.na(.x))) { NA_real_ } else { .x %>% stringr::str_remove_all("\\(|\\)") %>% as.numeric() %>% min(na.rm = TRUE) }),
      ## The determination date is taken from the report title where it states
      ## one, then from an explicit "Denied on <date>" statement, and finally
      ## from the first date printed in the document. That last source carries
      ## most of the coverage: 792 of 1,356 reports name no month anywhere in
      ## their title, and for those the date at the head of the document is the
      ## determination date (verified against the cached dataset, which this
      ## reproduces). Without it this column is NA for well over half the rows,
      ## and any analysis filtering on the date silently loses them.
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
        .fns = ~ stringr::str_split(.x, " ") %>% purrr::map_chr(~ .[1]) %>% as.numeric)) %>%
    dplyr::select(-pa_per_capita_impact_countywide_1) %>%
    dplyr::select(disaster_number, dplyr::matches("^event"), dplyr::matches("^pa"), dplyr::everything())

  return(result2)
}

#' Columns holding a dollar total, and the smallest total that is believable
#'
#' FEMA prints a superscript footnote marker next to several field labels, and
#' `pdftools` renders it inline with the value, so a mis-parsed total arrives as
#' the marker itself -- a number between 1 and 20 -- rather than as a cost. No
#' genuine statewide or tribal cost estimate is that small: the smallest
#' non-zero total in the 2026-07-30 archive is roughly 250,000 dollars. A floor
#' of 1,000 therefore separates a footnote marker from a real total with a wide
#' margin on both sides. Exact zeros are left alone, because a report can
#' legitimately state a zero cost for a program it did not request.
#'
#' @return A named list with `columns` and `floor`.
#' @noRd
pda_dollar_columns = function() {
  list(
    columns = c("pa_cost_estimate_total", "ia_cost_estimate_total"),
    floor = 1000)
}

#' Plausible ranges for FEMA's statutory per capita thresholds
#'
#' These two fields are not estimates but published statutory dollar
#' thresholds, so their values are known in advance and a value outside the
#' range is a mis-extraction rather than an unusual disaster. The statewide
#' threshold has run from 1.24 to 1.94 dollars and the countywide from 3.11 to
#' 4.60 across the whole archive; the ranges below are widened slightly to
#' allow for future indexation. The two are close enough in form that one
#' field's value is readily extracted into the other's column -- a statewide
#' indicator of 3.11 is the countywide threshold in the wrong place -- and the
#' non-overlapping ranges are what make that detectable.
#'
#' @return A named list of two-element numeric vectors.
#' @noRd
pda_indicator_ranges = function() {
  list(
    pa_per_capita_impact_indicator_statewide = c(1.00, 2.50),
    pa_per_capita_impact_indicator_countywide = c(3.00, 5.00))
}

#' Run quality checks over the assembled PDA dataset
#'
#' Every field in this dataset is read out of an unstructured PDF by pattern
#' matching, so a change in how FEMA lays out a report does not raise an error:
#' it silently produces a wrong value or an empty column. These checks look for
#' the shapes that failure takes -- a value that cannot be what the field
#' measures, a footnote marker standing in for a dollar total, a column that
#' matched nothing at all -- and report them, so that a broken pattern is
#' visible when the dataset is built rather than after it has been published.
#'
#' Definite errors are raised as a single `warning()` listing every problem
#' found. Descriptive counts that need a human judgment rather than a rule --
#' the missingness of each field among the reports that should state it -- are
#' reported with `message()`.
#'
#' Nothing is modified. Values are left as parsed so that a problem can be
#' investigated against the source report, rather than being replaced by `NA`
#' and losing the evidence.
#'
#' @param pda_df A dataframe of extracted PDA records.
#' @return `pda_df`, invisibly and unchanged.
#' @noRd
check_pda_quality = function(pda_df) {

  ## a value this many times the median of a column's non-zero values is
  ## reported as an outlier. The archive's most extreme genuine values sit
  ## around 60 times their column median (a 537 million dollar Public
  ## Assistance estimate against an 8.6 million median), so 100 flags the
  ## mis-parses without reporting the largest real disasters.
  outlier_multiple = 100

  ## a column emptier than this among the reports that should state it is
  ## treated as a broken extraction pattern rather than as sparse reporting.
  ## Set high deliberately: several fields are genuinely absent from 80-90% of
  ## reports because older report layouts do not contain them at all.
  empty_column_threshold = 0.95

  issues = character(0)
  note = function(...) { issues <<- c(issues, stringr::str_c(...)) }

  has = function(column) { column %in% names(pda_df) && is.numeric(pda_df[[column]]) }

  ## counts, dollars, percentages, and ratios; the 0/1 flags and the identity
  ## columns are excluded because the checks below do not describe them
  flag_columns = c(
    "event_native_flag", "ia_requested", "pa_requested",
    "pa_preemptive_declaration")

  measure_columns = names(pda_df) %>%
    purrr::discard(~ .x %in% c(non_extracted_columns(), flag_columns, "parser_version")) %>%
    purrr::keep(~ is.numeric(pda_df[[.x]]))

  percent_columns = measure_columns %>% purrr::keep(~ stringr::str_detect(.x, "percent$"))

  ## reports the rows behind a count, so a problem can be traced back to the
  ## source PDF rather than only counted
  example_reports = function(is_problem, limit = 3) {
    paths = pda_df$path[which(is_problem)]
    if (length(paths) == 0) { return("") }
    stringr::str_c(
      " (e.g. ",
      stringr::str_c(basename(utils::head(paths, limit)), collapse = ", "),
      ")") }

  ## 1. Values that are not finite numbers. Inf and -Inf are what max() and
  ## min() return over an empty vector, so they mark a row where nothing was
  ## extracted rather than a genuinely extreme value, and they propagate
  ## silently through any later arithmetic.
  purrr::walk(measure_columns, function(column) {
    x = pda_df[[column]]
    n_bad = sum(!is.na(x) & !is.finite(x))
    if (n_bad > 0) {
      note(
        n_bad, " value(s) of ", column, " are Inf or NaN rather than numbers",
        example_reports(!is.na(x) & !is.finite(x)),
        ". These are usually the result of summarising an empty set of matches ",
        "and should be NA.") } })

  ## 2. Negative values. Every measure here is a count, a dollar amount, a
  ## share, or a ratio, none of which can fall below zero.
  purrr::walk(measure_columns, function(column) {
    x = pda_df[[column]]
    is_negative = !is.na(x) & is.finite(x) & x < 0
    if (any(is_negative)) {
      note(
        sum(is_negative), " value(s) of ", column, " are negative (minimum ",
        min(x[is_negative]), ")", example_reports(is_negative),
        ", which this field cannot be.") } })

  ## 3. Percentages outside 0-100. drop_impossible_percentages() sets these to
  ## NA when the dataset is generated, so anything found here came from a
  ## cached file written before that step existed.
  purrr::walk(percent_columns, function(column) {
    x = pda_df[[column]]
    is_impossible = !is.na(x) & is.finite(x) & (x < 0 | x > 100)
    if (any(is_impossible)) {
      note(
        sum(is_impossible), " value(s) of ", column, " fall outside 0-100 ",
        "(maximum ", max(x[is_impossible]), ")", example_reports(is_impossible),
        ". The text extracted for these was not the intended figure.") } })

  ## 4. Dollar totals small enough to be a footnote marker rather than a cost.
  dollars = pda_dollar_columns()
  purrr::walk(dollars$columns, function(column) {
    if (!has(column)) { return(invisible(NULL)) }
    x = pda_df[[column]]
    is_too_small = !is.na(x) & is.finite(x) & x > 0 & x < dollars$floor
    if (any(is_too_small)) {
      note(
        sum(is_too_small), " value(s) of ", column, " are under $",
        dollars$floor, " (observed: ",
        stringr::str_c(sort(unique(x[is_too_small])), collapse = ", "), ")",
        example_reports(is_too_small),
        ". A total this small is almost certainly the label's footnote number ",
        "rather than a cost estimate.") } })

  ## 5. Statutory per capita thresholds outside their published range, which
  ## most often means the statewide and countywide values were swapped.
  purrr::iwalk(pda_indicator_ranges(), function(range, column) {
    if (!has(column)) { return(invisible(NULL)) }
    x = pda_df[[column]]
    is_outside = !is.na(x) & is.finite(x) & (x < range[1] | x > range[2])
    if (any(is_outside)) {
      note(
        sum(is_outside), " value(s) of ", column, " fall outside the published ",
        "range of ", range[1], "-", range[2], " dollars (observed: ",
        stringr::str_c(sort(unique(x[is_outside])), collapse = ", "), ")",
        example_reports(is_outside),
        ". This field is a statutory threshold, so a value outside that range ",
        "is a mis-extraction -- commonly the other geography's threshold.") } })

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

    if (any(exceeds_total)) {
      note(
        sum(exceeds_total), " report(s) have damage categories summing to more ",
        "than their stated total of impacted residences",
        example_reports(exceeds_total),
        ". At least one of the five figures was read from the wrong field.") } }

  ## 7. Individual and Public Assistance values recorded for a program the
  ## report states was not requested. These cannot both be true, and the usual
  ## cause is the "Not Requested" wording changing so the flag is wrong.
  ##
  ## The two statutory per capita thresholds are excluded. They are reference
  ## values FEMA prints in every report's summary table whether or not Public
  ## Assistance was requested -- a report reading "Public Assistance - Not
  ## requested" still states "Statewide per capita impact indicator: $1.29" --
  ## so their presence says nothing about what was requested. Counting them
  ## reported 148 contradictions where only 7 rows held an actual Public
  ## Assistance measurement.
  purrr::iwalk(
    list(ia_requested = "^ia_", pa_requested = "^pa_"),
    function(prefix, flag) {
      if (!has(flag)) { return(invisible(NULL)) }
      program_columns = measure_columns %>%
        purrr::keep(~ stringr::str_detect(.x, prefix)) %>%
        purrr::discard(~ .x %in% names(pda_indicator_ranges()))
      if (length(program_columns) == 0) { return(invisible(NULL)) }

      has_any_value = program_columns %>%
        purrr::map(~ !is.na(pda_df[[.x]])) %>%
        purrr::reduce(`|`)

      contradicts = !is.na(pda_df[[flag]]) & pda_df[[flag]] == 0 & has_any_value
      if (any(contradicts)) {
        note(
          sum(contradicts), " report(s) carry values for a program that ", flag,
          " records as not requested", example_reports(contradicts),
          ". Either the flag or the values are wrong.") } })

  ## 8. Demographic shares of exactly zero. No populated jurisdiction has no
  ## residents over 65, no children, nobody in poverty, or no unemployment, so
  ## a zero here is not a credible measurement.
  ##
  ## There are two causes and the check cannot tell them apart, so it reports
  ## the value and leaves the judgment to a person. Most were a field the
  ## report left blank, where the dash placeholder was stripped and the next
  ## label's footnote number read in its place; the cleanup in
  ## `extract_pda_attributes()` now discards those, which removed 255 of the
  ## 258 zeros this check originally found. The three that remain are printed
  ## as "0.00%" in the source document, so the extraction is faithful and the
  ## implausible figure is FEMA's own.
  ##
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
    is_zero = !is.na(x) & x == 0
    if (any(is_zero)) {
      note(
        sum(is_zero), " value(s) of ", column, " are exactly zero",
        example_reports(is_zero),
        ". A share of zero is not a credible value for this measure. Check the ",
        "source report: either it left the field blank and a footnote number ",
        "was read in its place, or it prints a zero that is itself wrong.") } })

  ## 9. Single values far above the rest of their column.
  purrr::walk(measure_columns, function(column) {
    x = pda_df[[column]]
    usable = x[!is.na(x) & is.finite(x) & x > 0]
    if (length(usable) < 30) { return(invisible(NULL)) }
    ceiling_value = stats::median(usable) * outlier_multiple
    is_outlier = !is.na(x) & is.finite(x) & x > ceiling_value
    if (any(is_outlier)) {
      note(
        sum(is_outlier), " value(s) of ", column, " exceed ", outlier_multiple,
        " times the column median (median ", stats::median(usable),
        ", maximum observed ", max(x[is_outlier]), ")",
        example_reports(is_outlier),
        ". Check these against the source reports before publishing.") } })

  ## 10. Disaster numbers that are not four digits. An approved request always
  ## receives one; a denied request never does, so only approvals are checked
  ## for absence.
  if ("disaster_number" %in% names(pda_df)) {
    numbers = as.character(pda_df$disaster_number)
    is_malformed = !is.na(numbers) & !stringr::str_detect(numbers, "^[0-9]{4}$")
    if (any(is_malformed)) {
      note(
        sum(is_malformed), " disaster number(s) are not four digits (",
        stringr::str_c(sort(unique(numbers[is_malformed])), collapse = ", "),
        ")", example_reports(is_malformed), ".") }

    if ("event_type" %in% names(pda_df)) {
      missing_on_approval =
        stringr::str_detect(pda_df$event_type, "approv") & is.na(numbers)
      if (any(missing_on_approval)) {
        note(
          sum(missing_on_approval), " approved report(s) have no disaster ",
          "number", example_reports(missing_on_approval),
          ", though an approved request is always assigned one.") } } }

  ## 11. Determination dates outside the period the archive covers. FEMA's
  ## earliest published report is from 2007, and a date in the future is a
  ## misread year.
  if ("event_date_determined" %in% names(pda_df)) {
    dates = suppressWarnings(as.Date(pda_df$event_date_determined))
    is_implausible =
      !is.na(dates) & (dates < as.Date("2000-01-01") | dates > Sys.Date() + 1)
    if (any(is_implausible)) {
      note(
        sum(is_implausible), " determination date(s) fall outside 2000 to ",
        "today (", stringr::str_c(sort(unique(as.character(dates[is_implausible]))), collapse = ", "),
        ")", example_reports(is_implausible), ".") } }

  ## 12. The same source PDF parsed into more than one row.
  if ("path" %in% names(pda_df)) {
    duplicated_paths = unique(pda_df$path[duplicated(pda_df$path)])
    if (length(duplicated_paths) > 0) {
      note(
        length(duplicated_paths), " source report(s) appear on more than one ",
        "row: ", stringr::str_c(basename(duplicated_paths), collapse = ", "),
        ".") } }

  ## 13. Columns that are empty, or nearly so, among the reports that should
  ## state them. This is the signature of a pattern that no longer matches the
  ## documents, which is otherwise indistinguishable from a field FEMA simply
  ## does not report.
  ##
  ## The denominator is what makes the check meaningful: an Individual
  ## Assistance field is expected to be missing wherever Individual Assistance
  ## was not requested, so only reports that requested the program and were
  ## approved are counted. The same holds for Public Assistance.
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
      note(
        nrow(empty_columns), " column(s) are at least ",
        round(empty_column_threshold * 100), "% missing among the reports that ",
        "requested the program and were approved, which usually means the ",
        "pattern that extracts them no longer matches the documents: ",
        stringr::str_c(
          empty_columns$column, " (",
          round(empty_columns$share_missing * 100, 1), "% of ",
          empty_columns$n_relevant, ")",
          collapse = "; "),
        ".") }

    message(
      "Missingness among reports that requested the program and were approved:\n",
      stringr::str_c(
        "  ", missingness$column, ": ",
        round(missingness$share_missing * 100, 1), "% of ",
        missingness$n_relevant,
        collapse = "\n")) }

  if (length(issues) > 0) {
    warning(
      stringr::str_c(
        length(issues), " quality problem(s) found in the preliminary damage ",
        "assessment data. Values are returned as parsed, so each can be checked ",
        "against its source report:\n",
        stringr::str_c("  ", seq_along(issues), ". ", issues, collapse = "\n")),
      call. = FALSE) }

  invisible(pda_df)
}

#' Correct disaster numbers shared by multiple, genuinely different PDA reports
#'
#' A handful of PDAs carry a typo'd (or, for FEMA's newest filename convention,
#' absent) disaster number, which surfaces as two different reports sharing one
#' `disaster_number`. The number printed in the report body (`FEMA-XXXX`) is
#' authoritative, so for any `disaster_number` duplicated across reports we
#' re-derive it from the text. A lenient `FEMA-XXXX` match is used -- rather than
#' requiring the `-DR` suffix, as the per-file extraction does -- because the cases
#' that slip through to become duplicates are exactly those where `-DR` is missing
#' or mangled in the text. The correction is guarded to non-`NA`, already-duplicated
#' numbers, so it only ever replaces a wrong number and never fills an `NA` (many
#' denied requests never receive an official number, and their filename-derived
#' value is our best guess). Duplication is undetectable per-file, so this must run
#' on the full combined dataset.
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
  ## report, the filename is preferred -- it is the only remaining independent
  ## evidence, and an unclaimed number cannot itself create a new collision.
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

#' Warn about disaster numbers shared by multiple approved PDA reports
#'
#' A disaster should map to exactly one approved PDA. Any `disaster_number` shared
#' by more than one approved report (after `correct_duplicate_disaster_numbers()`)
#' is incorrect -- a typo the text-based recovery could not resolve, or a
#' duplicated source file -- and is surfaced for manual review.
#'
#' @param pda_df A dataframe of extracted PDA records (must contain `event_type`
#'   and `disaster_number`).
#' @return `pda_df`, invisibly and unchanged; called for the side effect of a
#'   `warning()` listing any offending disaster numbers.
#' @noRd
warn_approved_disaster_number_duplicates = function(pda_df) {
  approved_duplicates = pda_df %>%
    dplyr::filter(
      stringr::str_detect(event_type, "approv"),
      !is.na(disaster_number)) %>%
    dplyr::add_count(disaster_number, name = "approved_count") %>%
    dplyr::filter(approved_count > 1)

  if (nrow(approved_duplicates) > 0) {
    warning(
      stringr::str_c(
        dplyr::n_distinct(approved_duplicates$disaster_number),
        " disaster number(s) map to more than one approved PDA report and are ",
        "likely incorrect: ",
        stringr::str_c(
          sort(unique(approved_duplicates$disaster_number)), collapse = ", ")),
      call. = FALSE) }

  invisible(pda_df)
}

#' Get Data from Preliminary Damage Assessments Submitted to FEMA for Disaster Declarations
#'
#' @description Retrieves data extracted from PDF preliminary damage assessment (PDA)
#'   reports submitted to FEMA for disaster declarations.
#'
#' @details Data are extracted from PDF reports hosted at
#'   \url{https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports}.
#'   Owing to the unstructured nature of the source documents, some fields may be incorrect
#'   in the data returned by the function, though significant quality checks have been
#'   implemented in an effort to produce a high-quality dataset.
#'
#'   Before the data are returned -- whether newly generated or read from the cache --
#'   they are checked for the ways that parsing an unstructured PDF fails silently:
#'   values that are not finite numbers, negative counts, percentages outside 0-100,
#'   cost estimates small enough to be a label's footnote number rather than a total,
#'   demographic shares of exactly zero (the signature of a blank field whose footnote
#'   number was read as the value),
#'   statutory per capita thresholds outside their published range, damage categories
#'   summing to more than the stated total of impacted residences, values recorded for
#'   a program the report says was not requested, values far above the rest of their
#'   column, malformed or missing disaster numbers, implausible determination dates,
#'   and columns that are almost entirely empty among the reports that should state
#'   them. Anything found is raised as a single `warning()` naming example source
#'   reports; the values themselves are returned as parsed, so each can be checked
#'   against its PDF. The share of each field that is missing among the reports that
#'   requested the program and were approved is reported with `message()`, since
#'   whether a given rate is a problem is a judgment rather than a rule.
#'
#' @param file_path The file path to the cached dataset, or if there is no cache, the path
#'   at which to cache the resulting data.
#' @param directory_path The path to the directory where PDA PDFs are stored.
#'   These files are not fetched by this function; run [scrape_pda_pdfs()] to
#'   download them and to refresh the archive as FEMA publishes new reports.
#' @param use_cache Boolean. Read the existing dataset stored at `file_path`? If FALSE,
#'   data will be generated anew. Else, if a file exists at `file_path`, this file will be returned.
#'
#' @return A dataframe of preliminary damage assessment reports. Columns include:
#'   \describe{
#'     \item{path}{The local file path to the source PDA PDF.}
#'     \item{disaster_number}{FEMA disaster number.}
#'     \item{event_type}{Type of decision: "approved", "denial", "appeal_approved", or
#'        "appeal_denial". The denial classes are read from FEMA's filename convention and the
#'        report title. "appeal_approved" is read from the report body instead, because an
#'        approved appeal is titled and named exactly like a first-instance approval and carries
#'        a disaster number; what identifies it is a narrative of a denied request that was
#'        subsequently appealed. Both halves of that narrative are required, so ordinary
#'        approvals that merely describe the appeals process are not misclassified.}
#'     \item{event_title}{Title/description of the disaster event.}
#'     \item{event_date_determined}{Date the PDA determination was made.}
#'     \item{event_native_flag}{1 if tribal request, 0 otherwise.}
#'     \item{pa_requested}{1 if Public Assistance was requested, 0 otherwise.}
#'     \item{pa_preemptive_declaration}{1 if the joint PDA requirement was waived due to the severity of the event, 0 otherwise.}
#'     \item{pa_primary_impact}{The primary type of impact described for Public Assistance purposes.}
#'     \item{pa_cost_estimate_total}{Estimated total Public Assistance cost.}
#'     \item{pa_per_capita_impact_statewide}{Statewide (or territory/commonwealth) per capita impact amount.}
#'     \item{pa_per_capita_impact_indicator_statewide}{FEMA's statutory statewide per capita
#'        *threshold* in dollars for the relevant year (observed range 1.24--1.94), not a ratio
#'        and not a "Met"/"Not Met" categorical despite the field's FEMA-assigned name. Compare
#'        it against `pa_per_capita_impact_statewide`, which is the estimated per capita impact
#'        in the same units; the ratio of the two is what indicates whether the threshold was met.}
#'     \item{pa_per_capita_impact_countywide}{Raw text of countywide per capita impact ratios (may list
#'        multiple values across affected counties for a multi-county event).}
#'     \item{pa_per_capita_impact_indicator_countywide}{FEMA's statutory countywide per capita
#'        threshold in dollars (observed range 3.11--4.60), on the same basis as the statewide
#'        indicator above.}
#'     \item{pa_per_capita_impact_countywide_max}{Maximum countywide per capita impact ratio parsed
#'        from `pa_per_capita_impact_countywide`.}
#'     \item{pa_per_capita_impact_countywide_min}{Minimum countywide per capita impact ratio parsed
#'        from `pa_per_capita_impact_countywide`.}
#'     \item{ia_requested}{1 if Individual Assistance was requested, 0 otherwise.}
#'     \item{ia_residences_impacted}{Total residences impacted.}
#'     \item{ia_residences_destroyed}{Number of residences destroyed.}
#'     \item{ia_residences_major_damage}{Number of residences with major damage.}
#'     \item{ia_residences_minor_damage}{Number of residences with minor damage.}
#'     \item{ia_residences_affected}{Number of residences affected (lowest damage category).}
#'     \item{ia_residences_insured_total_percent}{Percentage of impacted residences with any insurance coverage.}
#'     \item{ia_residences_insured_flood_percent}{Percentage of impacted residences with flood insurance coverage.}
#'     \item{ia_households_poverty_percent}{Percentage of households in poverty (or low income,
#'        depending on report vintage).}
#'     \item{ia_households_owner_percent}{Percentage of households that are owner-occupied.}
#'     \item{ia_population_other_government_assistance_percent}{Percentage of the population receiving
#'        other government assistance (e.g. SSI, SNAP).}
#'     \item{ia_pre_disaster_unemployment_percent}{Pre-disaster unemployment rate.}
#'     \item{ia_65plus_percent}{Percentage of the population age 65 and older.}
#'     \item{ia_18below_percent}{Percentage of the population age 18 and under.}
#'     \item{ia_disability_percent}{Percentage of the population with a disability.}
#'     \item{ia_ihp_cost_to_capacity_ratio}{Individuals and Households Program (IHP) Cost to Capacity (ICC) ratio.}
#'     \item{ia_cost_estimate_total}{Estimated total Individual Assistance cost.}
#'     \item{text}{The cleaned text extracted from the PDA PDF used to derive the fields above.}
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
    use_cache = TRUE) {

  if (!file.exists(file_path) | use_cache == FALSE) {
    if (!is.null(directory_path)) {
      file_paths = list.files(directory_path, recursive = TRUE, full.names = TRUE) %>%
        purrr::keep(~ stringr::str_detect(.x, "pdf$"))

      ## isolate per-file parsing failures (rather than letting one bad PDF abort the
      ## entire regeneration) and surface a summary of any parsing warnings, rather than
      ## blanket-suppressing them across the whole batch (which previously hid, among
      ## other things, the -Inf/Inf sentinel bug fixed in extract_pda_attributes())
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

      ## Correct disaster numbers duplicated across genuinely different reports (a
      ## typo'd or missing number colliding two disasters into one), then flag any
      ## approved-report collisions that could not be resolved from the text.
      pda_df2 = pda_df1 %>%
        correct_duplicate_disaster_numbers() %>%
        drop_impossible_percentages() %>%
        dplyr::mutate(parser_version = pda_parser_version())

      warn_approved_disaster_number_duplicates(pda_df2)
      check_pda_quality(pda_df2)

      readr::write_csv(pda_df2, file_path)

      return(pda_df2)
    } }

  if (use_cache == TRUE) {
    message("Reading cached preliminary damage assessment data from disk.")
    check_cache_parser_version(file_path)
    ## self-heal caches written before the duplicate-correction logic existed, and
    ## surface any approved-report collisions that remain
    pda_df = readr::read_csv(file_path) %>% correct_duplicate_disaster_numbers()
    warn_approved_disaster_number_duplicates(pda_df)
    ## also run on the cached path: a cache written by older parsing logic can
    ## carry problems the current code no longer produces, and reading it must
    ## not present them as clean
    check_pda_quality(pda_df)
    return(pda_df)
  }

  stop("Unable to generate preliminary damage assessment data; ensure specified file and/or directory paths are valid.")
}

utils::globalVariables(c(
  "event_title_pda", "pa_per_capita_impact_indicator_statewide_pda", "pa_per_capita_impact_statewide_pda" ,
  "pa_program_declared_openfema", "pci", "pci_threshold_current",
  "project_amount_federal_share_no_administrative_costs",
  "project_amount_total_no_administrative_costs", "public_assistance",
  "text_pda", "tribal_request_openfema", "federal_cost_share_rate", "funding_lost_flag_any",
  "funding_lost_flag_cost_share", "funding_lost_flag_pci", "funding_lost_flag_pci_snowstorm",
  "funding_lost_flag_snowstorm", "date_match_string", "declaration_date_openfema",
  "disaster_number_count", "event_date_determined", "event_native_flag", "event_title",
  "disaster_number", "disaster_number_from_text", "approved_count", "event_type", "text",
  "first_date_match_string", "disaster_number_filename", "parser_version",
  "uses_tribal_layout", "filename_lower",
  "ia_cost_estimate_total", "ia_residences_insured_total_percent",
  "pa_per_capita_impact_countywide", "pa_per_capita_impact_countywide_1",
  "pa_per_capita_impact_indicator_countywide", "pa_per_capita_impact_indicator_statewide",
  "pa_primary_impact", "base_name", "base_name_count", "needs_hash", "destination_file",
  "status", "share_missing"))
