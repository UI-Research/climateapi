#' Download a PDF from a URL
#'
#' @param url URL to the pdf
#' @param directory The folder where the PDF should be saved
#'
#' @return Nothing; side-effect only.
#' @noRd
save_pdf = function(url, directory) {

  tryCatch({
    ## otherwise the FEMA domain blocks our requests
    headers = c(
      "User-Agent" = "Mozilla/5.0 (Linux; Android 11; SAMSUNG SM-G973U) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/14.2 Chrome/87.0.4280.141 Mobile Safari/537.36",
      "Accept" = "application/pdf",
      "Accept-Language" = "en-US",
      "Connection" = "keep-alive")

    ## pull out the last piece of the PDF url
    destfile = file.path(
      directory,
      stringr::str_c(
        stringr::str_split(url, "/") |>
          purrr::map_chr(~ .x[length(.x)]) |>
          stringr::str_remove_all("\\%|\\.pdf"),
        ".pdf"))

    if (!file.exists(destfile)) {
      utils::download.file(
        url = url,
        destfile = destfile,
        headers = headers,
        mode = "wb") }},
    error = function(e) { print(stringr::str_c("Error downloading PDF")) })
}

#' Save Preliminary Damage Assessment (PDA) Reports to Disk for Processing
#'
#' To determine the correct page range, review https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports
#' and identify the corresponding page numbers that reflect PDAs not already downloaded
#' to disk.
#'
#' @param pages The website page ranges that contain as-yet-undownloaded PDA reports.
#'
#' @return Nothing. Scraped PDFs are written to `cache_directory`.
#' @noRd
scrape_pda_pdfs = function(
    pages,
    cache_directory = file.path(climateapi::get_box_path(), "hazards", "urban", "preliminary-damage-assessments", "pdfs")) {

  base_url = "https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports?page="

  options(timeout = 200)
  urls1 = purrr::map(
    c(0:1),
    function(page_number) {
      tryCatch({
        page = rvest::read_html_live(stringr::str_c(base_url, page_number))

        pda_reports = page |>
          rvest::html_elements("body") |>
          rvest::html_elements("a") |>
          rvest::html_attr("href") |>
          purrr::discard(~ is.na(.x)) |>
          purrr::keep(~ stringr::str_detect(.x, "pdf"))

        Sys.sleep(sample(x = 1:3, replace = TRUE, size = 1))

        return(pda_reports)},
        error = function(e) {
          print("Some error, likely timeout")
        })
    }) |>
    unlist()

  urls2 = stringr::str_c("https://www.fema.gov", urls1)

  purrr::walk(urls2, ~ save_pdf(url = .x, directory = cache_directory))
}

#' Helper function to extract values from the PDAs
#' @param text The inputted string of text
#' @param term1 Where to begin matching
#' @param term2 Where to finish matching
#'
#' @return All the text between term1 and term2, but not including either of the terms themselves
#' @noRd
extract_value = function(text, term1, term2) {
  stringr::str_extract(text, stringr::str_c(term1, ".*", term2)) %>%
    stringr::str_remove(term1) %>%
    stringr::str_remove(term2) %>%
    stringr::str_squish() %>%
    stringr::str_trim()
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

  event_type = "approved"
  if (stringr::str_detect(text1, "Denial of Appeal")) { event_type = "appeal_denial" }
  if (stringr::str_detect(text1, "Denial Denied")) { event_type = "denial" }
  if (event_type == "approved" & stringr::str_detect(text1, "Denial|denial|Denied|denied")) {
    event_type = "appeal_approved" }

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
  result = tibble::tibble(
      path = path,
      disaster_number = path %>% stringr::str_extract("[0-9]{4}"),
      event_type = event_type,
      event_title = text_event_name,
      event_native_flag = dplyr::if_else(
        stringr::str_detect(
          event_title,
          "Native|native|Tribe|tribe|Indians|indians|Nation|nation|Band|band|Cooperative|cooperative|Pueblo|pueblo|4844"), 1, 0),
      ia_requested = dplyr::if_else(stringr::str_detect(text, "Individual Assistance \\u2013 (N|n)ot (R|r)equested"), 0, 1),
      ia_residences_impacted = text %>% extract_value(term1 = "Residences Impacted:", term2 = "Destroyed -"),
      ia_residences_destroyed = text %>% extract_value(term1 = "Destroyed -", term2 = "Major Damage -"),
      ia_residences_major_damage = text %>% extract_value(term1 = "Major Damage -", term2 = "Minor Damage -"),
      ia_residences_minor_damage = text %>% extract_value(term1 = "Minor Damage -", term2 = "Affected -"),
      ia_residences_affected = text %>% extract_value(term1 = "Affected -", term2 = "Percentage of insured residences:"),
      ia_residences_insured_total_percent = text %>% extract_value(term1 = "Percentage of insured residences:", term2 = "Flood"),
      ia_residences_insured_flood_percent = text %>% stringr::str_extract("[0-9]{1,2}\\.[0-9]\\%( Flood|Flood)") %>% stringr::str_remove("Flood") %>% stringr::str_squish(),
      ia_households_poverty_percent = text %>% extract_value(term1 = "Percentage of poverty households:|Percentage of low income households:", term2 = "Percentage of ownership households:|Percentage of elderly households:"),
      ia_households_owner_percent = text %>% extract_value(term1 = "Percentage of ownership households:", term2 = "Population receiving other government assistance such as SSI and SNAP:"),
      ia_population_other_government_assistance_percent = text %>% extract_value(term1 = "Population receiving other government assistance such as SSI and SNAP:", term2 = "Pre-Disaster Unemployment"),
      ia_pre_disaster_unemployment_percent = text %>% extract_value(term1 = "Pre-Disaster Unemployment", term2 = "Age 65 and older:"),
      ia_65plus_percent = text %>% extract_value(term1 = "Age 65 and older:", term2 = "Age 18 and under:"),
      ia_18below_percent = text %>% extract_value(term1 = "Age 18 and under:", term2 = "Disability:"),
      ia_disability_percent = text %>% extract_value(term1 = "Disability:", term2 = "IHP Cost to Capacity \\(ICC\\) Ratio: "),
      ia_ihp_cost_to_capacity_ratio = text %>% extract_value(term1 = "IHP Cost to Capacity ICC Ratio: ", term2 = "Total Individual Assistance cost estimate"),
      ia_cost_estimate_total = text %>% extract_value(term1 = "Total Individual Assistance cost estimate", term2 = "Primary Impact"),
      pa_requested = dplyr::if_else(stringr::str_detect(text, "Public Assistance . (N|n)ot (R|r)equested|Public Assistance - Not requested"), 0, 1),
      pa_preemptive_declaration = dplyr::if_else(stringr::str_detect(text, "requirement for a joint PDA may be waived"), 1, 0),
      pa_primary_impact = text %>% extract_value(term1 = "Primary Impact", term2 = "Total Public Assistance cost estimate:"),
      pa_cost_estimate_total = text %>% extract_value(term1 = "Total Public Assistance cost estimate:", term2 = "(Statewide|Territory|Commonwealth|District) per capita impact:"),
      pa_per_capita_impact_statewide = text %>% extract_value(term1 = "(Statewide|Territory|Commonwealth) per capita impact", term2 = "(Statewide|Territory|Commonwealth|District) per capita impact indicator"),
      pa_per_capita_impact_indicator_statewide = text %>% extract_value(term1 = "(Statewide|Territory|Commonwealth) per capita impact indicator", term2 = "(Countywide per capita impact|\\$[0-9]{1}\\.[0-9]{1,2} [0-9]{1})"),
      pa_per_capita_impact_countywide = text %>% extract_value(term1 = "Countywide per capita impact", term2 = "Countywide per capita impact indicator"),
      pa_per_capita_impact_indicator_countywide = text %>% extract_value(term1 = "Countywide per capita impact indicator:", term2 = "$"),
      text = text1) %>%
    dplyr::mutate(
      ia_residences_insured_total_percent = stringr::str_extract(ia_residences_insured_total_percent, ".* "),
      ia_cost_estimate_total = stringr::str_remove(ia_cost_estimate_total, "Public Assistance"),
      pa_per_capita_impact_indicator_statewide = stringr::str_split(pa_per_capita_impact_indicator_statewide, " ") %>% purrr::map_chr(~ .[1]),
      ## in the case of Samoa, this is the last value
      pa_per_capita_impact_indicator_statewide = dplyr::if_else(
        nchar(pa_per_capita_impact_indicator_statewide) < 3,
        text %>% extract_value(term1 = "Statewide per capita impact indicator", term2 = "$") %>% stringr::str_split(" ") %>% purrr::map_chr(~ .[1]),
        pa_per_capita_impact_indicator_statewide),
      pa_per_capita_impact_indicator_countywide = stringr::str_sub(pa_per_capita_impact_indicator_countywide, 1, 5),
      dplyr::across(dplyr::everything(), ~ stringr::str_remove_all(.x, "\\%|\\:|\\$|\\,") %>% stringr::str_trim() %>% stringr::str_squish()),
      dplyr::across(dplyr::everything(), ~ stringr::str_remove_all(.x, "\\%|\\:|\\$|\\,") %>% stringr::str_trim() %>% stringr::str_squish()))

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
  date_match_string = stringr::str_c("Denied (on |)(", months, ") [0-9]{1,2} [0-9]{4}")
  result2 = result %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ stringr::str_squish(.x) %>% stringr::str_trim()),
      dplyr::across(dplyr::everything(), ~ stringr::str_remove_all(.x, "^- |\\$|\\:|\\,")),
      dplyr::across(dplyr::everything(), ~ dplyr::if_else(.x == "-", NA_character_, .x)),
      dplyr::across(dplyr::everything(), ~ dplyr::if_else(stringr::str_detect(.x, "^N.A$"), NA_character_, .x)),
      pa_per_capita_impact_countywide_1 = pa_per_capita_impact_countywide %>%
        stringr::str_extract_all("[0-9]{1,4}\\.[0-9]{1,3}"),
      pa_per_capita_impact_countywide_max = dplyr::if_else(
        is.na(pa_per_capita_impact_countywide_1), NA,
        pa_per_capita_impact_countywide_1 %>%
          purrr::map_dbl(~ .x %>% as.numeric %>% max(na.rm = TRUE))),
      pa_per_capita_impact_countywide_min = dplyr::if_else(
        is.na(pa_per_capita_impact_countywide_1), NA,
        pa_per_capita_impact_countywide_1 %>%
          purrr::map_dbl(~ .x %>% stringr::str_remove_all("\\(|\\)") %>% as.numeric %>% min(na.rm = TRUE))),
      event_date_determined = event_title %>% date_string_to_date,
      event_date_determined = dplyr::if_else(
        is.na(event_date_determined),
        stringr::str_extract(text, date_match_string) %>% stringr::str_remove("Denied (on |)") %>% date_string_to_date,
        event_date_determined),
      dplyr::across(
        .cols = -c(path, disaster_number, event_title, event_type, event_date_determined,
                   event_native_flag, pa_per_capita_impact_countywide, pa_primary_impact, text),
        .fns = ~ stringr::str_split(.x, " ") %>% purrr::map_chr(~ .[1]) %>% as.numeric),
      id = dplyr::row_number()) %>%
    dplyr::add_count(disaster_number, name = "disaster_number_count") %>%
    dplyr::mutate(
      ## a very limited number of cases have typos in the event number in their URLs
      ## we correct by extracting these directly from the text of the PDA
      disaster_number = dplyr::if_else(
        disaster_number_count > 1,
        text %>% extract_value("FEMA-", "-DR"),
        disaster_number)) %>%
    dplyr::select(-c(pa_per_capita_impact_countywide_1, id, disaster_number_count)) %>%
    dplyr::select(disaster_number, dplyr::matches("^event"), dplyr::matches("^pa"), dplyr::everything())

  return(result2)
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
#' @param file_path The file path to the cached dataset, or if there is no cache, the path
#'   at which to cache the resulting data.
#' @param directory_path The path to the directory where PDA PDFs are stored. Use
#'   `scrape_pda_pdfs` to generate these files.
#' @param use_cache Boolean. Read the existing dataset stored at `file_path`? If FALSE,
#'   data will be generated anew. Else, if a file exists at `file_path`, this file will be returned.
#'
#' @return A dataframe of preliminary damage assessment reports. Key columns include:
#'   \describe{
#'     \item{disaster_number}{FEMA disaster number.}
#'     \item{event_type}{Type of decision: "approved", "denial", "appeal_approved", or "appeal_denial".}
#'     \item{event_title}{Title/description of the disaster event.}
#'     \item{event_date_determined}{Date the PDA determination was made.}
#'     \item{event_native_flag}{1 if tribal request, 0 otherwise.}
#'     \item{ia_requested}{1 if Individual Assistance was requested, 0 otherwise.}
#'     \item{ia_residences_impacted}{Total residences impacted.}
#'     \item{ia_residences_destroyed}{Number of residences destroyed.}
#'     \item{ia_residences_major_damage}{Number of residences with major damage.}
#'     \item{ia_residences_minor_damage}{Number of residences with minor damage.}
#'     \item{ia_cost_estimate_total}{Estimated total Individual Assistance cost.}
#'     \item{pa_requested}{1 if Public Assistance was requested, 0 otherwise.}
#'     \item{pa_cost_estimate_total}{Estimated total Public Assistance cost.}
#'     \item{pa_per_capita_impact_statewide}{Statewide per capita impact amount.}
#'     \item{pa_per_capita_impact_indicator_statewide}{Met/Not Met indicator for statewide threshold.}
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

  suppressWarnings({
    if (!file.exists(file_path) | use_cache == FALSE) {
      if (!is.null(directory_path)) {
        pda_df = list.files(directory_path, recursive = TRUE, full.names = TRUE) %>%
          purrr::keep(~ stringr::str_detect(.x, "pdf$")) %>%
          purrr::map_dfr(extract_pda_attributes)

        readr::write_csv(pda_df, file_path)

        return(pda_df)
      } } })

  if (use_cache == TRUE) {
    message("Reading cached preliminary damage assessment data from disk.")
    pda_df = readr::read_csv(file_path)
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
  "ia_cost_estimate_total", "ia_residences_insured_total_percent",
  "pa_per_capita_impact_countywide", "pa_per_capita_impact_countywide_1",
  "pa_per_capita_impact_indicator_countywide", "pa_per_capita_impact_indicator_statewide",
  "pa_primary_impact"))
