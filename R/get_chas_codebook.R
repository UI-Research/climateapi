#' Obtain a codebook for HUD CHAS variables
#'
#' @description Reads the CHAS data dictionary that ships alongside the data and returns
#'   it as a tibble: one row per variable, giving the descriptive column name that
#'   [get_chas_housing_affordability()] assigns, the source's original variable code, and
#'   a plain-English definition. Use it to find the variables you want before pulling
#'   data, and to interpret the columns you get back.
#'
#' @param end_year The last year of the five-year ACS period, from 2009 to 2021. Defaults
#'   to 2021 (the most recent period with a published data dictionary). If no dictionary
#'   for this period is on disk, the most recent one that is available is used instead
#'   and a warning reports the substitution.
#' @param directory_path The directory containing the CHAS data dictionaries. Defaults to
#'   the CHAS folder under the C&C Box path.
#'
#' @details CHAS variables are organized into numbered tables, each of which
#'   cross-tabulates households by a different combination of characteristics (tenure,
#'   income relative to HUD Area Median Family Income, cost burden, race and ethnicity,
#'   household type, and so on). Each variable's definition is published as a hierarchy of
#'   up to five nested clauses, which are collapsed here into a single sentence
#'   (`definition`) and a single snake_case name (`column_name`).
#'
#'   Because those clauses repeat the same long phrases in nearly every variable, the
#'   name uses the abbreviations below. `definition` always keeps the source's full
#'   wording, so use it whenever a name is unclear.
#'   \describe{
#'     \item{`lte`, `gt`}{"less than or equal to" and "greater than", as in `income_gt_30_lte_50_hamfi` for household income above 30% and at most 50% of HAMFI.}
#'     \item{`hh`}{household, as in `hh_type` and `hh_size`.}
#'     \item{`unit_problems`}{the source's "4 housing unit problems": cost burden above 30% of income, more than one person per room, and incomplete kitchen or plumbing facilities.}
#'     \item{`severe_unit_problems`}{the source's "4 severe housing unit problems", a stricter version of the same four conditions (cost burden above 50% of income, more than 1.5 persons per room). This is a distinct measure from `unit_problems`, so the two are never collapsed into one name.}
#'     \item{`facilities`}{"complete kitchen and plumbing facilities"; `lacks_facilities` is the unit lacking them.}
#'     \item{`ppr`}{persons per room, the source's overcrowding measure, as in `gt_1_ppr`.}
#'     \item{`nocalc`}{cost burden not computed, which the source reports for households with zero or negative income.}
#'     \item{`1ormore`, `none`}{"has one or more of" and "has none of", used for the housing-problem, age, and disability breakdowns.}
#'     \item{`nh`, `pi`, `aian`}{non-Hispanic, Pacific Islander, and American Indian or Alaska Native.}
#'   }
#'
#'   `column_name` is unique within a CHAS table but not across tables -- every table has
#'   its own "owner occupied" total, for example -- so filter to one `chas_table` when a
#'   unique key is needed. [get_chas_housing_affordability()] handles this by only
#'   renaming columns whose descriptive name is unambiguous among the columns it returns.
#'
#'   Only estimate variables are described. The source's margin-of-error variables
#'   (`_moe` rather than `_est`) share their estimate's definition and so would not have a
#'   distinct name; they are omitted here and keep their original names in the data.
#'
#'   HUD publishes one dictionary per five-year release, and the variable codes change
#'   between releases as tables are added or revised. The `vintage` column records which
#'   release the returned definitions come from. When the requested `end_year` has no
#'   dictionary on disk, the most recent available release is used as a stand-in; its
#'   definitions are usually still correct, but codes present in one release and not the
#'   other will be missing or mismatched, so check `vintage` when the warning appears.
#'
#' @return A tibble with one row per CHAS estimate variable and the following columns:
#'   \describe{
#'     \item{`column_name`}{The descriptive snake_case name assigned by [get_chas_housing_affordability()].}
#'     \item{`column_name_source`}{The source's original variable code, e.g. `"T8_est4"`.}
#'     \item{`chas_table`}{The CHAS table the variable belongs to, e.g. `"T8"`.}
#'     \item{`column_type`}{Whether the variable is a `"Total"`, a `"Subtotal"`, or a `"Detail"` line. Totals and subtotals are sums of the lines nested beneath them, so adding across types double counts households.}
#'     \item{`definition`}{The variable's full definition as a single sentence.}
#'     \item{`vintage`}{The last year of the five-year period whose dictionary these definitions were read from. This equals `end_year` unless no dictionary for `end_year` was found.}
#'   }
#'   If no usable data dictionary is found, a message is emitted and a zero-row tibble
#'   with these columns is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' codebook = get_chas_codebook(end_year = 2021)
#'
#' ## find the cost-burden subtotals in table 8 (tenure by income by cost burden)
#' codebook |>
#'   dplyr::filter(chas_table == "T8", column_type == "Subtotal") |>
#'   dplyr::filter(stringr::str_detect(definition, "cost burden"))
#' }
get_chas_codebook = function(
    end_year = 2021,
    directory_path = file.path(
      climateapi::get_box_path(), "built-environment", "hud",
      "comprehensive-housing-affordability-strategies")) {

  if (!is.numeric(end_year) || length(end_year) != 1) {
    stop("`end_year` must be a single numeric year (the last year of a five-year ACS period).") }

  empty_codebook = tibble::tibble(
    column_name = character(),
    column_name_source = character(),
    chas_table = character(),
    column_type = character(),
    definition = character(),
    vintage = numeric())

  ## locate every dictionary on disk and label each with the period it describes (the
  ## folder and file names embed the period as, for example, "2017thru2021"). If none is
  ## found (for example, when the Box mirror is not synced) return an empty codebook
  ## rather than erroring, so that callers can fall back to the source's column names.
  dictionary_paths = directory_path |>
    list.dirs(full.names = TRUE) |>
    list.files(full.names = TRUE) |>
    purrr::keep(~ stringr::str_detect(.x, "dictionary"))

  dictionaries = tibble::tibble(path = dictionary_paths) |>
    dplyr::mutate(
      vintage = path |>
        stringr::str_extract("thru[0-9]{4}") |>
        stringr::str_remove("thru") |>
        as.numeric()) |>
    dplyr::filter(!is.na(vintage))

  if (nrow(dictionaries) == 0) {
    message(
      "No CHAS data dictionary was found under `directory_path`.")
    return(empty_codebook) }

  ## fall back to the most recent dictionary on disk when the requested period has none:
  ## definitions change little between releases, so a stand-in is more useful than no
  ## codebook at all, but the substitution is worth flagging
  codebook_vintage = if (end_year %in% dictionaries$vintage) {
    end_year } else {
    max(dictionaries$vintage) }

  if (codebook_vintage != end_year) {
    warning(
      "No CHAS data dictionary for ", end_year, " was found under `directory_path`; ",
      "using the most recent one available (", codebook_vintage, ") instead. Variable ",
      "codes can differ between releases, so some may be missing or mismatched.") }

  codebook_paths = dictionaries |>
    dplyr::filter(vintage == codebook_vintage) |>
    dplyr::pull(path)

  if (length(codebook_paths) > 1) {
    message(
      "Multiple candidate CHAS data dictionaries found; using the first: ",
      basename(codebook_paths[[1]])) }

  ## the variable definitions live on the dictionary's "All Tables" sheet, but its
  ## position varies by release (sheet 2 through 2014-2018, sheet 4 from 2015-2019 on),
  ## so locate it by name rather than by number
  codebook_sheets = readxl::excel_sheets(codebook_paths[[1]])
  all_tables_sheet = which(
    stringr::str_detect(codebook_sheets, stringr::regex("all tables", ignore_case = TRUE)))

  if (length(all_tables_sheet) == 0) {
    message(
      "The CHAS data dictionary at ", basename(codebook_paths[[1]]),
      " has no \"All Tables\" sheet.")
    return(empty_codebook) }

  codebook_raw = readxl::read_excel(
    codebook_paths[[1]], sheet = all_tables_sheet[[1]], guess_max = 5000) |>
    janitor::clean_names()

  ## the column holding the source's variable codes was renamed from `column_name` to
  ## `column_variable_name` in the 2015-2019 release
  if (!"column_name" %in% names(codebook_raw) && "column_variable_name" %in% names(codebook_raw)) {
    codebook_raw = codebook_raw |>
      dplyr::rename(column_name = column_variable_name) }

  if (!"column_name" %in% names(codebook_raw)) {
    message(
      "The CHAS data dictionary at ", basename(codebook_paths[[1]]),
      " has no recognizable variable-code column.")
    return(empty_codebook) }

  codebook_raw |>
    ## keep only the per-table estimate variables, dropping both the margin-of-error
    ## variables and the handful of identifier rows (source, sumlevel, geoid, ST, CTY,
    ## ...). Earlier releases carried a `file_name` column that made this filter possible;
    ## later ones do not, so filter on the shape of the variable code itself, which is
    ## stable across every release.
    dplyr::filter(stringr::str_detect(column_name, "^T[0-9]{1,2}[A-C]?_est[0-9]+$")) |>
    ## the definition is published as up to five nested clauses, collapsed here both into a
    ## readable sentence and into a single snake_case column name
    tidyr::unite(
      definition,
      description_1, description_2, description_3, description_4, description_5,
      sep = " ", na.rm = TRUE, remove = FALSE) |>
    tidyr::unite(
      column_name_new,
      description_1, description_2, description_3, description_4, description_5,
      sep = "_", na.rm = TRUE, remove = TRUE) |>
    dplyr::mutate(
      definition = definition |> stringr::str_squish(),
      ## the name is built in three passes. First, punctuation and filler words are
      ## stripped so that the remaining words are separated by single underscores.
      column_name_new = column_name_new |>
        stringr::str_to_lower() |>
        stringr::str_squish() |>
        stringr::str_replace_all(c(
          " " = "_",
          ## the asterisk is a footnote marker the source attaches to some category
          ## labels ("family, no spouse*"); left in, it would make the column name
          ## unusable without backticks
          ":|\\/|,|-|\\(|\\)|%|\\*" = "",
          "_in_|_with_|_and_|_is_|_to_|or_|_the_|_per_|_but_|_has_|_of_" = "_",
          "raceethnicity" = "race",
          "has_1_more_4_housing_unit_problems" = "1ormore_housing_problems",
          "household_income" = "income",
          "africanamerican" = "",
          "american_indian_alaska_native" = "aian",
          "greater_than" = "_gt",
          "less_than_equal" = "_lte",
          "with_either" = "",
          "\\+" = "plus",
          "__" = "_")) |>
        ## Second, the long phrases CHAS repeats in nearly every definition are shortened
        ## to the abbreviations listed under Details. These run in order, and several
        ## depend on an earlier one having fired -- "4_severe_unit_problems" only exists
        ## once "housing_problems" has become "unit_problems", for example -- so the
        ## sequence matters.
        stringr::str_replace_all(c(
          "less_than_equal" = "lte",
          "more_than" = "gt",
          "american_indian_alaska_native" = "aian",
          "person_room" = "ppr",
          "nonhispanic" = "nh",
          "pacific_islander" = "pi",
          "has_1_more" = "1ormore",
          "1_more" = "1ormore",
          "housing_unit" = "unit",
          "housing_problems" = "unit_problems",
          "the_4_unit_problems" = "unit_problems",
          "4_severe_housing_problems" = "severe_unit_problems",
          "kitchen_plumbing" = "facilities",
          "complete_plumbing_kitchen_facilities" = "facilities",
          "has_none" = "none",
          "household" = "hh",
          "_the_" = "_",
          "small_family" = "",
          "large_family" = "",
          "contains_at_least_1_person" = "1ormore",
          "contains_no_one" = "none",
          "household_member_a_hearing_vision_impairment" = "1ormore_hearing_vision",
          "household_member_an_ambulatory_limitation" = "1ormore_ambulatory",
          "household_member_a_cognitive_limitation" = "1ormore_cognitive",
          "household_member_a_selfcare_independent_living_limitation" = "1ormore_indpendent_living",
          "household_member_none_the_above_limitations" = "none_limitations",
          "all_all_all" = "all",
          "not_computed" = "nocalc",
          "all_all" = "all",
          "__" = "_",
          "none_unit_problems_cost_burden_nocalc_none_or_3_unit_problems" = "none_unit_problems_or_cost_burden_nocalc",
          "none_4_severe_unit_problems_cost_burden_nocalc_none_or_3_severe_unit_problems" = "none_severe_unit_problems_or_cost_burden_nocalc",
          "4_severe_unit_problems" = "severe_unit_problems",
          "housing_cost_burden" = "cost_burden",
          "unit_lacks_facilities" = "lacks_facilities",
          "unit_facilities" = "facilities",
          "persons_room" = "ppr",
          "at_least_one" = "1ormore",
          "cannot_be_computed" = "nocalc",
          "cost_burden_cannot_be_computed_none_3_other_unit_problems" = "none_unit_problems_or_cost_burden_nocalc",
          "_between_" = "",
          "_built_" = "",
          "_contains_" = "",
          "facilities_facilities" = "facilities",
          "vacantforsale" = "vacant_for_sale",
          "vacantforrent" = "vacant_for_rent",
          "has_complete" = "",
          "either_both" = "1ormore")) |>
        ## Third, tidy up the seams: deleting a phrase leaves the underscores that
        ## surrounded it, which would otherwise show up as runs of two or more
        stringr::str_replace_all("_{2,}", "_") |>
        stringr::str_remove_all("^_|_$"),
      chas_table = stringr::str_extract(column_name, "^T[0-9]{1,2}[A-C]?"),
      column_name_source = column_name) |>
    dplyr::filter(!is.na(column_name_new), column_name_new != "") |>
    dplyr::transmute(
      column_name = column_name_new,
      column_name_source,
      chas_table,
      column_type,
      definition,
      vintage = as.numeric(codebook_vintage)) |>
    dplyr::distinct()
}

utils::globalVariables(c(
  "column_name", "column_name_new", "column_variable_name", "column_type", "chas_table",
  "definition", "description_1", "description_2", "description_3", "description_4",
  "description_5", "path", "vintage"))
