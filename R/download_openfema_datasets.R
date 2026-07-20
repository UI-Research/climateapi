#' @title List available OpenFEMA dataset endpoints
#'
#' @description Queries the OpenFEMA metadata API and returns a tibble of all
#'   available datasets with their names, versions, record counts, and download
#'   URLs by format.
#'
#' @returns A tibble with columns:
#'   \describe{
#'     \item{name}{The API endpoint name (e.g., "DisasterDeclarationsSummaries").}
#'     \item{title}{Human-readable dataset title.}
#'     \item{version}{API version number.}
#'     \item{record_count}{Number of records in the dataset.}
#'     \item{formats}{Comma-separated list of available download formats.}
#'     \item{url_parquet}{Download URL for parquet format (NA if unavailable).}
#'     \item{url_csv}{Download URL for CSV format (NA if unavailable).}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' endpoints <- list_openfema_endpoints()
#' endpoints |> dplyr::filter(stringr::str_detect(name, "Nfip"))
#' }
list_openfema_endpoints <- function() {

  metadata_url <- "https://www.fema.gov/api/open/v1/OpenFemaDataSets"
  response <- jsonlite::fromJSON(metadata_url, simplifyDataFrame = FALSE)

  records <- response[["OpenFemaDataSets"]]
  if (is.null(records)) {
    stop("Unexpected API response structure from OpenFEMA metadata endpoint.")
  }

  rows <- purrr::map(records, function(rec) {
    dist <- rec[["distribution"]]
    if (is.null(dist)) dist <- list()

    format_urls <- purrr::map(dist, function(d) {
      list(format = tolower(d[["format"]] %||% ""), url = d[["accessURL"]] %||% NA_character_)
    })

    all_formats <- purrr::map_chr(format_urls, "format")
    get_url <- function(fmt) {
      match <- purrr::keep(format_urls, ~ .x$format == fmt)
      if (length(match) > 0) match[[1]]$url else NA_character_
    }

    tibble::tibble(
      name = rec[["name"]] %||% NA_character_,
      title = rec[["title"]] %||% NA_character_,
      version = rec[["version"]] %||% NA_integer_,
      record_count = rec[["recordCount"]] %||% NA_integer_,
      formats = paste(all_formats, collapse = ", "),
      url_parquet = get_url("parquet"),
      url_csv = get_url("csv")
    )
  })

  dplyr::bind_rows(rows)
}


#' @title Download full OpenFEMA datasets
#'
#' @description Downloads full data files for OpenFEMA API endpoints. Prefers
#'   parquet format, falling back to CSV. Uses the OpenFEMA metadata API to
#'   dynamically resolve download URLs.
#'
#' @param endpoints A character vector of dataset endpoint names (e.g.,
#'   `"DisasterDeclarationsSummaries"`). Use [list_openfema_endpoints()] to see
#'   available names. Default `NULL` downloads all endpoints.
#' @param download_directory Directory path where files will be saved. Created if it does
#'   not exist. Defaults to `"."`.
#' @param format_preference Character vector specifying format preference order.
#'   The first available format is used. Defaults to `c("parquet", "csv")`.
#' @param overwrite Logical. If `FALSE` (default), skips files that already
#'   exist in `download_directory`.
#'
#' @returns A tibble summarizing the results with columns: `name`, `format`,
#'   `file_path`, `status` (one of "downloaded", "skipped", "failed", "no_format").
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download a single small dataset
#' download_openfema_datasets(
#'   endpoints = "DisasterDeclarationsSummaries",
#'   download_directory = "data/openfema")
#'
#' # Download all datasets
#' download_openfema_datasets(download_directory = "data/openfema")
#'
#' # See what's available first
#' list_openfema_endpoints()
#' }
download_openfema_datasets <- function(
    endpoints = NULL,
    download_directory = ".",
    format_preference = c("parquet", "csv"),
    overwrite = FALSE) {

  metadata <- list_openfema_endpoints()

  # Validate requested endpoints
  if (!is.null(endpoints)) {
    unknown <- setdiff(endpoints, metadata$name)
    if (length(unknown) > 0) {
      stop(
        "Unknown endpoint(s): ", paste(unknown, collapse = ", "),
        "\nUse list_openfema_endpoints() to see available names."
      )
    }
    metadata <- metadata |> dplyr::filter(.data$name %in% endpoints)
  }

  if (!dir.exists(download_directory)) {
    dir.create(download_directory, recursive = TRUE)
    message("Created directory: ", download_directory)
  }

  # Process each dataset
  results <- purrr::pmap(
    list(metadata$name, metadata$url_parquet, metadata$url_csv, metadata$formats),
    function(name, url_parquet, url_csv, formats) {

      # Build lookup of format -> URL
      url_lookup <- c(parquet = url_parquet, csv = url_csv)

      # Pick best available format
      chosen_format <- NA_character_
      chosen_url <- NA_character_
      for (fmt in format_preference) {
        url_candidate <- url_lookup[[fmt]]
        if (!is.na(url_candidate)) {
          chosen_format <- fmt
          chosen_url <- url_candidate
          break
        }
      }

      if (is.na(chosen_format)) {
        message("[", name, "] No preferred format available (has: ", formats, "). Skipping.")
        return(tibble::tibble(
          name = name, format = NA_character_,
          file_path = NA_character_, status = "no_format"
        ))
      }

      date_stamp <- format(Sys.Date(), "%Y_%m_%d")
      safe_name <- gsub("-", "_", name)
      file_name <- paste0(safe_name, "_", date_stamp, ".", chosen_format)
      file_path <- file.path(download_directory, file_name)

      if (file.exists(file_path) && !overwrite) {
        message("[", name, "] File already exists, skipping. Use overwrite = TRUE to re-download.")
        return(tibble::tibble(
          name = name, format = chosen_format,
          file_path = file_path, status = "skipped"
        ))
      }

      message("[", name, "] Downloading ", chosen_format, " from: ", chosen_url)
      ## download to a temporary path and rename only on success: a truncated file
      ## would otherwise be served by find_openfema_cache_file() and fail to parse.
      ## curl::multi_download() resumes interrupted transfers, which multi-GB
      ## datasets (e.g. FimaNfipPolicies, ~5 GB) need on connections that drop
      ## mid-transfer; the .partial file is kept on failure so a later attempt or
      ## re-run picks up where it left off rather than restarting from zero
      temp_path <- paste0(file_path, ".partial")
      dl_result <- tryCatch({
        attempts <- 0
        repeat {
          attempts <- attempts + 1
          download_result <- curl::multi_download(chosen_url, destfiles = temp_path, resume = TRUE)
          if (isTRUE(download_result$success) && download_result$status_code %in% c(200, 206)) break
          if (attempts >= 5) {
            stop(
              "download failed after ", attempts, " attempts (last status: ",
              download_result$status_code, "): ", download_result$error) }
        }
        file.rename(temp_path, file_path)
        "downloaded"
      }, error = function(e) {
        warning("[", name, "] Download failed: ", conditionMessage(e))
        "failed"
      })

      tibble::tibble(
        name = name, format = chosen_format,
        file_path = file_path, status = dl_result)
    }
  )

  dplyr::bind_rows(results)
}
