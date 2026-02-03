#' Cache an object to a parquet file; optionally read from disk
#'
#' This function writes an R object to a parquet file with an automatic datestamp
#' (YYYY_MM_DD format) in the filename. It can also read from an existing cached
#' file if one exists. For sf objects, the function automatically uses sfarrow for
#' reading/writing and adds "_sf" to the filename to indicate the file format.
#'
#' @param object A dataframe, tibble, or sf object to cache. Can be provided as
#'   either a quoted or unquoted name. Optional when reading from cache - in this
#'   case, file_name must be provided.
#' @param file_name File name (without extension). Optional when object is provided
#'   (uses object's name). Required when object is missing and reading from cache.
#'   Must not contain path separators or invalid filename characters.
#' @param path Directory path where the file should be saved/read. Defaults to
#'   current directory ("."). If the path does not exist, the user will be prompted
#'   to create it (in interactive sessions) or an error will be thrown (in
#'   non-interactive sessions).
#' @param read Logical or character. TRUE by default.
#'   - TRUE: Find and read the most recent cached version based on datestamp.
#'   - FALSE: Skip reading, always write a new cached file
#'   - Character: Read the specific file with this exact filename (including extension).
#'   Defaults to TRUE.
#' @param keep_n Integer. Maximum number of cached versions to keep. When writing
#'   a new file, older versions beyond this limit are deleted. Defaults to 5.
#'   Set to NULL or Inf to keep all versions.
#'
#' @return The object that was cached (either written or read)
#'
#' @examples
#' \dontrun{
#' ## Note: datestamps in filenames are illustrative; user results will
#' ## vary depending on the date at runtime
#'
#' # Regular data frames
#' my_data <- tibble(x = 1:10, y = letters[1:10])
#'
#' # Cache with automatic naming and datestamp (writes to current directory)
#' cache_it(my_data)  # Creates: ./my_data_2025_12_07.parquet
#'
#' # Cache with custom filename and path
#' cache_it(my_data, file_name = "custom_name", path = "data")
#'
#' # Read most recent cached version if exists, otherwise write
#' cached_data <- cache_it(my_data, read = TRUE)
#'
#' # Always write a new file, don't read from cache
#' cache_it(my_data, read = FALSE)
#'
#' # Read a specific cached file by name
#' old_data <- cache_it(my_data, read = "my_data_2025_12_01.parquet")
#'
#' # Read from cache when object doesn't exist in environment yet (using file_name)
#' my_data <- cache_it(file_name = "my_data", read = TRUE)
#'
#' # Read from cache when object doesn't exist (using quoted name)
#' my_data <- cache_it("my_data", read = TRUE)
#'
#' # Read from cache when object doesn't exist (using unquoted name)
#' my_data <- cache_it(my_data, read = TRUE)
#'
#' # Read specific file when object doesn't exist
#' old_data <- cache_it(read = "my_data_2025_12_01.parquet")
#'
#' # Keep only the 3 most recent cached versions
#' cache_it(my_data, keep_n = 3)
#'
#' # Keep all cached versions (no cleanup)
#' cache_it(my_data, keep_n = NULL)
#'
#' # SF objects (automatically uses sfarrow)
#' my_sf <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' cache_it(my_sf)  # Creates: ./my_sf_2025_12_07_sf.parquet
#'
#' # Read most recent sf cached file
#' cached_sf <- cache_it(my_sf, read = TRUE)
#'
#' # Read specific sf cached file
#' old_sf <- cache_it(my_sf, read = "my_sf_2025_12_01_sf.parquet")
#' }
#'
#' @export
cache_it <- function(object,
                     file_name = NULL,
                     path = ".",
                     read = TRUE,
                     keep_n = 5) {

  # Determine if object parameter was provided
 object_provided <- !missing(object)

  # Get the name to use for the file and check if we have an actual object value
  is_string_literal <- FALSE
  if (is.null(file_name)) {
    if (!object_provided) {
      stop("Either 'object' or 'file_name' must be provided")
    }

    # Get what was passed to object
    obj_expr <- substitute(object)

    # If a string literal was passed, use it as file_name but note we don't have the object
    if (is.character(obj_expr)) {
      file_name <- obj_expr
      is_string_literal <- TRUE
    } else {
      # Otherwise deparse to get the name
      file_name <- deparse(obj_expr)
    }
  }

  # Validate file_name: no path separators or invalid filename characters
 invalid_chars <- c("/", "\\", ":", "*", "?", "\"", "<", ">", "|")
  if (any(stringr::str_detect(file_name, stringr::fixed(invalid_chars)))) {
    stop("file_name contains invalid characters. Must not contain: / \\ : * ? \" < > |")
  }

  # Try to access the actual object value (if provided and not a string literal)
  has_object_value <- FALSE
  if (object_provided && !is_string_literal) {
    has_object_value <- tryCatch({
      # Force evaluation of object
      force(object)
      TRUE
    }, error = function(e) {
      warning("Object '", file_name, "' could not be evaluated: ", conditionMessage(e))
      FALSE
    })
  }

  # Check if object is an sf object (only if we have access to the value)
  is_sf <- FALSE
  if (has_object_value) {
    is_sf <- inherits(object, "sf")
  }

  # Add datestamp to filename, with _sf suffix for sf objects
  date_str <- format(Sys.Date(), "%Y-%m-%d") |> stringr::str_replace_all("-", "_")
  if (is_sf) {
    full_file_name <- stringr::str_c(file_name, "_", date_str, "_sf.parquet")
  } else {
    full_file_name <- stringr::str_c(file_name, "_", date_str, ".parquet")
  }

  # Construct full file path
  full_path <- file.path(path, full_file_name)

  # If the specified `path` does not exist, check with user about creating it
  if (!dir.exists(path)) {
    if (interactive()) {
      create_dir <- readline(prompt = stringr::str_c("The specified `path` does not exist. Do you want to create a directory at ", path, "? Y/N: "))
      if (create_dir %in% c("Y", "y")) {
        dir.create(path, recursive = TRUE)
      } else {
        stop("Specify alternate parameters.")
      }
    } else {
      stop("Path does not exist: ", path)
    }
  }

  # Escape regex metacharacters in file_name for pattern matching
  file_name_escaped <- stringr::str_replace_all(
    file_name,
    "([\\.\\^\\$\\*\\+\\?\\{\\}\\[\\]\\\\\\|\\(\\)])",
    "\\\\\\1"
  )

  # Helper function to find cached files
  find_cached_files <- function() {
    pattern <- stringr::str_c("^", file_name_escaped, "_\\d{4}_\\d{2}_\\d{2}(_sf)?\\.parquet$")
    list.files(path, pattern = pattern, full.names = TRUE)
  }

  # Handle reading based on read parameter
  if (isTRUE(read)) {
    cached_files <- find_cached_files()

    if (length(cached_files) > 0) {
      # Extract dates from filenames and find the most recent
      file_dates <- cached_files |>
        basename() |>
        stringr::str_extract("\\d{4}_\\d{2}_\\d{2}") |>
        stringr::str_replace_all("_", "-") |>
        as.Date()

      most_recent_file <- cached_files[which.max(file_dates)]
      most_recent_date <- format(max(file_dates), "%Y_%m_%d")

      # Check if file is an sf object based on filename
      file_is_sf <- stringr::str_detect(most_recent_file, "_sf\\.parquet$")

      message("Reading cached file: ", basename(most_recent_file), " (dated ", most_recent_date, ")")

      if (file_is_sf) {
        return(sfarrow::st_read_parquet(most_recent_file))
      } else {
        return(arrow::read_parquet(most_recent_file))
      }
    } else {
      message("No cached files found for '", file_name, "'. Writing new file.")
    }

  } else if (is.character(read)) {
    # Read specific file
    specific_path <- file.path(path, read)

    if (file.exists(specific_path)) {
      # Check if file is an sf object based on filename
      file_is_sf <- stringr::str_detect(specific_path, "_sf\\.parquet$")

      message("Reading cached file: ", read)

      if (file_is_sf) {
        return(sfarrow::st_read_parquet(specific_path))
      } else {
        return(arrow::read_parquet(specific_path))
      }
    } else {
      stop("Specified file does not exist: ", specific_path)
    }

  } else if (isFALSE(read)) {
    message("Writing new cached file.")
  }

  # Write object to parquet file
  if (!has_object_value) {
    stop("No cached file found and no object provided to write. Please provide an object or check the file_name/path.")
  }

  if (is_sf) {
    sfarrow::st_write_parquet(obj = object, dsn = full_path)
  } else {
    arrow::write_parquet(object, full_path, compression = "snappy")
  }
  message("Cached to: ", basename(full_path))

  # Clean up old versions if keep_n is set
  if (!is.null(keep_n) && is.finite(keep_n) && keep_n > 0) {
    cached_files <- find_cached_files()

    if (length(cached_files) > keep_n) {
      file_dates <- cached_files |>
        basename() |>
        stringr::str_extract("\\d{4}_\\d{2}_\\d{2}") |>
        stringr::str_replace_all("_", "-") |>
        as.Date()

      # Sort by date (oldest first) and identify files to delete
      date_order <- order(file_dates)
      files_to_delete <- cached_files[date_order[seq_len(length(cached_files) - keep_n)]]

      for (f in files_to_delete) {
        file.remove(f)
      }
      message("Removed ", length(files_to_delete), " old cached file(s) (keeping ", keep_n, " most recent).")
    }
  }

  return(object)
}
