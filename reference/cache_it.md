# Cache an object to a parquet file; optionally read from disk

This function writes an R object to a parquet file with an automatic
datestamp (YYYY_MM_DD format) in the filename. It can also read from an
existing cached file if one exists. For sf objects, the function
automatically uses sfarrow for reading/writing and adds "\_sf" to the
filename to indicate the file format.

## Usage

``` r
cache_it(object, file_name = NULL, path = ".", read = TRUE, keep_n = 5)
```

## Arguments

- object:

  A dataframe, tibble, or sf object to cache. Can be provided as either
  a quoted or unquoted name. Optional when reading from cache - in this
  case, file_name must be provided.

- file_name:

  File name (without extension). Optional when object is provided (uses
  object's name). Required when object is missing and reading from
  cache. Must not contain path separators or invalid filename
  characters.

- path:

  Directory path where the file should be saved/read. Defaults to
  current directory ("."). If the path does not exist, the user will be
  prompted to create it (in interactive sessions) or an error will be
  thrown (in non-interactive sessions).

- read:

  Logical or character. TRUE by default.

  - TRUE: Find and read the most recent cached version based on
    datestamp.

  - FALSE: Skip reading, always write a new cached file

  - Character: Read the specific file with this exact filename
    (including extension). Defaults to TRUE.

- keep_n:

  Integer. Maximum number of cached versions to keep. When writing a new
  file, older versions beyond this limit are deleted. Defaults to 5. Set
  to NULL or Inf to keep all versions.

## Value

The object that was cached (either written or read)

## Examples

``` r
if (FALSE) { # \dontrun{
## Note: datestamps in filenames are illustrative; user results will
## vary depending on the date at runtime

# Regular data frames
my_data <- tibble(x = 1:10, y = letters[1:10])

# Cache with automatic naming and datestamp (writes to current directory)
cache_it(my_data)  # Creates: ./my_data_2025_12_07.parquet

# Cache with custom filename and path
cache_it(my_data, file_name = "custom_name", path = "data")

# Read most recent cached version if exists, otherwise write
cached_data <- cache_it(my_data, read = TRUE)

# Always write a new file, don't read from cache
cache_it(my_data, read = FALSE)

# Read a specific cached file by name
old_data <- cache_it(my_data, read = "my_data_2025_12_01.parquet")

# Read from cache when object doesn't exist in environment yet (using file_name)
my_data <- cache_it(file_name = "my_data", read = TRUE)

# Read from cache when object doesn't exist (using quoted name)
my_data <- cache_it("my_data", read = TRUE)

# Read from cache when object doesn't exist (using unquoted name)
my_data <- cache_it(my_data, read = TRUE)

# Read specific file when object doesn't exist
old_data <- cache_it(read = "my_data_2025_12_01.parquet")

# Keep only the 3 most recent cached versions
cache_it(my_data, keep_n = 3)

# Keep all cached versions (no cleanup)
cache_it(my_data, keep_n = NULL)

# SF objects (automatically uses sfarrow)
my_sf <- sf::st_read(system.file("shape/nc.shp", package="sf"))
cache_it(my_sf)  # Creates: ./my_sf_2025_12_07_sf.parquet

# Read most recent sf cached file
cached_sf <- cache_it(my_sf, read = TRUE)

# Read specific sf cached file
old_sf <- cache_it(my_sf, read = "my_sf_2025_12_01_sf.parquet")
} # }
```
