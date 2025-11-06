# Download a .xlsx file(s) from a URL(s)

Download a .xlsx file(s) from a URL(s)

## Usage

``` r
read_xlsx_from_url(urls, directory, file_names = NULL, silent = TRUE)
```

## Arguments

- urls:

  A character vector of URLs of length one or greater.

- directory:

  The path to a single directory–not to a file–where the .xlsx file(s)
  will be saved.

- file_names:

  Optionally, a character vector of the same length as `urls` containing
  only the file names (not the full paths) with which the downloaded
  files should be named. If NULL (default), file names are extracted
  from `urls`.

- silent:

  If TRUE (default), files are saved silently. If FALSE, downloaded
  files are read and returned as a list.

## Value

Either nothing (silent == TRUE) or a list of dataframes from the
specified URLs.
