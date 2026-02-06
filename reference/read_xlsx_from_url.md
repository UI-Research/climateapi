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

When `silent = TRUE` (default): Returns NULL invisibly. Files are
downloaded and saved to `directory`. When `silent = FALSE`: Returns a
list of data frames, one per URL, containing the contents of each
downloaded .xlsx file as read by
[`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html).
List elements are in the same order as the input `urls`.
