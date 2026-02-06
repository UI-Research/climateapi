# Get the path to the C&C Box folder

Get the path to the C&C Box folder

## Usage

``` r
get_box_path()
```

## Value

A character string containing the full file path to the Climate and
Communities (C&C) Box folder. On Windows, returns
"C:/Users/username/Box/METRO Climate and Communities Practice
Area/github-repository". On Mac, checks for Box at "/Users/username/Box"
or "/Users/username/Library/CloudStorage/Box-Box", using whichever
exists. Throws an error if the Box folder cannot be found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_box_path()
} # }
```
