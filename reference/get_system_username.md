# Get the user's username

Get the user's username

## Usage

``` r
get_system_username()
```

## Value

A character string containing the system username. Uses
`Sys.info()["user"]` which works reliably across Windows, Mac, and
Linux.

## Examples

``` r
if (FALSE) { # \dontrun{
get_system_username()
} # }
```
