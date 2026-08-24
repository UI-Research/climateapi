# Download Preliminary Damage Assessment (PDA) Reports to Disk

Downloads every PDA report PDF that FEMA publishes into a local
directory, so that
[`get_preliminary_damage_assessments()`](https://ui-research.github.io/climateapi/reference/get_preliminary_damage_assessments.md)
has a complete and current set of source documents to parse. Run this
before regenerating the dataset;
[`get_preliminary_damage_assessments()`](https://ui-research.github.io/climateapi/reference/get_preliminary_damage_assessments.md)
parses whatever is already on disk and never fetches anything itself.

## Usage

``` r
scrape_pda_pdfs(
  cache_directory = file.path(climateapi::get_box_path(), "hazards", "urban",
    "preliminary-damage-assessments", "pdfs"),
  pages = NULL,
  max_pages = 200,
  attempts_per_page = 5,
  delay_seconds = 2,
  quiet = FALSE
)
```

## Arguments

- cache_directory:

  The folder where scraped PDFs are written.

- pages:

  Which listing pages to read, as a numeric vector. The default `NULL`
  walks the whole listing until a page returns no links, which is the
  only setting that guarantees complete coverage. FEMA lists newest
  first, so `pages = 0:2` is enough to pick up recently published
  reports and is much faster. Two caveats when this is set: cached files
  that are no longer listed cannot be detected, because the full listing
  was never read; and filename collisions are resolved only against the
  pages requested, so a name could be assigned that a page outside the
  range also claims.

- max_pages:

  A guard against an unbounded walk if the listing ever stops returning
  empty pages. Raises an error if reached.

- attempts_per_page:

  How many times to try a listing page before treating it as a failure.

- delay_seconds:

  Seconds to pause between listing pages, to avoid hammering FEMA's
  site. This is what dominates the running time of a full walk – roughly
  70 pages – not the downloads, which are skipped for reports already in
  `cache_directory`.

- quiet:

  Suppress progress messages? The walk covers roughly 70 pages with a
  pause between each and is otherwise silent for several minutes, so
  progress is reported by default. Warnings about failed downloads and
  about cached files no longer listed are always raised, regardless of
  this setting.

## Value

Invisibly, a tibble with one row per report found on the site,
containing `url`, `destination_file`, and `status` (`"cached"`,
`"downloaded"`, or `"failed"`). Called for its side effect: PDFs are
written to `cache_directory`.

## Details

Walks every page of FEMA's PDA report listing at
https://www.fema.gov/disaster/how-declared/preliminary-damage-assessments/reports,
advancing until a page returns no PDF links, and downloads any report
not already present in `cache_directory`. Because the full listing is
traversed on every run, the set of files on disk after a successful run
is the complete set of reports FEMA publishes – coverage does not depend
on the caller working out which page numbers hold new reports.

A page that errors is retried, and exhausting the retries raises an
error rather than ending the walk. This matters because a transient
failure and a genuinely empty final page are otherwise
indistinguishable, and treating the former as the end of the listing
would silently truncate the archive.

The listing is server-rendered, so the PDF links are present in the HTML
that [`httr::GET()`](https://httr.r-lib.org/reference/GET.html) returns
and no headless browser is needed. An earlier version used
[`rvest::read_html_live()`](https://rvest.tidyverse.org/reference/read_html_live.html),
which requires `chromote`; that dependency is not declared by this
package and is frequently absent, which made the function fail outright
rather than merely run slowly.

Note that FEMA's bot protection rejects requests that do not carry a
browser-like `User-Agent`, and rejects them from some networks
regardless, so a failure here is not necessarily a change to the
listing.

## Examples

``` r
if (FALSE) { # \dontrun{
## refresh the local archive, then rebuild the dataset from it
scrape_pda_pdfs()
get_preliminary_damage_assessments(use_cache = FALSE)
} # }
```
