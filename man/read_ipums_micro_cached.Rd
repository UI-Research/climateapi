% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_ipums_micro_cached.R
\name{read_ipums_micro_cached}
\alias{read_ipums_micro_cached}
\title{Read cached IPUMS microdata}
\usage{
read_ipums_micro_cached(
  filename,
  download_directory,
  extract_definition,
  refresh = FALSE
)
}
\arguments{
\item{filename}{The name of the file (not the full file path)}

\item{download_directory}{A relative path specifying where to download the data}

\item{extract_definition}{A \code{define_extract_micro()} object}

\item{refresh}{If true, execute the API query, even if data are already stored locally. Defaults to FALSE}
}
\value{
A dataframe corresponding to the supplied extract_definition
}
\description{
Read cached IPUMS microdata
}
\examples{
\dontrun{
read_ipums_micro_cached(
  filename = "acs_insurance_race_2022_1yr_repweights",
  download_directory = "data",
  extract_definition = define_micro_extract(
    collection = "usa",
    description = "2022 ACS 1-year sample with replicate weights - insurance and race",
    samples = c("us2022a"),
    variables = list(
      "HCOVANY",
      var_spec("RACE", case_selections = c("1", "2")),
      "REPWT"),
  refresh = FALSE))
}
}
