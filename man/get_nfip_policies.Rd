% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nfip_policies.R
\name{get_nfip_policies}
\alias{get_nfip_policies}
\title{Access county-level data on NFIP policies}
\usage{
get_nfip_policies(
  county_geoids,
  file_name = "fima_nfip_policies_2024_10_13.parquet",
  api = TRUE
)
}
\arguments{
\item{county_geoids}{A character vector of five-digit county codes.}

\item{file_name}{The name (not the full path) of the Box file containing the raw data.}

\item{api}{If TRUE (default), query the API.}
}
\value{
A dataframe comprising county-level data on current NFIP policies
}
\description{
Access county-level data on NFIP policies
}
\examples{
\dontrun{
get_nfip_policies(
   county_geoids = c("01001", "01003"),
   api = TRUE)
}
}
