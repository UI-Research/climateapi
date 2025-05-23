% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_zoning_envelope.R
\name{estimate_zoning_envelope}
\alias{estimate_zoning_envelope}
\title{Calculate the number of units allowed on a parcel}
\usage{
estimate_zoning_envelope(
  parcels,
  development_size_maximum = 300,
  standard_unit_sqft_multifamily = 1000,
  standard_parking_stall_sqft = 325,
  parking_model = "singlestory"
)
}
\arguments{
\item{parcels}{A dataframe containing parcel-level attributes and zoning regulations}

\item{development_size_maximum}{A cap on the number of units that can be developed on a parcel. Note that larger values increase the length of computation.}

\item{standard_unit_sqft_multifamily}{The square footage of a standard multifamily unit}

\item{standard_parking_stall_sqft}{The square footage of a standard parking space}

\item{parking_model}{One of c("singlestory", "multistory"). How to model parking requirements: are stalls all on the ground floor or distributed vertically?}
}
\value{
The input parcels dataframe with an additional column, \code{maximum_development_capacity_zoned}, that contains the maximum number of units that can be developed on each parcel per zoning regulations
}
\description{
Calculate the number of units allowed on a parcel
}
\examples{
df = tibble::tibble(
 parcel_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
 parcel_area_sqft = c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000),
 setback_front = c(10),
 setback_rear = c(10),
 setback_side = c(10),
 parcel_area_sqft_minimum = c(1000, 1000, 1000, 1000, 1000, 5000, 5000, 5000, 5000, 5000),
 units_per_parcel_maximum = c(10),
 units_per_acre_maximum = NA,
 parcel_coverage_percent_maximum = c(70),
 parcel_coverage_percent_maximum_building = c(70),
 open_space_ratio_minimum = c(0.2),
 floor_area_ratio_maximum = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
 height_stories_maximum = c(3, 3, 3, 3, 3, 5, 5, 5, 5, 5),
 height_feet_maximum = NA,
 parking_stalls_per_parcel_minimum = c(1),
 parking_stalls_per_unit_minimum = c(2, 2, 2, 2, 2, 1, 1, 1, 1, 1))

 estimate_zoning_envelope(
   parcels = df)
}
