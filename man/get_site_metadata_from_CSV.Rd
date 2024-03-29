% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Site_metadata.R
\name{get_site_metadata_from_CSV}
\alias{get_site_metadata_from_CSV}
\title{Tries to gather metadata from the included site CSV}
\usage{
get_site_metadata_from_CSV(
  metadata = NA,
  incl_processing = TRUE,
  model,
  site_csv_file = system.file("extdata", "Site_metadata.csv", package = "FluxnetLSM")
)
}
\value{
metadata list
}
\description{
Tries to gather metadata from the included site CSV
}
