% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConvertSpreadsheetToNcdf.R
\name{convert_fluxnet_to_netcdf}
\alias{convert_fluxnet_to_netcdf}
\title{ConvertSpreadsheetToNcdf.R}
\usage{
convert_fluxnet_to_netcdf(
  site_code,
  infile,
  era_file = NA,
  out_path,
  conv_opts = get_default_conversion_options(),
  plot = c("annual", "diurnal", "timeseries"),
  site_csv_file = system.file("extdata", "Site_metadata.csv", package = "FluxnetLSM"),
  ...
)
}
\arguments{
\item{site_code}{Fluxnet site code e.g. "AU-How"}

\item{infile}{input filename,
e.g. "FULLSET/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv".
La Thuile data is expected to be in the format sitecode.year.xxxx.csv,
e.g. "AU-How.2001.synth.hourly.allvars.csv".}

\item{era_file}{ERA-Interim input file (needed if using ERAinterim to gapfill met variables)
e.g. "FULLSET/FLX_AU-How_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv"}

\item{out_path}{output path e.g. "./FLUXNET2016_processing/"}

\item{conv_opts}{options for the conversion.
See \code{\link{get_default_conversion_options}}.}

\item{plot}{Should annual, diurnal and/or 14-day running mean plots be produced?
Set to NA if not required.}

\item{site_csv_file}{CSV file with site meta-data which is parsed to populate
ancillary data provided with the flux data. By default the hard coded
packaged data are used, but alternatively an external file can be
provided. This limits the need to recompile the package when processing
files which were not originally selected.}
}
\description{
Converts data from a FLUXNET2015 formatted spreadsheet to
NetCDF.
}
\details{
See ?get_default_conversion_options for all conversion options.

Gapfilling options:

EraInterim:
Downscaled ERAInterim estimates are provided for all meteorological variables
in the FLUXNET2015 release. Any missing time steps are replaced with the corresponding
ERAInterim estimates.

Statistical:
Met variables gapfilled with a combination of linear interpolation (for short gaps) and
copyfill (for longer gaps). Air pressure and incoming longwave radiation are synthesised
from air temperature, elevation and relative humidity.

Flux variables gapfilled using a combination of linear interpolation (for short gaps) and
and a linear regression against met variables (incoming shortwave, and air temperature and 
relative humidity, if available.). If regfill is set to NA, copyfill is used instead.


author: Anna Ukkola UNSW 2017.
Main function to convert Fluxnet2015 CSV-files to NetCDF
}
