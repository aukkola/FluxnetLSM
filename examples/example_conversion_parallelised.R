#' Example data conversion using the Howard Springs site.
#'
#' Converts useful variables from a Fluxnet 2015 spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.

library(FluxnetProcessing)  # convert_fluxnet_to_netcdf
library(parallel)

# initial garbage collection
rm(list=ls(all=TRUE))

# This directory should contain appropriate data from http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "~/Documents/FLUXNET2016_processing/"

#Outputs will be saved to this directory
out_path <- "~/Documents/FLUXNET2016_processing/"

#Fluxnet site ID
site_code <- "AU-How"

# Fluxnet file (using FULLSET in this example)
fname  <- sprintf("FLX_%s_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv", site_code)
infile <- paste(in_path, fname, sep="")

# ERAinterim meteo file for gap-filling (set to FALSE if not desired)
ERA_gapfill  <- TRUE
fname        <- sprintf("FLX_%s_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv", site_code)
ERA_file     <- paste(in_path, fname, sep="")

#Code produces plots to visualise outputs (set to NA if not desired)
#Annual: average monthly cycle
#Diurnal: average diurnal cycle by season
#Timeseries: 14-day running mean time series
plot <- c("annual", "diurnal","timeseries")

convert_fluxnet_to_netcdf(infile=infile, site_code=site_code, out_path=out_path, lib_path="./R",
                          ERA_file=ERA_file, ERA_gapfill=ERA_gapfill, plot=plot)
