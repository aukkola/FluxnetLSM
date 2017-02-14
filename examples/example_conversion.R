#' Example data conversion using the Howard Springs site.
#'
#' Converts useful variables from a Fluxnet 2015 spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.

library(FluxnetProcessing)  # convert_fluxnet_to_netcdf

# initial garbage collection
rm(list=ls(all=TRUE))


###################################
###--- Set paths and options ---###
###################################


###--- Required inputs ---###

# This directory should contain appropriate data from http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "~/Documents/FLUXNET2016_processing/Inputs/"

#Outputs will be saved to this directory
out_path <- "~/Documents/FLUXNET2016_processing/Outputs/"

#Fluxnet site ID
site_code <- "AU-How"

# Fluxnet file (using FULLSET in this example)
fname  <- sprintf("FLX_%s_FLUXNET2015_FULLSET", site_code)
infile <- list.files(in_path, pattern=fname, full.names=TRUE)


###--- Optional settings ---###

# ERAinterim meteo file for gap-filling met data (set to FALSE if not desired)
ERA_gapfill  <- TRUE
eraname      <- sprintf("FLX_%s_FLUXNET2015_ERAI", site_code)
ERA_file     <- list.files(in_path, pattern=eraname, full.names=TRUE)

#Name and version of dataset being processed
datasetname    <- "Fluxnet2015"
datasetversion <- "Nov16"

#How many percent of time steps allowed to be missing in any given year?
#and minimum number of consecutive years to process
gap_threshold <- 20
min_yrs       <- 2

#Should code produce plots to visualise outputs? (set to NA if not desired)
#(annual: average monthly cycle; diurnal: average diurnal cycle by season;
#timeseries: 14-day running mean time series)
plot <- c("annual", "diurnal","timeseries")



##########################
###--- Run analysis ---###
##########################

convert_fluxnet_to_netcdf(infile=infile, site_code=site_code, out_path=out_path, lib_path="./R",
                          ERA_file=ERA_file, ERA_gapfill=ERA_gapfill, datasetname=datasetname, 
                          datasetversion=datasetversion, gap_threshold=gap_threshold, 
                          min_yrs=min_yrs, plot=plot)

