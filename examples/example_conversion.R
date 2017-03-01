#' Example data conversion using the Howard Springs site.
#'
#' Converts useful variables from a Fluxnet 2015 spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.
#' 
#' The user must provide the input file name (with full path),
#' output directory path and site code. All other settings
#' are optional and are set to their default values in this
#' example.
#' 

library(FluxnetProcessing)  # convert_fluxnet_to_netcdf

#clear R environment
rm(list=ls(all=TRUE))


#############################
###--- Required inputs ---###
#############################

#Fluxnet site ID (see http://fluxnet.fluxdata.org/sites/site-list-and-pages/)
site_code <- "AU-How"


# This directory should contain appropriate data from 
# http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "~/phd/data/Fluxnet2015/FULLSET/AU-How/"

#Outputs will be saved to this directory
out_path <- "~/phd/data/Fluxnet2015/processed/"


# Name and version of dataset being processed (e.g. "FLUXNET2015" and "1.3")
datasetname="FLUXNET2015"
datasetversion="1.3"

# Input Fluxnet data file (using FULLSET in this example, se R/Helpers.R for details)
infile <- get_fluxnet_files(in_path, site_code,
                            datasetname=datasetname,
                            datasetversion=datasetversion)

###############################
###--- Optional settings ---###
###############################

# ERAinterim meteo file for gap-filling met data (set to FALSE if not desired)
ERA_gapfill  <- TRUE
ERA_file <- get_fluxnet_erai_files(in_path, site_code,
                                   datasetname = datasetname,
                                   datasetversion = datasetversion)

#What percentage of time steps allowed to be missing
#or gap-filled in any given year? And minimum number of 
#consecutive years to process
#Note: Always checks for missing values. If no gapfilling 
#thresholds set, will not check for gap-filling.
missing      <- 15 #max. percent missing (must be set)
gapfill_all  <- 20 #max. percent gapfilled (optional)
gapfill_good <- NA #max. percent good-quality gapfilled (optional, ignored if gapfill_all set)
gapfill_med  <- NA #max. percent medium-quality gapfilled (optional, ignored if gapfill_all set)
gapfill_poor <- NA #max. percent poor-quality gapfilled (optional, ignored if gapfill_all set)
min_yrs      <- 2  #min. number of consecutive years

#Should code produce plots to visualise outputs? (set to NA if not desired)
#(annual: average monthly cycle; diurnal: average diurnal cycle by season;
#timeseries: 14-day running mean time series)
plot <- c("annual", "diurnal","timeseries")


##########################
###--- Run analysis ---###
##########################

convert_fluxnet_to_netcdf(infile=infile, site_code=site_code, out_path=out_path,
                          ERA_file=ERA_file, ERA_gapfill=ERA_gapfill, datasetname=datasetname, 
                          datasetversion=datasetversion, missing = missing, 
                          gapfill_all=gapfill_all, gapfill_good=gapfill_good, 
                          gapfill_med=gapfill_med, gapfill_poor=gapfill_poor,
                          min_yrs=min_yrs, plot=plot)


#Save warnings and errors to a log COPMLETE !!!!!!!!!!!!!!!
warnings()
