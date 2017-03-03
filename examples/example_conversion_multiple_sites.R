#' Example data conversion for multiple sites.
#'
#' Converts useful variables from a Fluxnet 2015 spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.
#' 
#' The user must provide the input directory path and
#' output directory path. Code will automatically retrieve
#' input files. All other input arguments are optional and 
#' are set to their default values in this example.
#' 

library(FluxnetLSM)  # convert_fluxnet_to_netcdf

#clear R environment
rm(list=ls(all=TRUE))


#############################
###--- Required inputs ---###
#############################

#--- User must define these ---#

# This directory should contain appropriate data from 
# http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "~/Documents/FLUXNET2016_processing/Inputs"

#Outputs will be saved to this directory
out_path <- "~/Documents/FLUXNET2016_processing/Outputs"

# Name and version of dataset being processed (e.g. "FLUXNET2015")
datasetname="FLUXNET2015"


#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data files (using FULLSET in this example, se R/Helpers.R for details)
infiles <- get_fluxnet_files(in_path, datasetname=datasetname, subset="FULLSET")

#Retrieve dataset versions
datasetversions <- sapply(infiles, get_fluxnet_version_no)

#Retrieve site codes
site_codes <- sapply(infiles, get_fluxnet_site_code)

                                         
###############################
###--- Optional settings ---###
###############################

# ERAinterim meteo file for gap-filling met data (set to FALSE if not desired)
# Find ERA-files corresponding to site codes
ERA_gapfill  <- TRUE
ERA_files <- sapply(site_codes, function(x) get_fluxnet_erai_files(in_path, site_code=x, 
                                                                  datasetname = datasetname))


#Thresholds for missing and gap-filled time steps
#Note: Always checks for missing values. If no gapfilling 
#thresholds set, will not check for gap-filling.
missing      <- 15 #max. percent missing (must be set)
gapfill_all  <- 20 #max. percent gapfilled (optional)
gapfill_good <- NA #max. percent good-quality gapfilled (optional, ignored if gapfill_all set)
gapfill_med  <- NA #max. percent medium-quality gapfilled (optional, ignored if gapfill_all set)
gapfill_poor <- NA #max. percent poor-quality gapfilled (optional, ignored if gapfill_all set)
min_yrs      <- 2  #min. number of consecutive years

#Should code produce plots to visualise outputs? Set to NA if not desired.
#(annual: average monthly cycle; diurnal: average diurnal cycle by season;
#timeseries: 14-day running mean time series)
plot <- c("annual", "diurnal","timeseries")

#Should all evaluation variables be included regardless of data gaps?
#If FALSE, removes evaluation variables with gaps in excess of thresholds
include_all_eval <- TRUE


##########################
###--- Run analysis ---###
##########################

#Loops through sites
mapply(function(w,x,y,z) convert_fluxnet_to_netcdf(infile=w, site_code=x, out_path=out_path,
                          ERA_file=y, ERA_gapfill=ERA_gapfill, datasetname=datasetname, 
                          datasetversion=z, missing = missing, 
                          gapfill_all=gapfill_all, gapfill_good=gapfill_good, 
                          gapfill_med=gapfill_med, gapfill_poor=gapfill_poor,
                          include_all_eval=include_all_eval,
                          min_yrs=min_yrs, plot=plot),
                          w=infiles, x=site_codes, 
                          y=ERA_files, z=datasetversions)


