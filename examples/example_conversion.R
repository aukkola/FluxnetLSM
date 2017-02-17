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

# This directory should contain appropriate data from 
# http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "~/Documents/FLUXNET2016_processing/Inputs/"

# Input Fluxnet data file (using FULLSET in this example)
fname  <- sprintf("FLX_%s_FLUXNET2015_FULLSET", site_code)    #append file name and site code
infile <- list.files(in_path, pattern=fname, full.names=TRUE) #find input file and full path

#Outputs will be saved to this directory
out_path <- "~/Documents/FLUXNET2016_processing/Outputs/"

#Fluxnet site ID (see http://fluxnet.fluxdata.org/sites/site-list-and-pages/)
site_code <- "AU-How"


###############################
###--- Optional settings ---###
###############################

# ERAinterim meteo file for gap-filling met data (set to FALSE if not desired)
ERA_gapfill  <- TRUE
eraname      <- sprintf("FLX_%s_FLUXNET2015_ERAI", site_code)         #append file name and site code
ERA_file     <- list.files(in_path, pattern=eraname, full.names=TRUE) #find file and full path

#Name and version of dataset being processed (e.g. "FLUXNET2015" and "v1-3")
#Stripped from file name in this example
vname          <- unlist(strsplit(infile, "_") )
datasetname    <- vname[[which(vname==site_code)+1]] #FLUXNET2015
datasetversion <- paste("v", substr(vname[[which(vname==site_code)+5]], 1, 3), sep="") #v1-3

#What percentage of time steps allowed to be missing
#or gap-filled in any given year? And minimum number of 
#consecutive years to process
#Note: Always checks for missing values. If no gapfilling 
#thresholds set, will not check for gap-filling.
missing      <- 10 #max. percent missing (must be set)
gapfill_all  <- NA #max. percent gapfilled
gapfill_good <- 10 #max. percent good-quality gapfilled (ignored if gapfill_all set)
gapfill_med  <- 10 #max. percent medium-quality gapfilled (ignored if gapfill_all set)
gapfill_poor <- 10 #max. percent poor-quality gapfilled (ignored if gapfill_all set)
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

