#' Example data conversion for multiple sites.
#'
#' Converts useful variables from a FLUXNET2015 spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.
#' 
#' The user must provide the input directory path and
#' output directory path. Code will automatically retrieve
#' input files. All other input arguments are optional. In this example,
#' meteorological data is gapfilled using ERA-Interim estimates. All other
#' options are left to their default values.
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
flx2015_version="FULLSET"


#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data files (using FULLSET in this example, se R/Helpers.R for details)
infiles <- get_fluxnet_files(in_path, datasetname=datasetname, subset=flx2015_version)

#Retrieve dataset versions
datasetversions <- sapply(infiles, get_fluxnet_version_no)

#Retrieve site codes
site_codes <- sapply(infiles, get_fluxnet_site_code)

                                         
###############################
###--- Optional settings ---###
###############################

# ERAinterim meteo file for gap-filling met data (set to FALSE if not desired)
# Find ERA-files corresponding to site codes
met_gapfill  <- "ERAinterim"
ERA_files     <- sapply(site_codes, function(x) get_fluxnet_erai_files(in_path, site_code=x, 
                                                                  datasetname = datasetname))

#Stop if didn't find ERA files
if(any(sapply(ERA_files, length)==0) & met_gapfill=="ERAinterim"){
  stop("No ERA files found, amend input path")
}

#Thresholds for missing and gap-filled time steps
#Note: Always checks for missing values. If no gapfilling 
#thresholds set, will not check for gap-filling.
missing      <- 15 #max. percent missing (must be set)
gapfill_all  <- 20 #max. percent gapfilled (optional)
min_yrs      <- 2  #min. number of consecutive years


##########################
###--- Run analysis ---###
##########################

#Loops through sites
mapply(function(w,x,y,z) try(convert_fluxnet_to_netcdf(infile=w, site_code=x, out_path=out_path,
                                                       datasetname=datasetname, datasetversion=z,
                                                       flx2015_version=flx2015_version,
                                                       met_gapfill=met_gapfill, era_file=y,
                                                       missing=missing, gapfill_all=gapfill_all,
                                                       min_yrs=min_yrs)),
                            w=infiles, x=site_codes, y=ERA_files)
                                                       
                   


