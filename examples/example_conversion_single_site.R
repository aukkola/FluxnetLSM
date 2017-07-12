#' Example data conversion using the Howard Springs site.
#'
#' Converts useful variables from a FLUXNET2015 FULLSET spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.
#' 
#' The user must provide the input file name (with full path),
#' output directory path and site code. All other settings
#' are optional. This example uses statistical gapfilling for
#' meteorological and flux variables, all other options are set 
#' to their default values in this example.
#' 

library(FluxnetLSM) 

#clear R environment
rm(list=ls(all=TRUE))


#############################
###--- Required inputs ---###
#############################

#--- User must define these ---#

#Fluxnet site ID (see http://fluxnet.fluxdata.org/sites/site-list-and-pages/)
site_code <- "AU-How"

# This directory should contain appropriate data from 
# http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "~/Documents/FLUXNET2016_processing/Inputs"

#Outputs will be saved to this directory
out_path <- "~/Documents/FLUXNET2016_processing/Outputs"



#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data file (using FULLSET in this example, se R/Helpers.R for details)
infile <- get_fluxnet_files(in_path, site_code)

#Retrieve dataset version
datasetversion <- get_fluxnet_version_no(infile)
  

###############################
###--- Optional settings ---###
###############################

# Gapfilling options
met_gapfill  <- "statistical"
flux_gapfill  <- "statistical"


##########################
###--- Run analysis ---###
##########################

convert_fluxnet_to_netcdf(infile=infile, site_code=site_code, out_path=out_path,
                          met_gapfill=met_gapfill, flux_gapfill=flux_gapfill)





