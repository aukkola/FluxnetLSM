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
in_path <- "./Inputs"

#Outputs will be saved to this directory
out_path <- "./Outputs"



#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data file (using FULLSET in this example, se R/Helpers.R for details)
infile <- get_fluxnet_files(in_path, site_code)

#Retrieve dataset version
datasetversion <- get_fluxnet_version_no(infile)
  

###############################
###--- Optional settings ---###
###############################

conv_opts <- get_default_conversion_options()
conv_opts$datasetversion <- datasetversion

# Gapfilling options
conv_opts$met_gapfill  <- "statistical"
conv_opts$flux_gapfill  <- "statistical"
conv_opts$copyfill <- 30
conv_opts$regfill <- 60


##########################
###--- Run analysis ---###
##########################

convert_fluxnet_to_netcdf(site_code = site_code,
                          infile = infile, out_path = out_path,
                          conv_opts = conv_opts)
