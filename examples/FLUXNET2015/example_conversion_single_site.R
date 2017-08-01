#' Example data conversion for the Howard Springs site using FLUXNET2015.
#'
#' See LaThuile/example_conversion_single_site.R for the corresponding La Thuile example.
#'
#' Converts useful variables from a FLUXNET2015 "FULLSET" spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings. For "SUBSET" data,
#' set flx2015_version="SUBSET" in the main function.
#' 
#' The user must provide the input directory path, output directory path 
#' and site code. All other settings are optional. This example uses ERAinterim gapfilling for
#' meteorological variables, all other options are set to their default values.
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

# Input Fluxnet data file (using FULLSET in this example, see R/Helpers.R for details)
infile <- get_fluxnet_files(in_path, site_code)

#Retrieve dataset version
datasetversion <- get_fluxnet_version_no(infile)
  
#Retrieve ERAinterim file
era_file <- get_fluxnet_erai_files(in_path, site_code)


###############################
###--- Optional settings ---###
###############################

#Retrieve default processing options
conv_opts <- get_default_conversion_options()

# Set gapfilling options to ERAinterim
conv_opts$met_gapfill  <- "ERAinterim"


##########################
###--- Run analysis ---###
##########################

convert_fluxnet_to_netcdf(site_code = site_code, infile = infile,
                          era_file=era_file, out_path = out_path,
                          conv_opts = conv_opts)


#Alternatively you can pass the gapfilling option directly to the main function:
# convert_fluxnet_to_netcdf(site_code = site_code, infile = infile, 
#                           era_file = era_file, out_path = out_path,
#                           met_gapfill="ERAinterim")




