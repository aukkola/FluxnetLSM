#' Example data conversion using the Harvard site using La Thuile synthesis.
#'
#' See FLUXNET2015/example_conversion_single_site.R for the corresponding FLUXNE2015 example.
#'
#' Converts useful variables from a La Thuile spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.
#' 
#' The user must provide the input directory path, output directory path 
#' and site code. For La Thuile datasetname must be set to "LaThuile" as the code
#' defaults to FLUXNET2015. All other settings are optional. 
#' This example extracts La Thuile years complying with "Fair Use" policy. 
#' All other options are set to their default values. 
#' 

library(FluxnetLSM) 

#clear R environment
rm(list=ls(all=TRUE))


#############################
###--- Required inputs ---###
#############################

#--- User must define these ---#

#Fluxnet site ID (see http://fluxnet.fluxdata.org/sites/site-list-and-pages/)
site_code   <- "US-Ha1"
datasetname <- "LaThuile"

# This directory should contain appropriate data from 
# http://fluxnet.fluxdata.org/data/la-thuile-dataset/
in_path <- "./Inputs"

#Outputs will be saved to this directory
out_path <- "./Outputs"


#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data file (using FULLSET in this example, see R/Helpers.R for details)
infile <- get_fluxnet_files(in_path, site_code, datasetname=datasetname)


##########################
###--- Run analysis ---###
##########################

convert_fluxnet_to_netcdf(site_code = site_code, infile = infile, 
                          out_path = out_path, datasetname=datasetname)







