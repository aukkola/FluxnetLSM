#' Example data conversion for multiple sites using La Thuile.
#'
#' See FLUXNET2015/example_conversion_multiple_sites.R for the corresponding FLUXNET2015 example.
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
datasetname <- "LaThuile"

# This directory should contain appropriate data from 
# http://fluxnet.fluxdata.org/data/la-thuile-dataset/
in_path <- "./Inputs"

#Outputs will be saved to this directory
out_path <- "./Outputs"


#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data files
infile <- get_fluxnet_files(in_path, datasetname=datasetname)

#Get site codes
site_codes <- unique(get_path_site_code(infile))

#Reorganise input files by site
infiles <- lapply(site_codes, function(site) get_fluxnet_files(in_path, site_code=site,
                                                              datasetname=datasetname))



##########################
###--- Run analysis ---###
##########################


#Loops through sites
mapply(function(site_code, infile) {
  try(
    convert_fluxnet_to_netcdf(site_code = site_code,
                              infile = infile,
                              out_path = out_path,
                              datasetname = datasetname
    )
  )},
  site_code = site_codes,
  infile = infiles)
