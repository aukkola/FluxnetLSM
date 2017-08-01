#' Example data conversion for multiple sites using FLUXNET2015.
#'
#' See LaThuile/example_conversion_multiple_sites.R for the corresponding La Thuile example.
#'
#' Converts useful variables from a FLUXNET2015 "FULLSET" spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings. For "SUBSET" data,
#' set flx2015_version="SUBSET" in the main function.
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
in_path <- "./Inputs"

#Outputs will be saved to this directory
out_path <- "./Outputs"


#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data files (using FULLSET in this example, se R/Helpers.R for details)
infiles <- get_fluxnet_files(in_path)

#Retrieve dataset versions
datasetversions <- sapply(infiles, get_fluxnet_version_no)

#Retrieve site codes
site_codes <- sapply(infiles, get_path_site_code)


###############################
###--- Optional settings ---###
###############################

conv_opts <- get_default_conversion_options()

# ERAinterim meteo file for gap-filling met data (set to NA if not desired)
# Find ERA-files corresponding to site codes
conv_opts$met_gapfill  <- "ERAinterim"
ERA_files     <- sapply(site_codes, function(x) get_fluxnet_erai_files(in_path, site_code=x))

#Stop if didn't find ERA files
if(any(sapply(ERA_files, length)==0) & conv_opts$met_gapfill=="ERAinterim"){
  stop("No ERA files found, amend input path")
}


##########################
###--- Run analysis ---###
##########################

#Loops through sites
mapply(function(site_code, infile, ERA_file, datasetversion) {
        try(
            convert_fluxnet_to_netcdf(site_code = site_code,
                                      infile = infile,
                                      out_path = out_path,
                                      era_file = ERA_file,
                                      conv_opts = conv_opts,
                                      datasetversion = datasetversion
                                      )
        )},
    site_code = site_codes,
    infile = infiles,
    ERA_file = ERA_files,
    datasetversion = datasetversions)
