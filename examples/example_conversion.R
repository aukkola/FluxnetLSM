#' Example data conversion using the Howard springs site.
#'
#' Converts useful variables from a Fluxnet 2015 spreatsheet format into two
#' netcdf files, one for fluxes, and one for met forcings.

library(FluxnetProcessing)  # convert_fluxnet_to_netcdf

# initial garbage collection
rm(list=ls(all=TRUE))

# This directory should contain appropriate data from http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "~/Documents/FLUXNET2016_processing/"
out_path <- "~/Documents/FLUXNET2016_processing/"

site_code <- "AU-How"

# Flux file
fname <- sprintf("FLX_%s_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv", site_code)
infile <- paste(in_path, fname, sep="")

# ERA file
fname <- sprintf("FLX_%s_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv", site_code)
era_file <- paste(in_path, fname, sep="")
era_gapfill = TRUE

convert_fluxnet_to_netcdf(infile=infile, site_code=site_code, out_path=out_path, lib_path="./R",
                          ERA_file=era_file, ERA_gapfill=era_gapfill)




