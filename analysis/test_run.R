files <- list.files("R","*", full.names = TRUE)

lapply(files, function(file){
  print(file)
  source(file)
})

#############################
###--- Required inputs ---###
#############################

#--- User must define these ---#

#Fluxnet site ID (see http://fluxnet.fluxdata.org/sites/site-list-and-pages/)
site_code <- "AU-How"

# This directory should contain appropriate data from 
# http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
in_path <- "/home/khufkens/Desktop/flux_data_kit/data-raw/flux_data/fluxnet2015/"
ERA_path <- "/home/khufkens/Desktop/flux_data_kit/data-raw/flux_data/fluxnet2015/"

#Outputs will be saved to this directory
out_path <- "~/Desktop/"

#--- Automatically retrieve all Fluxnet files in input directory ---#

# Input Fluxnet data file (using FULLSET in this example, see R/Helpers.R for details)
infile <- get_fluxnet_files(
  in_path,
  site_code,
  #datasetversion = "[A-Z]{4}-[0-9]{1}"
  )

#Retrieve dataset version
datasetversion <- get_fluxnet_version_no(
  infile
  )

#Retrieve ERAinterim file
era_file <- get_fluxnet_erai_files(
  ERA_path,
  site_code,
  #datasetversion = "[A-Z]{4}-[0-9]{1}"
  )


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

convert_fluxnet_to_netcdf(
  site_code = site_code,
  infile = infile,
  era_file=era_file,
  out_path = out_path,
  conv_opts = conv_opts
  )

#Alternatively you can pass the gapfilling option directly to the main function:
# convert_fluxnet_to_netcdf(site_code = site_code, infile = infile, 
#                           era_file = era_file, out_path = out_path,
#   