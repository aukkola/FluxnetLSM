# initial garbage collection
rm(list=ls(all=TRUE))

lib_path = paste(out_path, "/scripts/R/", sep="")
source(paste(lib_path, "/ConvertSpreadsheetToNcdf.R", sep=""))

site_code = "AU-How"
in_path <- "~/Documents/FLUXNET2016_processing/"
out_path="~/Documents/FLUXNET2016_processing/"


# Flux file
fname <- sprintf("FLX_%s_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv", site)
infile <- paste(in_path, fname, sep="")

# ERA file
fname <- sprintf("FLX_%s_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv", site)
era_file <- paste(in_path, fname, sep="")
era_gapfill = TRUE

convert_fluxnet_to_netcdf(infile=infile, site_code=site_code, out_path=out_path,
                          lib_path=lib_path, ERA_file=era_file,
                          ERA_gapfill=era_gapfill)
