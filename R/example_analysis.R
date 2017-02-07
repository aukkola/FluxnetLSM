

# initial garbage collection
rm( list=ls(all=TRUE) )


infile="~/Documents/FLUXNET2016_processing//FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv"
site_code="AU-How"
out_path="~/Documents/FLUXNET2016_processing/"
lib_path=paste(out_path, "/scripts/R/", sep="")
era_file="~/Documents/FLUXNET2016_processing//FLX_AU-How_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv"
era_gapfill=TRUE


source(paste(lib_path, "/ConvertSpreadsheetToNcdf.R", sep=""))

convert_fluxnet_to_netcdf(infile=infile, site_code=site_code, out_path=out_path,
                          lib_path=lib_path, ERA_file=era_file, ERA_gapfill=era_gapfill)

