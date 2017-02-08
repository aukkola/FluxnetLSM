# TODO: update when converted to package
source("./R/ConvertSpreadsheetToNcdf.R")

#Example call
convert_fluxnet_to_netcdf(infile="~/Documents/FLUXNET2016_processing/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv",
                          site_code="AU-How",
                          out_path="~/Documents/FLUXNET2016_processing/",
                          lib_path="~/Documents/FLUXNET2016_processing/scripts/R",    #TEMPORARY, remove when code turned into a package
                          era_file="~/Documents/FLUXNET2016_processing/FLX_AU-How_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv",
                          era_gapfill=TRUE,
                          datasetname="Fluxnet2015",
                          datasetversion="Nov16",
                          gap_threshold=20,
                          min_yrs=2)

