# ConvertSpreadsheetToNcdf.R
#
# Converts data from a PALS formatted spreadhseet to
# netcdf.
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)

#NEED TO SET THESE ELSEWHERE IN FINAL CODE !!!!!!!!!

#path
lib_path <- "~/Documents/FLUXNET2016_processing/scripts/R"

#input filename
infile <- "~/Documents/FLUXNET2016_processing/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv"

#output file
metfilename <- "~/Documents/FLUXNET2016_processing/output_test_met.nc"
fluxfilename <- "~/Documents/FLUXNET2016_processing/output_test_flux.nc"

datasetname <- "Fluxnet2015" 
datasetversion <- "Nov16"

#Fill and/or synthesize LWdown?
LWdown_fill <- TRUE

defaultLWsynthesis = 'Abramowitz (2012)' # 'Brutsaert (1975)' or 'Swinbank (1963)' or 'Abramowitz (2012)'


#----------------------------------------------------------------------#


#Read variable data
#File contains desired variables (refer to Fluxnet2015 documentation for full variable descriptions; 
#http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/)
vars <- read.csv(paste(lib_path, "/auxiliary_data/variables.csv", sep=""), header=TRUE, 
                 colClasses=c("character", "character", "character", 
                              "character", "character", "character",
                              "numeric",   "numeric",   "logical", 
                              "character"))

#Read site information (lon, lat, elevation)   NOT YET WORKING
site_info <- read.csv()


# Read text file containing flux data:
DataFromText = ReadTextFluxData(fileinname=infile, vars=vars)

# Make sure whole number of days in dataset:
CheckSpreadsheetTiming(DataFromText)

# Check which variables are actually present:
gaps_found <- CheckDataGaps(datain=DataFromText, missing_val=SprdMissingVal) # list of variables, TRUE or FALSE


### Synthesize LWdown if not found, 
### and gap-fill otherwise if missing values present
if(LWdown_fill) DataFromText <- LWdown_check_and_fill(indata=DataFromText, 
                                                      defaultLWsynthesis=defaultLWsynthesis, 
                                                      vars=vars, gaps_found=gaps_found)


#Remove variables if a variable has too many gaps
#Abort code if any of these essential




# Change units:  NOT YET WORKING
ConvertedData <- ChangeUnits(DataFromText, site_info$elevation)


# Run preliminary tests on data:   #NOT YET WORKING
CheckTextDataRanges(ConvertedData)




# Create netcdf met driving file: NOT YET CONVERTING DATA, CHANGE indata VARIABLE ONCE DONE THAT !!!!!!!!!!!!!!
CreateMetNcFile(metfilename=metfilename, datain=DataFromText, 
                latitude=NA, longitude=NA, 
                datasetname=datasetname, datasetversion=datasetversion, 
                defaultLWsynthesis=defaultLWsynthesis, 
                starttime=DataFromText$starttime, 
                timestepsize=DataFromText$timestepsize,
                elevation=NA, measurementheight=NA, canopyheight=NA,
                vegetationtype=NA, utcoffset=NA, avprecip=NA, avtemp=NA)


# Create netcdf flux data file:
CreateFluxNcFile(fluxfilename=fluxfilename, datain=DataFromText, 
                 latitude=NA, longitude=NA, 
                 datasetname=datasetname, datasetversion=datasetversion, 
                 starttime=DataFromText$starttime, 
                 timestepsize=DataFromText$timestepsize,
                 elevation=NA, measurementheight=NA, canopyheight=NA,
                 vegetationtype=NA, utcoffset=NA, avprecip=NA, avtemp=NA)
                 



