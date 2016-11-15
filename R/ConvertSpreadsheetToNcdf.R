# ConvertSpreadsheetToNcdf.R
#
# Converts data from a PALS formatted spreadhseet to
# netcdf.
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)

#NEED TO SET THESE ELSEWHERE IN FINAL CODE !!!!!!!!!

#path
lib_path <- "~/Documents/FLUXNET2016_processing/scripts/R"

#filename
file <- "~/Documents/FLUXNET2016_processing/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv"

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
                              "numeric", "numeric", "logical"))

# Read text file containing flux data:
DataFromText = ReadTextFluxData(fileinname=file, vars=vars)

# Make sure whole number of days in dataset:
CheckSpreadsheetTiming(DataFromText)

# Check which variables are actually present:
gaps_found <- CheckDataGaps(datain=DataFromText, missing_val=SprdMissingVal) # list of variables, TRUE or FALSE


### Synthesize LWdown if not found, 
### and gap-fill otherwise if missing values present
if(LWdown_fill) DataFromText <- LWdown_check_and_fill(DataFromText)




#Remove variables if a variable has too many gaps
#Abort code if any of these essential





# Change units:
ConvertedData = ChangeMetUnits(DataFromText,found,elevation)


# Run preliminary tests on data:
CheckTextDataRanges(ConvertedData,found)


# Create netcdf met driving file:
CreateMetNcFile(metfilename,ConvertedData,latitude,longitude,
	DataFromText$timestepsize,sitename,datasetversion,defaultLWsynthesis,
	found,DataFromText$starttime,
	elevation=elevation,measurementheight=measurementheight,
	canopyheight=canopyheight,vegetationtype=vegetationtype,
	utcoffset=utcoffset,avprecip=avprecip,avtemp=avtemp)



# Create netcdf flux data file:
CreateFluxNcFile(fluxfilename, ConvertedData,latitude,longitude,
	DataFromText$timestepsize,sitename,datasetversion,
	found,DataFromText$starttime,DataFromText$templateVersion,
	elevation=elevation,measurementheight=measurementheight,
	canopyheight=canopyheight,vegetationtype=vegetationtype,
	utcoffset=utcoffset,avprecip=avprecip,avtemp=avtemp)




