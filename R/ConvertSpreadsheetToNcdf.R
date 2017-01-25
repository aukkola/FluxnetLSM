# ConvertSpreadsheetToNcdf.R
#
# Converts data from a PALS formatted spreadhseet to
# netcdf.
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)

library(R.utils)
#NEED TO SET THESE ELSEWHERE IN FINAL CODE !!!!!!!!!

#path
lib_path <- "~/Documents/FLUXNET2016_processing/scripts/R"


source(paste(lib_path, "/functions/Constants.R", sep=""))
source(paste(lib_path, "/functions/Timing_general.R", sep=""))
source(paste(lib_path, "/functions/Conversions.R", sep=""))
source(paste(lib_path, "/functions/UtilityFunctions.R", sep=""))
source(paste(lib_path, "/functions/Check_and_Gapfill.R", sep=""))
source(paste(lib_path, "/functions/Timing_netcdf.R", sep=""))
source(paste(lib_path, "/functions/FluxtowerSpreadsheetToNc.R", sep=""))


#input filename
infile <- "~/Documents/FLUXNET2016_processing/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv"


#ERA input file (needed if using ERAinterim to gapfill met variables)
era_file <- "~/Documents/FLUXNET2016_processing/FLX_AU-How_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv"


#How many percent of time steps allowed to be missing in any given year?
threshold <- 20

#Minimum numbers of years to process
min_yrs <- 2

#output file
metfilename <- "~/Documents/FLUXNET2016_processing/output_test_met.nc"
fluxfilename <- "~/Documents/FLUXNET2016_processing/output_test_flux.nc"

site_name <- "AU-How"

datasetname <- "Fluxnet2015" 
datasetversion <- "Nov16"

#Synthesize LWdown?
LWdown_synthesize <- FALSE

defaultLWsynthesis <- 'Abramowitz (2012)' # 'Brutsaert (1975)' or 'Swinbank (1963)' or 'Abramowitz (2012)'

#Gapfill met variables using ERAinterim?
ERA_gapfill <- TRUE


#----------------------------------------------------------------------#


#Read variable data
#File contains desired variables (refer to Fluxnet2015 documentation for full variable descriptions; 
#http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/)
vars <- read.csv(paste(lib_path, "/auxiliary_data/variables.csv", sep=""), header=TRUE, 
                 colClasses=c("character", "character", "character", 
                              "character", "character", "character",
                              "character",
                              "numeric",   "numeric",   
                              "logical", "logical",
                              "character"))


#Name of time stamp variables
time_vars <- c("TIMESTAMP_START", "TIMESTAMP_END")


#Read site information (lon, lat, elevation)   NOT YET WORKING
site_info_all <- read.csv(paste(lib_path, "/auxiliary_data/Site_info_tier1_only.csv", sep=""), header=TRUE)

#Extract info for site being processed
site_info <- site_info_all[which(site_info_all$SiteCode==site_name),]

#Should site be excluded? If so, abort and print reason.
#This option is set in the site info file (inside auxiliary data folder)
#Mainly excludes sites with mean annual ET excluding P, implying
#irrigation or other additional water source.
if(site_info$Exclude){
  CheckError(paste("Site not processed. Reason:", site_info$Exclude_reason,
                   ". This is set in site info file, change >Exclude< options 
                   in the file to process site"))
}


# Read text file containing flux data:
DataFromText = ReadTextFluxData(fileinname=infile, vars=vars,
                                time_vars=time_vars)


# Make sure whole number of days in dataset:
CheckSpreadsheetTiming(DataFromText)

# Check if variables have gaps in the time series:
gaps  <- CheckDataGaps(datain = DataFromText, missing_val = SprdMissingVal,
                            threshold = threshold,
                            essential_met = vars$ALMA_variable[which(vars$Essential_met)],
                            preferred_eval = vars$ALMA_variable[which(vars$Preferred_eval)]) 

  
  
#Remove evaluation variables that have too many gaps
  
  
  


### Synthesize LWdown if not found, 
### and gap-fill otherwise if missing values present
if(LWdown_synthesize) {
  
  DataFromText <- LWdown_check_and_fill(indata=DataFromText, 
                                        defaultLWsynthesis=defaultLWsynthesis, 
                                        vars=vars, gaps_found=gaps_found)
}






###--- Gapfill meteorological variables ---### 
# gapfill using ERA-interim data provided as part of FLUXNET2015
if(ERA_gapfill){
  
  era_data <- read.csv(era_file, header=TRUE, colClasses=c("character", "character", 
                                                           rep("numeric", 7)))
  
  #ERAinterim data provided for 1989-2014, need to extract common years with flux obs
  #Find start and end
  obs_start <- DataFromText$time$TIMESTAMP_START
  start_era <- which(era_data$TIMESTAMP_START == obs_start[1])
  end_era   <- which(era_data$TIMESTAMP_START == obs_start[length(obs_start)])
  #Extract correct time steps
  era_data  <- era_data[start_era:end_era,]
  

  #Find indices for met variables to be gapfilled
  ind <- which(DataFromText$categories=="Met")
  
  #Retrieve VPD and air temp units. Used to convert ERAinterim VPD to RH in gapfill function
  tair_units <- DataFromText$units$original_units[which(DataFromText$vars=="Tair")]
  vpd_units  <- DataFromText$units$original_units[which(DataFromText$vars=="VPD")]
  
  #Gapfill met variables
  temp_data <- GapfillMet(datain=DataFromText$data[,ind], era_data=era_data,
                          era_vars=DataFromText$era_vars[ind],
                          tair_units=tair_units, vpd_units=vpd_units,
                          missing_val=SprdMissingVal)
  
  
  #Check that column names of temp_data and data to be replaced match. Stop if not
  if(!all(colnames(temp_data)==colnames(DataFromText$data[,ind])){
    CheckError("Error gap-filling met data with ERAinterim. Column names of data to be replaced don't match")
  }
  
  
  #Replace original met variables with gap-filled variables
  DataFromText$data[,ind] <- temp_data
  
}

  




# Change units:  NOT YET WORKING
# also add specific humidity variable
ConvertedData <- ChangeUnits(DataFromText, site_info$elevation)


# Check that data are within acceptable ranges:   #NOT YET WORKING
CheckTextDataRanges(ConvertedData)



#Determine number of files to be written
no_files <-



# TO DO: !!!!!!!!

#write years to output file name  !!!!!!!!!!!!!!
   
#write github revision number in netcdf attributes  
  

for(k in 1:no_files){
  
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
  
  
}  
  


