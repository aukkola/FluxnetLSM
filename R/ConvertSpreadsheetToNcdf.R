#' ConvertSpreadsheetToNcdf.R
#'
#' Converts data from a PALS formatted spreadhseet to
#' netcdf.
#'
#' Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)

#' Main function to convert Fluxnet2015 CSV-files to NetCDF
#'
#' @param infile input filename,
#'   e.g. "FULLSET/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv"
#' @param era_file ERA input file (needed if using ERAinterim to gapfill met variables)
#'   e.g. "FULLSET/FLX_AU-How_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv"
#' @param threshold How many percent of time steps allowed to be missing in any given year?
#' @param min_yrs Minimum number of consecutive years to process
#' @param out_path output path e.g. "~/Documents/FLUXNET2016_processing/"
#' @param site_code Fluxnet site code e.g. "AU-How"
#' @param ERA_gapfill Gapfill met variables using ERAinterim?
#' 
#' @export
convert_fluxnet_to_netcdf <- function(infile, site_code, out_path, lib_path,   #REMOVE lib_path from final code !!!!!
                                      ERA_file=NA, ERA_gapfill=FALSE,
                                      datasetname="Fluxnet2015", datasetversion="Nov16",
                                      gap_threshold=20, min_yrs=2) {
  
  library(R.utils)
    
  # TODO: Merge these files with palsR where possible.
  source(paste(lib_path, "/functions/Constants.R", sep=""))
  source(paste(lib_path, "/functions/Timing_general.R", sep=""))
  source(paste(lib_path, "/functions/Conversions.R", sep=""))
  source(paste(lib_path, "/functions/UtilityFunctions.R", sep=""))
  source(paste(lib_path, "/functions/Check_and_Gapfill.R", sep=""))
  source(paste(lib_path, "/functions/Timing_netcdf.R", sep=""))
  source(paste(lib_path, "/functions/FluxtowerSpreadsheetToNc.R", sep=""))
  source(paste(lib_path, "/Site_metadata.R", sep=""))
  

  ################################
  ###--- Read variable data ---###
  ################################

  #File contains desired variables (refer to Fluxnet2015 documentation for full variable descriptions; 
  #http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/)
  vars <- read.csv(paste(lib_path, "/../data/variables.csv", sep=""), header=TRUE,
                   colClasses=c("character", "character", "character", 
                                "character", "character", "character",
                                "character",
                                "numeric",   "numeric",   
                                "logical", "logical",
                                "character"))
  
  
  #Name of time stamp variables
  time_vars <- c("TIMESTAMP_START", "TIMESTAMP_END")
  
  
  #Read site information (lon, lat, elevation)
  site_info <- get_site_metadata(site_code)
  
  #Should site be excluded? If so, abort and print reason.
  #This option is set in the site info file (inside data folder)
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
                              threshold = gap_threshold, min_yrs=min_yrs,
                              essential_met = vars$ALMA_variable[which(vars$Essential_met)],
                              preferred_eval = vars$ALMA_variable[which(vars$Preferred_eval)]) 
  
    
    
  #Remove evaluation variables that have too many gaps    COMPLETE !!!!!!
  
  
  
  
  
  
  
  
  
  
  ##############################################
  ###--- Gapfill meteorological variables ---### 
  ##############################################

  # gapfill using ERA-interim data provided as part of FLUXNET2015
  if(ERA_gapfill){
    
    era_data <- read.csv(ERA_file, header=TRUE, colClasses=c("character", "character", 
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
    if(!all(colnames(temp_data)==colnames(DataFromText$data[,ind]))){
      CheckError("Error gap-filling met data with ERAinterim. Column names of data to be replaced do not match")
    }
    
    
    #Replace original met variables with gap-filled variables
    DataFromText$data[,ind] <- temp_data
    
  }
  
  
  
  # Convert data units from original Fluxnet units
  # to desired units as set in variables.csv
  ConvertedData <- ChangeUnits(DataFromText)
  
   
  # Check that data are within acceptable ranges:   #FUNCTION WORKS BUT:   FIX Qair and VPD!!!!
  CheckTextDataRanges(ConvertedData, missingval=NcMissingVal)
  
  
  #Replace original data with converted data
  DataFromText <- ConvertedData

  
  #Determine number of files to be written (split site according to data gaps if necessary)
  no_files <- length(unique(gaps$consec))
  
  
  
     
  #write github revision number in netcdf attributes  
    
  ####################################################
  ###--- Write output met and flux NetCDF files ---###
  ####################################################

  for(k in 1:no_files){
    
    
    #Find start year, day and hour
    nc_starttime <- findStartTime(start = strptime(DataFromText$time[gaps$tseries_start[k],1], "%Y%m%d%H%M"))
  
    
    #Extract start and end years
    start_yr <- substring(DataFromText$time[gaps$tseries_start[k],1], 1, 4)
    end_yr <- substring(DataFromText$time[gaps$tseries_end[k],1], 1, 4)
    
    
    #Create output file names
    #If only one year, only write start year, else write time period
    if(start_yr==end_yr){
      metfilename  <- paste(out_path, "/", site_code, "_", start_yr, "_", datasetname, "_Met.nc", sep="")
      fluxfilename <- paste(out_path, "/", site_code, "_", start_yr, "_", datasetname, "_Flux.nc", sep="")
      
    } else {   
      metfilename  <- paste(out_path, "/", site_code, "_", start_yr, "-", end_yr, "_", datasetname, "_Met.nc", sep="")
      fluxfilename <- paste(out_path, "/", site_code, "_", start_yr, "-", end_yr, "_", datasetname, "_Flux.nc", sep="")
      
    }
    
  
    ### Create netcdf met driving file ###
    CreateMetNcFile( metfilename=metfilename, datain=DataFromText,                   
                     latitude=site_info$SiteLatitude, 
                     longitude=site_info$SiteLongitude,                
                     site_code=site_code,
                     long_sitename=site_info$Fullname,
                     datasetversion=datasetversion, 
                     github_rev=site_info$Processing$git_rev,
                     tier=site_info$Tier,                                
                     ind_start=gaps$tseries_start[k], 
                     ind_end=gaps$tseries_end[k],                  
                     starttime=nc_starttime, 
                     timestepsize=DataFromText$timestepsize,             
                     elevation=site_info$SiteElevation, 
                     towerheight=site_info$TowerHeight,        
                     canopyheight=site_info$CanopyHeight,                      
                     short_veg_type=site_info$IGBP_vegetation_short, 
                     long_veg_type=site_info$IGBP_vegetation_long)
      
      
      
    ### Create netcdf flux data file ###
    CreateFluxNcFile(fluxfilename=fluxfilename, datain=DataFromText,                   
                     latitude=site_info$SiteLatitude, 
                     longitude=site_info$SiteLongitude,                
                     site_code=site_code,
                     long_sitename=site_info$Fullname,
                     datasetversion=datasetversion, 
                     github_rev=site_info$Processing$git_rev,
                     tier=site_info$Tier,                                
                     ind_start=gaps$tseries_start[k], 
                     ind_end=gaps$tseries_end[k],                  
                     starttime=nc_starttime, 
                     timestepsize=DataFromText$timestepsize,             
                     elevation=site_info$SiteElevation, 
                     towerheight=site_info$TowerHeight,        
                     canopyheight=site_info$CanopyHeight,                      
                     short_veg_type=site_info$IGBP_vegetation_short, 
                     long_veg_type=site_info$IGBP_vegetation_long)
      
      
  }  
   


} #function
