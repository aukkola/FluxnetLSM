# Handlers_csv.R
#
# A collection of functions to read and convert 
# flux tower data from spreadsheet.
#
# author: Anna Ukkola UNSW 2017
#


#' Reads comma-delimited text files containing Fluxnet2015 data
#' @param fileinname fluxnet data file, including directory
#' @param vars data.table of variables and their attributes
#' @param time_vars vector of time variables
#' @return list of flux data, variables and timing information
#' @export
ReadCSVFluxData <- function(fileinname, vars, datasetname, time_vars, site_log, ...){
  
  ####### First read available variables, corresponding units and ranges ####
  
	# Get column names and classes:
  # La Thuile sites often have multiple data files, only use first
  # instance here
  tcol <- findColIndices(fileinname=fileinname[1],
                         var_names=vars$Fluxnet_variable,
                         var_classes=vars$Fluxnet_class,
                         essential_vars=vars$Essential_met,
                         preferred_vars=vars$Preferred_eval,
                         time_vars=time_vars, site_log, 
                         datasetname=datasetname)  
  

  # If using La Thuile dataset, convert to Fluxnet2015 format
  if(datasetname=="LaThuile"){
    
    FluxData <- convert_LaThuile(infiles=fileinname, 
                                 tcol=tcol,
                                 site_log=site_log, ...)    
    
    #Rename time vars and column names to match new structure
    time_vars <- c("TIMESTAMP_START", "TIMESTAMP_END")
    
    tcol$time_names <- time_vars
    tcol$all_names  <- c(tcol$time_names, tcol$names)

    colnames(FluxData) <- c(time_vars, colnames(FluxData)[(length(time_vars)+1):ncol(FluxData)])

  #Else assume Fluxnet2015 format
  } else {
    
    # Read flux tower data (skips unwanted columns):
    FluxData <- read.csv(file=fileinname, header=TRUE,	colClasses=tcol$classes)  

  }
  
  
  #Sanity check, does variable order in file match that specified in tcols?
  #Ignore any possible duplicates in tcol$all_names before check
  if(any(colnames(FluxData) != unique(tcol$all_names))) {
    error <- paste("Check variable ordering, variables don't match data", 
               "retrieved from file [ function:", match.call()[[1]], "]")
    stop_and_log(error, site_log)
    return(site_log)
  }
  

  #Split time variables from other variables
  #Extract time stamp data
  time_ind <- sapply(time_vars, function(x) which(colnames(FluxData)==x))
  FluxTime <- FluxData[,time_ind]
  #Remove time stamp variables from Data variable
  FluxData <- FluxData[,-time_ind]
  
  
  #Set all missing values to NA
  FluxData[FluxData==Sprd_MissingVal] <- NA
  
  
  #Duplicate Fluxnet data column if the same variable needs to be
  #processed several times (e.g. RH converted to RH and Qair)
  if(ncol(FluxData) != length(tcol$names))
  {
    
    FluxData <- duplicate_columns(data=FluxData, vars=tcol$names)
    
    #Make sure FluxData now has correct no. of columns
    if(ncol(FluxData) != length(tcol$names)){
      error <- paste("Duplicate variable names exist but columns could", 
                     "not be be duplicated correctly [ function:", 
                     match.call()[[1]], "]")
      stop_and_log(error, site_log)
      return(site_log)
    }
    
  }
  
  #Retrieve original and target units for variables present:
  units <- retrieve_units(vars_present=tcol$names, 
                          all_vars=vars)
  
  #Retrieve acceptable variable ranges:
  var_ranges <- retrieve_ranges(vars_present=tcol$names, 
                                all_vars=vars)
  
  #Retrieve variable attributes (original Fluxnet name, long name and CF name):
  attributes <- retrieve_atts(vars_present=tcol$names, 
                              all_vars=vars)
  
  #Retrieve variable categories (met/eval):
  categories <- retrieve_varinfo(vars_present=tcol$names, 
                                 all_vars=vars, attribute="Category")
  
  #Retrieve names of ERAinterim variables
  era_vars <- retrieve_varinfo(vars_present=tcol$names, 
                               all_vars=vars, attribute="ERAinterim_variable")
  
  #Retrieve output variable names
  out_vars <- retrieve_varinfo(vars_present=tcol$names, 
                               all_vars=vars, attribute="Output_variable")
   
  #Retrieve output variable names
  ess_met <- retrieve_varinfo(vars_present=tcol$names, 
                              all_vars=vars, attribute="Essential_met")

  #Retrieve output variable names
  pref_eval <- retrieve_varinfo(vars_present=tcol$names, 
                                all_vars=vars, attribute="Preferred_eval")
  
  #Retrieve aggregation method
  aggr_method <- retrieve_varinfo(vars_present=tcol$names, 
                                all_vars=vars, attribute="Aggregate_method")
  
  
  ###### Get time step and date information #######
  
	# Note number of time steps in data:
	ntsteps <- nrow(FluxTime)
  
	if(!(ntsteps>=12 && ntsteps < 1e9)){
    error <- paste('Unable to determine number of time steps in:',
			                stripFilename(fileinname))
    stop_and_log(error, site_log)
    return(site_log)
	}
  
	# and time step size (convert to date string)
  start <- strptime(FluxTime$TIMESTAMP_START[1], "%Y%m%d%H%M")
  end   <- strptime(FluxTime$TIMESTAMP_END[1], "%Y%m%d%H%M")
  
	timestepsize <- as.numeric(end) - as.numeric(start)
  
	if( !(timestepsize>=300 && timestepsize<=86400) ){
    error <- paste("Time step size must be between",
                   "300 and 86400 seconds. Time step size",
                   timestepsize, "found in file")
    stop_and_log(error, site_log)
    return(site_log)
	}
  
  # Time steps in a day and number of days in data set
	tstepinday <- 86400/timestepsize 
	ndays      <- ntsteps/tstepinday 

  
  # Find starting date / time:
  starttime <- findStartTime(start=start)
  
  intyears  <- Yeardays(starttime$syear,ndays)
  
  #Create list for function exit:
	filedata <- list(data=FluxData, vars=tcol$names, era_vars=era_vars, 
                  attributes=attributes, out_vars=out_vars,
                  essential_met=ess_met, preferred_eval=pref_eval,
                  units=units, var_ranges=var_ranges, categories=categories,
                  aggr_method=aggr_method, time=FluxTime, ntsteps=ntsteps, 
                  starttime=starttime, timestepsize=timestepsize, 
                  daysPerYr=intyears$daysperyear,
                  ndays=ndays, whole=intyears$whole)

  return(filedata)

}

#-----------------------------------------------------------------------------

#' Reads ERA data and extracts time steps corresponding to obs
#' @export
read_era <- function(ERA_file, datain){
  
  #read data
  era_data <- read.csv(ERA_file, header=TRUE, colClasses=c("character", "character",
                                                           rep("numeric", 7)))
  
  #ERAinterim data provided for 1989-2014, need to extract common years with flux obs
  #Find start and end
  obs_start <- datain$time$TIMESTAMP_START
  start_era <- which(era_data$TIMESTAMP_START == obs_start[1])
  end_era   <- which(era_data$TIMESTAMP_START == obs_start[length(obs_start)])
  
  #Extract correct time steps
  era_data  <- era_data[start_era:end_era,]
  
  return(era_data)
  
}

#-----------------------------------------------------------------------------

#' Converts La Thuile files to FLUXNET2015 format
#' @export
convert_LaThuile <- function(infiles, fair_usage=NA, fair_usage_vec=NA, 
                             min_yrs, tcol, site_log, site_code){
  
  library(R.utils) #seqToIntervals
  
  ### Find files to process ###
  
  #Find all available data years
  all_years <- sapply(infiles, function(x) strsplit(x, "[.]")[[1]][2])
  
  #Find Fair Use years if applicable
  if(!is.na(fair_usage)){
    
    #Find indices for years that comply with fair use policy
    fair_ind <- unlist(sapply(fair_usage, function(x) which(fair_usage_vec==x)))
    
    #Extract years
    fair_use_years <- names(fair_usage_vec)[fair_ind]
    
    #Find years that are fair use and have files for
    years <- as.numeric(intersect(all_years,fair_use_years))
    
  } else {
    
    warning("Not using Fair Use policy to extract years")
    
    #Find years that are fair use and have files for
    years <- as.numeric(all_years)
    
  }
  

  #Check what years are consecutive
  consec <- seqToIntervals(years)
  
  #Find and remove consecutive time periods that are shorter than min_yrs
  rm_ind <- apply(consec, MARGIN=1, function(x) length(x[1]:x[2]) < min_yrs)

  if(any(rm_ind)) consec <- consec[-which(rm_ind), ]
    
  
  #If no periods to process, return error
  if(nrow(consec)==0){
    
    error <- paste("No years to process,",
                   "available time period too short.")
    stop_and_log(error, site_log)
    return(site_log)    
  }
  
   
  ### Read data for each year ###
  
  #Initialise (this should probably be written differently,
  #so data frame is pre-allocated properly)
  data <- vector()
  
  ### If all years consecutive
  if(nrow(consec)==1){
    
    #Loop through years
    for(y in consec[1,1]:consec[1,2]){
      
      data <- rbind(data, read.csv(infiles[grepl(y, infiles)], 
                                   header=TRUE, 
                                   colClasses=tcol$classes))
    }
    
  ### Some years non-consecutive
  } else {
    
    #Time period from min to max
    tperiod <- seq(min(consec), max(consec), by=1)
    
    #Available years
    avail_yrs <- apply(consec, MARGIN=1, function(x) x[1]:x[2])
    avail_yrs <- sort(unlist(avail_yrs))
        
    #Find years missing in the middle
    missing_yrs <- setdiff(tperiod, avail_yrs)
    
    
    #Loop through years
    #If year available, read file. Else create 
    #an empty year with correct no. of time steps
    
    #First, remove path from file names. This is in case the file path
    #contains the same string as the year(s). This could lead to wrong file
    #being read
    infile_short <- sapply(infiles, function(x) strsplit(x, site_code)[[1]][2])

    for(y in 1:length(tperiod)){
      
      #Year available
      if(any(avail_yrs==tperiod[y])){
             
        #Find file corresponding to year
        file_to_read <- which(grepl(tperiod[y], infile_short))
        
        data <- rbind(data, read.csv(infiles[file_to_read], 
                                     header=TRUE, 
                                     colClasses=tcol$classes))
        
        #Determine number of time steps per day
        if(y==1) tstep_per_day <- (24*60) / (data$Time[1] * 60)
        
      } else {
        
        #Create time information
        time_vec <- create_dummy_year(year=tperiod[y], tstep=tstep_per_day, time=tcol$time_names)
        
        #Set other variables to missing value (set colnames to row-binding works)
        dummy_mat <- matrix(data=SprdMissingVal, nrow=nrow(time_vec), ncol=ncol(data)-length(tcol$time_names))
        colnames(dummy_mat) <- colnames(data)[(length(tcol$time_names)+1):ncol(data)]
        
        #Append to data frame
        data <- rbind(data, cbind(time_vec, dummy_mat))  
        
      }
    } #years

  } #consec/non-consec
  
   

  #### Convert time stamps to Fluxnet2015 format ####
  
  #Define timestep size. Maybe already done, in which case can pass it to function?
  tstepsize <- (data$Time[2] * 60) - (data$Time[1] * 60)

  #Find which columns have time info
  time_cols <- sapply(tcol$time_names, function(x) which(colnames(data)==x))
    
  #Convert time
  new_time <- t(apply(data[,time_cols], MARGIN=1, function(x) convert_LaThuile_time(x, tstepsize=tstepsize)))
  
  
  #Check that time and data matrices have same dimensions
  if(nrow(new_time) != nrow(data)){
    stop("Problem appending La Thuile data into a Fluxnet2015 format")
  }
  
  
  #Append new time and data
  converted_data <- cbind(new_time, data[,-time_cols])
  
  return(converted_data) 
  
}
