# FluxtowerSpreadsheetToNc.R
#
# A collection of functions to read and convert 
# flux tower data from spreadsheet to netcdf.
#
# author: Anna Ukkola UNSW 2017
#
# TODO: Check and merge back in to palsR
#
#

#-----------------------------------------------------------------------------

# TODO: This function exists in palsR/Gab and has a different signature. Merge?
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
                         time_vars=time_vars, site_log, ...)  
  

  # If using La Thuile dataset, convert to Fluxnet2015 format
  if(datasetname=="LaThuile"){
    
    FluxData <- convert_LaThuile(infiles=fileinname, 
                               #  fair_usage=fair_use,
                                # fair_usage_vec=fair_use_vec,
                                 min_yrs=min_yrs,
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
  
	if( !(timestepsize>=300 && timestepsize<=3600) ){
    error <- paste("Time step size must be between",
                   "300 and 3600 seconds. Time step size",
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
                  time=FluxTime, ntsteps=ntsteps, starttime=starttime, 
                  timestepsize=timestepsize, daysPerYr=intyears$daysperyear,
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
  library(pals)
  
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



#-----------------------------------------------------------------------------

# TODO: This function exists in palsR/Gab and has a different signature. Merge?
#' Creates a netcdf file for flux variables
#' @export
CreateFluxNetcdfFile = function(fluxfilename, datain,                  #outfile file and data
                            latitude, longitude,                   #lat, lon
                            site_code, long_sitename,              #Fluxnet site code and full site name
                            datasetversion, github_rev,            #Dataset version and github revision
                            tier=NA,                               #Fluxnet site tier
                            ind_start, ind_end,                    #time period indices
                            starttime, timestepsize,               #timing info
                            flux_varname, cf_name,                 #Original Fluxnet variable names and CF_compliant names
                            elevation=NA, towerheight=NA,          #Site elevation and flux tower height
                            canopyheight=NA,                       #Canopy height
                            short_veg_type=NA, long_veg_type=NA,   #Long and short IGBP vegetation types
                            missing, gapfill_all, gapfill_good,    #thresholds used in processing
                            gapfill_med, gapfill_poor, min_yrs, 
                            total_missing, total_gapfilled,        #Percentage missing and gap-filled
                            QCmeasured, QCgapfilled,               #QC flag values
                            infile,                                #Input file name
                            var_ind){                              #Indices to extract variables to be written
    
  
  
  # load netcdf library
  library(ncdf4) 
  
  
  #Extract time period to be written
  datain$data <- datain$data[ind_start:ind_end,]
  
  # default missing value for all variables
  missing_value=NcMissingVal
  
  # Define x, y and z dimensions
  xd = ncdim_def('x',vals=c(1),units='')	
  yd = ncdim_def('y',vals=c(1),units='')
  zd = ncdim_def('z',vals=c(1),units='')
  
  # Determine data start date and time:
  timeunits = CreateTimeunits(starttime)
  
  # Create time dimension variable:
  tt=c(0:(length(ind_start:ind_end)-1))
  timedata = as.double(tt*timestepsize)
  
  # Define time dimension:
  td = ncdim_def('time', unlim=TRUE, units=timeunits, vals=timedata)
  
  # VARIABLE DEFINITIONS ##############################################  
  
  #Create variable definitions for time series variables
  var_defs <- lapply(var_ind, function(x) ncvar_def(name=datain$out_vars[x],
                                                    units=datain$units$target_units[x], 
                                                    dim=list(xd,yd,zd,td), 
                                                    missval=missing_value, 
                                                    longname=datain$attributes[x,2]))
  
  
  # First necessary non-time variables:
  # Define latitude:
  latdim <- ncvar_def('latitude','degrees_north',dim=list(xd,yd),
                      missval=missing_value, longname='Latitude')  
  # Define longitude:
  londim <- ncvar_def('longitude','degrees_east',dim=list(xd,yd),
                      missval=missing_value,longname='Longitude')
  
  
  #Then optional non-time variables:
  opt_vars <- list()
  ctr <- 1
  # Define measurement height on tower:
  if(!is.na(towerheight)){
    towheight=ncvar_def('tower_height','m',dim=list(xd,yd),
                        missval=missing_value,longname='Height of flux tower')
    opt_vars[[ctr]] = towheight
    ctr <- ctr + 1
  }  
  # Define site canopy height:
  if(!is.na(canopyheight)){
    canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
                        missval=missing_value,longname='Canopy height')
    opt_vars[[ctr]] = canheight
    ctr <- ctr + 1
  }
  #Define site elevation:
  if(!is.na(elevation)){
    elev=ncvar_def('elevation','m',dim=list(xd,yd),
                   missval=missing_value,longname='Site elevation')
    opt_vars[[ctr]] = elev
    ctr <- ctr + 1
  }
  # Define IGBP short vegetation type:
  if(!is.na(short_veg_type)){
    short_veg=ncvar_def('IGBP_veg_short','-',dim=list(xd,yd), missval=NULL,
                        longname='IGBP vegetation type (short)', prec="char")
    opt_vars[[ctr]] = short_veg
    ctr <- ctr + 1
  }
  # Define IGBP long vegetation type:
  if(!is.na(long_veg_type)){
    long_veg=ncvar_def('IGBP_veg_long','-',dim=list(xd,yd), missval=NULL,
                       longname='IGBP vegetation type (long)', prec="char")
    opt_vars[[ctr]] = long_veg
    ctr <- ctr + 1 
  }
  
  
  # END VARIABLE DEFINITIONS #########################################
  
  ### Create netcdf file ###
  if(length(opt_vars)==0) {
    ncid = nc_create(fluxfilename, vars=append(var_defs, c(list(latdim), list(londim))))
  } else {
    ncid = nc_create(fluxfilename, vars=append(var_defs, c(list(latdim), list(londim), opt_vars)))
  }
  
  
  #### Write global attributes ###
  ncatt_put(ncid,varid=0,attname='Production_time',
            attval=as.character(Sys.time()))
  ncatt_put(ncid,varid=0,attname='Github_revision',  
            attval=github_rev, prec="text")
  ncatt_put(ncid,varid=0,attname='site_code',
            attval=site_code, prec="text")
  ncatt_put(ncid,varid=0,attname='site_name',
            attval=as.character(long_sitename), prec="text")
  ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
            attval=datasetversion, prec="text")
  ncatt_put(ncid,varid=0,attname='Input_file',
            attval=infile, prec="text")
  ncatt_put(ncid,varid=0,attname='Processing_thresholds(%)',
            attval=paste("missing: ", missing,
                         ", gapfill_all: ", gapfill_all,
                         ", gapfill_good: ", gapfill_good,
                         ", gapfill_med: ", gapfill_med,
                         ", gapfill_poor: ", gapfill_poor,
                         ", min_yrs: ", min_yrs,
                         sep=""), prec="text")
  ncatt_put(ncid,varid=0,attname='QC_flag_descriptions',
            attval=paste("Measured: ", QCmeasured, 
                         ", Good-quality gapfilled: ", QCgapfilled[1], 
                         ", Medium-quality gapfilled: ", QCgapfilled[2], 
                         ", Poor-quality gapfilled: ", QCgapfilled[3], 
                         ", ERA-Interim gapfilled: ", QCgapfilled[4], 
                         sep=""), prec="text")  
  ncatt_put(ncid,varid=0,attname='Package contact',
            attval='a.ukkola@unsw.edu.au')
  ncatt_put(ncid,varid=0,attname='PALS contact',
            attval='palshelp@gmail.com')
  if(!is.na(tier)) {
    ncatt_put(ncid,varid=0,attname='Fluxnet site tier',
              attval=tier) }
    
  # Add variable data to file:
  ncvar_put(ncid, latdim, vals=latitude)
  ncvar_put(ncid, londim, vals=longitude)
  
  
  # Optional meta data for each site:
  if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
  if(!is.na(towerheight)) {ncvar_put(ncid,towheight,vals=towerheight)}
  if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
  if(!is.na(short_veg_type)) {ncvar_put(ncid,short_veg,vals=short_veg_type)}
  if(!is.na(long_veg_type)) {ncvar_put(ncid,long_veg,vals=long_veg_type)}
  
  
  
  # Time dependent variables:
  lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid, 
                                                   varid=var_defs[[x]], 
                                                   vals=datain$data[,var_ind[x]]))
  
  
  #Add original Fluxnet variable name to file
  lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   attname="Fluxnet_name", 
                                                   attval=datain$attributes[var_ind[x],1], 
                                                   prec="text"))  
  
  #Add CF-compliant name to file (if not missing)
  lapply(1:length(var_defs), function(x)  ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                    attname="Standard_name", 
                                                    attval=datain$attributes[var_ind[x],3], 
                                                    prec="text"))
  
  
  #Add missing percentage to file
  lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   attname="Missing_%", 
                                                   attval=round(total_missing[x],1)))  
  
  #Add gap-filled percentage to file
  lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   attname="Gap-filled_%", 
                                                   attval=round(total_gapfilled[x],1)))  
  
  
  
  
  
  # Close netcdf file:
  nc_close(ncid)
}


#-----------------------------------------------------------------------------


# TODO: This function exists in palsR/Gab and has a different signature. Merge?
#' Creates a netcdf file for met variables
#' @export
CreateMetNetcdfFile = function(metfilename, datain,                   #outfile file and data
                           latitude, longitude,                   #lat, lon
                           site_code, long_sitename,              #Fluxnet site code and full site name
                           datasetversion, github_rev,            #Dataset version and github revision
                           tier=NA,                               #Fluxnet site tier
                           ind_start, ind_end,                    #time period indices
                           starttime, timestepsize,               #timing info
                           flux_varname, cf_name,                 #Original Fluxnet variable names and CF_compliant names
                           elevation=NA, towerheight=NA,          #Site elevation and flux tower height
                           canopyheight=NA,                       #Canopy height
                           short_veg_type=NA, long_veg_type=NA,   #Long and short IGBP vegetation types
                           av_precip=NA,                          #average annual rainfall
                           missing, gapfill_all, gapfill_good,    #thresholds used in processing
                           gapfill_med, gapfill_poor, min_yrs,
                           total_missing, total_gapfilled,        #Percentage missing and gap-filled
                           QCmeasured, QCgapfilled,               #QC flag values
                           ERA_gapfill=ERA_gapfill,               #Was ERA gapfilling used
                           infile,                                #Input file name
                           var_ind){                              #Indices to extract variables to be written
    
  
  # load netcdf library
	library(ncdf4) 
  

  #Extract time period to be written
  datain$data <- datain$data[ind_start:ind_end,]
  
	# default missing value for all variables
	missing_value=NcMissingVal
  
	# Define x, y and z dimensions
	xd = ncdim_def('x',vals=c(1),units='')	
	yd = ncdim_def('y',vals=c(1),units='')
	zd = ncdim_def('z',vals=c(1),units='')
  
	# Determine data start date and time:
	timeunits = CreateTimeunits(starttime)
  
	# Create time dimension variable:
	tt=c(0:(length(ind_start:ind_end)-1))
	timedata = as.double(tt*timestepsize)
  
	# Define time dimension:
	td = ncdim_def('time', unlim=TRUE, units=timeunits, vals=timedata)
  
	# VARIABLE DEFINITIONS ##############################################

  
  #Create variable definitions for time series variables
  var_defs <- lapply(var_ind, function(x) ncvar_def(name=datain$out_vars[x],
                                                    units=datain$units$target_units[x], 
                                                    dim=list(xd,yd,zd,td), 
                                                    missval=missing_value, 
                                                    longname=datain$attributes[x,2]))
  
  
	# First necessary non-time variables:
	# Define latitude:
	latdim <- ncvar_def('latitude','degrees_north',dim=list(xd,yd),
	                   missval=missing_value, longname='Latitude')  
	# Define longitude:
	londim <- ncvar_def('longitude','degrees_east',dim=list(xd,yd),
	                   missval=missing_value,longname='Longitude')
  
  
	#Then optional non-time variables:
	opt_vars <- list()
  ctr <- 1
	# Define measurement height on tower:
	if(!is.na(towerheight)){
	  towheight=ncvar_def('tower_height','m',dim=list(xd,yd),
	                      missval=missing_value,longname='Height of flux tower')
	  opt_vars[[ctr]] = towheight
    ctr <- ctr + 1
	}  
  # Define site canopy height:
	if(!is.na(canopyheight)){
	  canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
	                      missval=missing_value,longname='Canopy height')
	  opt_vars[[ctr]] = canheight
	  ctr <- ctr + 1
	}
  #Define site elevation:
	if(!is.na(elevation)){
	  elev=ncvar_def('elevation','m',dim=list(xd,yd),
	                      missval=missing_value,longname='Site elevation')
	  opt_vars[[ctr]] = elev
	  ctr <- ctr + 1
	}
	# Define IGBP short vegetation type:
	if(!is.na(short_veg_type)){
	  short_veg=ncvar_def('IGBP_veg_short','-',dim=list(xd,yd), missval=NULL,
                        longname='IGBP vegetation type (short)', prec="char")
	  opt_vars[[ctr]] = short_veg
	  ctr <- ctr + 1
	}
	# Define IGBP long vegetation type:
	if(!is.na(long_veg_type)){
	  long_veg=ncvar_def('IGBP_veg_long','-',dim=list(xd,yd), missval=NULL,
                       longname='IGBP vegetation type (long)', prec="char")
	  opt_vars[[ctr]] = long_veg
	  ctr <- ctr + 1 
	}
  #Define AvPrecip (average annual precip) if outputting rainfall
	if(!is.na(av_precip)){
	  av_rain=ncvar_def('avPrecip','mm yr-1',dim=list(xd,yd), missval=NA,
	                     longname='Mean annual precipitation')
	  opt_vars[[ctr]] = av_rain
	  ctr <- ctr + 1  
	}
	

	# END VARIABLE DEFINITIONS #########################################
  
  ### Create netcdf file ###
	if(length(opt_vars)==0) {
    ncid = nc_create(metfilename, vars=append(var_defs, c(list(latdim), list(londim))))
	} else {
	  ncid = nc_create(metfilename, vars=append(var_defs, c(list(latdim), list(londim), opt_vars)))
	}
  
  
	#### Write global attributes ###
  ncatt_put(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	ncatt_put(ncid,varid=0,attname='Github_revision',  
		attval=github_rev, prec="text")
	ncatt_put(ncid,varid=0,attname='site_code',
		attval=site_code, prec="text")
  ncatt_put(ncid,varid=0,attname='site_name',
          attval=as.character(long_sitename), prec="text")
  ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
		attval=datasetversion, prec="text")	 
	ncatt_put(ncid,varid=0,attname='Input_file',
	          attval=infile, prec="text")
	ncatt_put(ncid,varid=0,attname='Processing_thresholds(%)',
	          attval=paste("missing: ", missing,
	                       ", gapfill_all: ", gapfill_all,
	                       ", gapfill_good: ", gapfill_good,
	                       ", gapfill_med: ", gapfill_med,
	                       ", gapfill_poor: ", gapfill_poor,
	                       ", min_yrs: ", min_yrs,
	                       sep=""), prec="text")
	ncatt_put(ncid,varid=0,attname='QC_flag_descriptions',
	          attval=paste("Measured: ", QCmeasured, 
                         ", Good-quality gapfilled: ",QCgapfilled[1], 
                         ", Medium-quality gapfilled: ", QCgapfilled[2], 
                         ", Poor-quality gapfilled: ", QCgapfilled[3], 
                         ", ERA-Interim gapfilled: ", QCgapfilled[4], 
                         sep=""), prec="text")  
	ncatt_put(ncid,varid=0,attname='Package contact',
	          attval='a.ukkola@unsw.edu.au')
	ncatt_put(ncid,varid=0,attname='PALS contact',
		attval='palshelp@gmail.com')
	if(!is.na(tier)) {
	  ncatt_put(ncid,varid=0,attname='Fluxnet site tier',
	            attval=tier) }
	
	# Add variable data to file:
	ncvar_put(ncid, latdim, vals=latitude)
	ncvar_put(ncid, londim, vals=longitude)

  
	# Optional meta data for each site:
	if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
	if(!is.na(towerheight)) {ncvar_put(ncid,towheight,vals=towerheight)}
  if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
	if(!is.na(short_veg_type)) {ncvar_put(ncid,short_veg,vals=short_veg_type)}
  if(!is.na(long_veg_type)) {ncvar_put(ncid,long_veg,vals=long_veg_type)}
	if(!is.na(av_precip)) {ncvar_put(ncid,av_rain,vals=av_precip)}

 
	# Time dependent variables:
  lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid, 
                                                   varid=var_defs[[x]], 
                                                   vals=datain$data[,var_ind[x]]))
  
      	
	#Add original Fluxnet variable name to file
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], attname="Fluxnet_name", 
                                                   attval=datain$attributes[var_ind[x],1], prec="text"))  
	
	#Add CF-compliant name to file (if not missing)
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   attname="Standard_name", 
                                                   attval=datain$attributes[var_ind[x],3], 
                                                   prec="text"))

	#Add ERA-Interim name to file when available (if used)
  if(ERA_gapfill){
    lapply(1:length(var_defs), function(x) if(!is.na(datain$era_vars[var_ind[x]])){ 
                                           ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                           attname="ERA-Interim variable used in gapfilling", 
                                           attval=datain$era_vars[var_ind[x]], 
                                           prec="text")})    
  }
 
	#Add missing percentage to file
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
	                                                 attname="Missing_%", 
	                                                 attval=round(total_missing[x],1)))  
	
	#Add gap-filled percentage to file
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
	                                                 attname="Gap-filled_%", 
	                                                 attval=round(total_gapfilled[x],1)))  
	
	

	# Close netcdf file:
	nc_close(ncid)
}

