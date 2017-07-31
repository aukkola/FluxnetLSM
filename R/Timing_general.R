#' Timing_general.R
#'
#' Functions that assess or convert timing variables.
#'
#' TODO: Check and merge back in to palsR
#'
#' 
findStartTime <- function(start){
  
  # Find starting date / time:
  sday   <- as.numeric(format(start, format="%d"))
  smonth <- as.numeric(format(start, format="%m"))
  syear  <- as.numeric(format(start, format="%Y"))
  
  shod   <- as.numeric(format(start, format="%H")) # starting hour of day
  
  # Collate start time variables:
  starttime <- list(syear=syear,smonth=smonth,sday=sday,shod=shod)
  
  
  return(starttime)

}

#-----------------------------------------------------------------------------

#' Checks that have whole number of days in dataset
CheckCSVTiming <- function(datain, site_log) 
{
  #This is equivalent to PALS CheckSpreadsheetTiming function
  #but reproduced here to allow different error handling
  
  tstepinday = 86400/datain$timestepsize
  ndays = datain$ntsteps/tstepinday
  if ((ndays - round(ndays)) != 0) {
    error <- paste("S2: Spreadsheet does not appear to contain a", 
                     "whole number of days of data. Please amend.")
    stop_and_log(error, site_log)
  }
  if ((datain$starttime$sday != 1) | (datain$starttime$smonth != 1)) {
    error <- paste("S2: Spreadsheet data does not appear to begin", 
                     "on 1st January. Please amend.")
    stop_and_log(error, site_log)
  }
}



#-----------------------------------------------------------------------------


#' Utility function to convert minutes to HHMM format
convertToHoursMins <- function(time, format = '%02d%02d') 
{
  hours  <-  floor(time / 60)
  minutes <-  (time %% 60)
  return(sprintf(format, hours, minutes))
}

#-----------------------------------------------------------------------------


#' Converts La Thuile time format to Fluxnet2015 time format
convert_LaThuile_time <- function(timestep, tstepsize){
  #Last time step of each year uses the next year, fix this
  if( timestep["DoY"] > 364 & timestep["Time"] == 0){
    timestep["Year"] <- timestep["Year"] - 1 
  }
  
  #Last time step of each day needs fixing
  #(change to 24:00 hrs on the day, instead of 00:00 hours next day)
  if(timestep["Time"] == 0){
    
    timestep["DoY"] <-  timestep["DoY"] - 1
    timestep["Time"] <-  timestep["Time"] + 24
    
  } 
  
  
  #Extract time info
  year <- timestep["Year"]
  doy  <- doydate(doy=timestep["DoY"], leap=is.leap(timestep["Year"]))
  
  hod  <-  convertToHoursMins((timestep["Time"] * 60) - tstepsize)
  
  
  #Create string in format YYYYMMDDHHMM
  starttime <- paste(year, Create2Uchar(doy$month), Create2Uchar(doy$day), 
                     hod, sep="")
  
  #Add time step size to create end time.
  #strptime changes time zone in some cases, resulting in wrong time stampe
  #maybe trying to convert between summer and standard time, not sure? 
  #Fixing this by setting time zone to GMT
  endtime <- strptime(starttime, "%Y%m%d%H%M", tz="GMT") + tstepsize * 60
  endtime <- format(endtime, "%Y%m%d%H%M")
  
  
  #Some sanity checks
  #Check that no NAs and endtime is greater than starttime
  if((is.na(starttime) | is.na(endtime)) | 
    (strptime(endtime,  "%Y%m%d%H%M", tz="GMT") <
    strptime(starttime,  "%Y%m%d%H%M", tz="GMT"))){
    
    stop("Cannot convert La Thuile time stamps correctly")
    
  }
  
  return(cbind(starttime, endtime))
  
}


#-----------------------------------------------------------------------------

#' Function for filling missing years in La Thuile dataframe
create_dummy_year <- function(year, tstep, time_vars){
  
  #Create a matrix for elements Year, DoY, Time and DTIME
  
  #Days (repeat tstep number of times)
  yr_days <- 1 : sum(getMonthDays(is.leap(year))$length)

  rep_days <- rep(yr_days, each=tstep)
  
  #Daily time steps (repeat yr_days number of times)
  tsteps_day <- seq(24/tstep, by=24/tstep, length.out=tstep)
  tsteps_day <- rep(tsteps_day, times=length(yr_days))
  
  
  #If lengths don't match, stop
  if(length(tsteps_day) != length(rep_days)){
    stop("Error creating time steps in create_dummy_year")
  }
  
  
  #Repeat year
  rep_year <- rep(year, times=length(yr_days))
  
  #Repeat DTIME (not used for anything so just use NA)
  dtime <- rep(NA, times=length(yr_days))
  
  #Collate to a matrix 
  time <- cbind(rep_year, rep_days, tsteps_day, dtime)
    
  #Set column names
  colnames(time) <- time_vars

  return(time)

}

#-----------------------------------------------------------------------------

#' Finds day and night time steps
DayNight <- function (SWdown, units) 
{
  
  #Adapted from PALS
  
  #Minor modifications to allow for PAR and SWdown
  if(units=="umol/m2/s"){
    threshold <- 5 / (1/2.3)
  } else{
    threshold <- 5
  }
  
  #Initialise with NA
  daynotnight = rep(NA, length(SWdown))
  
  #Set daytime to TRUE
  daynotnight[SWdown > threshold] <- TRUE
  
  #Set nighttime to FALSE
  daynotnight[SWdown <= threshold] <- FALSE
  
  
  return(daynotnight)
}

#-----------------------------------------------------------------------------

# These functions are reproduced from PALS
# Author: Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)

#' Finds number of days per year
Yeardays <- function(startyear,ndays) {
  # Returns: an integer vector of possible number of days in each year of a 
  # dataset, and whether it contains a whole number of years
  if(ndays<365){
    whole=FALSE
    daysperyear = ndays
  }
  daysperyear = c()
  ctr=0 # initialise
  year = startyear # initialise
  days=ndays # initialise
  lpyrs = 0
  # Incrementally remove year of days from total number of days:
  repeat {
    ctr = ctr + 1
    if(is.leap(year)){	
      days = days - 366
      daysperyear[ctr] = 366
      lpyrs = lpyrs + 1
    }else{
      days = days - 365
      daysperyear[ctr] = 365
    }
    year = year + 1
    if(days<365){
      if(days>0 && days!=(365-lpyrs)){ # ie. after removing whole years, days are left over
        daysperyear[ctr+1] = days
        whole=FALSE
      }else if(days==(365-lpyrs)){ # i.e. non leap year data set
        daysperyear[ctr+1] = days
        whole=TRUE
      }else{ # =0
        whole=TRUE
      }
      break
    }
  }
  # Create return list:
  yeardays = list(daysperyear=daysperyear,whole=whole)
  return(yeardays)
}

#----

#' Gets month indices
getMonthDays = function(leap=FALSE) {
  # The days on which each month begins:
  if (leap) {    #  J   F   M   A    M    J    J    A    S    O    N    D    J
    month_start=c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367)
    feb = 29
  } else {
    month_start=c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
    feb = 28
  }
  month_length=c(31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  return(list(start=month_start,length=month_length))
}

#----

#' Finds leap years
is.leap = function(year){
  if((((year %% 4)==0) & ((year %% 100)!=0)) || 
       (((year %% 4)==0) & ((year %% 400)==0))){
    leap=TRUE	
  }else{
    leap=FALSE
  }
  return(leap)
}

#----

#' Creates time unit
CreateTimeunits = function(starttime) {
  # Determine data start date and time:
  shour = floor(starttime$shod)
  smin = floor((starttime$shod - shour)*60)
  ssec = floor(((starttime$shod - shour)*60 - smin)*60)
  start_hod = paste(Create2Uchar(shour),':',Create2Uchar(smin),':',Create2Uchar(ssec),sep='')
  timeunits=paste('seconds since ',as.character(starttime$syear),'-',
                  Create2Uchar(starttime$smonth),'-',Create2Uchar(starttime$sday),' ',
                  start_hod,sep='')
  return(timeunits)
}

#----

#' Creates time stamp string
Create2Uchar = function(intin){
  # Creates string of length 2 from integer of length 1 or 2
  if(intin<10){
    temp=as.character(intin)
    charout=paste('0',temp,sep='')
  }else if(intin>99){
    charout='NA'
    CheckError('I3: Character variable too long in function Create2Uchar.')
  }else{
    charout=as.character(intin)	
  }
  return(charout)
}

#----

#' Gets NC file timing
GetTimingNcfile = function(fid){
  # This function gets the time step size, number of timesteps
  # and start date and time details from a netcdf file.
  errtext='ok'
  # Get the name of the time variable in this file
  timevar = FindTimeVarName(fid)
  if(timevar$err){ # Report fatal error
    timing=list(errtext=timevar$errtext,err=TRUE)
    return(timing)
  }
  # Get time units:
  tunits = GetTimeUnits(fid,timevar$name)
  if(tunits$err){ # Report fatal error
    timing=list(errtext=tunits$errtext,err=TRUE)
    return(timing)
  }
  # Get number of time steps:
  ntsteps = GetNumberTimesteps(fid,timevar)
  
  # Get the time step size:
  tstep = GetTimestepSize(fid,timevar$name,tunits,ntsteps)
  if(tstep$err){ # Report fatal error
    timing=list(errtext=tstep$errtext,err=TRUE)
    return(timing)
  }
  
  # Create return list:
  timing = list(err=FALSE,errtext=errtext,tstepsize=tstep$size,tsteps=ntsteps,
                syear=tunits$syear,smonth=tunits$smonth,sdoy=tunits$sdoy,whole=tstep$wholeyear,interval=tstep$interval)
  return(timing)
}

#----

#' Finds time variable name
FindTimeVarName = function(fid){
  # Finds the name of the time variable in an open netcdf file.
  errtext='ok' 
  exists = FALSE # initialise
  nvars = length(fid$var) # number of variables in netcdf file
  ndims = length(fid$dim) # number of dimensions in netcdf file
  for (v in 1:nvars){ # Search through all variables in netcdf file
    if((substr(fid$var[[v]]$name,1,6)=='t_ave_') | # i.e. ORCHIDEE file
         (substr(fid$var[[v]]$name,1,5)=='mscur') | 
         (fid$var[[v]]$name == 'time')){
      # i.e. 	ORCHIDEE file, CLM file or non-dimension variable named time, respectively
      exists = TRUE
      dimvar = FALSE # i.e. time variable is not a dimension variable
      timevarname = fid$var[[v]]$name
      timedimid = fid$var[[v]]$dimids
      break # leave for loop for variables
    }
  }
  if(!exists){ # i.e. none of the above time variables were found
    # Search for time as a dimension variable:
    for (d in 1:ndims){ # Search through all dimensions in netcdf file
      if(fid$dim[[d]]$name=='time' | fid$dim[[d]]$name=='t' |
           fid$dim[[d]]$name=='time_counter' | fid$dim[[d]]$name=='Time'){
        # Now check for time dimension variable:
        if(fid$dim[[d]]$dimvarid$id != -1){ # i.e. dim var exists	
          exists = TRUE # i.e. found time dimension variable
          dimvar = TRUE # time variable is a dimension variable
          timevarname = fid$dim[[d]]$name
          timedimid = fid$dim[[d]]$id
        }else{ # i.e. time dim exists but no dim var
          errtext = paste('T1: Cannot interpret timing in ',stripFilename(fid$filename),
                          ': time dimension exists but no dimension variable.',sep='')
          timevar = list(err=TRUE,errtext=errtext)
          return(timevar)
        }
        break	
      }
    }
    if(!exists){ # Still cannot identify time variable
      # Return to parent function with error:
      errtext = paste('T1: Unable to ascertain name of time variable in', stripFilename(fid$filename))
      timevar = list(err=TRUE,errtext=errtext)
      return(timevar)
    }
  }
  # Return result:
  timevar = list(err=FALSE, errtext=errtext,name=timevarname,dimid=timedimid,dimvar=dimvar)
  return(timevar)
}

#----

#' Gets time units
GetTimeUnits = function(fid,timevarname){
  # Fetches and processes time units from a netcdf file.
  errtext = 'ok'
  if(substr(timevarname,1,5)=='mscur'){ # i.e. CLM file
    # Read date variable:
    date1=as.character(ncvar_get(fid,'mcdate',start=1,count=1))
    syear = as.numeric(substr(date1,1,4))
    smonth = as.numeric(substr(date1,5,6))
    sdoy = as.numeric(substr(date1,7,8))
    units = 'clm_blah'
  }else{
    units = ncatt_get(fid,timevarname,'units')
    if(! units$hasatt){
      errtext = paste('T1: Unable to find time units in', stripFilename(fid$filename))
      tunits = list(err=TRUE,errtext=errtext)
      return(tunits)	
    }
    if(substr(units$value,1,4)=='seco'){ # time units are seconds
      syear = as.numeric(substr(units$value,15,18))
      smonth = as.numeric(substr(units$value,20,21))
      sdoy = as.numeric(substr(units$value,23,24))
      units = 'seconds'
    }else if(substr(units$value,1,4)=='days'){ # time units are days
      syear = as.numeric(substr(units$value,11,14))
      smonth = as.numeric(substr(units$value,16,17))
      sdoy = as.numeric(substr(units$value,19,20))
      units = 'days'
    }else{
      errtext = paste('T1: Unable to interpret time units in', stripFilename(fid$filename))
      tunits = list(err=TRUE,errtext=errtext)
      return(tunits)	
    }
  }
  tunits = list(err=FALSE,errtext=errtext,syear=syear,smonth=smonth,sdoy=sdoy,units=units)
  return(tunits)	
}

#----

#' Gets time step size
GetTimestepSize = function(fid,timevarname,tunits,ntsteps){
  # Fetches time step size
  errtext = 'ok'
  # Read first 2 timesteps of time variable:
  time=ncvar_get(fid,timevarname,start=1,count=2)
  time_end=ncvar_get(fid,timevarname,start=ntsteps,count=1)
  # Define time step size:
  tsize=time[2]-time[1]
  tperiod=time_end - time[1] + tsize
  
  if(tunits$units == 'days'){
    if((tsize >359) && (tsize < 367)){
      interval = 'annual'
      wholeyear = TRUE
    }else if((tsize>27) && (tsize<32)){
      interval = 'monthly'
      if((ntsteps %% 12) == 0){
        wholeyear = TRUE	
      }else{
        wholesyear==FALSE
      }	
    }else if(tsize==1){
      interval = 'daily'
      intyear = Yeardays(tunits$syear,tperiod)
      wholeyear = intyear$whole	
    }else{
      errtext = paste('T1: Unable to interpret time step size in', stripFilename(fid$filename))
      tstep = list(err=TRUE,errtext=errtext)
      return(tstep)	
    }
  }else if(tunits$units == 'seconds'){
    if(tsize <= (3600*3)){ # i.e. less than 3-hourly
      interval = 'timestep'
      tstepinday=86400/tsize # time steps in a day
      ndays = ntsteps/tstepinday # number of days in file
      intyear = Yeardays(tunits$syear,ndays)
      wholeyear = intyear$whole
    }else if(tsize == (3600*24)){
      interval = 'daily'
      intyear = Yeardays(tunits$syear,(tperiod/3600/24))
      wholeyear = intyear$whole
    }else if( (tsize > (27*24*3600)) && (tsize < (32*24*3600)) ){
      interval = 'monthly'
      if((ntsteps %% 12) == 0){
        wholeyear = TRUE	
      }else{
        wholesyear==FALSE
      }	
    }else if( (tsize > (359*24*3600)) && (tsize < (367*24*3600)) ){
      interval = 'annual'
      wholeyear = TRUE
    }else{
      errtext = paste('T1: Unable to interpret time step size in', stripFilename(fid$filename))
      tstep = list(err=TRUE,errtext=errtext)
      return(tstep)	
    }
    
    
    
  }else{
    errtext = paste('T1: Unable to interpret time units in', stripFilename(fid$filename))
    tstep = list(err=TRUE,errtext=errtext)
    return(tstep)
  }
  tstep = list(err=FALSE,errtext=errtext,size=tsize,interval=interval,wholeyear=wholeyear)
  return(tstep)
}

#----

#' Gets the number of time steps
GetNumberTimesteps = function(fid,timevar){		
  # Gets the number of time steps in a netcdf file
  ndims = length(fid$dim)
  # Find out how many time steps there are in the model file:
  for (d in 1:ndims){ # Search through all dimensions in netcdf file
    if(fid$dim[[d]]$id == timevar$dimid){ # i.e. this is the dim of the time variable
      ntsteps = fid$dim[[d]]$len
      break # stop searching for unlim dim
    }
  }
  return(ntsteps)
}

#----

#' Gets the day of month and month, given day of year
doydate = function(doy,leap=FALSE){
  # Doydate returns the day of month and month, given day of year:
  month=getMonthDays(leap)
  # Find month of this doy
  for(m in 1:12){
    if(doy >= month$start[m] && doy < month$start[m+1]){
      doymonth = m
      doyday = doy - month$start[m] + 1
    }
  }
  date = list(month = doymonth, day = doyday)
  return(date)
}




#-----------------------------------------------------------------------------



