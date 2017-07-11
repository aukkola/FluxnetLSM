#' Timing_general.R
#'
#' Functions that assess or convert timing variables.
#'
#' TODO: Check and merge back in to palsR
#'
#' @export
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
#' @export
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
#' @export
convertToHoursMins <- function(time, format = '%02d%02d') 
{
  hours  <-  floor(time / 60)
  minutes <-  (time %% 60)
  return(sprintf(format, hours, minutes))
}

#-----------------------------------------------------------------------------


#' Converts La Thuile time format to Fluxnet2015 time format
#' @export
convert_LaThuile_time <- function(timestep, tstepsize){
  
  library(pals)
          
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
#' @export
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
#' @export
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
#' @export
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
#' Finds leap years
#' @export
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
#' @export
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
#' @export
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

#-----------------------------------------------------------------------------



