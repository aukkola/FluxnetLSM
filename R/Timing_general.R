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
CheckTiming <- function(datain, site_log) 
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

