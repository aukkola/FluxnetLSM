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


