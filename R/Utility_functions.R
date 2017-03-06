# Utility_functions.R
# 
# Functions for error and warning handling


#' Append error message to log
#' @export
log_error <- function(errtext, site_log){
  
  site_log["Errors"] <- paste(site_log["Errors"], errtext, sep="")
  site_log["Processed"] <- FALSE
  
  return(site_log)
}

#-----------------------------------------------------------------------------

#' Appends warning to site log
#' @export
log_warning <- function(warn, site_log){
  
  if(nchar(warn) > 0){
    site_log["Warnings"] <- paste(site_log["Warnings"], warn, sep=" ##### ")
  }
  return(site_log)
  
}

#-----------------------------------------------------------------------------

#' Checks if an error has occurred
#' @export
no_error <- function(site_log){
 
 no_error_found <- nchar(site_log["Errors"]) < 1
 return(no_error_found)
}

#-----------------------------------------------------------------------------

#' Writes site log
#' @export
write_log <- function(site_log){
  
  #extract output path
  out_path <- site_log["log_path"]
  
  #remove log path, don't need to write it to file
  site_log <- site_log[-which(names(site_log)=="log_path")]
  
  #Save log to CSV file
  write.csv(t(as.matrix(site_log)), paste(out_path, "/", site_log["Site_code"], 
                                          "_FluxnetLSM_processing_log_",  
                                          Sys.Date(), ".csv", sep=""))
}

#-----------------------------------------------------------------------------

#' Writes site log and then aborts, reporting error
#' @export
stop_and_log <- function(error, site_log){
  site_log <- log_error(error, site_log)
  write_log(site_log)
  stop(site_log["Errors"], call.=FALSE)
}

#-----------------------------------------------------------------------------

#' Writes site log and then aborts, reporting error
#' @export
warn_and_log <- function(warn, site_log){
  site_log <- log_warning(warn, site_log)
  warning(warn, call.=FALSE)
  return(site_log)
}

#-----------------------------------------------------------------------------

#' Appends warning message and calls warning
#' @export
append_and_warn <- function(warn_message, warnings){
  warnings <- paste(warnings, warn, sep=" ##### ")
  if(nchar(warn_message > 0)){ 
    warning(warn_message, call.=FALSE) 
  }
  return(warnings)
}



