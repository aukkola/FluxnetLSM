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
    if(nchar(site_log["Warnings"]) > 0) {
      site_log["Warnings"] <- paste(site_log["Warnings"], warn, sep=" ##### ")
    } else {
      site_log["Warnings"] <- warn
    }
    
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
  if(nchar(warn) > 0){
    site_log <- log_warning(warn, site_log)
    warning(warn, call.=FALSE)
  }
  return(site_log)
}

#-----------------------------------------------------------------------------

#' Appends warning message and calls warning
#' @export
append_and_warn <- function(warn, warnings, call=TRUE){
  if(nchar(warn) > 0){ 
    
    if(nchar(warnings) > 0) {
      warnings <- paste(warnings, warn, sep=" ##### ")
    } else {
      warnings <- warn
    }
    
    if(call) { warning(warn, call.=FALSE) }
  
  }
  return(warnings)
}

#-----------------------------------------------------------------------------

#' Creates output directories and returns log output path
#' @export
create_outdir <- function(outdir, site, plots){
  
  #NetCDF files
  outpath_nc <- paste(outdir, "/Nc_files", sep="")
  dir.create(outpath_nc, showWarnings = FALSE, recursive=TRUE)
  
  #Log 
  outpath_log <- paste(outdir, "/Logs", sep="")
  dir.create(outpath_log, showWarnings = FALSE, recursive=TRUE)
  
  #Plots (if code set to plot)
  if(!any(is.na(plots))){
    outpath_plot <- paste(outdir, "/Figures/", site, sep="")
    dir.create(outpath_plot, showWarnings = FALSE, recursive=TRUE)
  }
  
  #Return log path for later use
  return(outpath_log)
}

#-----------------------------------------------------------------------------

#' Initialises site log
#' @export
initialise_sitelog <- function(site, logpath){
  
  site_log <- vector(length=9)
  names(site_log) <- c("Site_code", "Processed", "Errors", 
                       "Warnings", "No_files", "Met_files", "Flux_files", 
                       "Excluded_eval", "log_path")
  
  site_log["Site_code"] <- site
  site_log["Errors"]    <- ''
  site_log["Warnings"]  <- ''
  site_log[c(5:8)]      <- NA
  site_log["log_path"]  <- logpath #removed when writing log to file
  
  return(site_log)
  
}

#-----------------------------------------------------------------------------

#' Retrieves QC flag information
#' @export
get_qc_flags <- function(dataset, subset=NA){
  
  #FLUXNET2015 subset
  if(dataset=="FLUXNET2015" & subset=="SUBSET"){

    #1: good quality gapfill, 2: ERA gapfilling, 3: statistical gapfilling
    QCmeasured  <- 0
    QCgapfilled <- c(1, 2, 3) 
    
    qc_info <- paste("Measured: ", QCmeasured, 
                     ", Good-quality gapfilling: ",QCgapfilled[1], 
                     ", ERA-Interim gapfilling: ", QCgapfilled[2], 
                     ", Statistical gapfilling: ", QCgapfilled[3],                      
                     sep="")
 
  #FLUXNET2015 fullset or La Thuile
  } else if ((dataset=="FLUXNET2015" & subset=="FULLSET") | dataset=="LaThuile"){
    
    #1: good quality gapfill, 2: medium, 3: poor, 
    #4: ERA gapfilling, 5: statistical gapfilling
    
    #These correspond to the "qc" variables in La Thuile (not qcOK)
    QCmeasured  <- 0
    QCgapfilled <- c(1, 2, 3, 4, 5) 
    
    qc_info <- paste("Measured: ", QCmeasured, 
                     ", Good-quality gapfilling: ",QCgapfilled[1], 
                     ", Medium-quality gapfilling: ", QCgapfilled[2], 
                     ", Poor-quality gapfilling: ", QCgapfilled[3], 
                     ", ERA-Interim gapfilling: ", QCgapfilled[4], 
                     ", Statistical gapfilling: ", QCgapfilled[5],
                     sep="")
    
  #Dataset not known  
  } else {
    
    stop(paste("Dataset name not recognised, cannot verify",
              "QC flag convention. Please amend. See code",
              "in R/Utility_functions.R for options"))
  }
  
  
  #Collate into a list
  qc_flags <- list(QCmeasured, QCgapfilled, qc_info)
  names(qc_flags) <- c("QC_measured", "QC_gapfilled", "qc_info")
  
  return(qc_info)
  
}

#-----------------------------------------------------------------------------

#' Checks  that FLUXNET2015 version defined correctly
#' @export
check_flx2015_version <- function(dataset, version){
  
  if(dataset=="FLUXNET2015" & (is.na(version) | 
     (version!="SUBSET" & version!="FULLSET"))){
    stop(paste("Version of FLUXNET2015 data product not",
               "specified correctly. Please set parameter",
               "flx2015_version to 'FULLSET' or 'SUBSET'"))
  }
}


