# Utility_functions.R
# 
# Functions for error and warning handling


#' Append error message to log
log_error <- function(errtext, site_log){
  
  site_log["Errors"] <- paste(site_log["Errors"], errtext, sep="")
  site_log["Processed"] <- FALSE
  
  return(site_log)
}

#-----------------------------------------------------------------------------

#' Appends warning to site log
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
no_error <- function(site_log){
 
 no_error_found <- nchar(site_log["Errors"]) < 1
 return(no_error_found)
}

#-----------------------------------------------------------------------------

#' Writes site log
write_log <- function(site_log){
  
  #extract output path
  out_path <- site_log["log_path"]
  
  #remove log and plot path, don't need to write it to file
  site_log <- site_log[-which(names(site_log)=="log_path")]
  site_log <- site_log[-which(names(site_log)=="plot_path")]
  
  #Save log to CSV file
  write.csv(t(as.matrix(site_log)), paste(out_path, "/", site_log["Site_code"], 
                                          "_FluxnetLSM_processing_log_",  
                                          Sys.Date(), ".csv", sep=""))
}

#-----------------------------------------------------------------------------

#' Writes site log and then aborts, reporting error
stop_and_log <- function(error, site_log){
  
  #remove plot path
  unlink(site_log["plot_path"], recursive=TRUE)
  
  site_log <- log_error(error, site_log)
  write_log(site_log)
  stop(site_log["Errors"], call.=FALSE)
}

#-----------------------------------------------------------------------------

#' Writes site log and then aborts, reporting error
warn_and_log <- function(warn, site_log){
  if(nchar(warn) > 0){
    site_log <- log_warning(warn, site_log)
    warning(warn, call.=FALSE)
  }
  return(site_log)
}

#-----------------------------------------------------------------------------

#' Appends warning message and calls warning
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
create_outdir <- function(outdir, site, plots){
  
  #NetCDF files
  outpath_nc <- paste(outdir, "/Nc_files", sep="")
  dir.create(outpath_nc, showWarnings = FALSE, recursive=TRUE)
  
  #Log 
  outpath_log <- paste(outdir, "/Logs", sep="")
  dir.create(outpath_log, showWarnings = FALSE, recursive=TRUE)
  
  paths <- list(nc=outpath_nc, log=outpath_log)

  #Plots (if code set to plot)
  if(!any(is.na(plots))){
    outpath_plot <- paste(outdir, "/Figures/", site, sep="")
    dir.create(outpath_plot, showWarnings = FALSE, recursive=TRUE)
    paths$plot <- outpath_plot
  }
  
  #Return log path for later use
  return(paths)
}

#-----------------------------------------------------------------------------

#' Initialises site log
initialise_sitelog <- function(site, paths){
  
  site_log <- vector(length=12)
  names(site_log) <- c("Site_code", "Processed", "Errors", 
                       "Warnings", "No_files", "Met_files", 
                       "Flux_files","Excluded_eval", "Gapfill_met", 
                       "Gapfill_flux", "log_path", "plot_path")
  
  site_log["Site_code"] <- site
  site_log["Errors"]    <- ''
  site_log["Warnings"]  <- ''
  site_log[c(5:10)]     <- NA
  site_log["log_path"]  <- paths$log  #removed when writing log to file
  if ('plot' %in% names(paths)) {
      site_log["plot_path"] <- paths$plot #removed when writing log to file
  }
  
  return(site_log)
  
}

#-----------------------------------------------------------------------------

#' Retrieves QC flag information
get_qc_flags <- function(dataset, subset=NA) {
  
  #FLUXNET2015 subset
  if (dataset=="FLUXNET2015" & subset=="SUBSET") {

    #1: good quality gapfill, 2: ERA gapfilling, 3: statistical gapfilling
    QCmeasured  <- 0
    QCgapfilled <- c(1, 2, 3) 
    
    names(QCgapfilled) <- c("good", "ERA", "statistical")
    
    qc_info <- paste("Measured: ", QCmeasured, 
                     ", Good-quality gapfilling: ",QCgapfilled[1], 
                     ", ERA-Interim gapfilling: ", QCgapfilled[2], 
                     ", Statistical gapfilling: ", QCgapfilled[3],                      
                     sep="")
 
  #FLUXNET2015 fullset or La Thuile
  } else if ((dataset=="FLUXNET2015" & subset=="FULLSET") | dataset=="LaThuile") {
    
    #1: good quality gapfill, 2: medium, 3: poor, 
    #4: ERA gapfilling, 5: statistical gapfilling
    
    #These correspond to the "qc" variables in La Thuile (not qcOK)
    QCmeasured  <- 0
    QCgapfilled <- c(1, 2, 3, 4, 5) 
    
    names(QCgapfilled) <- c("good", "medium", "poor", "ERA", "statistical")
    
    qc_info <- paste("Measured: ", QCmeasured, 
                     ", Good-quality gapfilling: ",QCgapfilled[1], 
                     ", Medium-quality gapfilling: ", QCgapfilled[2], 
                     ", Poor-quality gapfilling: ", QCgapfilled[3], 
                     ", ERA-Interim gapfilling: ", QCgapfilled[4], 
                     ", Statistical gapfilling: ", QCgapfilled[5],
                     sep="")
    
  } else if (dataset == "OzFlux") {  
    
    
    #QC flags in original data
    qc_flags <- c(L1_missing      = "QA/QC: Missing value in L1 dataset", 
                  csat_diag       = "QA/QC: CSAT Diagnostic", 
                  li7500_diag     = "QA/QC: LI7500 Diagnostic",
                  L2_diurnal      = "QA/QC: L2 Diurnal SD Check", 
                  excl_dates      = "QA/QC: Excluded Dates",
                  excl_hrs        = "QA/QC: Excluded Hours", 
                  missing_qc      = "QA/QC: Missing value found with QC flag = 0",
                  linear          = "Corrections: Apply Linear",
                  coor_rot        = "Corrections/Combinations: Coordinate Rotation (Ux, Uy, Uz, UxT, UyT, UzT, UxA, UyA, UzA, UxC, UyC, UzC, UxUz, UxUx, UxUy, UyUz, UxUy, UyUy)",
                  atten_corr      = "Corrections/Combinations: Massman Frequency Attenuation Correction (Coord Rotation, Tv_CSAT, Ah_HMP, ps)",
                  actual_fh       = "Corrections/Combinations: Virtual to Actual Fh (Coord Rotation, Massman, Ta_HMP)",
                  density_corr    = "Corrections/Combinations: WPL correction for flux effects on density measurements (Coord Rotation, Massman, Fhv to Fh, Cc_7500_Av)",
                  ta_tv           = "Corrections/Combinations: Ta from Tv",
                  L3_range        = "Corrections/Combinations: L3 Range Check",
                  L3_diurnal      = "Corrections/Combinations: L3 Diurnal SD Check",
                  ustar_filter    = "Corrections/Combinations: u* filter",
                  gap_coord       = "Corrections/Combinations: Gap coordination",
                  driver_access   = "GapFilling: Driver gap filled using ACCESS",
                  non_rotated_cov = "GapFilling: Used non-rotated covariance",
                  flux_ann        = "GapFilling: Flux gap filled by ANN (SOLO)",
                  flux_not_ann    = "GapFilling: Flux gap not filled by ANN",
                  L4_range        = "GapFilling: L4 Range Check",
                  L4_diurnal      = "GapFilling: L4 Diurnal SD Check",
                  climatology     = "GapFilling: Gap filled by climatology",
                  interpolated    = "GapFilling: Gap filled by interpolation",
                  flux_ratios     = "GapFilling: Flux gap filled using ratios",
                  statistical     = "Statistical gapfilling performed by FluxnetLSM") #added for package-performed gapfilling
    
    
    #QC flags (70 added for statistical gapfilling, others provided with original data)
    #Measured
    QCmeasured <- 0
    
    #Gapfilled
    QCgapfilled <- c(1:8, 10:21, 30:31, 38:40, 50, 60, 70)
    names(QCgapfilled) <- names(qc_flags)
    
    #Append qc flags
    qc_info <- paste0("Measured: ", QCmeasured, ", ", 
                      paste(mapply(function(qc, name) paste0(name, ": ", qc),
                            qc=QCgapfilled, name=qc_flags), collapse=(", ")))
    
    

    
    
    
  #Dataset not known  
  } else {
    
    stop(paste("Dataset name not recognised, cannot verify",
              "QC flag convention. Please amend. See code",
              "in R/Utility_functions.R for options"))
  }
  
  
  #Collate into a list
  qc_flags        <- list(QCmeasured, QCgapfilled, qc_info)
  names(qc_flags) <- c("QC_measured", "QC_gapfilled", "qc_info")
  
  return(qc_flags)
  
}


#-----------------------------------------------------------------------------

#' Checks  that FLUXNET2015 version defined correctly
check_flx2015_version <- function(dataset, version) {
  
  if (dataset=="FLUXNET2015" & (is.na(version) | 
     (version!="SUBSET" & version!="FULLSET"))) {
            stop(paste("Version of FLUXNET2015 data product not",
            "specified correctly. Please set parameter",
            "flx2015_version to 'FULLSET' or 'SUBSET'"))
  }
}

#-----------------------------------------------------------------------------

#' Gets possible varnames for FLUXNET FULLSET/SUBSET and La Thuile
get_varnames <- function(datasetname, flx2015_version) {
  
  #These are used for unit conversions etc.
  
  #First instance is FLUXNET2015 FULLSET,
  #Second FLUXNET2015 SUBSET
  #Third La Thuile
  
  if(datasetname=="FLUXNET2015" & flx2015_version=="SUBSET"){
    ind <- 2
  } else if (datasetname=="LaThuile"){
    ind <- 3
  } else if (datasetname=="OzFlux"){
    ind <- 4
  } else {
    #Else assume FLUXNET2015 fullset format, i.e. if (datasetname=="FLUXNET2015" & flx2015_version=="FULLSET")
    ind <- 1
  } 
  
  tair        <- list(c("TA_F_MDS", "TA_F", "TA_ERA"),
                      c("TA_F"),
                      c("Ta_f"),
                      c("Ta"))
  precip      <- list(c("P", "P_F", "P_ERA"),
                      c("P_F"), 
                      c("Precip_f"),
                      c("Precip"))
  airpressure <- list(c("PA", "PA_ERA", "PA_F"), 
                      c("PA_F"), 
                      c("NULL"),
                      c("ps"))
  co2         <- list(c("CO2_F_MDS"),
                      c("CO2_F_MDS"),
                      c("CO2"),
                      c("C_ppm"))
  par         <- list(c("PPFD_IN"),
                      c("PPFD_IN"),
                      c("PPFD_f"),
                      c("NULL")) #check if available?
  relhumidity <- list(c("RH"),
                      c("RH"),
                      c("Rh"),
                      c("RH"))
  lwdown      <- list(c("LW_IN_F_MDS", "LW_IN_ERA", "LW_IN_F"),
                      c("LW_IN_F"),
                      c("LWin"),
                      c("Fld"))
  swdown      <- list(c("SW_IN_F_MDS", "SW_IN_ERA", "SW_IN_F"),
                      c("SW_IN_F"),
                      c("SWin"),
                      c("Fsd"))
  vpd         <- list(c("VPD_F_MDS", "VPD_ERA", "VPD_F"),
                      c("VPD_F"),
                      c("VPD_f"),
                      c("VPD"))
  wind        <- list(c("WS", "WS_ERA", "WS_F"),
                      c("WS_F"),
                      c("WS_f"),
                      c("Ws"))
  
  
  outs <- list(tair=tair[[ind]], precip=precip[[ind]], airpressure=airpressure[[ind]],
               co2=co2[[ind]], par=par[[ind]], relhumidity=relhumidity[[ind]],
               lwdown=lwdown[[ind]], vpd=vpd[[ind]], wind=wind[[ind]])
  
  return(outs)
  
}





