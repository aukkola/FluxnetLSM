# Checks.R
# Functions to perform basic checks on data
# and checks for missing values and
# gapfilling
#
# Author: Anna Ukkola, UNSW 2017
#


#' Checks for missing and gap-filled data and determines output years
#' 
#' 
#' @param datain Input data list
#' @param qc_flags Values of qc flags
#' @param missing_met Threshold for missing values per year for met variables (as percentage)
#' @param missing_flux Threshold for missing values per year for flux variables (as percentage)
#' @param gapfill_met_tier1 Threshold for all gap_filling per year for Tier 1 met variables (as percentage)
#' @param gapfill_met_tier2 Threshold for all gap_filling per year for Tier 2 met variables (as percentage) 
#' @param gapfill_flux Threshold for all gap_filling per year for flux variables (as percentage)
#' @param gapfill_good Threshold for good quality gap_filling 
#' per year (as percentage, ignored if gapfill_all set)
#' @param gapfill_med Threshold for medium quality 
#' gap_filling per year (as percentage, ignored if gapfill_all set)
#' @param gapfill_poor Threshold for poor quality gap_filling 
#' per year (as percentage, ignored if gapfill_all set)
#' @param min_yrs Minimum number of consecutive years
#' @param qc_name Name of QC variable of dataset
#' @param showWarn Print warning?
#' @param site_log Site log
#' @return out

CheckDataGaps <- function(datain, qc_flags, missing_met, missing_flux, 
                          gapfill_met_tier1, gapfill_met_tier2, gapfill_flux,
                          gapfill_good, gapfill_med, gapfill_poor,
                          gapfill_era, gapfill_stat, min_yrs, 
                          qc_name, aggregate=NA, site_log){
  
  #Checks the existence of data gaps and determines which
  #years should be outputted depending on the percentage of missing
  #and/or gapfilled data (set by thresholds) and the number of consecutive 
  #years available (at least the number of yrs set by min_yrs)
  
  #Years with too many gapfilled/missing values in ANY essential met variables
  # or ALL preferred evaluation variables will not be processed and outputted.
  
  library(R.utils) #seqToIntervals
  
  #initialise warning messages
  warnings <- ""
  
  # 'Missing' percentage must be set, return
  # an error if it is not. Cannot check for data 
  # gaps otherwise as not all variables come with QC flags
  if(any(is.na(c(missing_met, missing_flux)))){
    error <- paste("Cannot check for missing time steps in data,",
                   "set 'missing' to a value between 0",
                   "(no missing values allowed) and 100",
                   "(unlimited missing values allowed)")
    stop_and_log(error, site_log)
    return(site_log)
  }
  
  
  #Find essential and preferred vars
  all_met   <- datain$vars[which(datain$categories=="Met")] 
  tier1_met <- datain$vars[which(datain$essential_met==1)] 
  tier2_met <- datain$vars[which(datain$essential_met==2)] 
  all_eval  <- datain$vars[which(datain$categories=="Eval")]
  preferred_eval  <- datain$vars[which(datain$preferred_eval)]
  
  
  #Determine what gapfilling thresholds to use
  #If 'gapfill_all' is set, use that
  #Otherwise, use good/medium/poor thresholds
  
  met_flx_gapfill_flags <- c(tier1 = gapfill_met_tier1, tier2 = gapfill_met_tier2,
                             flux = gapfill_flux)
  
  good_med_poor_gapfill_flags <- c(gapfill_good, gapfill_med,
                                   gapfill_poor, gapfill_era,
                                   gapfill_stat)
  
  #Can't use general met and flux flags, as well as good/med/poor
  #quality flags
  if (any(!is.na(met_flx_gapfill_flags)) & any(!is.na(good_med_poor_gapfill_flags))) {
    stop("Cannot use gapfill_met_tier1/2 and gapfill_flux flags with gapfill_good/med/poor.")
  }
  
  #If met tier1/2 or flux gapfilling options set
  if(any(!is.na(met_flx_gapfill_flags))){
    
    threshold <- met_flx_gapfill_flags
  
    #if any NA, set to 100 (unlimited)
    threshold[is.na(threshold)] <- 100 
    
    #Set method
    method <- "met_flux"
    
  #Use flags checking for gapfilling quality (good, med, poor etc.)
  } else if (any(!is.na(good_med_poor_gapfill_flags))) {
    
    threshold <- good_med_poor_gapfill_flags
    
    #if any NA, set to 100 (unlimited)
    threshold[is.na(threshold)] <- 100 
    
    #Set method
    method <- "gc_quality"
    
  #If none of these are set, return a warning      
  } else {
    
    threshold <- NA
    warn <-  paste("Cannot check for the percentage of",
                   "gap-filled data, no thresholds set.",
                   "Set at least one of 'gapfill_all' (default),",
                   "'gapfill_good', 'gapfill_med',",
                   "'gapfill_poor', 'gapfill_era' or",
                   "'gapfill_stat' to check for gapfilling")
    
    warnings <- append_and_warn(warn=warn, warnings)    
  }
  

  #Find indices for each start and end of year
  secs_per_day   <- 60*60*24
  tsteps_per_day <- secs_per_day/datain$timestepsize
  tsteps_per_yr  <- datain$daysPerYr * tsteps_per_day
  
  end   <- cumsum(tsteps_per_yr)
  start <- end - tsteps_per_yr + 1

    
  ### Check how many missing and gapfilled values per year, per variable ###
  
  perc_missing  <- list()
  perc_gapfilled <- list()
  for (k in 1:ncol(datain$data)) {
    
    data <- datain$data[,k]
    
    
    ### Missing ###
    
    #Calculate the percentage of data missing each year
    perc_missing[[k]] <- sapply(1:length(start), function(x)
                         length(which(is.na(data[start[x]:end[x]] ))) /
                         length(start[x]:end[x]) * 100)
    
    ### Gap-filled ###
    
    #Initialise gapfilled percentage as zeros
    if (is.na(aggregate) & method == "qc_quality") {
      perc_gapfilled[[k]] <- matrix(0, nrow=length(threshold), ncol=length(start))
    } else {
      perc_gapfilled[[k]] <- matrix(0, nrow=1, ncol=length(start))
    }
    
    #If threshold set, check for gap-filling
    if (any(!is.na(threshold))) {
      
      #Check if QC variable exists
      qc_var <- which(datain$vars==paste0(datain$vars[k], qc_name)) 
      
      #If duplicates, pick the correct one
      #Not ideal, hard-codes output variable name as "_qc". Cannot work out a way round this
      if (length(qc_var) > 1) { qc_var <- which(datain$out_vars==paste0(datain$out_vars[k], "_qc"))  }
      
      
      #If found QC variable, calculate percentage of gap-filling
      if (length(qc_var) > 0) {
        
        #Extract QC flag data
        qcdata <- datain$data[,qc_var] 
        
        #Time steps not aggregated
        if (is.na(aggregate)) {
          
          #If using gapfill_met_tier1/2 or gapfill_flux
          if (method == "met_flux") {
            
            #Find values that are not measured or missing
            perc_gapfilled[[k]] <- sapply(1:length(start), function(x)
                                          length( which(!(qcdata[start[x]:end[x]] %in% qc_flags$QC_measured) &
                                          !is.na(qcdata[start[x]:end[x]]))) /
                                          length(start[x]:end[x]) * 100)
            
            #Convert to matrix so compatible with vars without QC
            perc_gapfilled[[k]] <- matrix(perc_gapfilled[[k]], nrow=1)
            
            
          #If using gapfill_good/med/poor         
          } else if (method == "qc_quality") {
            
            #Loop through the five gap-filling flags
            percs <- matrix(NA, nrow=length(threshold), ncol=length(start))
            for(g in 1:length(threshold)){
              percs[g,] <- sapply(1:length(start), function(x)
                                                   length( which(qcdata[start[x]:end[x]] == qc_flags$QC_gapfilled[g])) /
                                                   length(start[x]:end[x]) * 100)
            }
            
            perc_gapfilled[[k]] <- percs
            
          } else {
              stop ("gapfill check method not recognised")
          }
        
          
        #Time steps aggregated
        } else {
          
          #Calculate total gapfilled (1 - QC frac)
          perc_gapfilled[[k]][1,] <- sapply(1:length(start), function(x)  
                                     sum(1 - qcdata[start[x]:end[x]]) /
                                     length(qcdata[start[x]:end[x]]) * 100) 
          
        } #aggregate
      } #qc var
    } #threshold
  } #variables
  
  #Set names    
  names(perc_gapfilled) <- colnames(datain$data)
  names(perc_missing)   <- colnames(datain$data)
  
  
  ### Check that essential variables have at least one common year of data
  ### without too many gaps
  ### and the year has one or more evaluation variables available
  all_met_ind   <- unique(unlist(sapply(all_met, function(x) which(names(perc_missing) == x)))) #add so can deal with duplicate vars
  tier1_met_ind <- sapply(tier1_met, function(x) which(names(perc_missing) == x))
  tier2_met_ind <- sapply(tier2_met, function(x) which(names(perc_missing) == x))
  
  preferred_ind <- sapply(preferred_eval, function(x) which(names(perc_missing) == x))
  eval_ind      <- sapply(all_eval, function(x) which(names(perc_missing)==x))
  
  #Initialise (years to keep)
  yr_keep <- rep(TRUE, length(start))
  
  #Also initialise variable to save info about evaluation
  #variables with gaps exceeding thresholds (used to remove
  #eval variables if option chosen)
  eval_remove <- list()
  

  #Loop through years
  for(k in 1:length(start)){

    year = datain$starttime$syear + k - 1
    
    #Extract gap lengths for the year
    gaps <- sapply(perc_gapfilled, function(x) x[,k])
    miss <- sapply(perc_missing, function(x) x[k])       
    
    #If any essential variables, or all evaluation variables,
    #have too many missing or gapfilled values, skip year
    
    #First check if too many missing values
    if(any(miss[all_met_ind] > missing_met) | all(miss[preferred_ind] > missing_flux))
    {
      message("Removing ", year, " due to too many missing values.")
      yr_keep[k] <- FALSE 
    }
    
    #Check if any evaluation variables have too many gaps
    eval_remove[[k]] <- vector() #initialise
    eval_remove[[k]] <- append(eval_remove[[k]], which(miss[eval_ind] > missing_flux))
    
    
    #If missing value threshold not exceeded, check for gapfilling (if threshold set)
    if(yr_keep[k] & any(!is.na(threshold))){
      
      #Using gapfill_all or aggregated
      if(!is.na(aggregate) | method == "met_flux"){
        
        # If ANY Tier 1/2 met vars have too many gapfilled or missing values OR 
        # ALL preferred flux vars have too many gapfilled or missing values,
        # don't process year 
        if( (any(gaps[tier1_met_ind] > threshold["tier1"]) | any(gaps[tier2_met_ind] > threshold["tier2"] )) | 
           all(gaps[preferred_ind] > threshold["flux"])){
          
          message("Removing ", year, " due to too many gapfilled or missing values.")
          yr_keep[k] <- FALSE 
        }
        
        #Check if any evaluation variables have too much gap-filling
        eval_remove[[k]] <- append(eval_remove[[k]], which(gaps[eval_ind] > threshold["flux"]))
      
          
      #Using gapfill_good/med/poor
      } else if (method == "qc_quality") {
        
        #As above, but loop through the three thresholds
        #The square brackets `[[` extract the x-th value from each list element in gaps[essential_ind]
        exclude_yr <- sapply(1:length(threshold), function(x) any(gaps[x,essential_ind] > threshold[x]) | 
                               all(gaps[x,preferred_ind] > threshold[x]))
        
        #Check if any evaluation variables have too much gap-filling
        eval_remove[[k]] <- append(eval_remove[[k]], sapply(1:length(threshold), 
                                  function(x) which(gaps[x,eval_ind] > threshold[x])))
        
        if(any(exclude_yr)){
          message("Removing ", year, " due to too much gapfilling in one or more Eval variables.")
          yr_keep[k] <- FALSE
        }
      }

    } #gap-filling
    
  } #years
  

  #Indices of year(s) to keep
  yr_ind <- which(yr_keep)
  
  ### If no years fulfilling criteria, abort. ###
  if(all(!yr_keep) | length(yr_ind) < min_yrs){
    error <- paste("No years to process, too many gaps present or",
                   "available time period too short.")
    stop_and_log(error, site_log)
    return(site_log)
  }
  
  
  #Are all years consecutive? If not, need to split site to
  #multiple files. Determine which years are consecutive
  #and how many files need to create (variables 'consec'
  #tells which years should go in which file)
  
  ## only one year
  if(length(yr_ind)==1){
    
    consec <- 1
    
    #determine start and end of time series
    tstart <- start[yr_ind]
    tend   <- end[yr_ind]
    
    ## two or more years that are not consecutive
  } else if (any(diff(yr_ind) > 1)) {
    
    #Find non-consecutive instances
    breaks <- seqToIntervals(yr_ind)
    
    #Create an index vector for grouping years
    consec <- vector()
    tstart <- vector()
    tend   <- vector()
    
    for(c in 1:(nrow(breaks))){
      
      ind_consec <- rep(c, times=breaks[c,2] - breaks[c,1] + 1)
      
      
      #If number of consecutive years less than min_yrs
      if(length(ind_consec) < min_yrs){
        
        #remove these years from yr_ind
        yr_ind <- yr_ind[-(which(yr_ind==breaks[c,1]):which(yr_ind==breaks[c,2]))]
        
        
        #If all years removed because all available periods shorter than min_yrs, abort
        if(length(yr_ind)==0){
          error <- paste("No years to process, all available time",
                         "periods too short (as set by min_yrs).")
          stop_and_log(error, site_log)
          return(site_log)
        }
        
        
        #At least min_yrs number of available years
      } else {
        
        consec <- append(consec, ind_consec)
        
        #determine start and end of time series
        tstart <- append(tstart, start[breaks[c,1]])
        tend   <- append(tend, end[breaks[c,2]])
      }
    }
    
    
    ## multiple years but all consecutive
  } else {
    
    consec <- rep(1, length(yr_ind))
    
    #determine start and end of time series
    tstart <- start[yr_ind[1]]
    tend   <- end[yr_ind[length(yr_ind)]]
    
  }
  
  
  #Find eval variables to be removed for final output years
  ints <- unique(consec) #time periods
  #Need to find separately for each output period
  eval_rm <- lapply(1:length(ints), function(x) unique(unlist(eval_remove[yr_ind[consec==ints[x]]])))
  eval_remove <- lapply(eval_rm, function(x) all_eval[x])
  
  
  #Determine overall percentage missing and gap-filled for
  #output periods (written as a NetCDF attribute for each variable)
  
  total_missing   <- list()
  total_gapfilled <- list()
  
  #Loop through output periods
  for(k in 1:length(unique(consec))){
    
    #Calculate % missing for each variable
    total_missing[[k]] <- apply(datain$data, MARGIN=2, function(x) 
                                length(which(is.na(x[tstart[k]:tend[k]])))
                                / length(x[tstart[k]:tend[k]]) *100)
    
    #Calculate % gap-filled
    #Find indices for QC variables (if exist)
    qc_ind <- sapply(datain$vars, function(x) 
      which(datain$vars==paste(x, qc_name, sep="")))
    
    total_gapfilled[[k]] <- vector()
    for(v in 1:length(qc_ind)){
      if(length(qc_ind[[v]])>0){ #QC var exists
        
        data <- datain$data[tstart[k]:tend[k],qc_ind[[v]]]
        
        #No time step aggreagation
        if(is.na(aggregate)){
          total_gapfilled[[k]][v] <- length(which(data %in% qc_flags$QC_gapfilled)) / 
                                     length(data) *100 
        #Aggregation
        } else {          
          total_gapfilled[[k]][v] <- sum(1 - data) / length(data) * 100
        }
  
      #No QC var  
      } else { 
        total_gapfilled[[k]][v] <- 0
      }
    }
    names(total_gapfilled[[k]]) <- colnames(datain$data)
  }        
  
  
  #Collate outputs
  out <- list(total_missing=total_missing, total_gapfilled=total_gapfilled, 
              eval_remove=eval_remove,
              yr_keep=yr_ind, consec=consec, 
              tseries_start=tstart, tseries_end=tend)
  
  return(list(out=out, warn=warnings))
}


#' Checks that whole years were extracted
#'
#' @param datain input data
#' @param gaps gaps
#' @param site_log log file
 
IsWholeYrs <- function(datain, gaps, site_log){
  
  start_times <- sapply(gaps$tseries_start, function(x) format(strptime(datain$time[x,1], 
                                                                        "%Y%m%d%H%M"), "%m%d"))
  end_times   <- sapply(gaps$tseries_end, function(x) format(strptime(datain$time[x,1], 
                                                                      "%Y%m%d%H%M"), "%m%d"))
  if(any(start_times != "0101") | any(end_times != "1231")){
    error <- paste("Gap check did not return whole years, aborting.")    
    stop_and_log(error, site_log)
  }
  
}

#' Checks that data are within specified ranges
#' as set in the "variables" auxiliary file
#'
#' @param datain input data 
#' @param site_log log file
#' @param action action to execute upon failure?
#' @return out
CheckDataRanges <- function(datain, site_log, action="stop"){

    # Loop through variables
    for (k in 1:length(datain$vars)){

        data <- datain$data[[k]]

        # If variable missing, skip (avoids warnings)
        if (all(is.na(data))){
            next
        }

        #First mask out missing values so not included in
        #determination of data range
        data_range <- range(data, na.rm=TRUE)

        # Get acceptable ranges for variables:
        valid_range <- datain$var_ranges[,k]

        # Check if variable outside specified range
        if (data_range[1] < valid_range[1] | data_range[2] > valid_range[2]){
          
            warning_tpl <- paste("Variable outside expected ranges.",
                                 "Check variable %s;",
                                 "data range is [%.1f, %.1f], valid range is [%.1f, %.1f].",
                                 "Check data or change data range in variables auxiliary file.")
            error <- sprintf(fmt=warning_tpl,
                             datain$vars[k],
                             data_range[1], data_range[2],
                             valid_range[1], valid_range[2]
                             )

            # And take action
            if (action == "stop") {
                error <- paste(error, "Stopping.")
                stop_and_log(error, site_log)
            } else if (action == "warn") {
                error <- paste(error, "Continuing.")
                site_log <- warn_and_log(error, site_log)
            } else if (action == "ignore") {
                # Do nothing
            } else if (action == "truncate") {
                error <- paste(error, "Truncating.")
                site_log <- warn_and_log(error, site_log)
                datain$data[[k]][datain$data[[k]] > valid_range[2]] <- valid_range[2]
                datain$data[[k]][datain$data[[k]] < valid_range[1]] <- valid_range[1]
            }

        } # if bad data

    } # variables

    return(list(data=datain, site_log=site_log))
} # function


#' Performs initial checks on function arguments
#'
#' @param opts options
#' @param era_file ERA file to process
#' 
InitialChecks <- function(opts, era_file){

  #Check that ERA file supplied if using ERAinterim met_gapfilling
  if(!is.na(opts$met_gapfill) && opts$met_gapfill=="ERAinterim"){
    if (length(era_file) == 0 || is.na(era_file)){
      stop("Must provide era_file when using ERAinterim gapfilling!")
    }
  }
  
  #Check that missing_met is between 0-100
  if(opts$missing_met < 0 || opts$missing_met >100 || is.na(opts$missing_met)){
    stop("Argument 'missing' not set correctly, must be a number between 0-100")
  }
  
  #Check that missing is between 0-100
  if(opts$missing_flux < 0 || opts$missing_flux >100 || is.na(opts$missing_flux)){
    stop("Argument 'missing' not set correctly, must be a number between 0-100")
  }
  
  #Check that aggregate time step is divisible by 24
  if(!is.na(opts$aggregate)){
    if(24 %% opts$aggregate != 0){
    stop("Aggregate time step must be divisible by 24 and greater than original data timestep, please amend.")
    }
  }
  
  #Check that using FULLSET or SUBSET as FLUXNET2015 version
  if(opts$datasetname=="FLUXNET2015" && opts$flx2015_version!="FULLSET" && opts$flx2015_version!="SUBSET"){
    stop("Argument 'flx2015_version' not set correctly, please use one of 'FULLSET' and 'SUBSET'.")
  }
  
  #Check that using gapfill_all with OzFlux. gapfill_good, gapfill_med and gapfill_poor not enabled
  #with OzFlux due to different QC flag convention
  if (opts$datasetname == "OzFlux" & any(!is.na(c(opts$gapfill_good, opts$gapfill_med, opts$gapfill_poor)))) {
    stop(paste0("Checking for good/medium/poor gap-filling not enabled for OzFlux due to different QC flags. ",
                "Please use conv_opts$gapfill_all and set conv_opts$gapfill_good/med/poor to NA"))
  }
  
}


