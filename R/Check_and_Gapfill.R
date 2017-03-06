# Check_and_Gapfill.R
#
# A collections of functions for checking for
# data gaps and ranges, and gap-filling
#
# TODO: Check and merge back in to palsR
#
# 
# 
#


#' Checks for missing and gap-filled data and determines output years
#' @param datain Input data list
#' @param missing_val Missing value
#' @param QCmeasured Value of measured qc flag
#' @param QCgapfilled Values of gap-filled qc flags
#' @param missing Threshold for missing values per year (as percentage)
#' @param gapfill_all Threshold for all gap_filling per year (as percentage)
#' @param gapfill_good Threshold for good quality gap_filling 
#' per year (as percentage, ignored if gapfill_all set)
#' @param gapfill_med Threshold for medium quality 
#' gap_filling per year (as percentage, ignored if gapfill_all set)
#' @param gapfill_poor Threshold for poor quality gap_filling 
#' per year (as percentage, ignored if gapfill_all set)
#' @param min_yrs Minimum number of consecutive years
#' @param essential_met Names of essential met variables
#' @param preferred_eval Names of preferred evaluation variables
#' @return out
#' @export
CheckDataGaps <- function(datain, missing_val, QCmeasured, 
                          QCgapfilled, missing, gapfill_all, 
                          gapfill_good, gapfill_med, gapfill_poor,
                          min_yrs, essential_met, preferred_eval,
                          all_eval, site_log){
    
    #Checks the existence of data gaps and determines which
    #years should be outputted depending on the percentage of missing
    #and/or gapfilled data (set by thresholds) and the number of consecutive 
    #years available (at lseast the number of yrs set by min_yrs)
  
    #Years with too many gapfilled/missing values in ANY essential met variables
    # or ALL preferred evaluation variables will not be processed and outputted.
    
    library(R.utils) #seqToIntervals
    
    
    # 'Missing' percentage must be set, return
    # an error if it is not. Cannot check for data 
    # gaps otherwise as not all variables come with QC flags
    if(is.na(missing)){
      error <- paste("Cannot check for missing time steps in data,",
                       "set 'missing' to a value between 0",
                       "(no missing values allowed) and 100",
                       "(unlimited missing values allowed)")
      stop_and_log(error, site_log)
      return(site_log)
    }
        
    
    #Determine what gapfilling thresholds to use
    #If 'gapfill_all' is set, use that
    #Otherwise, use good/medium/poor thresholds
    
    #If gaps_all is set, use that
    if(!is.na(gapfill_all)){
      
      threshold <- gapfill_all
    
    #Else use any of good/medium/poor gapfilling thresholds if set
    } else if(any(!is.na(c(gapfill_good, gapfill_med,
                           gapfill_poor)))){
      
      threshold <- c(gapfill_good, gapfill_med, gapfill_poor)
      threshold[is.na(threshold)] <- 100 #if any NA, set to 100 (unlimited)
      
    #If none of these are set, return a warning      
    } else {
      
      threshold <- NA
      warn <- paste("Cannot check for the percentage of gap-filled data,",
                              "no thresholds set. Set at least one of",
                              "'gapfill_all', 'gapfill_good', 'gapfill_med'",
                              "or 'gapfill_poor' to check for gapfilling")
      warning(warn)
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
    for(k in 1:ncol(datain$data)){
        
        data <- datain$data[,k]
        

        ### Missing ###
        #Calculate the percentage of data missing each year
        perc_missing[[k]] <- sapply(1:length(start), function(x)
                                    length( which(data[start[x]:end[x]] == missing_val)) /
                                    length(start[x]:end[x]) * 100)
              
        ### Gap-filled ###
        #Initialise gapfilled percentage as zeros
        perc_gapfilled[[k]] <- matrix(0, nrow=length(threshold), ncol=length(start))
        
        #If threshold set, check for gap-filling
        if(any(!is.na(threshold))){
          
          #Check if QC variable exists
          qc_var <- which(datain$vars==paste(datain$vars[k], "QC", sep="_")) 
          
          #If found QC variable, calculate percentage of gap-filling
          if(length(qc_var) > 0){
            
            #Extract QC flag data
            qcdata <- datain$data[,qc_var] 
            
            #If using gapfill_all
            if(length(threshold)==1){  
              
              #Find values that are not measured or missing
              perc_gapfilled[[k]] <- sapply(1:length(start), function(x)
                                     length( which(qcdata[start[x]:end[x]] != QCmeasured &
                                                   qcdata[start[x]:end[x]] != missing_val)) /
                                     length(start[x]:end[x]) * 100)
              
              #Convert to matrix so compatible with vars without QC
              perc_gapfilled[[k]] <- matrix(perc_gapfilled[[k]], nrow=1)
              
              
            #If using gapfill_good/med/poor         
            } else {
              
              #Loop through the three gap-filling flags
              percs <- matrix(NA, nrow=3, ncol=length(start))
              for(g in 1:3){
                percs[g,] <- sapply(1:length(start), function(x)
                                length( which(qcdata[start[x]:end[x]] == QCgapfilled[g])) /
                                length(start[x]:end[x]) * 100)
              }
              
              perc_gapfilled[[k]] <- percs
              
            } 
          }
        }
        
     
    } #variables
    
    #Set names    
    names(perc_gapfilled) <- colnames(datain$data)
    names(perc_missing)   <- colnames(datain$data)
    
    
    ### Check that essential variables have at least one common year of data
    ### without too many gaps
    ### and the year has one or more evaluation variables available
    essential_ind <- sapply(essential_met, function(x) which(names(perc_missing) == x))
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
        
        #Extract gap lengths for the year
        gaps <- sapply(perc_gapfilled, function(x) x[,k])
        miss <- sapply(perc_missing, function(x) x[k])       

        #If any essential variables, or all evaluation variables,
        #have too many missing or gapfilled values, skip year
        
        #First check if too many missing values
        if(any(miss[essential_ind] > missing) | all(miss[preferred_ind] > missing))
        {
          yr_keep[k] <- FALSE 
        }
        
        
        #Check if any evaluation variables have too many gaps
        eval_remove[[k]] <- vector() #initialise
        eval_remove[[k]] <- append(eval_remove[[k]], which(miss[eval_ind] > missing))
        
        
        #If missing value threshold not exceeded, check for gapfilling (if threshold set)
        if(yr_keep[k] & any(!is.na(threshold))){
          
          #Using gapfill_all
          if(length(threshold)==1){
               
            # If ANY essential vars have too many gapfilled or missing values OR 
            # ALL preferred vars have too many gapfilled or missing values,
            # don't process year 
            if(any(gaps[essential_ind] > threshold) | all(gaps[preferred_ind] > threshold)){
              yr_keep[k] <- FALSE 
            }
          
            #Check if any evaluation variables have too much gap-filling
            eval_remove[[k]] <- append(eval_remove[[k]], which(gaps[eval_ind] > threshold))
                        
          #Using gapfill_good/med/poor
          } else {
            
            #As above, but loop through the three thresholds
            exclude_yr <- sapply(1:length(threshold), function(x) any(gaps[x,essential_ind] > threshold[x]) | 
                                                                  all(gaps[x,preferred_ind] > threshold[x]))
                                             
            #Check if any evaluation variables have too much gap-filling
            eval_remove[[k]] <- append(eval_remove[[k]], sapply(1:length(threshold), 
                                               function (x) which(gaps[x,eval_ind] > threshold[x])))
            
            if(any(exclude_yr)){
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
                   "available time period too short. Aborting.")
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
                                 "period too short (as set by min_yrs).",
                                  "Aborting [ function:", match.call()[[1]], "]")
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
                            length(which(x[tstart[k]:tend[k]] == missing_val)) 
                            / length(x[tstart[k]:tend[k]]) *100)
        
      #Calculate % gap-filled
      #Find indices for QC variables (if exist)
      qc_ind <- sapply(datain$vars, function(x) 
                          which(datain$vars==paste(x, "_QC", sep="")))
      
      total_gapfilled[[k]] <- vector()
      for(v in 1:length(qc_ind)){
        if(length(qc_ind[[v]])>0){ #QC var exists
          
          data <- datain$data[tstart[k]:tend[k],qc_ind[[v]]]
          
          total_gapfilled[[k]][v] <- length(which(data %in% QCgapfilled)) / 
                                     length(data) *100      
        } else { #No QC var
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
    
    return(out)
}



#-----------------------------------------------------------------------------

#' Gapfills met data
#' @return out
#' @export
GapfillMet <- function(datain, era_data, era_vars, tair_units, vpd_units,
                       missing_val, out_vars, site_log){
    
    #ERAinterim estimates are provided for TA, SW_in,
    #LW_IN, VPD, PA, P and WS
    #Gapfill observed met data using these estimates
    #And create QC variables if not available to
    #save information about which time steps ERA-gapfilled
    
    #Check that Fluxnet and ERA data dimensions agree
    if(nrow(datain) != nrow(era_data)) {
        error <- paste("Observed flux data and ERAinterim data dimensions",
                        "do not match, aborting [ function:", match.call()[[1]], "]")
        stop_and_log(error, site_log)
        return(site_log)
    }
    
    
    #List available ERA variables
    ind  <- which(!is.na(era_vars))
    
    avail_era  <- era_vars[ind]
    avail_flux <- colnames(datain)[ind]
    avail_out  <- out_vars[ind]

    #Initialise list for new QC variables
    #created if no existing QC flags for a variable
    new_qc   <- vector()
    qc_names <- vector()
    
    #Loop through available variables
    for(k in 1:length(avail_era)){
        
        #Find flux data column index and ERAinterim column index
        #for variable being processed
        flx_col <- which(colnames(datain)==avail_flux[k])
        
        era_col <- which(colnames(era_data)==avail_era[k])
        
        #If gaps in met data variable, gapfill
        if(any(datain[,flx_col]==missing_val)){
            
            #Find missing values to fill
            missing <- which(datain[,flx_col]==missing_val)
            
            
            ### Relative humidity ###
            #If Flux variable relative humidity, but ERA variable VPD, convert
            if(avail_flux[k] == "RH" & avail_era[k]=="VPD_ERA"){
                
                era_tair_col <- which(colnames(era_data)=="TA_ERA")
                
                if(length(era_tair_col) == 0){
                  error <- paste("Cannot find ERAinterim air temperature data.",
                                 "Cannot convert ERA VPD to relative humidity")
                  stop_and_log(error, site_log)
                  return(site_log)
                }
                
                # Convert ERAinterim VPD to relative humidity
                # Assuming that ERA vpd and tair units the same as observed units
                era_rh <-  VPD2RelHum(VPD=era_data[,era_col], airtemp=era_data[,era_tair_col],
                                      vpd_units=vpd_units, tair_units=tair_units, site_log)
                
                #Gapfill
                datain[missing,flx_col] <- era_rh[missing]
                
                
                ### Other variables ###
                #ERAinterim equivalent should exist, use that directly
            } else {
                
                #Gapfill
                datain[missing, flx_col] <- era_data[missing, era_col]
                
            }
            
            
            ## Set QC flags to "4" for time steps filled with ERA data ##
            #Find corresponding qc variable, if available
            qc_col <- which(colnames(datain)==paste(avail_flux[k], "_QC", sep=""))
            
            
            #Replace era gap-filled time steps with "4"
            if(length(qc_col) > 0){
                datain[missing,qc_col] <- 4
                
            } else {
                message(paste("Could not find QC flag for variable", 
                avail_flux[k], "gap-filled with ERA data. Creating QC flag."))
                
                #Initialise QC flag with zeros and replace with "4" where gap-filled
                qc_var <- rep(0, length(datain[,flx_col]))
                qc_var[missing] <- 4
                
                #Create name for QC variable and save data and name to data.frame
                #Use output variable name to create qc flag name
                qc_names <- cbind(qc_names, paste(avail_out[k],"_qc", sep=""))
                new_qc <- cbind(new_qc, qc_var)
                
            }
            
            
        } #if
    } #vars
    

    #Assign new QC variable names to data frame column names
    if(length(new_qc) > 0){ colnames(new_qc) <- qc_names}
    
    out <- list(datain=datain, new_qc=new_qc)
    
    return(out)
    
} #function


#-----------------------------------------------------------------------------

# TODO: This function exists in palsR/Gab in pals/R/FluxtowerSpreadsheetToNc.R and has a different signature. Merge?
#' Checks that data are within specified ranges
#' @export
CheckDataRanges = function(datain, missingval, site_log){
    
    #Checks that variables are within acceptable ranges
    # as set in the "variables" auxiliary file
    
    #Loop through variables
    for(k in 1:length(datain$vars)){
        
        data <- datain$data[[k]]
      
        #If variable missing, skip (avoids warnings to be produced)
        if(all(data==missingval)){
          next
        }
      
        #First mask out missing values so not included in
        #determination of data range
        data[data==missingval] <- NA
        data_range <- range(data, na.rm=TRUE)
        
        # Get acceptable ranges for variables:
        valid_range <- datain$var_ranges[,k]
        
        
        #Return error if variable outside specified range
        if(data_range[1] < valid_range[1] | data_range[2] > valid_range[2]){
            error <- paste("Variable outside expected ranges. Check variable ",
                           datain$vars[k], "; data range is [", data_range[1], 
                           ", ", data_range[2], "], valid range is [", 
                           valid_range[1], ", ", valid_range[2],
                           "]. Check data or change data range in variables auxiliary file",
                           sep="")
            stop_and_log(error, site_log)
            return(site_log)
        }
        
        
        
    } #variables
} #function


#-----------------------------------------------------------------------------

#'Creates attributes for a new QC variable
#' @return datain
#' @export
create_qc_var <- function(datain, qc_name){
    
    #Appends attributes for new QC variable to indata
    #In some cases, need to add a column name as not set automatically
    
    #vars
    datain$vars <- append(datain$vars, qc_name)
    
    #output vars
    datain$out_vars <- append(datain$out_vars, qc_name)
    
    #era_vars
    datain$era_vars <- append(datain$era_vars, NA)
    names(datain$era_vars)[length(datain$era_vars)] <- qc_name
    
    #attributes
    Fluxnet_var <- NULL
    Longname <- paste(strsplit(qc_name, "_qc"), "quality control flag")
    CF_name <- NULL
    datain$attributes <- rbind(datain$attributes, c(Fluxnet_var, Longname, CF_name))  
       
    #units
    datain$units$original_units <- append(datain$units$original_units, "-")
    datain$units$target_units   <- append(datain$units$target_units, "-")
    
    #var_ranges
    datain$var_ranges <- cbind(datain$var_ranges, rbind(0,4))
    colnames(datain$var_ranges)[ncol(datain$var_ranges)] <- qc_name
    
    #categories
    datain$categories <- append(datain$categories, "Met")
    names(datain$categories)[length(datain$categories)] <- qc_name
    
    
    return(datain)
}

#-----------------------------------------------------------------------------


#' Fills QC flags with 3 (poor gap-filling) when
#' QC flag missing but data variable available
#' @return datain
#' @export
fill_qcvar_missing <- function(datain, missingVal, gapfillVal){
  
  #Find QC variables and corresponding data variables
  qc_ind  <- which(grepl("_QC", datain$vars))  
  qc_vars <- datain$vars[qc_ind]
  data_vars <- unlist(strsplit(qc_vars, "_QC"))
  
  
  #Check when flag missing but data available
  for(k in 1:length(data_vars)){
    
    #Find these instances
    ind <- which(datain$data[data_vars[k]]!=missingVal & 
                   datain$data[qc_vars[k]]==missingVal)
    
    #If they exists, replace missing QC flag with poor gapfilling
    if(length(ind) > 0){
      
      datain$data[qc_vars[k]][ind,1] <- gapfillVal[3]
    
    }  
  }
  
  return(datain)
  
}

#-----------------------------------------------------------------------------

#' Calculates mean annual precipitation
#' @export
calc_avPrecip <- function(datain, gaps){
  
  ind_start <- gaps$tseries_start
  ind_end   <- gaps$tseries_end
  
  av_precip <- list()
  
  #Find unique time perios
  no_periods <- unique(gaps$consec)
  
  for(k in 1:length(no_periods)){
    
    #total of all years
    total <- sum(datain$data$P[ind_start[k]:ind_end[k]])
    
    #no years
    no_yrs <- length(which(gaps$consec==no_periods[k]))
      
    #average annual
    av_precip[[k]] <- total / no_yrs
  }
  
  return(av_precip)
}


#-----------------------------------------------------------------------------

#' Checks that whole years were extracted
#' @export
is_whole_yrs <- function(gaps, site_log){
  
  start_times <- sapply(gaps$tseries_start, function(x) format(strptime(DataFromText$time[x,1], 
                                                                        "%Y%m%d%H%M"), "%m%d"))
  end_times   <- sapply(gaps$tseries_end, function(x) format(strptime(DataFromText$time[x,1], 
                                                                      "%Y%m%d%H%M"), "%m%d"))
  if(any(start_times != "0101") | any(end_times != "1231")){
    error <- paste("Gap check did not return whole years, aborting.")    
    stop_and_log(error, site_log)
  }
  
}

#-----------------------------------------------------------------------------

#' Finds indices for flux variables to be outputted
#' @export
find_flux_ind <- function(datain, exclude_eval, k, site_log){
  
  #Find eval variable indices
  flux_ind <- which(datain$categories=="Eval")
  
  #If eval variables to exclude, remove these now
  if(any(!is.na(exclude_eval))){    
    
    rm_ind   <- sapply(1:length(exclude_eval), function(x) 
                       which(DataFromText$vars[flux_ind]==exclude_eval[x]))
    flux_ind <- flux_ind[-rm_ind]
    
  }        
  
  
  #Check that have at least one eval variable to write, skip time period if not
  if(length(flux_ind[[k]])==0){
    
    #If no eval vars for any time period, abort
    if(k==no_files & all(sapply(flux_ind, length)==0)){
      
      error <- paste("No evaluation variables to process for any output",
                     "time periods. Site not processed.")       
      stop_and_log(error, site_log)
      
    } else {
      #Return warning and skip time period
      warn <- (paste("File ", k, ": No evaluation variables to process, ",
                     "all variables have too many missing values or gap-filling. Try",
                     "setting include_all_eval to TRUE to process variables. Skipping ",
                     "time period", sep=""))
      site_log <- warn_and_log(warn, site_log)
      next  
    }          
  }
  
  return(flux_ind)
  
}

#-----------------------------------------------------------------------------

#' Finds evaluation variables to exlude
#' @export
find_exclude_eval <- function(datain, all_missing){
  
  #Extract names of evaluation variables
  cats <- datain$categories
  eval_vars <- names(cats[cats=="Eval"])
  
  #Find eval variables with all values missing
  exclude_eval <- lapply(all_missing, intersect, eval_vars)
  
  #Only exclude QC variables if corresponding data variable excluded as well. Keep otherwise
  #Find all QC variables
  qc_vars <- lapply(exclude_eval, function(x) x[grepl("_QC", x)])
  
  if(any(sapply(qc_vars, length) > 0)){
    
    #Find QC variables with corresponding data variable
    remove_qc <-  mapply(function(x,y) is.element(gsub("_QC", "", y), x), x=exclude_eval, y=qc_vars)
    
    #If any QC vars without a corresponding data variable, don't include
    #them in excluded variables
    for(k in 1:length(remove_qc)){
      if(any(!remove_qc[[k]])){
        exclude_eval[[k]] <- exclude_eval[[k]][-which(exclude_eval[[k]]==qc_vars[[k]][!remove_qc[[k]]])] 
      }          
    }
  }
  
  return(exclude_eval)
}
  


