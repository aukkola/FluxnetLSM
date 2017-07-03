# Gapfill.R
#
# A collections of functions for gap-filling
#
# Author: Anna Ukkola, UNSW 2017
# 
#


#' Gapfills meteorological data with down-scaled ERAinterim estimates
#' @export
GapfillMet_with_ERA <- function(indata, ERA_file, qc_name, ...){
  
  #Read ERA data and extract time steps corresponding to obs
  era_data <- read_era(ERA_file=ERA_file, datain=datain)
  
  #Find indices for met variables to be gapfilled
  ind <- which(datain$categories=="Met")
  
  #Retrieve VPD and air temp units. Used to convert ERAinterim VPD to RH in gapfill function
  tair_units <- datain$units$original_units[which(vars=="TA_F_MDS" | vars=="Ta_f")]
  vpd_units  <- datain$units$original_units[which(vars=="VPD_F_MDS" | vars=="VPD_f")]
  
  #If not found, set to unknown
  if(length(tair_units)==0){ tair_units = "UNKNOWN" } 
  if(length(vpd_units)==0){ vpd_units = "UNKNOWN" }
  
  #Gapfill met variables
  temp_data <- gapfill_with_ERA(datain=datain$data[,ind], era_data=era_data,
                                era_vars=datain$era_vars[ind],
                                tair_units=tair_units, vpd_units=vpd_units,
                                missing_val=Sprd_MissingVal,
                                out_vars=datain$out_vars[ind],
                                qc_name=qc_name, qc_flags, site_log)
  
  
  #Check that column names of temp_data and data to be replaced match. Stop if not
  if(!all(colnames(temp_data$datain)==colnames(datain$data[,ind]))){
    error <- paste("Error gap-filling met data with ERAinterim.", 
                   "Column names of data to be replaced do not match")
    stop_and_log(error, site_log)
  }
  
  
  #Replace original met variables with gap-filled variables
  datain$data[,ind] <- temp_data$datain
  
  #If new QC variables were created, create and append
  #variable attributes to data frame
  if(length(temp_data$new_qc) > 0){
    
    #Append qc time series to data
    datain$data <- cbind(datain$data, temp_data$new_qc)
    
    qc_vars <- colnames(temp_data$new_qc)
    
    for(k in 1:length(qc_vars)){
      datain <- create_qc_var(datain, qc_name=qc_vars[k], qc_flags)
    }
  }
  
  #Basic sanity check
  if(ncol(datain$data)!=length(datain$vars)){
    error <- "Error creating new QC flags"
    stop_and_log(error, site_log)
  }
  
  
  
  return(datain)

}


#-----------------------------------------------------------------------------

#' Gapfill meteorological variables using statistical methods
#' @export
GapfillMet_statistical <- function(datain, qc_name, qc_flags,
                                   copyfill, linfill, lwdown_method,
                                   elevation, gaps, site_log){
  
  #Uses several gapfilling methods depending on variable:
  #LWdown and air pressure: synthesis
  #Rainfall: copyfill
  #Others: linfill short gaps, else copyfill
  
  #Copyfill uses the mean of other available years for the 
  #missing time step(s).
    
  #QC value for statistical gapfilling
  qc_value <- qc_flags$QC_gapfilled["statistical"]
  

  #Find indices for met variables to be gapfilled
  ind <- which(datain$categories=="Met")
  
  #Remove QC vars
  qc_ind <- which(grepl(qc_name, substr(names(ind), 
                  nchar(names(ind))-2, nchar(names(ind)))))
  if(length(qc_ind) > 0) ind <- ind[-qc_ind]
  
  #Var names
  vars <- names(ind)
  
  #Find lwdown and air pressure indices (note, no air pressure in La Thuile)
  lwdown_ind <- find_ind_and_qc(ind, var=c("LW_IN_F_MDS", "LW_IN_F", "LW_in"))
  pair_ind   <- find_ind_and_qc(ind, var=c("PA", "PA_F"))
  
  
  #Find indices for other variables
  if(length(c(unlist(lwdown_ind), unlist(pair_ind))) > 0){
    ind_others <- ind[-c(unlist(lwdown_ind), unlist(pair_ind))]
  } else {
    ind_others <- ind
  }
   
  
  
  ### First gapfill variables other than LWdown and air pressure ###
  
  #Need some of these for LWdown and air pressure
  for(k in ind_others){
    
    #Rainfall (only use copyfill)
    if(any(vars[k]==c("P", "P_F", "Precip_f"))){
      
      temp_data <- copyfill_met(data=datain$data[,vars[k]], tsteps=datain$time,
                                tstepsize=datain$timestepsize,copyfill, 
                                start=gaps$tseries_start,
                                end=gaps$tseries_end,
                                varname=vars[k], site_log)
       
    #Other variables
    } else {
      
      #First use linear interpolation for short
      #subdiurnal gaps
      temp_data <- linfill_met(data=datain$data[,vars[k]], 
                                tsteps=datain$time,
                                tstepsize=datain$timestepsize,
                                linfill)

      #Save gapfilled tsteps
      gapfilled <- temp_data$missing
      
      #Then use copyfill for longer gaps
      temp_data <- copyfill_met(data=temp_data$data, tsteps=datain$time,
                                tstepsize=datain$timestepsize,
                                copyfill, start=gaps$tseries_start,
                                end=gaps$tseries_end,
                                varname=vars[k], site_log)
      
      #Append gapfilled with new temp_data
      if(length(gapfilled) > 0){
        temp_data$missing <- append(temp_data$missing, gapfilled)
      }
    }
    
    
    #Repalce data with gapfilled data
    datain$data[,vars[k]] <- temp_data$data
    
    
    #### TODO: SHOULD MOVE ALL THIS INTO A FUNCTION, ALSO BELOW
    if(length(temp_data$missing) > 0){
      
      ### Save information to QC flags (creat qc flag if doesn't exist) ###
      qc_col <- which(datain$vars==paste(vars[k], qc_name, sep=""))
      
      #QC variable exists, replace gapfilled tsteps with correct flag
      if(length(qc_col) > 0){
        
        datain$data[temp_data$missing, qc_col] <- qc_value
        
      } else {
        
        message(paste("Could not find QC flag for variable", 
                      vars[k], "gap-filled with statistical",
                      "methods. Creating QC flag."))     
        
        #Initialise QC flag with zeros and replace with "4" where gap-filled
        qc_var <- rep(qc_flags$QC_measured, length(datain$data[,vars[k]]))
        qc_var[temp_data$missing] <- qc_value
        
        #Create name for QC variable and save data and name to data.frame
        #Use output variable name to create qc flag name
        qc_varname <- paste(datain$out_vars[vars[k]],"_qc", sep="")
              
        #Append qc time series to data
        datain$data <- cbind(datain$data, qc_var)
        
        #Set column name correctly
        colnames(datain$data)[ncol(datain$data)] <- qc_varname
        
        #Add new QC var to attributes
        datain <- create_qc_var(datain, qc_varname, qc_flags)
        
      }
    } #qc
  } #vars
  
  
  
  ### Then gapfill LWdown and air pressure ###

  #Find Tair index
  tair_ind <- find_ind_and_qc(ind, var=c("TA_F_MDS", "TA_F", "Ta_f"))
  
  ## LWdown ##s
  if(length(lwdown_ind) > 0){
    
    #Find indices for rel humidity/VPD
    rh_ind   <- find_ind_and_qc(ind, var=c("RH", "Rh"))
    
    if(length(rh_ind) ==0){
      rh_ind <- find_ind_and_qc(ind, var=c("VPD_F_MDS", "VPD_F", "VPD_f"))
    }
    
    #Do not have both available, stop
    if(length(tair_ind)==0 | length(rh_ind)==0){
      error <- paste("Cannot gapfill incoming longwave radiation",
                     "do not have both air temperature and relative",
                     "humidity or vapour pressure deficit available.")
      stop_and_log(error=error, site_log=site_log)
    }
    
    
    #Synthesize LWdown
    temp_data <- gapfill_LWdown_Pair(datain, var="LWdown", var_ind=lwdown_ind, 
                                     TairK=tair_ind, RH=rh_ind[1], 
                                     technique=lwdown_method, site_log=site_log)
        
    #Replace with gapfilled data
    datain$data[,names(lwdown_ind)] <- temp_data$data
    
    
    if(length(temp_data$missing) > 0){
      
      ### Save information to QC flags (creat qc flag if doesn't exist) ###
      qc_col <- which(datain$vars==paste(names(lwdown_ind), qc_name, sep=""))
      
      #QC variable exists, replace gapfilled tsteps with correct flag
      if(length(qc_col) > 0){
        
        datain$data[temp_data$missing, qc_col] <- qc_value
        
      } else {
        
        message(paste("Could not find QC flag for variable", 
                      vars[k], "gap-filled with statistical",
                      "methods. Creating QC flag."))     
        
        #Initialise QC flag with zeros and replace with "4" where gap-filled
        qc_var <- rep(qc_flags$QC_measured, length(datain$data[,names(lwdown_ind)]))
        qc_var[temp_data$missing] <- qc_value
        
        #Create name for QC variable and save data and name to data.frame
        #Use output variable name to create qc flag name
        qc_varname <- paste(datain$out_vars[names(lwdown_ind)],"_qc", sep="")
        
        #Append qc time series to data
        datain$data <- cbind(datain$data, qc_var)
        
        #Set column name correctly
        colnames(datain$data)[ncol(datain$data)] <- qc_varname
        
        #Add new QC var to attributes
        datain <- create_qc_var(datain, qc_varname, qc_flags)
        
      }
    }
    
  }
  
  
  ## Air Pressure ##
  if(length(pair_ind) >0){
    
    #Do not have both available, stop
    if(length(tair_ind)==0 | is.na(elevation)){
      error <- paste("Cannot gapfill air pressure, do not have",
                     "both air temperature and elevation available.",
                     "Elevation is set in site metadata file, please",
                     "amend if missing.")
      stop_and_log(error=error, site_log=site_log)
    }
    
    
    #Synthesize LWdown
    temp_data <- gapfill_LWdown_Pair(datain, var="Pair", var_ind=pair_ind, 
                                     TairK=tair_ind, elev=elevation,
                                     site_log=site_log)
    
    #Replace with gapfilled data
    datain$data[,names(lwdown_ind)] <- temp_data$data
    
    
    if(length(temp_data$missing) > 0){
      
      ### Save information to QC flags (creat qc flag if doesn't exist) ###
      qc_col <- which(datain$vars==paste(names(pair_ind), qc_name, sep=""))
      
      #QC variable exists, replace gapfilled tsteps with correct flag
      if(length(qc_col) > 0){
        
        datain$data[temp_data$missing, qc_col] <- qc_value
        
      } else {
        
        message(paste("Could not find QC flag for variable", 
                      "Air Pressure, gap-filled with statistical",
                      "methods. Creating QC flag."))     
        
        #Initialise QC flag with zeros and replace with "4" where gap-filled
        qc_var <- rep(qc_flags$QC_measured, length(datain$data[,names(pair_ind)]))
        qc_var[temp_data$missing] <- qc_value
        
        #Create name for QC variable and save data and name to data.frame
        #Use output variable name to create qc flag name
        qc_varname <- paste(datain$out_vars[names(pair_ind)],"_qc", sep="")
        
        #Append qc time series to data
        datain$data <- cbind(datain$data, qc_var)
        
        #Set column name correctly
        colnames(datain$data)[ncol(datain$data)] <- qc_varname
        
        #Add new QC var to attributes
        datain <- create_qc_var(datain, qc_varname, qc_flags)
        
      }
    }
    
    
  }
  
  
  #Return modified data frame
  return(datain)
  

}


#-----------------------------------------------------------------------------

#'Creates attributes for a new QC variable
#' @return datain
#' @export
create_qc_var <- function(datain, qc_name, qc_flags){
    
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
    Longname    <- paste(strsplit(qc_name, "_qc"), "quality control flag")
    CF_name     <- NULL
    datain$attributes <- rbind(datain$attributes, c(Fluxnet_var, Longname, CF_name))  
       
    #units
    datain$units$original_units <- append(datain$units$original_units, "-")
    datain$units$target_units   <- append(datain$units$target_units, "-")
    
    #var_ranges
    qc_range <- c(qc_flags$QC_measured, qc_flags$QC_gapfilled)
    datain$var_ranges <- cbind(datain$var_ranges, rbind(min(qc_range),
                                                        max(qc_range)))
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
FillQCvarMissing <- function(datain, missingVal, 
                             gapfillVal, qc_name){
  
  #Find QC variables and corresponding data variables
  qc_ind  <- which(grepl(qc_name, datain$vars))  
  qc_vars <- datain$vars[qc_ind]
  data_vars <- unlist(strsplit(qc_vars, qc_name))
  
  
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


#' Finds indices for flux variables to be outputted
#' @export
FindFluxInd <- function(datain, exclude_eval, k, site_log){
  
  #initialise warnings
  warnings <- ""
  
  #Find eval variable indices
  flux_ind <- which(datain$categories=="Eval")
  
  #If eval variables to exclude, remove these now
  if(any(!is.na(exclude_eval))){    
    
    rm_ind   <- sapply(1:length(exclude_eval), function(x) 
                       which(datain$vars[flux_ind]==exclude_eval[x]))
    flux_ind <- flux_ind[-rm_ind]
    
  }        
  
  
  #Check that have at least one eval variable to write, skip time period if not
  if(length(flux_ind)==0){
    
      #Return warning and skip time period
      warn <- (paste("File ", k, ": No evaluation variables to process, ",
                     "all variables have too many missing values or gap-filling. Try",
                     "setting include_all_eval to TRUE to process variables. Skipping ",
                     "time period", sep=""))
      warnings <- append_and_warn(warn=warn, warnings)          
  }
  
  return(list(out=flux_ind, warn=warnings))
  
}

#-----------------------------------------------------------------------------

#' Finds evaluation variables to exlude
#' @export
FindExcludeEval <- function(datain, all_missing, gaps, include_all){
  
  #Extract names of evaluation variables
  cats <- datain$categories
  eval_vars <- names(cats[cats=="Eval"])
  
  #Find eval variables with all values missing
  exclude_eval <- lapply(all_missing, intersect, eval_vars)
  
  
  if(!include_all){
    exclude_eval <- mapply(function(x,y) unique(c(x, y)), x=exclude_eval, 
                           y=gaps$eval_remove, SIMPLIFY=FALSE)

  }
  
  
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
  
#-----------------------------------------------------------------------------

#' Find index for a variable and its QC flag
#' @export
find_ind_and_qc <- function(inds, var, qc_name=NA){
  
  var_ind <- sapply(var, function(x) which(names(ind)==x))
  
  #Remove any possible duplicate indices (happens if variable 
  #outputted several times, e.g. RH)
  var_ind <- lapply(var_ind, function(x) if(length(x) > 1) x[1] else x=x)
  
  if(!is.na(qc_name)){
    qc_ind  <- sapply(var, function(x) which(names(ind)==paste(x, qc_name, sep=""))) 
    
    #combine and remove empty entries
    inds <- c(unlist(var_ind), unlist(qc_ind))
    
  } else {
    inds <- unlist(var_ind)
  }
    
  return(inds)
}


