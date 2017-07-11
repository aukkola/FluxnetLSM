# Gapfill.R
#
# A collections of functions for gap-filling
#
# Author: Anna Ukkola, UNSW 2017
# 
#


#' Gapfills meteorological data with down-scaled ERAinterim estimates
#' @export
GapfillMet_with_ERA <- function(datain, ERA_file, qc_name, varnames, ...){
  
  #Read ERA data and extract time steps corresponding to obs
  era_data <- read_era(ERA_file=ERA_file, datain=datain)
  
  #Find indices for met variables to be gapfilled
  ind <- which(datain$categories=="Met")
  
  #Retrieve VPD and air temp units. Used to convert ERAinterim VPD to RH in gapfill function
  tair_units <- datain$units$original_units[varnames$tair]
  vpd_units  <- datain$units$original_units[varnames$vpd]
  
  #If not found, set to unknown
  if(length(tair_units)==0){ tair_units = "UNKNOWN" } 
  if(length(vpd_units)==0){ vpd_units = "UNKNOWN" }
  
  #Gapfill met variables
  temp_data <- gapfill_with_ERA(datain=datain$data[,ind], era_data=era_data,
                                era_vars=datain$era_vars[ind],
                                tair_units=tair_units, vpd_units=vpd_units,
                                out_vars=datain$out_vars[ind],
                                qc_name=qc_name, qc_flags, 
                                varnames=varnames, site_log)
    
  #Add new category to indata for saving gapfilling method
  datain$gapfill_met <- rep(NA, length(ind))
  names(datain$gapfill_met) <- names(ind)
    
  #Save methods
  methods <- unlist(temp_data$method)
  datain$gapfill_met[names(methods)] <- methods
  
  
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
  if(length(temp_data$new_qc$data) > 0){
    
    #Append qc time series to data
    datain$data <- cbind(datain$data, temp_data$new_qc$data)
    
    qc_vars <- colnames(temp_data$new_qc$data)
    
    for(k in 1:length(qc_vars)){
      datain <- create_qc_var(datain, qc_name=qc_vars[k], qc_flags, 
                              outname=temp_data$new_qc$outname[k], cat="Met")
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
                                   elevation, gaps, varnames, site_log){
  
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
  
  if(length(ind)==0){
    warning("No met variables found, cannot perform met data gapfilling")
    return()
  }
  

  #Remove QC vars
  qc_ind <- which(grepl(qc_name, substr(names(ind), (nchar(qc_name)-1), 
                                        nchar(names(ind)))))
  if(length(qc_ind) > 0) {ind <- ind[-qc_ind]}
  
  
  #Find lwdown and air pressure indices (note, no air pressure in La Thuile)
  lwdown_ind <- find_ind_and_qc(ind, var=varnames$lwdown)
  pair_ind   <- find_ind_and_qc(ind, var=varnames$airpressure)
  
  
  #Find indices for other variables
  if(length(c(unlist(lwdown_ind), unlist(pair_ind))) > 0){
    ind_others <- ind[-c(unlist(lwdown_ind), unlist(pair_ind))]
  } else {
    ind_others <- ind
  }
  
  
  #Var names and their corresponding output varnames
  vars_other <- names(ind_others)
  out_vars   <- datain$out_vars[ind_others]
  
  ### First gapfill variables other than LWdown and air pressure ###
  
  #Convert time steps to monts, days and hours (no year)
  #Used for copyfill, done here so only have to do once
  tsteps <- format(strptime(datain$time[,1], "%Y%m%d%H%M"), 
                   format="%m%d%H%M")
  
  #Add new category to indata for saving gapfilling method
  datain$gapfill_met <- rep(NA, length(ind))
  names(datain$gapfill_met) <- names(ind)
  
  
  #Need some of these for LWdown and air pressure
  for(k in 1:length(ind_others)){
      
    #Save method for each variable
    method <- vector()
    
    #First use linear interpolation for short
    #subdiurnal gaps
    temp_data <- linfill_data(data=datain$data[,vars_other[k]], 
                              tstepsize=datain$timestepsize,
                              linfill)

    #Save gapfilled tsteps
    gapfilled <- temp_data$missing
    
    #Then use copyfill for longer gaps
    temp_data <- copyfill_data(data=temp_data$data, tsteps=tsteps,
                              tstepsize=datain$timestepsize,
                              copyfill, start=gaps$tseries_start,
                              end=gaps$tseries_end,
                              varname=vars_other[k], site_log)
    
    
    if(length(temp_data$missing) > 0) {
      method <- append(method, "copyfill") 
    } 
    
    #Append gapfilled with new temp_data
    if(length(gapfilled) > 0){
      temp_data$missing <- append(temp_data$missing, gapfilled)
      method <- append(method, "linfill")
    }
       
    #Replace data with gapfilled data
    datain$data[,vars_other[k]] <- temp_data$data
        
    if(length(temp_data$missing) > 0){
      
      #Save information to QC flags (creat qc flag if doesn't exist)
      datain <- update_qc(datain, temp_data, vars_other[k], qc_name, qc_value, 
                          qc_flags, outname=out_vars[k], cat="Met")  
      
      #Save gapfilling methods
      datain$gapfill_met[vars_other[k]] <- paste(method, collapse="; ")
         
    } #qc
    
    #Update site log
    site_log <- temp_data$site_log
    
  } #vars
  
  
  
  ### Then gapfill LWdown and air pressure ###

  #Find Tair index
  tair_ind <- find_ind_and_qc(ind, var=varnames$tair)
  
  ## LWdown ##s
  if(length(lwdown_ind) > 0){
    
    #Find indices for rel humidity/VPD
    rh_ind   <- find_ind_and_qc(ind, var=varnames$relhumidity)
    
    if(length(rh_ind) ==0){
      rh_ind <- find_ind_and_qc(ind, var=varnames$vpd)
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
                                     technique=lwdown_method, varnames=varnames,
                                     site_log=site_log)
        
    #Replace with gapfilled data
    datain$data[,names(lwdown_ind)] <- temp_data$data
        
    if(length(temp_data$missing) > 0){
      
      #Save information to QC flags (creat qc flag if doesn't exist)
      datain <- update_qc(datain, temp_data, names(lwdown_ind), qc_name, qc_value, 
                          qc_flags, outname=datain$out_vars[lwdown_ind], cat="Met")  
    
      #Add gapfilling method 
      datain$gapfill_met[names(lwdown_ind)] <- paste("Synthesis based on", lwdown_method)
      
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
                                     varnames=varnames,
                                     site_log=site_log)
    
    #Replace with gapfilled data
    datain$data[,names(pair_ind)] <- temp_data$data
    
    
    if(length(temp_data$missing) > 0){
      
      #Save information to QC flags (creat qc flag if doesn't exist)
      datain <- update_qc(datain, temp_data, names(pair_ind), qc_name, qc_value, 
                          qc_flags, outname=datain$out_vars[pair_ind], cat="Met")  
      
      #Add gapfilling method 
      datain$gapfill_met[names(pair_ind)] <-"Synthesis based on Tair and elevation"
    
    }
  }
  
  
  #Return modified data frame
  return(list(data=datain, site_log=site_log))
  

}

#-----------------------------------------------------------------------------

#' Gapfill flux variables using statistical methods
#' @export
GapfillFlux <- function(datain, qc_name, qc_flags, regfill, 
                        linfill, copyfill, gaps, varnames, site_log){
  
  #Gapfills short gaps (up to linfill length of time) using
  #linear interpolation
  
  #Gapfills longer gaps (up to regfill length of time) using
  #linear regression against met variables
  
  #QC value for statistical gapfilling
  qc_value <- qc_flags$QC_gapfilled["statistical"]
  
  #Find indices for met variables to be gapfilled
  ind <- which(datain$categories=="Eval")
  
  if(length(ind)==0){
    warning("No flux variables found, cannot perform flux data gapfilling")
    return()
  }
  
  #Remove QC vars
  qc_ind <- which(grepl(qc_name, substr(names(ind), (nchar(qc_name)-1), nchar(names(ind)))))
  if(length(qc_ind) > 0) {ind <- ind[-qc_ind]}
  
  
  #Var names and their corresponding output var names
  vars     <- names(ind)
  out_vars <- datain$out_vars[ind]
  
  
  #Find Tair, RH/VPD and SWdown index for regression gapfilling
  all_vars   <- datain$vars
  tair_ind   <- which(all_vars==varnames$tair)[1]
  swdown_ind <- which(all_vars=="SW_IN_F_MDS" | all_vars=="SW_IN_F" | all_vars=="PPFD_f")[1] #leaving this in case of swdown/par issues elsewhere
  
  #Find indices for rel humidity/VPD
  rh_ind   <- which(all_vars==varnames$relhumidity)[1]
  
  if(length(rh_ind) ==0){
    rh_ind <- which(all_vars==varnames$vpd)[1]
  }
  
  
  #Convert time steps to monts, days and hours (no year)
  #Used for copyfill, done here so only have to do once
  tsteps <- format(strptime(datain$time[,1], "%Y%m%d%H%M"), 
                   format="%m%d%H%M")
  
  
  #Add new category to indata for saving gapfilling method
  datain$gapfill_flux <- rep(NA, length(ind))
  names(datain$gapfill_flux) <- vars
  
  
  #Loop throug variables
  for(k in 1:length(ind)){  
      
    #Save method for each variable
    method <- vector()
      
    #First use linear interpolation for short
    #subdiurnal gaps
    temp_data <- linfill_data(data=datain$data[,vars[k]], 
                             tstepsize=datain$timestepsize,
                             linfill)
            
    #Save gapfilled tsteps
    gapfilled <- temp_data$missing
    
    #If regfill chosen, do that
    if(!is.na(regfill)){
      
      #Then use regfill for longer gaps
      temp_data <- regfill_flux(ydata=temp_data$data,
                                traindata=datain$data,
                                tstepsize=datain$timestepsize,
                                regfill, varname=vars[k], 
                                swdown_ind, tair_ind, rh_ind,
                                start=gaps$tseries_start, end=gaps$tseries_end, 
                                site_log=site_log, swdown_units=datain$units$original_units[swdown_ind])
    
      
      #Save method info is used regression fill
      if(length(temp_data$missing) > 0 & !is.na(temp_data$method)[1]) {
        method <- append(method, paste("regfill based on", paste(temp_data$method, 
                                                                 collapse=", "))) 
      }
            
      
    #Else copyfill
    } else {
      
      #Then use copyfill for longer gaps
      temp_data <- copyfill_data(data=temp_data$data, tsteps=tsteps,
                                 tstepsize=datain$timestepsize,
                                 copyfill, start=gaps$tseries_start,
                                 end=gaps$tseries_end,
                                 varname=vars[k], site_log)
      
      #Save method info
      if(length(temp_data$missing) > 0) { method <- append("copyfill") } 
    }
     
    
    #Append gapfilled with new temp_data
    if(length(gapfilled) > 0){
      method <- append(method, "linfill")
      temp_data$missing <- append(temp_data$missing, gapfilled)
    }
    
    #Replace data with gapfilled data
    datain$data[,vars[k]] <- temp_data$data
    
    if(length(temp_data$missing) > 0){

      #Save information to QC flags (creat qc flag if doesn't exist)
      datain <- update_qc(datain, temp_data, vars[k], qc_name, qc_value, 
                          qc_flags, outname=out_vars[k], cat="Eval")  
      
      #Save gapfilling methods
      datain$gapfill_flux[vars[k]] <- paste(method, collapse="; ")
      
    } #qc
    
    #Update site log
    site_log <- temp_data$site_log
    
  } #vars
    
  return(list(dataout=datain, site_log=site_log))

}

#-----------------------------------------------------------------------------

#' Fills QC flags with 3 (poor gap-filling) when
#' QC flag missing but data variable available
#' @return datain
#' @export
FillQCvarMissing <- function(datain, gapfillVal, qc_name){
  
  #Find QC variables and corresponding data variables
  qc_ind  <- which(grepl(qc_name, datain$vars))  
  qc_vars <- datain$vars[qc_ind]
  data_vars <- unlist(strsplit(qc_vars, qc_name))
  
  
  #Check when flag missing but data available
  for(k in 1:length(data_vars)){
    
    #Find these instances
    ind <- which(!is.na(datain$data[data_vars[k]]) & 
                   is.na(datain$data[qc_vars[k]]))
    
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
FindExcludeEval <- function(datain, all_missing, gaps, include_all, qc_name){
  
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
  qc_vars <- lapply(exclude_eval, function(x) x[grepl(qc_name, x)])
  
  if(any(sapply(qc_vars, length) > 0)){
    
    #Find QC variables with corresponding data variable
    remove_qc <-  mapply(function(x,y) is.element(gsub(qc_name, "", y), x), x=exclude_eval, y=qc_vars)
    
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
  
  var_ind <- sapply(var, function(x) which(names(inds)==x))
  
  #Remove any possible duplicate indices (happens if variable 
  #outputted several times, e.g. RH)
  if(length(var_ind) > 1) { var_ind <- var_ind[1,] }
  
  if(!is.na(qc_name)){
    qc_ind  <- sapply(var, function(x) which(names(ind)==paste(x, qc_name, sep=""))) 
    
    #combine and remove empty entries
    inds <- c(unlist(var_ind), unlist(qc_ind))
    
  } else {
    inds <- unlist(var_ind)
  }
    
  return(inds)
}

#-----------------------------------------------------------------------------

#'Creates attributes for a new QC variable
#' @return datain
#' @export
create_qc_var <- function(datain, qc_name, qc_flags, outname, cat){
  
  #Appends attributes for new QC variable to indata
  #In some cases, need to add a column name as not set automatically
  
  #vars
  datain$vars <- append(datain$vars, qc_name)
  
  #output vars
  datain$out_vars <- append_qc(datain$out_vars, paste(outname, "_qc", sep=""), qc_name)
    
  #era_vars
  datain$era_vars <- append_qc(datain$era_vars, NA, qc_name)
  
  #era_vars
  datain$aggr_method <- append_qc(datain$aggr_method, NA, qc_name)
  
  #attributes
  Fluxnet_var <- "NULL"
  Longname    <- paste(outname, "quality control flag")
  CF_name     <- "NULL"
  datain$attributes <- rbind(datain$attributes, c(Fluxnet_var, Longname, CF_name))  
  
  #units
  datain$units$original_units <- append_qc(datain$units$original_units, "-", qc_name)  
  datain$units$target_units   <- append_qc(datain$units$target_units, "-", qc_name)
  
  #var_ranges
  qc_range <- c(qc_flags$QC_measured, qc_flags$QC_gapfilled)
  datain$var_ranges <- cbind(datain$var_ranges, rbind(min(qc_range),
                                                      max(qc_range)))
  colnames(datain$var_ranges)[ncol(datain$var_ranges)] <- qc_name
  
  #categories
  datain$categories <- append_qc(datain$categories, cat, qc_name)  
  
  #Essential met / preferred eval
  datain$essential_met  <- append_qc(datain$essential_met, FALSE, qc_name)
  datain$preferred_eval <- append_qc(datain$preferred_eval, FALSE, qc_name)
  
  return(datain)
}

#-----------------------------------------------------------------------------

#' Updates QC flags after gap-filling
#' @export
update_qc <- function(data, temp_data, varname, qc_name, qc_value, qc_flags,...){
  
  #Find index for QC variable
  qc_col <- which(data$vars==paste(varname, qc_name, sep=""))
  
  #QC variable exists, replace gapfilled tsteps with correct flag
  if(length(qc_col) > 0){
    
    data$data[temp_data$missing, qc_col] <- qc_value
    
  } else {
    
    message(paste("Could not find QC flag for variable", 
                  varname, "gap-filled with statistical",
                  "methods. Creating QC flag."))     
    
    #Initialise QC flag with zeros and replace with "4" where gap-filled
    qc_var <- rep(qc_flags$QC_measured, length(data$data[,varname]))
    qc_var[temp_data$missing] <- qc_value
    
    #Create name for QC variable and save data and name to data.frame
    #Use output variable name to create qc flag name
    qc_varname <- paste(varname, qc_name, sep="")
    
    #Append qc time series to data
    data$data <- cbind(data$data, qc_var)
    
    #Set column name correctly
    colnames(data$data)[ncol(data$data)] <- qc_varname
    
    #Add new QC var to attributes
    data <- create_qc_var(data, qc_varname, qc_flags,  ...)
    
  }
  
  return(data)
}

#-----------------------------------------------------------------------------

#' Appends information for new QC flag
#' @export
append_qc <- function(old_data, new_value, new_name){
  
  old_data <- append(old_data, new_value)
  names(old_data)[length(old_data)] <- new_name
  
  return(old_data)
  
}

