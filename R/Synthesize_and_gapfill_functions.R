# Synthesize_and_gapfill_functions.R
#
# Functions for synthesizing or gapfilling timeseries.
#
# author: Anna Ukkola UNSW Jun 2017


#' Gapfills met data
#' @return out
gapfill_with_ERA <- function(datain, era_data, era_vars, tair_units, vpd_units,
                             out_vars, qc_name, qc_flags, varnames, 
                             site_log){
  
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
  new_qc     <- vector()
  qc_names   <- vector()
  qc_outname <- vector()
  
  #Save method if any tsteps gapfilled
  method <- vector("list", length=length(ind))
  names(method) <- names(ind)
    
  #Loop through available variables
  for(k in 1:length(avail_era)){
    
    #Initialise method
    method[[k]] <- vector()
    
    #Find flux data column index and ERAinterim column index
    #for variable being processed
    flx_col <- which(colnames(datain)==avail_flux[k])
    
    era_col <- which(colnames(era_data)==avail_era[k])
    
    #If gaps in met data variable, gapfill
    if(any(is.na(datain[,flx_col]))){
      
      #Find missing values to fill
      missing <- which(is.na(datain[,flx_col]))
      
      
      ### Relative humidity ###
      #If Flux variable relative humidity, but ERA variable VPD, convert
      if(avail_flux[k] %in% varnames$relhumidity & avail_era[k]=="VPD_ERA"){
        
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
      
      #Save method
      method[[k]] <- append(method[[k]], "ERA-Interim")
      
      ## Set QC flags to "4" for time steps filled with ERA data ##
      #Find corresponding qc variable, if available
      qc_col <- which(colnames(datain)==paste(avail_flux[k], qc_name, sep=""))
      
      
      #Replace era gap-filled time steps with ERA QC flag
      if(length(qc_col) > 0){
        datain[missing,qc_col] <- qc_flags$QC_gapfilled["ERA"]
        
      } else {
        message(paste("Could not find QC flag for variable", 
                      avail_flux[k], "gap-filled with ERA data. Creating QC flag."))
        
        #Initialise QC flag with zeros and replace with ERA value where gap-filled
        qc_var <- rep(qc_flags$QC_measured, length(datain[,flx_col]))
        qc_var[missing] <- qc_flags$QC_gapfilled["ERA"]
        
        #Create name for QC variable and save data and name to data.frame
        #Use output variable name to create qc flag name
        qc_names   <- cbind(qc_names, paste(avail_flux[k], qc_name, sep=""))
        new_qc     <- cbind(new_qc, qc_var)
        qc_outname <- cbind(qc_outname, avail_out[k])
        
      }   
    } #if
  } #vars
  
  
  #Assign new QC variable names to data frame column names
  if(length(new_qc) > 0){ 
    colnames(new_qc) <- qc_names
  } 
  new_qc_info <- list(new_qc, qc_outname)
  names(new_qc_info) <- c("data","outname")

  #Collate outputs
  out <- list(datain=datain, new_qc=new_qc_info, method=method)
  
  return(out)
  
} #function


#-----------------------------------------------------------------------------

#' Performs linear interpolation gapfilling
linfill_data <- function(data, tstepsize,
                        linfill=10){
  
  #Max number of consecutive time steps allowed
  #to be missing for linfill
  max_gap <- (linfill*60*60)/tstepsize
  
  #All missing values
  missing <- which(is.na(data))
  
  consec <- seqToIntervals(missing)
  
  #Find consecutive periods shorter/equal to max_gap
  tperiods <- which((consec[,2]-consec[,1]) <= max_gap)
  
  missing_linfilled <- vector()
  
  if(length(tperiods) > 0){
    
    for(k in tperiods){
      
      start <- consec[k,1]
      end   <- consec[k,2]
      
      #If first time step of data series, use
      #first available value
      if(start==1){
        data[start:end] <- data[end+1]
      
      #If last time step of data series,
      #use last available value
      } else if(end==length(data)){
        data[start:end] <- data[start-1]
        
      #Else linearly interpolate
      } else {
        data[start:end] <- (data[start-1] + data[end+1])/2
      }
   
      #Save time steps linearly interpolated
      missing_linfilled <- append(missing_linfilled, c(start:end))
    } 
    
  }
  
  
  #Return gap-filled data and index of missing values
  return(list(data=data, missing=missing_linfilled))  
  
}

#-----------------------------------------------------------------------------

#' Performs copyfill gapfilling
copyfill_data <- function(data, tsteps, tstepsize, copyfill=10,
                          start, end, varname, site_log){
  
  
  #Max number of consecutive time steps allowed
  # to be missing
  max_gap <- (copyfill*60*60*24)/tstepsize
  
  
  #First check that no gaps are longer than "copyfill"
  for(n in 1:length(start)){
    
    #Find missing values
    missing <- which(is.na(data[start[n]:end[n]]))
    
    consec <- seqToIntervals(missing)
    consec <- matrix(consec, ncol=2) #convert to matrix
    
    #One or several gaps too large, skip time period
    if(any(consec[,2] - consec[,1] + 1 > max_gap)){
    
      warn <- paste("Data gap too long in variable ",
                     varname, " to be gapfilled. Currently set to a maximum ",
                     "consecutive gap of ", copyfill,
                     " days. Amend parameter 'copyfill' to ",
                     "change this.", sep="")
      site_log <- warn_and_log(warn=warn, site_log=site_log)
      next
    }
    
    
    #If found missing, gapfill
    if(length(missing) > 0){
      
      #Loop through missing values
      for(k in missing){
        
        #add start to k
        k <- k + start[n] -1 
        
        #Find same time step for other years
        eqv_tsteps <- which(tsteps==tsteps[k])
        
        #Find data for the same time steps, set missing to NA
        fill_data <- data[eqv_tsteps]
        
        #Calculate average
        fill_value <- mean(fill_data, na.rm=TRUE)
                
        #Replace missing value with this
        data[k] <- fill_value
        
      } 
    }
    
  } # time periods
  

  #Return gap-filled data and index of missing values
  return(list(data=data, missing=missing, site_log=site_log))
  
}


#-----------------------------------------------------------------------------
#' Synthesizes incoming longwave radiation and air pressure
gapfill_LWdown_Pair <- function(data, var, var_ind, TairK=NA, RH=NA, 
                               technique=NA, elev=NA, varnames, site_log){
  
  #Get data to gapfill and Tair
  data_to_fill <- data$data[,names(var_ind)]
  
  tair <- data$data[,names(TairK)]

  #Get Tair units 
  tair_units <- data$units$original_units[names(TairK)]
  
  #Convert Tair if necessary (need Kelvin)
  if(tair_units == "C"){
    tair       <- celsius_to_kelvin(tair)
    tair_units <- "K"
  } else if (tair_units != "K"){
    error <- paste("Cannot recognise air temperature units for",
                   "synthesizing LWdown and/or air pressure,", 
                   "use Celsius (C) or Kelvin (K).")
    stop_and_log(error, site_log)
  }
  
  
  ### LWdown ###
  if(var=="LWdown"){
    
    #Extract rel humidity data    
    rh   <- data$data[,names(RH)]
    
    #First check that have relative humidity in %, not VPD
    if(names(RH) %in% varnames$vpd){
      vpd_units  <- data$units$original_units[names(RH)]
      rh         <- VPD2RelHum(VPD=rh, airtemp=tair, vpd_units, tair_units, site_log)
    }
    
    #Find missing indices
    missing <- which(is.na(data_to_fill))
    
    if(length(missing) > 0){
    
      #Synthesize
      for(i in missing){
        data_to_fill[i] <- SynthesizeLWdown(tair[i], rh[i], technique)  
      }

    }
      
      
  ### Air pressure ### 
  } else if (var=="Pair"){
    
    #Find missing indices
    missing <- which(is.na(data_to_fill))
    
    if(length(missing) > 0){
      
      #Synthesize
      for(i in missing){
        data_to_fill[i] <- SynthesizePSurf(tair[i], elev, data$units$original_units[names(data$units$original_units) %in% 
                                                                                      varnames$airpressure])
      }
    }
  }
  
  #Return gap-filled data and index of missing values
  outs <- list(data=data_to_fill, missing=missing)
  
  return(outs)

}


#-----------------------------------------------------------------------------

#' Gapfills flux data using linear regression against met variables
regfill_flux <- function(ydata, traindata, tstepsize, regfill, varname, 
                         swdown_ind, tair_ind, rh_ind,
                         start, end, site_log, ...){
  

  #Max number of consecutive time steps allowed
  # to be missing
  max_gap <- (regfill*60*60*24)/tstepsize
  
  missing_all <- vector()
  
  #First check that no gaps are longer than "regfill"
  for(n in 1:length(start)){
    
    #Find missing values
    missing <- which(is.na(ydata[start[n]:end[n]]))
    
    if(length(missing) > 0){
      
      consec <- seqToIntervals(missing)
      
      #One or several gaps too large, return warning
      if(any(consec[,2] - consec[,1] + 1 > max_gap)){
        
        warn <- paste("Data gap too long in variable ",
                      varname, " to be gapfilled using regfill. Currently set to a maximum ",
                      "consecutive gap of ", regfill," days. Amend parameter 'regfill' to ",
                      "change this.", sep="")
        site_log <- warn_and_log(warn, site_log)
      }
      
      
      #Only add indices for time periods shorter than regfill
      rm_ind <- which((consec[,2] - consec[,1] + 1) > max_gap)
      if(length(rm_ind) > 0) { 
        consec <- consec[-rm_ind,] 
      }
      
      consec <- matrix(consec, ncol=2) #convert to matrix
      
      #Create sequences
      seq    <- apply(consec, MARGIN=1, function(x) seq(from=x[1], to=x[2]))
      
      #Append to missing
      missing_all <- append(missing_all, unlist(seq)+start[n]-1)    
      
    }
   }
    
  
  #If found missing values:
  if(length(missing_all) > 0){  
    
    #SWdown, Tair and humidity available
    if(length(swdown_ind)>0 & length(tair_ind)>0 & length(rh_ind)>0){
      
      #Collate training data
      train_data <- as.matrix(cbind(traindata[swdown_ind], traindata[tair_ind],
                                    traindata[rh_ind]))
      colnames(train_data) <- c("SWdown", "Tair", "RH")
      
      #Only SWdown available
    } else if (length(swdown_ind)>0){
      
      #Collate training data
      train_data <- as.matrix(traindata[swdown_ind])
      colnames(train_data) <- c("SWdown")
      
      #None available, return
    } else {
      
      #Log warning to site log that no met variables were available to gapfill fluxes
      warn <- paste("Cannot perform regression gapfilling of flux variables",
                    "no SWdown or PAR available")
      
      site_log <- log_warning(warn, site_log)
      outs     <- list(y=ydata, site_log=site_log, method=NA, missing=c())
      return(outs)
    }
        

    #Obtain regression parameters, separately for day and night
    reg_params <- regtrain(train_data, ydata, ...)
    
    #Predict y using regression
    predicted_y <- regpredict(reg_params$rgrp, reg_params$traindata,
                              reg_params$dayn)

    #Replace missing values with predicted values
    ydata[missing_all] <- predicted_y[missing_all]
   
    #Save method
    method <- colnames(train_data)
  } else {
    method <- ""
  }  
  
  #Collate outputs
  outs <- list(data=ydata, site_log=site_log, method=method, missing=missing)
  
  return(outs)
  
}

#-----------------------------------------------------------------------------

#' Trains multiple linear regression for flux gap-filling 
#' separately for day and night
regtrain <- function(traindata, ydata, ...){
    
  # Separate day and night:
  dayn <- DayNight(as.double(traindata[,"SWdown"]), ...)
  
  #Day and night training datasets
  Yday     <- ydata[dayn]
  trainDay <- traindata[dayn,]
  
  Ynight     <- ydata[!dayn]
  trainNight <- traindata[!dayn,]
  
  #Collate to dataframes
  data_day           <- cbind(Yday, trainDay)
  colnames(data_day) <- c("y", colnames(traindata))
  
  data_night           <- cbind(Ynight, trainNight)
  colnames(data_night) <- c("y", colnames(traindata))
  
  
  # Train regression parameters (dot means use all variables
  # in data frame as predictors):
  rgrp_day   <- lm(y ~ ., data=as.data.frame(data_day), na.action=na.omit)
  rgrp_night <- lm(y ~ ., data=as.data.frame(data_night), na.action=na.omit)
  
  #Return as list
  rgrp       <- list(day = rgrp_day, night = rgrp_night)
  
  return(list(rgrp=rgrp, traindata=traindata, dayn=dayn))
}

#-----------------------------------------------------------------------------

#' Predict flux values using linear regression parameters
regpredict <- function(rgrp,traindata, dayn){
  
  # Use existing parameters to make empirical prediction:
  daycoefs   <- coef(rgrp$day)
  nightcoefs <- coef(rgrp$night)
  
  #Calculate each x * coef
  x_vals_day   <- traindata
  x_vals_night <- traindata
  
  for(k in 1:ncol(traindata)){
    x_vals_day[,k]   <- x_vals_day[,k] * daycoefs[1+k]
    x_vals_night[,k] <- x_vals_night[,k] * nightcoefs[1+k]
  }
  
  #Predict y
  day_y   <- rowSums(x_vals_day) + daycoefs[1]
  night_y <- rowSums(x_vals_night) + nightcoefs[1]
  
  #Combine and mask day/night
  predicted_y <- dayn
  predicted_y[which(dayn)]  <- day_y[which(dayn)]
  predicted_y[which(!dayn)] <- night_y[which(!dayn)]
  
  return(predicted_y)
}

#-----------------------------------------------------------------------------

#' Synthesises downward longwave radiation based on Tair and rel humidity
SynthesizeLWdown <- function(TairK,RH,technique){
  
  #Three techniques available, see Abramowitz et al. (2012),
  #Geophysical Research Letters, 39, L04808 for details
  
  zeroC <- 273.15
  
  #Inputs missing, set lwdown missing
  if(is.na(TairK) | is.na(RH)){
    lwdown <- NA

  #Else synthesise value
  } else {
      
    if(technique=='Swinbank_1963'){
      # Synthesise LW down from air temperature only:
      lwdown <- 0.0000094*0.0000000567*TairK^6
      
    }else if(technique=='Brutsaert_1975'){
      satvapres <- 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
      vapres    <- pmax(5,RH)/100*satvapres
      emiss     <- 0.642*(vapres/TairK)^(1/7)
      lwdown    <- emiss*0.0000000567*TairK^4
      
    }else if(technique=='Abramowitz_2012'){
      satvapres <- 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
      vapres    <- pmax(5,RH)/100*satvapres
      lwdown    <- 2.648*TairK + 0.0346*vapres - 474
      
    }else{
      CheckError('S4: Unknown requested LWdown synthesis technique.')
    }
  }
  

  return(lwdown)
}

#-----------------------------------------------------------------------------

#' Synthesises air pressure based on Tair and elevation
SynthesizePSurf <- function(TairK, elevation, pair_units){
  # Synthesizes PSurf based on temperature and elevation
  
  #If Tair missing, set Pair missing
  if(is.na(TairK)){
    PSurf <- NA
    
  #Else synthesise (in Pa)
  }else {
    PSurf <- 101325 * (TairK / (TairK + 0.0065*elevation))^(9.80665/287.04/0.0065)
  }
  
  #Convert to kPa if necessary
  if(pair_units=="kPa"){
    PSurf <- PSurf/1000
  } else if (pair_units != "Pa"){
    stop(paste("Cannot synthesise air pressure, do not recognise air pressure units.",
               "Please use Pa or kPa"))
  }
  
  
  return(PSurf)
}



