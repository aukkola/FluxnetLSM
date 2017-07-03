# Synthesize_and_gapfill_functions.R
#
# Functions for synthesizing or gapfilling timeseries.
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com),
# edited by Anna Ukkola UNSW Jun 2017




######################################################
# Below are functions previously used for gapfilling Fluxnet formatted spreadhseets
# Does linear gap filling between met data and flux:
regressionfill = function(PALSt,varname,templateVersion,starttime,
                          regThreshold=4,winsize=8760){
  tsteps = length(PALSt[,1]) - 3
  # Get index of variable in question:
  vidx = varIndex(varname,templateVersion)
  ingap = FALSE
  ctr = 0
  for(t in 4:(tsteps+3)){
    if(PALSt[t,vidx] == SprdMissingVal){ # i.e. missing data
      ingap = TRUE
      ctr = ctr + 1
      if(t==(tsteps+3)){ # i.e. data missing from last timestep
        if(ctr<=regThreshold){ # i.e. there are very few missing timesteps
          # Just make last few missing timesteps equal to last real value:
          PALSt[(t-ctr):t,vidx] = PALSt[(t-ctr-1),vidx]
          PALSt[(t-ctr):t,(vidx+1)] = 0 # following fluxdata.org
          nowstart = dateFromTstep(starttime,t-ctr)
          nowend = dateFromTstep(starttime,t)
          cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
              nowstart$year,'through to',nowend$time,'hr,',nowend$day,
              month.abb[nowend$month],nowend$year,':\n')
          cat('Filled timestep',t-ctr,'to',t,'(',ctr,'in total) of',varname,
              'with previous non-missing value:',PALSt[(t-ctr-1),vidx],' \n')
        }else{
          # Get regression parameters based on previous section of timeseries:
          rtrain = regtrain(PALSt,vidx,(t-ctr),t,'previous',templateVersion,winsize)
          # Use these to empirically gap-fill missing section:
          regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),t,templateVersion)
          PALSt[(t-ctr):t,vidx] = regresult
          PALSt[(t-ctr):t,(vidx+1)] = 0 # following fluxdata.org
          nowstart = dateFromTstep(starttime,t-ctr)
          nowend = dateFromTstep(starttime,t)
          cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
              nowstart$year,'through to',nowend$time,'hr,',nowend$day,
              month.abb[nowend$month],nowend$year,':\n')
          cat('Filled timestep',t-ctr,'to',t,'(',ctr,'in total) of',varname,
              'with regression based on met drivers. \n')
        }
      }
    }else{ # not missing data
      if(ingap){ # i.e. 1st timestep after gap
        if(ctr <= regThreshold){ # i.e. there are very few missing timesteps
          # Just use a linear fit between surrounding timesteps
          if((t-ctr) == 4){ # i.e. the first time step was missing
            # Just make first few missing timesteps equal first real value:
            PALSt[(t-ctr):(t-1),vidx] = PALSt[t,vidx]
            PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
            nowstart = dateFromTstep(starttime,t-ctr)
            nowend = dateFromTstep(starttime,t-1)
            cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
                nowstart$year,'through to',nowend$time,'hr,',nowend$day,
                month.abb[nowend$month],nowend$year,':\n')
            cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
                'with first non-missing value:',PALSt[t,vidx],' \n')
          }else{ # there is a variable value before and after gap 
            # Fill gap with linear approximation:
            fill_values = as.single(PALSt[(t-ctr-1),vidx]) + c(1:ctr) *
              (as.single(PALSt[t,vidx])-as.single(PALSt[(t-ctr-1),vidx]))/(ctr+1)
            PALSt[(t-ctr):(t-1),vidx] = fill_values
            # Note in variable flag that this is crude gap-filling:
            PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
            nowstart = dateFromTstep(starttime,t-ctr)
            nowend = dateFromTstep(starttime,t-1)
            cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
                nowstart$year,'through to',nowend$time,'hr,',nowend$day,
                month.abb[nowend$month],nowend$year,':\n')
            cat('Filled timestep',t-ctr,'to',t-1 ,'(',ctr,
                'in total) of',varname,'with linear map of surrounding timesteps. \n')
          }	
        }else if((t-ctr) <  ceiling(winsize/2)){ # Gap close to the start of the timeseries:
          # Get regression parameters based on previous section of timeseries:
          rtrain = regtrain(PALSt,vidx,(t-ctr),(t-1),'next',templateVersion,winsize)
          # Use these to empirically gap-fill missing section:
          regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),(t-1),templateVersion)
          PALSt[(t-ctr):(t-1),vidx] = regresult
          PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
          nowstart = dateFromTstep(starttime,t-ctr)
          nowend = dateFromTstep(starttime,t-1)
          cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
              nowstart$year,'through to',nowend$time,'hr,',nowend$day,
              month.abb[nowend$month],nowend$year,':\n')
          cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
              'with regression based on met drivers. \n')
        }else if((t + ceiling(winsize/2)) > tsteps){# Gap close to the end of the timeseries:
          # Get regression parameters based on previous section of timeseries:
          rtrain = regtrain(PALSt,vidx,(t-ctr),(t-1),'previous',templateVersion,winsize)
          # Use these to empirically gap-fill missing section:
          regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),(t-1),templateVersion)
          PALSt[(t-ctr):(t-1),vidx] = regresult
          PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
          nowstart = dateFromTstep(starttime,t-ctr)
          nowend = dateFromTstep(starttime,t-1)
          cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
              'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
          cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
              'with regression based on met drivers. \n')
        }else{ # there is space to train with data before and after gap
          # Get regression parameters based on previous section of timeseries:
          rtrain = regtrain(PALSt,vidx,(t-ctr),(t-1),'around',templateVersion,winsize)
          # Use these to empirically gap-fill missing section:
          regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),(t-1),templateVersion)
          PALSt[(t-ctr):(t-1),vidx] = regresult
          PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
          nowstart = dateFromTstep(starttime,t-ctr)
          nowend = dateFromTstep(starttime,t-1)
          cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
              'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
          cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
              'with regression based on met drivers. \n')
        }
        # Reset gap timestep counter:
        ingap = FALSE
        ctr = 0
      }
    }		
  }
  return(PALSt)	
}



# Train multiple linear regression:
regtrain = function(PALSt,vidx,startT,endT,sourceDirection,templateVersion,winsize=3000){
  swidx = varIndex('SWdown',templateVersion)
  tidx = varIndex('Tair',templateVersion)
  hidx = varIndex('Qair',templateVersion)
  if(sourceDirection=='previous'){
    trainStart = startT - winsize
    trainEnd = startT - 1
    trainY = PALSt[trainStart:trainEnd,vidx]
    trainX = matrix(NA,(trainEnd-trainStart+1),3)
    trainX[,1] = PALSt[trainStart:trainEnd,swidx]
    trainX[,2] = PALSt[trainStart:trainEnd,tidx]
    trainX[,3] = PALSt[trainStart:trainEnd,hidx]
  }else if(sourceDirection=='next'){
    trainStart = endT + 1
    trainEnd = endT + winsize
    trainY = PALSt[trainStart:trainEnd,vidx]
    trainX = matrix(NA,(trainEnd-trainStart+1),3)
    trainX[,1] = PALSt[trainStart:trainEnd,swidx]
    trainX[,2] = PALSt[trainStart:trainEnd,tidx]
    trainX[,3] = PALSt[trainStart:trainEnd,hidx]
  }else if(sourceDirection=='around'){
    trainStartpre = startT - ceiling(winsize/2)
    trainEndpre = startT - 1
    trainStartpost = endT + 1
    trainEndpost = endT + ceiling(winsize/2)
    trainY = c()
    trainY[1:ceiling(winsize/2)] = PALSt[trainStartpre:trainEndpre,vidx]
    trainY[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2)] = 
      PALSt[trainStartpost:trainEndpost,vidx]
    trainX = matrix(NA,(ceiling(winsize/2)*2),3)
    trainX[1:ceiling(winsize/2),1] = PALSt[trainStartpre:trainEndpre,swidx]
    trainX[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2),1] = 
      PALSt[trainStartpost:trainEndpost,swidx]
    trainX[1:ceiling(winsize/2),2] = PALSt[trainStartpre:trainEndpre,tidx]
    trainX[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2),2] = 
      PALSt[trainStartpost:trainEndpost,tidx]
    trainX[1:ceiling(winsize/2),3] = PALSt[trainStartpre:trainEndpre,hidx]
    trainX[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2),3] = 
      PALSt[trainStartpost:trainEndpost,hidx]
  }else{
    stop('Unknown sourceDirection [regtrain]')
  }
  # Separate day and night:
  dayn = DayNight(as.double(trainX[,1]))
  trainYday = as.double(trainY[dayn])
  trainXday1 = as.double(trainX[dayn,1])
  trainXday2 = as.double(trainX[dayn,2])
  trainXday3 = as.double(trainX[dayn,3])
  trainYnight = as.double(trainY[!dayn])
  trainXnight1 = as.double(trainX[!dayn,1])
  trainXnight2 = as.double(trainX[!dayn,2])
  trainXnight3 = as.double(trainX[!dayn,3])
  # Convert missing values to NAs so regression copes:
  for(t in 1:length(trainYday)){
    if(trainYday[t] == SprdMissingVal){trainYday[t] = NA}
    if(trainXday1[t] == SprdMissingVal){trainXday1[t] = NA}
    if(trainXday2[t] == SprdMissingVal){trainXday2[t] = NA}
    if(trainXday3[t] == SprdMissingVal){trainXday3[t] = NA}
  }
  for(t in 1:length(trainYnight)){
    if(trainYnight[t] == SprdMissingVal){trainYnight[t] = NA}
    if(trainXnight1[t] == SprdMissingVal){trainXnight1[t] = NA}
    if(trainXnight2[t] == SprdMissingVal){trainXnight2[t] = NA}
    if(trainXnight3[t] == SprdMissingVal){trainXnight3[t] = NA}
  }
  
  # Train regression parameters:
  rgrp_day = lm(trainYday ~ trainXday1 + trainXday2 + 
                  trainXday3,na.action=na.omit)
  rgrp_night = lm(trainYnight ~ trainXnight1 + 
                    trainXnight2 + trainXnight3,na.action=na.omit)
  rgrp = list(day = rgrp_day, night = rgrp_night)
  return(rgrp)
}

# Use multiple linear regression parameters to predict time series:
regpredict = function(rgrp,PALSt,vidx,startT,endT,templateVersion){
  ntsteps = endT - startT + 1
  # Get indices of predictor variables in matrix:
  swidx = varIndex('SWdown',templateVersion)
  tidx = varIndex('Tair',templateVersion)
  hidx = varIndex('Qair',templateVersion)	
  testX = matrix(NA,ntsteps,3)
  # Write variables to predictor matrix:
  testX[,1] = as.double(PALSt[startT:endT,swidx])
  testX[,2] = as.double(PALSt[startT:endT,tidx])
  testX[,3] = as.double(PALSt[startT:endT,hidx])
  # Separate day and night:
  dayn = DayNight(as.double(testX[,1]))
  
  empflux = c()
  # Use existing parameters to make empirical prediction:
  daycoefs = coef(rgrp$day)
  nightcoefs = coef(rgrp$night)
  for(t in 1:ntsteps){
    if(dayn[t]){ # i.e. daytime time step
      empflux[t] = sum(daycoefs[2:4]*testX[t,]) + daycoefs[1]
    }else{
      empflux[t] = sum(nightcoefs[2:4]*testX[t,]) + nightcoefs[1]
    }
  }
  return(empflux)
}



#-----------------------------------------------------------------------------

#' Gapfills met data
#' @return out
#' @export
gapfill_with_ERA <- function(datain, era_data, era_vars, tair_units, vpd_units,
                             missing_val, out_vars, qc_name, qc_flags, site_log){
  
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
      if(any(avail_flux[k] == c("RH", "Rh")) & avail_era[k]=="VPD_ERA"){
        
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

#' Performs linear interpolation gapfilling
#' @export
linfill_met <- function(data, tsteps, tstepsize,
                        linfill=10){
  
  #Max number of consecutive time steps allowed
  #to be missing for linfill
  max_gap <- (linfill*60*60)/tstepsize
  
  #All missing values
  missing <- which(data==Sprd_MissingVal)
  
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
#' @export
copyfill_met <- function(data, tsteps, tstepsize,
                         copyfill=10,
                         start, end,
                         varname,
                         site_log){
  
  
  #Max number of consecutive time steps allowed
  # to be missing
  max_gap <- (copyfill*60*60*24)/tstepsize
  
  #First check that no gaps are longer than "copyfill"
  for(k in 1:length(start)){
    
    missing <- which(data[start[k]:end[k]]==Sprd_MissingVal)
    
    consec <- seqToIntervals(missing)
    
    #One or several gaps too large, return error
    if(any(consec[,2] - consec[,1]) > max_gap){
    
      error <- paste("Data gap too long in variable ",
                     varname, " to be gapfilled. Currently set to a maximum ",
                     "consecutive gap of ", copyfill,
                     " days. Amend parameter 'copyfill' to ",
                     "change this.", sep="")
        
      stop_and_log(error, site_log)
      
    }
  }
  
  
  #All missing values
  missing <- which(data==Sprd_MissingVal)
  
  if(length(missing) > 0){
    
    #Convert time steps to monts, days and hours (no year)
    tsteps <- format(strptime(tsteps[,1], "%Y%m%d%H%M"), 
                     format="%m%d%H%M")
    
    for(k in missing){
      
      eqv_tsteps <- which(tsteps==tsteps[k])
      
      #Find data for the same time steps, set missing to NA
      fill_data <- data[eqv_tsteps]
      fill_data[fill_data==Sprd_MissingVal] <- NA
      
      fill_value <- mean(fill_data, na.rm=TRUE)
      
      #Set to missing value if couldn't calculate fill value
      if(is.na(fill_value)) fill_value <- Sprd_MissingVal
      
      #Replace missing value with this
      data[k] <- fill_value
      
    }
    
  }

  #Return gap-filled data and index of missing values
  return(list(data=data, missing=missing))
  
}


#-----------------------------------------------------------------------------

gapfill_LWdown_Pair <- function(data, var, var_ind, TairK=NA, RH=NA, 
                               technique=NA, elev=NA, site_log){
  
  #Get data to gapfill and Tair
  data_to_fill <- datain$data[,names(var_ind)]
  
  tair <- datain$data[,names(TairK)]

  #Get Tair units 
  tair_units <- datain$units$original_units[names(tairK)]
  
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
    rh   <- datain$data[,names(RH)]
    
    #First check that have relative humidity in %, not VPD
    if(any(names(RH)==c("VPD_F_MDS", "VPD_F", "VPD_f"))){
      vpd_units  <- datain$units$original_units[names(RH)]
      rh         <- VPD2RelHum(VPD=rh, airtemp=tair, vpd_units, tair_units, site_log)
    }
    
    #Find missing indices
    missing <- which(data_to_fill==Sprd_MissingVal)
    
    if(length(missing) > 0){
    
      #Synthesize
      for(i in missing){
        data_to_fill[i] <- SynthesizeLWdown(tair[i], rh[i], technique)  
      }

    }
      
      
  ### Air pressure ### 
  } else if (var=="Pair"){
    
    #Find missing indices
    missing <- which(data_to_fill==Sprd_MissingVal)
    
    if(length(missing) > 0){
      
      #Synthesize
      for(i in missing){
        data_to_fill[i] <- SynthesizePSurf(tair[i], elev)
      }
    }
  }
  
  #Return gap-filled data and index of missing values
  outs <- list(data=data_to_fill, missing=missing)
  
  return(outs)

}


#-----------------------------------------------------------------------------

#' Synthesises downward longwave radiation based on Tair and rel humidity
#' @export
SynthesizeLWdown <- function(TairK,RH,technique){
  
  #Three techniques available, see Abramowitz et al. (2012),
  #Geophysical Research Letters, 39, L04808 for details
  
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
  return(lwdown)
}

#-----------------------------------------------------------------------------

#' Synthesises air pressure based on Tair and elevation
#' @export
SynthesizePSurf <- function(TairK,elevation){
  # Synthesizes PSurf based on temperature and elevation
  PSurf <- 101325 * (TairK / (TairK + 0.0065*elevation))^(9.80665/287.04/0.0065)
  return(PSurf)
}



