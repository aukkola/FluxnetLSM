#
#
# Needs comments
#
#

# Check the existence of missing values:
CheckDataGaps <- function(datain, missing_val=SprdMissingVal,
                          threshold,
                          essential_met, preferred_eval){
  
  gaps_found <- apply(datain$data, MARGIN=2, function(x) any(x==missing_val))
  
  #Find indices for each start and end of year
  secs_per_day   <- 60*60*24
  tsteps_per_day <- secs_per_day/datain$timestepsize
  tsteps_per_yr  <- datain$daysPerYr * tsteps_per_day
  
  end   <- cumsum(tsteps_per_yr)
  start <- end - tsteps_per_yr + 1
  
    
  gap_length <- list()
  #Check how many missing values per year, per variable
  for(k in 1:length(gaps_found)){
    
    data <- datain$data[,k]
    
    #Calculate the percentage of data missing each year
    gap_length[[k]] <- sapply(1:length(start), function(x) 
                                               length( which(data[start[x]:end[x]] == missing_val)) / 
                                               length(start[x]:end[x]) * 100)
  }
  
  names(gap_length) <- names(gaps_found)
  
  
  
  ### Check that essential variables have at least one common year of data
  ### without too many gaps 
  ### and the year has one or more evaluation variables available
  
  essential_ind <- sapply(essential_met, function(x) which(names(gap_length) == x))
  preferred_ind <- sapply(preferred_eval, function(x) which(names(gap_length) == x))

  yr_keep <- vector()
  
  #Loop through years
  for(k in 1:length(start)){
    
    #Extract gap lengths for the year
    gaps <- sapply(gap_length, function(x) x[k])
    
    #If any essential variables, or all evaluation variables, 
    #have too many missing values, skip year
    if(any(gaps[essential_ind] > threshold) | all(gaps[preferred_ind] > threshold))
    {
      yr_keep[k] <- FALSE
    
    #Else, process year
    } else {
      yr_keep[k] <- TRUE
    }
    
  }
  
  
  
  ### If no years fulfilling criteria, abort. ###
  if(all(!yr_keep)){
    CheckError("No years to process, too many gaps present. Aborting.")
  }
  
  
  #Indices of year(s) to keep
  yr_ind <- which(yr_keep)
    
  #Are all years consecutive? If not, need to split site to
  #multiple files. Determine which years are consecutive
  #and how many files need to create
  
  ## only one year
  if(length(yr_ind)==1){  
  
    consec <- 1    
  
    #determine start and end of time series
    tstart <- start[yr_ind]
    tend   <- start[yr_ind]
    
  ## two or more years that are not consecutive
  } else if (any(diff(yr_ind) > 1)) { 
    
    #Find non-consecutive instances
    breaks <- seqToIntervals(yr_ind)
    
    #Create an index vector for grouping years
    consec <- vector()
    tstart <- vector()
    tend   <- vector()
    
    for(c in 1:(nrow(breaks))){
      
      consec <- append(consec, rep(c, times=breaks[c,2] - breaks[c,1] + 1))

      #determine start and end of time series
      tstart[c] <- start[breaks[c,1]]
      tend[c]   <- end[breaks[c,2]]
        
    }
    
  ## multiple years but all consecutive
  } else { 
    
    consec <- rep(1, length(yr_ind))
  
    #determine start and end of time series
    tstart <- start[yr_ind[1]]
    tend   <- start[yr_ind[length(yr_ind)]]
    
  }
    
  
  
  return(list(gap_length=gap_length, yr_keep=yr_ind, consec=consec, tseries_start=tstart, tseries_end=tend))
}



#-----------------------------------------------------------------------------

GapfillMet <- function(datain, era_data, tair_units, 
                       vpd_units){
  
  #ERAinterim estimates are provided for TA, SW_in,
  #LW_IN, VPD, PA, P and WS
  #Gapfill met data using these estimates
  
  #Check that Fluxnet and ERA data dimensions agree
  
  if(nrow(datain$data) != nrow(era_data)) {
    CheckError("Observed flux data and ERAinterim data dimensions do not match, aborting.")
  }
  
  
  browser()
  
  #if xx$ALMA_variable == "RelH"
  
  #convert ERAinterim VPD to relative humidity
 # era_relH <-  VPD2RelHum(...      vpd_units=vpd_units, tair_units=tair_units)
  
  
  
}


#-----------------------------------------------------------------------------





CheckSpreadsheetTiming = function(DataFromText) {
  # Checks that uploaded spreadsheet data is compatible 
  # with time step size in web form; that a whole number of 
  # days are present; and whether there are an integer 
  # number of years.
  tstepinday=86400/DataFromText$timestepsize # time steps in a day
  ndays = DataFromText$ntsteps/tstepinday # number of days in data set
  if((ndays - round(ndays)) != 0){
    CheckError(paste('S2: Spreadsheet does not appear to contain a',
                     'whole number of days of data. Please amend.'))
  }
  if((DataFromText$starttime$sday != 1) | (DataFromText$starttime$smonth != 1)){
    CheckError(paste('S2: Spreadsheet data does not appear to begin',
                     'on 1st January. Please amend.'))
  }
}


#-----------------------------------------------------------------------------

LWdown_check_and_fill <- function(indata, defaultLWsynthesis, vars, gaps_found){    ############# CHECK REQUIRED Qair UNITS !!!!!!!!!!!!!!!!!!!!!
  
  found <- indata$vars
  
  # 1) Do nothing if variable 100% complete already
  if(any(found=="LWdown") & !gaps_found[which(found=="LWdown")]) return(indata)


  #Find index for LWdown and LWdown 
  ind <- which(vars$ALMA_variable=="LWdown" | vars$ALMA_variable=="LWdown_qc")
  
  #If doesn't exist in variables.csv, return error as can't create variable information
  if(length(ind)!=2) CheckError("Cannot find LWdown and LWdown_qc in auxiliary 
                                  variable file, cannot synthesize/gap-fill LWdown")
  
  
  # 2) No LWdown found, synthesize as long as Tair and Qair present
  if(!any(found=="LWdown") & any(found=="Tair") & any(found=="Qair")) {
    indata$data$LWdown = SynthesizeLWdown((indata$data$Tair+zeroC),
                                                indata$data$Qair,defaultLWsynthesis)

    #Create quality control variable
    indata$data$LWdown_qc = rep(3, indata$ntsteps)         ##### NEED TO LOOK INTO QUALITY FLAGS, 3 means poor gap-filling   !!!!!!!!!!!!
    
    # Add LWdown and LWdown_qc to variable information
    indata$vars                 <- append(indata$vars, vars$ALMA_variable[ind])
    indata$units$target_units   <- append(indata$units$target_units, vars$ALMA_unit[ind])
    indata$units$original_units <- append(indata$units$original_units, vars$ALMA_unit[ind])
    indata$longnames            <- append(indata$longnames, vars$Longname[ind])
    indata$categories           <- append(indata$categories, vars$Category[ind])
    
    

  # 3)LWdown present but has gaps, gap-fill as long as Qair and Tair present
  } else if(any(found=="LWdown") & gaps_found[which(found=="LWdown")] &
             any(found=="Qair") & any(found=="Tair")) {
    
    #if temperature units in Celsius, convert to Kelvin here not in function below? !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    filledLWdown = gapfillLWdown(indata$data$LWdown,
                                 (indata$data$Tair+zeroC), indata$data$Qair,
                                 defaultLWsynthesis)
    indata$data$LWdown = filledLWdown$data
    indata$data$LWdown_qc = filledLWdown$flag
    
    if(!any(found=="LWdown_qc")) {
      qc_ind <- which(vars$ALMA_variable=="LWdown_qc")
      indata$vars                 <- append(indata$vars, vars$ALMA_variable[qc_ind]) 
      indata$units$target_units   <- append(indata$units$target_units, vars$ALMA_unit[qc_ind]) 
      indata$units$original_units <- append(indata$units$original_units, vars$ALMA_unit[qc_ind]) 
      indata$categories           <- append(indata$categories, vars$Category[qc_ind])
    }
    
  # 4) Don't have all info available to gap-fill, abort
  } else if(!any(found=="Tair") | !any(found=="Qair")) {
    CheckError("Tair or Qair not available, cannot synthesize/gap-fill LWdown")
  
  }
  
  return(indata)
  
}

#-----------------------------------------------------------------------------

CheckTextDataRanges = function(datain){
  # Get acceptable ranges for variables:	
  range = datain$var_ranges
  
  # Check variable ranges:
  if(any(datain$data$SWdown<range$SWdown[1])|
       any(datain$data$SWdown>range$SWdown[2])){
    badval = FindRangeViolation(datain$data$SWdown,range$SWdown)
    errtext = paste('S2: Downward SW radiation outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$SWdown[1]),':',
                    as.character(range$SWdown[2]),']',sep='')
    CheckError(errtext)
  }
  if(found$LWdown){
    if(any(datain$data$LWdown<range$LWdown[1])|
         any(datain$data$LWdown>range$LWdown[2])){
      badval = FindRangeViolation(datain$data$LWdown,range$LWdown)
      errtext = paste('S2: Downward LW radiation outside expected',
                      ' ranges: ',as.character(badval),' [',
                      as.character(range$LWdown[1]),':',
                      as.character(range$LWdown[2]),']',sep='')
      CheckError(errtext)
    }
  }
  if(any(datain$data$Tair<range$Tair[1])|
       any(datain$data$Tair>range$Tair[2])){
    badval = FindRangeViolation(datain$data$Tair,range$Tair)
    errtext = paste('S2: Surface air temperature outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$Tair[1]),':',
                    as.character(range$Tair[2]),']',sep='')
    CheckError(errtext)
  }
  if(any(datain$data$Qair<range$Qair[1])|
       any(datain$data$Qair>range$Qair[2])){
    badval = FindRangeViolation(datain$data$Qair,range$Qair)
    errtext = paste('S2: Specific humidity outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$Qair[1]),':',
                    as.character(range$Qair[2]),']',sep='')
    CheckError(errtext)
  }
  if(any(datain$data$Wind<range$Wind[1])|
       any(datain$data$Wind>range$Wind[2])){
    badval = FindRangeViolation(datain$data$Wind,range$Wind)
    errtext = paste('S2: Scalar windspeed outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$Wind[1]),':',
                    as.character(range$Wind[2]),']',sep='')
    CheckError(errtext)
  }
  if(any(datain$data$Rainf<range$Rainf[1])|
       any(datain$data$Rainf>range$Rainf[2])){
    badval = FindRangeViolation(datain$data$Rainf,range$Rainf)
    errtext = paste('S2: Rainfall rate outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$Rainf[1]),':',
                    as.character(range$Rainf[2]),']',sep='')
    CheckError(errtext)
  }
  if(found$Snowf){
    if(any(datain$data$Snowf<range$Snowf[1])|
         any(datain$data$Snowf>range$Snowf[2])){
      badval = FindRangeViolation(datain$data$Snowf,range$Snowf)
      errtext = paste('S2: Snowfall rate outside expected',
                      ' ranges: ',as.character(badval),' [',
                      as.character(range$Snowf[1]),':',
                      as.character(range$Snowf[2]),']',sep='')
      CheckError(errtext)
    }
  }
  if(any(datain$data$PSurf<range$PSurf[1])|
       any(datain$data$PSurf>range$PSurf[2])){
    badval = FindRangeViolation(datain$data$PSurf,range$PSurf)
    errtext = paste('S2: Surface air pressure outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$PSurf[1]),':',
                    as.character(range$PSurf[2]),']',sep='')
    CheckError(errtext)
  }
  if(any(datain$data$Qle<range$Qle[1])|
       any(datain$data$Qle>range$Qle[2])){
    badval = FindRangeViolation(datain$data$Qle,range$Qle)
    errtext = paste('S2: Latent heat flux outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$Qle[1]),':',
                    as.character(range$Qle[2]),']',sep='')
    CheckError(errtext)
  }
  if(any(datain$data$Qh<range$Qh[1])|
       any(datain$data$Qh>range$Qh[2])){
    badval = FindRangeViolation(datain$data$Qh,range$Qh)
    errtext = paste('S2: Sensible heat flux outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$Qh[1]),':',
                    as.character(range$Qh[2]),']',sep='')
    CheckError(errtext)
  }
  if(any(datain$data$NEE<range$NEE[1])|
       any(datain$data$NEE>range$NEE[2])){
    badval = FindRangeViolation(datain$data$NEE,range$NEE)
    errtext = paste('S2: Net ecosystem exchange outside expected',
                    ' ranges: ',as.character(badval),' [',
                    as.character(range$NEE[1]),':',
                    as.character(range$NEE[2]),']',sep='')
    CheckError(errtext)
  }
}


#-----------------------------------------------------------------------------
