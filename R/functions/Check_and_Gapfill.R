#
#
# Needs comments
#
#

# Check the existence of missing values:
CheckDataGaps <- function(datain, missing_val=SprdMissingVal){
  
  gaps_found <- apply(datain$data, MARGIN=2, function(x) any(x==missing_val))
  
  ### Want to include any of these checks?  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #  # First check that all essential variables are present:
  #  if(any(datain$data$SWdown==SprdMissingVal)){
  #    CheckError('S2: Downward shortwave has missing values.')
  #  }
  #  if(any(datain$data$Tair==SprdMissingVal)){
  #    CheckError('S2: Air temperature has missing values.')
  #  }
  #  if(any(datain$data$Qair==SprdMissingVal)){
  #    CheckError('S2: Humidity has missing values.')
  #  }
  #  if(any(datain$data$Wind==SprdMissingVal)){
  #    CheckError('S2: Windspeed has missing values.')
  #  }
  #  if(any(datain$data$Rainf==SprdMissingVal)){
  #    CheckError('S2: Rainfall has missing values.')
  #  }
  
  return(gaps_found)  
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
