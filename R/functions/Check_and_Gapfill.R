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

LWdown_check_and_fill <- function(indata){
  
  found <- indata$vars
  
  #No LWdown found, synthesize as long as Tair and Qair present
  if(!any(found=="LWdown") & any(found=="Tair") & any(found=="Qair")){
    DataFromText$data$LWdown = SynthesizeLWdown((indata$data$Tair+zeroC),
                                                indata$data$Qair,defaultLWsynthesis)
    indata$data$LWdownFlag = 3                                  ##### NEED TO LOOK INTO QUALITY FLAGS, 3 means poor gap-filling   !!!!!!!!!!!!
    indata$vars <- append(DataFromText$vars, "LWdown")
    
    
    
    #LWdown present but has gaps, gap-filled as long as Qair and Tair present
  }else if(any(found=="LWdown") & gaps_found[which(found=="LWdown")] &
             any(found=="Qair") & any(found=="Tair")){
    filledLWdown = gapfillLWdown(DataFromText$data$LWdown,
                                 (DataFromText$data$Tair+zeroC), DataFromText$data$Qair,
                                 defaultLWsynthesis)
    DataFromText$data$LWdown = filledLWdown$data
    DataFromText$data$LWdown_qc = filledLWdown$flag
    
    if(!any(found=="LWdown_qc")) {
      DataFromText$vars <- append(DataFromText, "LWdown_qc") 
      DataFromText$units <- append(DataFromText, "LWdown_qc") 
      
    }
    
  }
  
  
}

