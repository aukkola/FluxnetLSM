# Conversions.R
#
# Functions for units changes
#
# author: Anna Ukkola UNSW 2017
#

#' Converts units from original Fluxnet to target ALMA units
#' @return datain
ChangeUnits <- function(datain, varnames, site_log){
    
  #Loop through variables. If original and target units do not match,
  #convert (or return error if conversion between units not known)
  
  #Retrieve original and target units
  flx_units  <- datain$units$original_units
  alma_units <- datain$units$target_units
  
  
  #Save timestep size (in seconds):
  tstep <- datain$timestepsize
  
  
  #track if variable converted or not
  #used if a conversion relies on several variables
  #which may or may not have been converted already
  converted <- rep(FALSE, length(flx_units))
  
  
  for(k in 1:length(flx_units)){
        
    #Check if units match, convert if not
    if(flx_units[k] != alma_units[k]){
      
      #Recognises original FLUXNET2015 and LaThuile variable names
      
      ## Air temperature (C to K)
      if(datain$vars[k] %in% varnames$tair & flx_units[k]=="C" & alma_units[k]=="K"){
        datain$data[[k]] <- celsius_to_kelvin(datain$data[[k]])
        
        
      ## CO2: different but equivalent units, do nothing
      } else if(datain$vars[k] %in% varnames$co2 & flx_units[k]=="umolCO2/mol" & alma_units[k]=="ppm"){
        next
        
        
      ## Rainfall (mm/timestep to mm/s)
      } else if(datain$vars[k] %in% varnames$precip & flx_units[k]=="mm" & alma_units[k]=="kg/m2/s"){
        datain$data[[k]] <- datain$data[[k]] / tstep
        
        
      ## Air pressure (kPa to Pa) (Not in La Thuile dataset)
      } else if(datain$vars[k] %in% varnames$airpressure & flx_units[k]=="kPa" & alma_units[k]=="Pa"){  
        datain$data[[k]] <- datain$data[[k]] * 1000
      
        
      ## Photosynthetically Active Radiation (PAR) to SWdown
      } else if(datain$vars[k] %in% varnames$par & flx_units[k]=="umol/m2/s" & alma_units[k]=="W/m2"){  
        
        #Conversion following Monteith & Unsworth (1990), Principles of Environmental Physics
        datain$data[[k]] <- datain$data[[k]] * (1 / 2.3)
        
        #Negative PAR values present, set these to PAR = 5 umol m-2 s-1, converted to SWdown
        datain$data[[k]][datain$data[[k]] < 0] <- 5 * (1 / 2.3)
        
        
      ## Specific humidity from relative humidity (in kg/kg, calculate from tair, rel humidity and psurf)
      } else if(datain$vars[k] %in% varnames$relhumidity & flx_units[k]=="%" & alma_units[k]=="kg/kg"){  
        
        #Find Tair and PSurf units
        psurf_units <- flx_units[names(flx_units) %in% varnames$airpressure]
        tair_units  <- flx_units[names(flx_units) %in% varnames$tair]
        
        #If already converted, reset units to new converted units
        if(converted[which(datain$vars %in% varnames$airpressure)]) {
          psurf_units <- alma_units[names(alma_units) %in% varnames$airpressure]         
        } 
        if (converted[which(datain$vars %in% varnames$tair)]){
          tair_units <- alma_units[names(alma_units) %in% varnames$tair]
        }          

        datain$data[[k]] <- Rel2SpecHumidity(relHum=datain$data[,colnames(datain$data) %in% varnames$relhumidity], 
                                             airtemp=datain$data[,colnames(datain$data) %in% varnames$tair], 
                                             tair_units=tair_units, 
                                             pressure=datain$data[,colnames(datain$data) %in% varnames$airpressure], 
                                             psurf_units=psurf_units,
                                             site_log)
      
        
      ## Specific humidity from VPD (in kg/kg, calculate from tair, VPD and psurf)
      } else if(datain$vars[k] %in% varnames$vpd & flx_units[k]=="hPa" & alma_units[k]=="kg/kg"){  
        
        browser()
        #Find Tair and PSurf units
        psurf_units <- flx_units[names(flx_units) %in% varnames$airpressure]
        tair_units  <- flx_units[names(flx_units) %in% varnames$tair]
        
        #If already converted, reset units to new converted units
        if(converted[which(datain$vars %in% varnames$airpressure)]) {
          psurf_units <- alma_units[names(alma_units) %in% varnames$airpressure]         
        } 
        if (converted[which(datain$vars %in% varnames$tair)]){
          tair_units <- alma_units[names(alma_units) %in% varnames$tair]
        }          
        
        temp_relhumidity <- VPD2RelHum(VPD=datain$data[,colnames(datain$data) %in% varnames$vpd],  
                                       airtemp=datain$data[,colnames(datain$data) %in% varnames$tair],  
                                       vpd_units=flx_units[names(flx_units) %in% varnames$vpd], 
                                       tair_units=tair_units, 
                                       site_log)
        
        datain$data[[k]] <- Rel2SpecHumidity(relHum=temp_relhumidity, 
                                             airtemp=datain$data[,colnames(datain$data) %in% varnames$tair], 
                                             tair_units=tair_units, 
                                             pressure=datain$data[,colnames(datain$data) %in% varnames$airpressure], 
                                             psurf_units=psurf_units,
                                             site_log)
        

        
      ###--- Template for adding a new conversion ---###
      #Use the Fluxnet variable name and unit, and output unit as specified in data/Output_variables_xxx.csv
#       } else if(datain$vars[k]=="Fluxnet variable name" & flx_units[k]=="Fluxnet variable unit" & 
#                   alma_units[k]=="Output variable unit"){  
#         
#         datain$data[[k]] <- datain$data[[k]] * [your conversion]
        
                
      ## If cannot find conversion, abort  
      } else {
        error <- paste("Unknown unit conversion. cannot convert between original ", 
                       "Fluxnet and required units, check variable: ", datain$vars[k], 
                       ". Available conversions: air temp C to K, rainfall mm to kg/m2/s, ",
                       "air pressure kPa to Pa, humidity from relative (%) to specific (kg/kg)",
                       "and PAR (umol/m2/s) to shortwave radiation (W/m2)",
                       sep="")
        stop_and_log(error, site_log)
      }
      
      
      #Set to TRUE after converting variable  
      converted[k] <- TRUE
      
    }
    
  } #variables
  
  
  return(datain)
}

#-----------------------------------------------------------------------------

#' Converts VPD (hPa) to relative humidity (percentage)
#' @return relative humidity as percentage
VPD2RelHum <- function(VPD, airtemp, vpd_units, tair_units, site_log){

  
  #Check that VPD in Pascals
  if(vpd_units != "hPa"){
    error <- paste("Cannot convert VPD to relative humidity. VPD units not recognised,",
                   "expecting VPD in hectopascals [ function:", match.call()[[1]], "]")
    stop_and_log(error, site_log)
  }
    
  #Check that temperature in Celcius. Convert if not
  if(tair_units=="K"){
    airtemp <- airtemp - 273.15
  }
   
  #Hectopascal to Pascal
  hPa_2_Pa <- 100
  
  #Saturation vapour pressure (Pa).
  esat <- calc_esat(airtemp) 
  
  #Relative humidity (%)
  RelHum <- 100 * (1 - ((VPD * hPa_2_Pa) / esat))
  
  #Make sure RH is within [0,100]
  RelHum[RelHum < 0]   <- 0.01
  RelHum[RelHum > 100] <- 100
  
  return(RelHum)
}

#-----------------------------------------------------------------------------

#' Converts relative humidity to specific humidity.
#' @return specific humidity in kg/kg
Rel2SpecHumidity <- function(relHum, airtemp, tair_units, 
                             pressure, psurf_units, site_log){
  
  # required units: airtemp - temp in C; pressure in Pa; relHum as %
  
  #Check that temperature in Celcius. Convert if not
  if(tair_units=="K"){
    airtemp <- airtemp - 273.15
  } else if(tair_units != "C"){
    error <- paste("Unknown air temperature units, cannot convert", 
                   "relative to specific humidity. Accepts air temperature in K or C", 
                   "[ function:", match.call()[[1]], "]")
    stop_and_log(error, site_log)
  }
  
  #Check that PSurf is in Pa. Convert if not
  if(psurf_units=="kPa"){
    pressure <- pressure * 1000
  } else if(psurf_units != "Pa"){
    error <- paste("Unknown air pressure units, cannot convert", 
                   "relative to specific humidity. Accepts air pressure",
                   "in kPa or Pa", match.call()[[1]], "]")
    stop_and_log(error, site_log)
  }
  
  
  # Sat vapour pressure in Pa (reference as above)
  esat <- calc_esat(airtemp)
  
  # Then specific humidity at saturation:
  ws <- 0.622*esat/(pressure - esat)
  
  # Then specific humidity:
  specHum <- (relHum/100) * ws
  
  return(specHum)
}


#-----------------------------------------------------------------------------

#' Calculates saturation vapour pressure
#' @return saturation vapour pressure
calc_esat <- function(airtemp){
  #Tair in degrees C
  
  #From Jones (1992), Plants and microclimate: A quantitative approach 
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * airtemp / (240.97+airtemp))
  
  return(esat)
}

#-----------------------------------------------------------------------------

#' Convert air temperature from Celsius to Kelvin
celsius_to_kelvin <- function(data){
  data <- data + 273.15
  return(data)
}





