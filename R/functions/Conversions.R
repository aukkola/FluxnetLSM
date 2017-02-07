# Conversions.R
#
# functions for units changes. zeroC is in Constants.R
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)


ChangeUnits = function(datain){
  
  ### Unit conversions from original Fluxnet to target ALMA units ###
  
  #Loop through variables. If original and target units do not match,
  #convert (or return error if conversion between units not known)
  
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
      
      
      ## Air temperature (C to K)
      if(datain$vars[k]=="Tair" & flx_units[k]=="C" & alma_units[k]=="K"){
        datain$data[[k]] <- datain$data[[k]] + 273.15
        
        
        ## CO2: different but equivalent units, do nothing
      } else if(datain$vars[k]=="CO2air" & flx_units[k]=="umolCO2/mol" & alma_units[k]=="ppm"){
        next
        
        
        ## Rainfall (mm/timestep to mm/s)
      } else if(datain$vars[k]=="Rainf" & flx_units[k]=="mm" & alma_units[k]=="mm/s"){
        datain$data[[k]] <- datain$data[[k]] / tstep
        
        
        ## Air pressure (kPa to Pa)
      } else if(datain$vars[k]=="PSurf" & flx_units[k]=="kPa" & alma_units[k]=="Pa"){  
        datain$data[[k]] <- datain$data[[k]] * 1000
        
        
        ## Qair (in kg/kg, calculate from tair, rel humidity and psurf)
      } else if(datain$vars[k]=="Qair" & flx_units[k]=="%" & alma_units[k]=="kg/kg"){  
        
        #Find Tair and PSurf units
        psurf_units <- flx_units[which(datain$vars=="PSurf")]
        tair_units  <- flx_units[which(datain$vars=="Tair")]
        
        #If already converted, reset units to new converted units
        if(converted[which(datain$vars=="PSurf")]) {
          psurf_units <- alma_units[which(datain$vars=="PSurf")]         
        } 
        if (converted[which(datain$vars=="Tair")]){
          tair_units <- alma_units[which(datain$vars=="Tair")]
        }          

        browser()
        datain$data[[k]] <- Rel2SpecHum(relHum=datain$data[[which(datain$vars=="Qair")]], 
                                        airtemp=datain$data[[which(datain$vars=="Tair")]], 
                                        tair_units=tair_units, 
                                        pressure=datain$data[[which(datain$vars=="PSurf")]], 
                                        psurf_units=psurf_units)
        
        
        ## If cannot find conversion, abort  
      } else {
        CheckError(paste("Unknown unit conversion, cannot convert between original 
                         Fluxnet and ALMA units, check variable:", datain$vars[k], ". 
                         Available conversions: air temp C to K, rainfall mm to mm/s,
                         air pressure kPa to Pa, humidity from relative (%) to specific (kg/kg)"))
      }
      
      
      #Set to TRUE after converting variable  
      converted[k] <- TRUE
      
    }
  } #variables
  
  
  
  
  return(datain)
}

#-----------------------------------------------------------------------------


VPD2RelHum <- function(VPD, airtemp, vpd_units, tair_units){

  #Converts VPD (hPa) to relative humidity (%)
  
  #Check that VPD in Pascals
  if(vpd_units != "hPa"){
    CheckError("Cannot convert VPD to relative humidity. VPD units not recognised,
               expecting VPD in hectopascals")
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
  RelHum <- 100 * (1 - (VPD * hPa_2_Pa) / esat)
  
  return(RelHum)
}

#-----------------------------------------------------------------------------

Rel2SpecHum <- function(relHum, airtemp, tair_units, pressure, psurf_units){
  # Converts relative humidity to specific humidity.
  # required units: airtemp - temp in C; pressure in Pa; relHum as %
  
  #Check that temperature in Celcius. Convert if not
  if(tair_units=="K"){
    airtemp <- airtemp - 273.15
  } else if(tair_units != "C"){
    CheckError("Unknown air temperature units, cannot convert relative to specific humidity")
  }
  
  #Check that PSurf is in Pa. Convert if not
  if(psurf_units=="kPa"){
    pressure <- pressure * 1000
  } else if(psurf_units != "Pa"){
    CheckError("Unknown air pressure units, cannot convert relative to specific humidity")
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

calc_esat <- function(airtemp){
  #Calculates saturation vapour pressure
  #Tair in degrees C
  
  #From Jones (1992), Plants and microclimate: A quantitative approach 
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * airtemp / (240.97+airtemp))
  
  return(esat)
}
