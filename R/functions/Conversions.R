# Conversions.R
#
# functions for units changes. zeroC is in Constants.R
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)

### Unit conversions from Fluxnet to ALMA units ###

ChangeUnits = function(datain,elevation,humidity_type='relative',pressure_type='mbar'){
  # Performs units changes from flux data provider
  # template units to netcdf met/flux file units.
  # First calculate timestep size:
  timestepsize = 
    (datain$data$LocHoD[2] - datain$data$LocHoD[1])*3600
  
  # Temperature from C to K:
  datain$data$Tair = datain$data$Tair + zeroC
  if(found$PSurf){
    if(pressure_type=='mbar'){
      # Pressure from mbar to Pa
      datain$data$PSurf = Mbar2Pa(datain$data$PSurf)
    }else if(pressure_type=='kpa'){
      # Pressure from kpa to Pa
      datain$data$PSurf = datain$data$PSurf*1000
    }
  }else{
    # Synthesize PSurf based on temperature and elevation
    datain$data$PSurf = SynthesizePSurf(datain$data$Tair,elevation)
    datain$data$PSurfFlag = 0 # i.e. all gap-filled
  }
  
  # Rainfall from mm/timestep to mm/s
  datain$data$Rainf = datain$data$Rainf/timestepsize
  if(found$Snowf){
    # Snowfall from mm/timestep to mm/s
    datain$data$Snowf = datain$data$Snowf/timestepsize
  }
  
  
  
  
  if(humidity_type=='relative'){
    # Relative to specific humidity:
    datain$data$Qair = Rel2SpecHum(datain$data$Qair,
                                   datain$data$Tair,datain$data$PSurf)
    
    
    
    
    
  }else if(humidity_type=='absolute'){
    # Absolute to specific humidity:
    datain$data$Qair = Abs2SpecHum(datain$data$Qair,
                                   datain$data$Tair,datain$data$PSurf)
  }
  
  
  
  
  
  
  
  return(datain)
}


#-----------------------------------------------------------------------------


#NEED TO CHECK THIS FUNCTION AND ADD A REFERENCE  !!!!!!!!!!!!!
VPD2RelHum <- function(VPD, Tair, vpd_units, tair_units){
  
  #Check that VPD in Pascals
  if(vpd_units != "hPa"){
    CheckError("Cannot convert VPD to relative humidity. VPD units not recognised,
               expecting VPD in hectopascals")
  }
    
  #Check that temperature in Celcius. Convert if not
  if(tair_units=="K"){
    Tair <- Tair-273.15
  }
    
  #Hectopascal to Pascal
  hPa_2_Pa <- 100
  
  #Saturation vapour pressure (Pa).
  #From Jones (1992), Plants and microclimate: A quantitative approach 
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * Tair / (240.97+Tair))
  
  #Relative humidity (%)
  RelHum <- 100 * (1 - (VPD * hPa_2_Pa) / esat)
  
  return(RelHum)
}


#-----------------------------------------------------------------------------

Rel2SpecHum = function(relHum,tk,PSurf){
  # Converts relative humidity to specific humidity.
  # tk - T in Kelvin; PSurf in Pa; relHum as %
  tempC = tk - zeroC
  # Sat vapour pressure in Pa
  esat = 610.78*exp( 17.27*tempC / (tempC + 237.3) )
  # Then specific humidity at saturation:
  ws = 0.622*esat/(PSurf - esat)
  # Then specific humidity:
  specHum = (relHum/100) * ws
  
  return(specHum)
}

