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

Spec2RelHum = function(specHum,tk,PSurf){
  # Converts relative humidity to specific humidity.
  # tk - T in Kelvin; PSurf in Pa; relHum as %
  tempC = tk - zeroC
  # Sat vapour pressure in Pa
  esat = 610.78*exp( 17.27*tempC / (tempC + 237.3) )
  # Then specific humidity at saturation:
  ws = 0.622*esat/(PSurf - esat)
  # Then relative humidity:
  relHum = pmax(pmin(specHum/ws*100, 100),0)
  
  return(relHum)
}

Mbar2Pa = function(PSurf_mbar){
  # Converts air pressure in mbar to pa
  PSurf_pa = PSurf_mbar * 100
  return(PSurf_pa)
}

Abs2SpecHum = function(absHum,tk,PSurf){
  # Converts absolute humidity to specific humidity.
  # From http://www.vaisala.com/Vaisala%20Documents/Application%20notes/Humidity_Conversion_Formulas_B210973EN-F.pdf
  # absHum in g/m3, tempC in °C, PSurf in Pa
  const = 2.16679 # gK/J
  tempC = tk - zeroC
  # Calculate the vapor pressure (pw) in Pa:
  pw = absHum * tk / const
  # Calculate saturation vapour pressure (pws) in Pa:
  pws = 610.78 * exp( 17.27*tempC / (tempC + 237.3) )
  # Calculate specific humidity at saturation (ws), PSurf in Pa:
  ws = 0.622 * pws / (PSurf - pws)
  # Calculate relative humidity (relHum):
  relHum = (pw / pws) * 100
  relHum[relHum > 100] = 100 # correction
  relHum[relHum < 0] = 0 # correction
  # Then specific humidity in kg/kg:
  specHum = (relHum/100) * ws
  return(specHum)
}