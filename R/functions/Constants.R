# Constants.R
#
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)
#
zeroC = 273.15
SprdMissingVal=-9999 # missing value in spreadsheet
NcMissingVal=-9999 # missing value in created netcdf files
CurrentTemplateVersion = '1.0.3'
KnownTemplateVersions = c('1.0.1','1.0.2','1.0.3')
#
# Variable order in spreadsheet template versions:
templateCols = function(templateVersion = CurrentTemplateVersion){
	if(templateVersion=='1.0.1'){
	  
		columnnames = c('LocDate','LocHoD','SWdown',
		'SWdownFlag','LWdown','LWdownFlag','Tair','TairFlag','Qair',
		'QairFlag','Wind','WindFlag','Rainf','RainfFlag','Snowf',
		'SnowfFlag','PSurf','PSurfFlag','CO2air','CO2airFlag','Rnet',
		'RnetFlag','SWup','SWupFlag','Qle','QleFlag','Qh',
		'QhFlag','NEE','NEEFlag','Qg','QgFlag')
		
		cclasses = c('character','numeric','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer')
		
	}else if(templateVersion=='1.0.2' | templateVersion=='1.0.3'){
		columnnames=c('LocDate','LocHoD','SWdown',
		'SWdownFlag','LWdown','LWdownFlag','Tair','TairFlag','Qair',
		'QairFlag','Wind','WindFlag','Rainf','RainfFlag','Snowf',
		'SnowfFlag','PSurf','PSurfFlag','CO2air','CO2airFlag','Rnet',
		'RnetFlag','SWup','SWupFlag','Qle','QleFlag','Qh',
		'QhFlag','NEE','NEEFlag','GPP','GPPFlag','Qg','QgFlag')
		cclasses = c('character','numeric','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric','integer')
	}else{
		CheckError('S3: Unknown template version [function: templateCols]')
	}
	tcols = list(names=columnnames,classes=cclasses)
	return(tcols)
}

#---------------------------------------------------------------------------

# Gets index of a template variable 
varIndex = function(varname,templateVersion){
	tcols = templateCols(templateVersion)
	idx = 0
	for(v in 1:length(tcols$names)){
		if(varname == tcols$names[v]){
			idx = v	
		}
	}
	return(idx)	
}

#---------------------------------------------------------------------------

# Acceptable variable ranges
GetVariableRanges = function(){
  
	SWdown = c(0,1360)       # surface incident shortwave rad [W/m^2]
	LWdown = c(0,750)        # surface incident longwave rad [W/m^2]
	Tair   = c(200,333)      # near surface air temperature [K]
	Qair   = c(0,0.1)        # near surface specific humidity [kg/kg]
	Rainf  = c(0,0.05)       # rainfall rate [mm/s]
	Snowf  = c(0,0.03)       # snowfall rate [mm/s]
	PSurf  = c(50000,110000) # surface air pressure [Pa]
	CO2air = c(160,2000)     # near surface CO2 concentration [ppmv]
	Wind   = c(0,75)         # scalar windspeed [m/s]
	Qle    = c(-1000,1000)   # latent heat flux [W/m^2]
	Qh     = c(-1000,1000)   # sensible heat flux [W/m^2]
	Qg     = c(-1000,1000)   # ground heat flux [W/m^2]
	NEE    = c(-100,100)     # net ecosystem exchange CO2 [umol/m^2/s]
	GPP    = c(-100,100)     # gross primary production [umol/m^2/s]
	SWup   = c(0,1350)       # reflected SW rad [W/m^2]
	Rnet   = c(-500,1250)    # net absorbed radiation [W/m^2]
	
	range  = list(SWdown=SWdown,LWdown=LWdown,Tair=Tair,
		Qair=Qair,Rainf=Rainf,Snowf=Snowf,PSurf=PSurf,
		CO2air=CO2air,Wind=Wind,Qle=Qle,Qh=Qh,NEE=NEE,
		Rnet=Rnet)
	return(range)	
}

