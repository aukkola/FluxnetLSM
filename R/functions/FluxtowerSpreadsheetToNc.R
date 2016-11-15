# SpreadsheetToNc.R
#
# A collection of functions to convert flux tower
# data from spreadsheet to netcdf.
#
# Gab Abramowitz UNSW 2016 (palshelp at gmail dot com)
#

#-----------------------------------------------------------------------------


ReadTextFluxData = function(fileinname, vars){
	# This function reads comma-delimited text files containing
	# met and flux data from Fluxnet data providers.
  
#######	# First get site name, lat, lon, elevation and time step size:

  
	# Get column names and classes:
  tcol <- findColIndices(fileinname, var_names=vars$Fluxnet_variable, var_classes=vars$Fluxnet_classes)
    
	# Read flux tower data (skips unwanted columns):
	FluxData = read.csv(file=fileinname, header=TRUE,	colClasses=tcol$classes)
  
  #Sanity check, does variable order in file match that specified in tcols?
  if(any(colnames(data)!=tcol$names)) stop("Check variable ordering, variables don't match data retrieved from file")
  
  #Change column names and tcol$names to match ALMA convention
  tcol$names         <- rename_vars(tcol$names)
  colnames(FluxData) <- tcol$names
    
	# Note number of time steps in data:
	ntsteps = nrow(FluxData)
  
	if(!(ntsteps>=12 && ntsteps < 1e9)){
		CheckError(paste('S5: Unable to determine number of time steps in:',
			stripFilename(fileinname)))
	}
  
	# and time step size:
  start <- strptime(FluxData$TIMESTAMP_START[1], "%Y%m%d%H%M") #convert to date string
  end   <- strptime(FluxData$TIMESTAMP_END[1], "%Y%m%d%H%M")
  
	timestepsize <- as.numeric(end) - as.numeric(start)
  
	if( !(timestepsize>=300 && timestepsize<=3600) ){
		CheckError(paste('S5: Unable to ascertain time step size in',
			stripFilename(fileinname)))
	}
  
	tstepinday=86400/timestepsize # time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set

  # Find starting date / time:
	#if(substr(FluxData$LocDate[1],2,2)=='/'){ # i.e. one char day
		sday = as.numeric(format(start, format="%d"))
		#if(substr(FluxData$LocDate[1],4,4)=='/'){ # i.e. one char month
		smonth = as.numeric(format(start, format="%m"))
    syear = as.numeric(format(start, format="%Y"))
	#		ystart=5
	#	}else if(substr(FluxData$LocDate[1],5,5)=='/'){ # i.e. two char month
  #			smonth = format(start, format="%d")
	#		ystart=6
	#	}else{
	#		CheckError(paste('S1: Error interpreting data set starting',
	#			'date from spreadsheet.'))
	#	}
	#}else if(substr(FluxData$LocDate[1],3,3)=='/'){ # i.e. two char day
	#	sday = as.numeric(substr(FluxData$LocDate[1],1,2))
	#	if(substr(FluxData$LocDate[1],5,5)=='/'){ # i.e. one char month
	#		smonth = as.numeric(substr(FluxData$LocDate[1],4,4))
	#		ystart=6
	#	}else if(substr(FluxData$LocDate[1],6,6)=='/'){ # i.e. two char month
	#		smonth = as.numeric(substr(FluxData$LocDate[1],4,5))
	#		ystart=7
	#	}else{
	#		CheckError(paste('S1: Error interpreting data set starting',
	#			'date from spreadsheet.'))
	#	}
#	}else{
#		CheckError(paste('S1: Error interpreting data set starting',
#			'date from spreadsheet.'))
#	}

#	# Create starting year:
#	ystr = substr(FluxData$LocDate[1],ystart,nchar(FluxData$LocDate[1]))
#	if(nchar(ystr)==2){ # two character yesr
#		if(as.numeric(ystr)<60){
#			nystr = paste('20',ystr,sep='')
#		}else{
#			nystr = paste('19',ystr,sep='')
#		}
#	}else if(nchar(ystr)==4){ # four character year
#		nystr = ystr
#		# do nothing	
#	}else{
#		CheckError(paste('S1: Error interpreting data set starting',
#			'date from spreadsheet.'))
#	}
#	syear = as.numeric(nystr)	
	shod = format(start, format="%H") # starting hour of day
	intyears = Yeardays(syear,ndays)

	# Collate start time variables:
	starttime=list(syear=syear,smonth=smonth,sday=sday,shod=shod)

# Create list for function exit:
	filedata = list(data=FluxData, ntsteps=ntsteps,
	               	starttime=starttime, timestepsize=timestepsize,
                  ndays=ndays,whole=intyears$whole)

  return(filedata)

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

CheckTextDataRanges = function(datain,found){
	# Get acceptable ranges for variables:	
	range = GetVariableRanges()
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

# Check the existence of optional variables:
CheckTextDataVars = function(datain){
	# First check that all essential variables are present:
	if(any(datain$data$SWdown==SprdMissingVal)){
		CheckError('S2: Downward shortwave has missing values.')
	}
	if(any(datain$data$Tair==SprdMissingVal)){
		CheckError('S2: Air temperature has missing values.')
	}
	if(any(datain$data$Qair==SprdMissingVal)){
		CheckError('S2: Humidity has missing values.')
	}
	if(any(datain$data$Wind==SprdMissingVal)){
		CheckError('S2: Windspeed has missing values.')
	}
	if(any(datain$data$Rainf==SprdMissingVal)){
		CheckError('S2: Rainfall has missing values.')
	}
	# Initialise list of found variables:
	LWdown = FALSE# surface incident longwave rad [W/m^2]
	LWdown_all = FALSE# gapless surface incident longwave rad [W/m^2]
	Snowf = FALSE # snowfall rate [mm/s]
	PSurf = FALSE # surface air pressure [Pa]
	CO2air = FALSE# near surface CO2 concentration [ppmv]
	Qle = FALSE   # latent heat flux [W/m^2]
	Qh = FALSE    # sensible heat flux [W/m^2]
	Qg = FALSE    # ground heat flux [W/m^2]
	NEE = FALSE   # net ecosystem exchange CO2 [umol/m^2/s]
	GPP = FALSE   # gross primary production CO2 [umol/m^2/s]
	SWup = FALSE  # reflected SW rad [W/m^2]
	Rnet = FALSE  # net absorbed radiation [W/m^2]
	SWdown_qc = FALSE
	Tair_qc = FALSE
	Qair_qc = FALSE
	Wind_qc = FALSE
	Rainf_qc = FALSE
	LWdown_qc = FALSE
	Snowf_qc = FALSE 
	PSurf_qc = FALSE 
	CO2air_qc = FALSE
	Qle_qc = FALSE   
	Qh_qc = FALSE    
	Qg_qc = FALSE    
	NEE_qc = FALSE
	GPP_qc = FALSE
	SWup_qc = FALSE  
	Rnet_qc = FALSE  
	found = list(LWdown=LWdown,LWdown_all=LWdown_all,Snowf=Snowf,
		PSurf=PSurf,CO2air=CO2air,Qle=Qle,Qh=Qh,Qg=Qg,NEE=NEE,
		SWup=SWup,Rnet=Rnet,SWdown_qc=SWdown_qc,Tair_qc=Tair_qc,
		Qair_qc=Qair_qc,Wind_qc=Wind_qc,Rainf_qc=Rainf_qc,
		LWdown_qc=LWdown_qc,Snowf_qc=Snowf_qc,PSurf_qc=PSurf_qc,
		CO2air_qc=CO2air_qc,Qle_qc=Qle_qc,Qh_qc=Qh_qc,
		Qg_qc=Qg_qc,NEE_qc=NEE_qc,GPP_qc=GPP_qc,SWup_qc=SWup_qc,
		Rnet_qc=Rnet_qc)
	
	# Begin checking:
	if(any(datain$data$LWdown!=SprdMissingVal)){ # note unusual condition
		found$LWdown = TRUE # some data present
		if(!any(datain$data$LWdown==SprdMissingVal)){
			found$LWdown_all = TRUE # all data present
		}
	}
	if(!any(datain$data$Snowf==SprdMissingVal)){
		found$Snowf = TRUE
	}
	if(!any(datain$data$PSurf==SprdMissingVal)){
		found$PSurf = TRUE
	}
	if(!any(datain$data$CO2air==SprdMissingVal)){
		found$CO2air = TRUE
	}
	if(!any(datain$data$Qle==SprdMissingVal)){
		found$Qle = TRUE
	}
	if(!any(datain$data$Qh==SprdMissingVal)){
		found$Qh = TRUE
	}
	if(!any(datain$data$Qg==SprdMissingVal)){
		found$Qg = TRUE
	}
	if(!any(datain$data$NEE==SprdMissingVal)){
		found$NEE = TRUE
	}
	if(!any(datain$data$GPP==SprdMissingVal) && 
		datain$templateVersion!='1.0.1'){
		found$GPP = TRUE
	}
	if(!any(datain$data$SWup==SprdMissingVal)){
		found$SWup = TRUE
	}
	if(!any(datain$data$Rnet==SprdMissingVal)){
		found$Rnet = TRUE
	}
	# Note change of "found" criteria for qc flags:
	if(any(datain$data$SWdownFlag!=SprdMissingVal)){
		found$SWdown_qc = TRUE
	}
	if(any(datain$data$TairFlag!=SprdMissingVal)){
		found$Tair_qc = TRUE
	}
	if(any(datain$data$QairFlag!=SprdMissingVal)){
		found$Qair_qc = TRUE
	}
	if(any(datain$data$RainfFlag!=SprdMissingVal)){
		found$Rainf_qc = TRUE
	}
	if(any(datain$data$LWdownFlag!=SprdMissingVal)){
		found$LWdown_qc = TRUE
	}
	if(any(datain$data$SnowfFlag!=SprdMissingVal)){
		found$Snowf_qc = TRUE
	}
	if(any(datain$data$PSurfFlag!=SprdMissingVal)){
		found$PSurf_qc = TRUE
	}else if(! found$PSurf){
		# If we didn't find PSurf, we know PALS will 
		# synthesize, and we'll mark that in qc variable.
		found$PSurf_qc = TRUE
	}
	if(any(datain$data$CO2airFlag!=SprdMissingVal)){
		found$CO2air_qc = TRUE
	}
	if(any(datain$data$WindFlag!=SprdMissingVal)){
		found$Wind_qc = TRUE
	}
	if(any(datain$data$QleFlag!=SprdMissingVal)){
		found$Qle_qc = TRUE
	}
	if(any(datain$data$QhFlag!=SprdMissingVal)){
		found$Qh_qc = TRUE
	}
	if(any(datain$data$QgFlag!=SprdMissingVal)){
		found$Qg_qc = TRUE
	}
	if(any(datain$data$NEEFlag!=SprdMissingVal)){
		found$NEE_qc = TRUE
	}
	if(any(datain$data$GPPFlag!=SprdMissingVal)){
		found$GPP_qc = TRUE
	}
	if(any(datain$data$SWupFlag!=SprdMissingVal)){
		found$SWup_qc = TRUE
	}
	if(any(datain$data$RnetFlag!=SprdMissingVal)){
		found$Rnet_qc = TRUE
	}
	# IF no LSM testing variables are found, report it:
	if((!found$Qle)&(!found$Qh)&(!found$NEE)&(!found$Rnet)&
		(!found$GPP)&(!found$SWup)&(!found$Qg)){
		CheckError(paste('S2: Could not find any LSM evaluation',
		'variables: Qle, Qh, Qg, NEE, GPP, SWup or Rnet.'))
	}
	return(found)	
}



#-----------------------------------------------------------------------------

CreateFluxNcFile = function(fluxfilename,datain,latitude,longitude,
	timestepsize,datasetname,datasetversion,found,starttime,templateVersion,
	elevation=NA,measurementheight=NA,canopyheight=NA,
	vegetationtype=NA,utcoffset=NA,avprecip=NA,avtemp=NA){
	# This function, sent observed flux variables uploaded by a user
	# in text format, creates a netcdf file which is downloadable for
	# the flux data provider and is used internally for analysis scripts.
	library(ncdf4) # load netcdf library
	missing_value=NcMissingVal # default missing value for all variables
	# Define x, y and z dimensions
	xd = ncdim_def('x',vals=c(1),units='')	
	yd = ncdim_def('y',vals=c(1),units='')
	# Determine data start date and time:
	timeunits = CreateTimeunits(starttime)
	# Create time dimension variable:
	tt=c(0:(datain$ntsteps-1))
	timedata = as.double(tt*timestepsize)
	# Define time dimension:
	td = ncdim_def('time',unlim=TRUE,units=timeunits,vals=timedata)
	# VARIABLE DEFINITIONS ##############################################
	# First, non-time variables:
	# Define latitude:
	lat=ncvar_def('latitude','degrees_north',dim=list(xd,yd),
		missval=missing_value,longname='Latitude')
	# Define longitude:
	lon=ncvar_def('longitude','degrees_east',dim=list(xd,yd),
		missval=missing_value,longname='Longitude')
	
	# Initialise variable list:
	fluxncvars = list(lat,lon)
	ctr = 3
	if(found$Qle){ # Define Qle variable:
		Qle=ncvar_def('Qle','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Latent heat flux from surface')
		fluxncvars[[ctr]] = Qle
		ctr = ctr + 1
	}
	if(found$Qh){ # Define Qh variable:
		Qh=ncvar_def('Qh','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Sensible heat flux from surface')
		fluxncvars[[ctr]] = Qh
		ctr = ctr + 1
	}
	if(found$NEE){ # Define NEE variable:
		NEE=ncvar_def('NEE','umol/m^2/s', dim=list(xd,yd,td),
			missval=missing_value,longname='Net ecosystem exchange of CO2')
		fluxncvars[[ctr]] = NEE
		ctr = ctr + 1
	}
	if(found$GPP){ # Define GPP variable:
		GPP=ncvar_def('GPP','umol/m^2/s', dim=list(xd,yd,td),
			missval=missing_value,longname='Gross primary poductivity of CO2')
		fluxncvars[[ctr]] = GPP
		ctr = ctr + 1
	}
	if(found$Qg){ # Define Qg variable:
		Qg=ncvar_def('Qg','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Ground heat flux')
		fluxncvars[[ctr]] = Qg
		ctr = ctr + 1
	}
	if(found$SWup){ # Define SWup and SWnet variables:
		SWup=ncvar_def('SWup','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Reflected shortwave radiation')
		fluxncvars[[ctr]] = SWup
		ctr = ctr + 1
		SWnet=ncvar_def('SWnet','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Net absorbed shortwave radiation')
		fluxncvars[[ctr]] = SWnet
		ctr = ctr + 1
	}
	if(found$Rnet){ # Define Rnet variable:
		Rnet=ncvar_def('Rnet','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Net absorbed radiation')
		fluxncvars[[ctr]] = Rnet
		ctr = ctr + 1
	}
	# Define quality control flag variables:
	if(found$Qle_qc){ # Define Qle_qc variable:
		Qle_qc=ncvar_def('Qle_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qle quality control flag')
		fluxncvars[[ctr]] = Qle_qc
		ctr = ctr + 1
	}
	if(found$Qh_qc){ # Define Qh_qc variable:
		Qh_qc=ncvar_def('Qh_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qh quality control flag')
		fluxncvars[[ctr]] = Qh_qc
		ctr = ctr + 1
	}
	if(found$NEE_qc){ # Define NEE_qc variable:
		NEE_qc=ncvar_def('NEE_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='NEE quality control flag')
		fluxncvars[[ctr]] = NEE_qc
		ctr = ctr + 1
	}
	if(found$GPP_qc){ # Define GPP_qc variable:
		GPP_qc=ncvar_def('GPP_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='GPP quality control flag')
		fluxncvars[[ctr]] = GPP_qc
		ctr = ctr + 1
	}
	if(found$Qg_qc){ # Define Qg_qc variable:
		Qg_qc=ncvar_def('Qg_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qg quality control flag')
		fluxncvars[[ctr]] = Qg_qc
		ctr = ctr + 1
	}
	if(found$SWup_qc){ # Define SWup_qc and SWnet_qc variables:
		SWup_qc=ncvar_def('SWup_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='SWup quality control flag')
		fluxncvars[[ctr]] = SWup_qc
		ctr = ctr + 1
		SWnet_qc=ncvar_def('SWnet_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='SWnet quality control flag')
		fluxncvars[[ctr]] = SWnet_qc
		ctr = ctr + 1
	}
	if(found$Rnet_qc){ # Define Rnet_qc variable:
		Rnet_qc=ncvar_def('Rnet_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Rnet quality control flag')
		fluxncvars[[ctr]] = Rnet_qc
		ctr = ctr + 1
	}
	# Define elevation:
	if(!is.na(elevation)){
		elev=ncvar_def('elevation','m',dim=list(xd,yd),
			missval=missing_value,longname='Site elevation above sea level')
		fluxncvars[[ctr]] = elev
		ctr = ctr + 1
	}
	# Define measurement height on tower:
	if(!is.na(measurementheight)){
		refheight=ncvar_def('reference_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Measurement height on flux tower')
		fluxncvars[[ctr]] = refheight
		ctr = ctr + 1
	}
	# Define canopy height:
	if(!is.na(canopyheight)){
		canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Maximum height of vegetation')
		fluxncvars[[ctr]] = canheight
		ctr = ctr + 1
	}
	# Define site time offset:
	if(!is.na(utcoffset)){
		timeoffset=ncvar_def('utc_offset','hours',dim=list(xd,yd),
			missval=missing_value,longname='Local time difference from UTC')
		fluxncvars[[ctr]] = timeoffset
		ctr = ctr + 1
	}
	# Define average precip:
	if(!is.na(avprecip)){
		averageprecip=ncvar_def('averagePrecip','mm',dim=list(xd,yd),
			missval=missing_value,longname='Average annual precipitation')
		fluxncvars[[ctr]] = averageprecip
		ctr = ctr + 1
	}
	# Define average temperature:
	if(!is.na(avtemp)){
		averagetemp=ncvar_def('averageTemp','K',dim=list(xd,yd),
			missval=missing_value,longname='Average temperature')
		fluxncvars[[ctr]] = averagetemp
		ctr = ctr + 1
	}
	# END VARIABLE DEFINITIONS #########################################
	# Create netcdf file:
	ncid = nc_create(fluxfilename,vars=fluxncvars)
	# Write global attributes:
	ncatt_put(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	ncatt_put(ncid,varid=0,attname='Production_source',
		attval='PALS netcdf conversion')
	ncatt_put(ncid,varid=0,attname='PALS_fluxtower_template_version',
		attval=templateVersion)
	ncatt_put(ncid,varid=0,attname='site_name',
		attval=datasetname)
	ncatt_put(ncid,varid=0,attname='site_dataset_version',
		attval=datasetversion)
	if(!is.na(vegetationtype)){
		ncatt_put(ncid,varid=0,attname='IGBP_vegetation_type',attval=vegetationtype)
	}
	ncatt_put(ncid,varid=0,attname='PALS_contact',
		attval='palshelp@gmail.com')
	# Add variable data to file:
	ncvar_put(ncid,lat,vals=latitude)
	ncvar_put(ncid,lon,vals=longitude)
	# Optional meta data for each site:
	if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
	if(!is.na(measurementheight)) {ncvar_put(ncid,refheight,vals=measurementheight)}
	if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
	if(!is.na(utcoffset)) {ncvar_put(ncid,timeoffset,vals=utcoffset)}
	if(!is.na(avprecip)) {ncvar_put(ncid,averageprecip,vals=avprecip)}
	if(!is.na(avtemp)) {ncvar_put(ncid,averagetemp,vals=avtemp)}
	# Time dependent variables:
	if(found$Qle){
		ncvar_put(ncid,Qle,vals=datain$data$Qle)
		ncatt_put(ncid,Qle,'CF_name','surface_upward_latent_heat_flux')
	}
	if(found$Qh){
		ncvar_put(ncid,Qh,vals=datain$data$Qh)
		ncatt_put(ncid,Qh,'CF_name','surface_upward_sensible_heat_flux')
	}
	if(found$NEE){
		ncvar_put(ncid,NEE,vals=datain$data$NEE)
		ncatt_put(ncid,NEE,'CF_name','surface_upward_mole_flux_of_carbon_dioxide **')
		ncatt_put(ncid,NEE,'CFname**','Note units are different')
	}
	if(found$GPP){
		ncvar_put(ncid,GPP,vals=datain$data$GPP)
		ncatt_put(ncid,GPP,'CF_name','gross_primary_productivity_of_biomass_expressed_as_carbon **')
		ncatt_put(ncid,GPP,'CFname**','Note units are different')
	}
	if(found$Rnet){
		ncvar_put(ncid,Rnet,vals=datain$data$Rnet)
		ncatt_put(ncid,Rnet,'CF_name','surface_net_downward_radiative_flux')
	}
	if(found$Qg){ncvar_put(ncid,Qg,vals=datain$data$Qg)}
	if(found$SWup){
		ncvar_put(ncid,SWup,vals=datain$data$SWup)
		ncatt_put(ncid,SWup,'CF_name','surface_upwelling_shortwave_flux_in_air')
		ncatt_put(ncid,SWnet,'CF_name','surface_net_downward_shortwave_flux')
		ncatt_put(ncid,SWnet,'source','defined as SWdown - SWup')
		ncvar_put(ncid,SWnet,vals=(datain$data$SWdown - datain$data$SWup))
	}
	if(found$Qle_qc){
		ncatt_put(ncid,Qle_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Qle_qc,vals=datain$data$QleFlag)
	}
	if(found$Qh_qc){
		ncatt_put(ncid,Qh_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Qh_qc,vals=datain$data$QhFlag)
	}
	if(found$NEE_qc){
		ncatt_put(ncid,NEE_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,NEE_qc,vals=datain$data$NEEFlag)
	}
	if(found$GPP_qc){
		ncatt_put(ncid,GPP_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,GPP_qc,vals=datain$data$GPPFlag)
	}
	if(found$Rnet_qc){
		ncatt_put(ncid,Rnet_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Rnet_qc,vals=datain$data$RnetFlag)
	}
	if(found$Qg_qc){
		ncatt_put(ncid,Qg_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Qg_qc,vals=datain$data$QgFlag)
	}
	if(found$SWup_qc){
		ncatt_put(ncid,SWup_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,SWup_qc,vals=datain$data$SWupFlag)
		ncatt_put(ncid,SWnet_qc,'source','defined as min(SWdown_qc - SWup_qc)')
		ncatt_put(ncid,SWnet_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,SWnet_qc,vals=pmin(datain$data$SWdownFlag,datain$data$SWupFlag))
	}
	
	# Close netcdf file:
	nc_close(ncid)
}

#-----------------------------------------------------------------------------

CreateMetNcFile = function(metfilename,datain,latitude,longitude,timestepsize,
	datasetname,datasetversion,defaultLWsynthesis,found,starttime,
	templateVersion,elevation=NA,measurementheight=NA,canopyheight=NA,
	vegetationtype=NA,utcoffset=NA,avprecip=NA,avtemp=NA){
	# This function, sent observed met variables uploaded by a user
	# in text format, creates a netcdf file which is downloadable for
	# the data provider and is used internally for analysis scripts.
	library(ncdf4) # load netcdf library
	missing_value=-9999 # default missing value for all variables
	# Define x, y and z dimensions
	xd = ncdim_def('x',vals=c(1),units='')	
	yd = ncdim_def('y',vals=c(1),units='')
	zd = ncdim_def('z',vals=c(1),units='')
	# Determine data start date and time:
	timeunits = CreateTimeunits(starttime)
	# Create time dimension variable:
	tt=c(0:(datain$ntsteps-1))
	timedata = as.double(tt*timestepsize)
	# Define time dimension:
	td = ncdim_def('time',unlim=TRUE,units=timeunits,vals=timedata)
	# VARIABLE DEFINITIONS ##############################################
	# First, non-time variables:
	# Define latitude:
	lat=ncvar_def('latitude','degrees_north',dim=list(xd,yd),
		missval=missing_value,longname='Latitude')
	# Define longitude:
	lon=ncvar_def('longitude','degrees_east',dim=list(xd,yd),
		missval=missing_value,longname='Longitude')
	# Define SWdown variable:
	SWdown=ncvar_def('SWdown','W/m^2', dim=list(xd,yd,td),
		missval=missing_value,longname='Surface incident shortwave radiation')
	# Define Tair variable:
	Tair=ncvar_def('Tair','K', dim=list(xd,yd,zd,td),
		missval=missing_value,longname='Near surface air temperature')
	# Define Rainf variable:
	Rainf=ncvar_def('Rainf','mm/s', dim=list(xd,yd,td),
		missval=missing_value,longname='Rainfall rate')
	# Define Qair variable:
	Qair=ncvar_def('Qair','kg/kg', dim=list(xd,yd,zd,td),
		missval=missing_value,longname='Near surface specific humidity')
	# Define Wind variable:
	Wind=ncvar_def('Wind','m/s', dim=list(xd,yd,zd,td),
		missval=missing_value,longname='Scalar windspeed')
	# Define PSurf variable:
	PSurf=ncvar_def('PSurf','Pa', dim=list(xd,yd,td),
		missval=missing_value,longname='Surface air pressure')
	# Define LWdown variable:
	LWdown=ncvar_def('LWdown','W/m^2', dim=list(xd,yd,td),
		missval=missing_value,longname='Surface incident longwave radiation')
	metncvars = list(lat,lon,SWdown,Tair,Rainf,Qair,Wind,PSurf,LWdown)
	ctr=10
	# Now optional variables:
	if(found$Snowf){
		# Define Snowf variable:
		Snowf=ncvar_def('Snowf','mm/s liq water equivalent', dim=list(xd,yd,td),
			missval=missing_value,longname='Snowfall rate')
		metncvars[[ctr]] = Snowf
		ctr = ctr + 1
	}
	if(found$CO2air){
		# Define CO2air variable:
		CO2air=ncvar_def('CO2air','ppmv', dim=list(xd,yd,zd,td),
			missval=missing_value,longname='Near surface CO2 concentration')
		metncvars[[ctr]] = CO2air
		ctr = ctr + 1
	}
	if(found$SWdown_qc){ # Define SWdown_qc variable:
		SWdown_qc=ncvar_def('SWdown_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='SWdown quality control flag',prec='integer')
		metncvars[[ctr]] = SWdown_qc
		ctr = ctr + 1
	}
	if(found$Tair_qc){ # Define Tair_qc variable:
		Tair_qc=ncvar_def('Tair_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Tair quality control flag',prec='integer')
		metncvars[[ctr]] = Tair_qc
		ctr = ctr + 1
	}
	if(found$Rainf_qc){ # Define Rainf_qc variable:
		Rainf_qc=ncvar_def('Rainf_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Rainf quality control flag',prec='integer')
		metncvars[[ctr]] = Rainf_qc
		ctr = ctr + 1
	}
	if(found$Qair_qc){ # Define Qair_qc variable:
		Qair_qc=ncvar_def('Qair_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qair quality control flag',prec='integer')
		metncvars[[ctr]] = Qair_qc
		ctr = ctr + 1
	}
	if(found$Wind_qc){ # Define Wind_qc variable:
		Wind_qc=ncvar_def('Wind_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Wind quality control flag',prec='integer')
		metncvars[[ctr]] = Wind_qc
		ctr = ctr + 1
	}
	if(found$PSurf_qc){ # Define PSurf_qc variable:
		PSurf_qc=ncvar_def('PSurf_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='PSurf quality control flag',prec='integer')
		metncvars[[ctr]] = PSurf_qc
		ctr = ctr + 1
	}
	if(found$Snowf_qc){ # Define Snowf_qc variable:
		Snowf_qc=ncvar_def('Snowf_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Snowf quality control flag',prec='integer')
		metncvars[[ctr]] = Snowf_qc
		ctr = ctr + 1
	}
	if(found$CO2air_qc){ # Define CO2air_qc variable:
		CO2air_qc=ncvar_def('CO2air_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='CO2air quality control flag',prec='integer')
		metncvars[[ctr]] = CO2air_qc
		ctr = ctr + 1
	}
	if(found$LWdown_qc){ # Define LWdown_qc variable:
		LWdown_qc=ncvar_def('LWdown_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='LWdown quality control flag',prec='integer')
		metncvars[[ctr]] = LWdown_qc
		ctr = ctr + 1
	}
	# Define elevation:
	if(!is.na(elevation)){
		elev=ncvar_def('elevation','m',dim=list(xd,yd),
			missval=missing_value,longname='Site elevation above sea level')
		metncvars[[ctr]] = elev
		ctr = ctr + 1
	}	
	# Define measurement height on tower:
	if(!is.na(measurementheight)){
		refheight=ncvar_def('reference_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Measurement height on flux tower')
		metncvars[[ctr]] = refheight
		ctr = ctr + 1
	}
	# Define canopy height:
	if(!is.na(canopyheight)){
		canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Maximum height of vegetation')
		metncvars[[ctr]] = canheight
		ctr = ctr + 1
	}
	# Define site time offset:
	if(!is.na(utcoffset)){
		timeoffset=ncvar_def('utc_offset','hours',dim=list(xd,yd),
			missval=missing_value,longname='Local time difference from UTC')
		metncvars[[ctr]] = timeoffset
		ctr = ctr + 1
	}
	# Define average precip:
	if(!is.na(avprecip)){
		averageprecip=ncvar_def('averagePrecip','mm',dim=list(xd,yd),
			missval=missing_value,longname='Average annual precipitation')
		metncvars[[ctr]] = averageprecip
		ctr = ctr + 1
	}
	# Define average temperature:
	if(!is.na(avtemp)){
		averagetemp=ncvar_def('averageTemp','K',dim=list(xd,yd),
			missval=missing_value,longname='Average temperature')
		metncvars[[ctr]] = averagetemp
		ctr = ctr + 1
	}

	# END VARIABLE DEFINITIONS #########################################
	# Create netcdf file:
	ncid = nc_create(metfilename,vars=metncvars)
	# Write global attributes:
	ncatt_put(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	ncatt_put(ncid,varid=0,attname='Production_source',
		attval='PALS netcdf conversion')
	ncatt_put(ncid,varid=0,attname='PALS_fluxtower_template_version',
		attval=templateVersion)
	ncatt_put(ncid,varid=0,attname='site_name',
		attval=datasetname)
	ncatt_put(ncid,varid=0,attname='PALS_dataset_version',
		attval=datasetversion)
	if(!is.na(vegetationtype)){
		ncatt_put(ncid,varid=0,attname='IGBP_vegetation_type',attval=vegetationtype)
	}
	ncatt_put(ncid,varid=0,attname='PALS contact',
		attval='palshelp@gmail.com')
	# Add variable data to file:
	ncvar_put(ncid,lat,vals=latitude)
	ncvar_put(ncid,lon,vals=longitude)
	# Optional meta data for each site:
	if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
	if(!is.na(measurementheight)) {ncvar_put(ncid,refheight,vals=measurementheight)}
	if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
	if(!is.na(utcoffset)) {ncvar_put(ncid,timeoffset,vals=utcoffset)}
	if(!is.na(avprecip)) {ncvar_put(ncid,averageprecip,vals=avprecip)}
	if(!is.na(avtemp)) {ncvar_put(ncid,averagetemp,vals=avtemp)}
	# Time dependent variables:
	ncvar_put(ncid,SWdown,vals=datain$data$SWdown)
	ncatt_put(ncid,SWdown,attname='CF_name',attval='surface_downwelling_shortwave_flux_in_air')
	ncvar_put(ncid,Tair,vals=datain$data$Tair)
	ncatt_put(ncid,Tair,attname='CF_name',attval='surface_temperature')
	ncvar_put(ncid,Rainf,vals=datain$data$Rainf)
	ncatt_put(ncid,Rainf,attname='CF_name',attval='precipitation_flux')
	ncvar_put(ncid,Qair,vals=datain$data$Qair)
	ncatt_put(ncid,Qair,attname='CF_name',attval='surface_specific_humidity')
	ncvar_put(ncid,Wind,vals=datain$data$Wind)
	ncatt_put(ncid,Wind,attname='CF_name',attval='wind_speed')
	ncvar_put(ncid,PSurf,vals=datain$data$PSurf)
	ncatt_put(ncid,PSurf,attname='CF_name',attval='surface_air_pressure')
	if(! found$PSurf){
		ncatt_put(ncid,PSurf,'source','Synthesized in PALS based on elevation and temperature')
	}
	ncvar_put(ncid,LWdown,vals=datain$data$LWdown)
	ncatt_put(ncid,LWdown,attname='CF_name',attval='surface_downwelling_longwave_flux_in_air')
	if(! found$LWdown){
		ncatt_put(ncid,LWdown,'source',paste('Entirely synthesized in PALS using',defaultLWsynthesis))
	}else if(found$LWdown & !found$LWdown_all){
		ncatt_put(ncid,LWdown,'gapfill_technique',paste('Gap-filled in PALS using',defaultLWsynthesis))
		ncatt_put(ncid,LWdown,'gapfill_note','Fluxdata.org template has no QC flag for LWdown - data are assumed original.')
	}
	if(found$Snowf){
		ncvar_put(ncid,Snowf,vals=datain$data$Snowf)
	}
	if(found$CO2air){
		ncvar_put(ncid,CO2air,vals=datain$data$CO2air)
	}
	if(found$SWdown_qc){
		ncatt_put(ncid,SWdown_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,SWdown_qc,vals=datain$data$SWdownFlag)
	}
	if(found$Tair_qc){
		ncatt_put(ncid,Tair_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Tair_qc,vals=datain$data$TairFlag)
	}
	if(found$Rainf_qc){
		ncatt_put(ncid,Rainf_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Rainf_qc,vals=datain$data$RainfFlag)
	}
	if(found$Qair_qc){
		ncatt_put(ncid,Qair_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Qair_qc,vals=datain$data$QairFlag)
	}
	if(found$Wind_qc){
		ncatt_put(ncid,Wind_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Wind_qc,vals=datain$data$WindFlag)
	}
	if(found$Snowf_qc){
		ncatt_put(ncid,Snowf_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,Snowf_qc,vals=datain$data$SnowfFlag)
	}
	if(found$PSurf_qc){
		ncatt_put(ncid,PSurf_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,PSurf_qc,vals=datain$data$PSurfFlag)
	}
	if(found$CO2air_qc){
		ncatt_put(ncid,CO2air_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,CO2air_qc,vals=datain$data$CO2airFlag)
	}
	if(found$LWdown_qc){
		ncatt_put(ncid,LWdown_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		ncvar_put(ncid,LWdown_qc,vals=datain$data$LWdownFlag)
	}
	# Close netcdf file:
	nc_close(ncid)
}

#-----------------------------------------------------------------------------

OzFlux2PALSQCFlag = function(flag){
  ## In PALS:
  # 1: original data
  # 0: not original data
  # -9999: unknown
  flag[flag>1]=0
  flag[flag<1]=0
  flag[is.na(flag)]=0 #-9999
  return(flag)
}

#-----------------------------------------------------------------------------

OzFluxNc2PALSQCFlag = function(flag){
  # Find these flag values in constants.R
  flagnew = as.integer(flag)
  flagnew[flag!=0]=QCnotmeasured
  flagnew[flag==0] = QCmeasured # original data
  flagnew[flag==10] = QCmeasured # instrument calibration correction, but good data
 # flagnew[flag==20] = QCmeasured # filled with nearby station or out-of-sample tested ACCESS forecast (met data only)
  flagnew[flag==-9999]=QCnotmeasured
  flagnew[is.na(flag)]=QCnotmeasured
  return(flagnew)
}

#-----------------------------------------------------------------------------

NA2MissVal = function(data){
  # Convert from NA to NcMissingVal for missing value
  data[which(is.na(data))] = NcMissingVal
  return(data)
}

#-----------------------------------------------------------------------------

FindMissingValue = function(PALSt,SprdMissingVal){
  if(any(PALSt[4:dim(PALSt)[1],3]==SprdMissingVal)){cat('Missing values in SWdown \n')}
  if(any(PALSt[4:dim(PALSt)[1],3]<0 & PALSt[4:dim(PALSt)[1],3]!=SprdMissingVal)){cat('Negative values in SWdown \n')}
  if(any(PALSt[4:dim(PALSt)[1],4]==SprdMissingVal)){cat('Missing values in SWdownFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],5]==SprdMissingVal)){cat('Missing values in LWdown \n')}
  if(any(PALSt[4:dim(PALSt)[1],6]==SprdMissingVal)){cat('Missing values in LWdownFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],7]==SprdMissingVal)){cat('Missing values in Tair \n')}
  if(any(PALSt[4:dim(PALSt)[1],8]==SprdMissingVal)){cat('Missing values in TairFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],9]==SprdMissingVal)){cat('Missing values in Qair \n')}
  if(any(PALSt[4:dim(PALSt)[1],10]==SprdMissingVal)){cat('Missing values in QairFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],11]==SprdMissingVal)){cat('Missing values in Wind \n')}
  if(any(PALSt[4:dim(PALSt)[1],12]==SprdMissingVal)){cat('Missing values in WindFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],13]==SprdMissingVal)){cat('Missing values in Rainfall \n')}
  if(any(PALSt[4:dim(PALSt)[1],14]==SprdMissingVal)){cat('Missing values in RainfFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],15]==SprdMissingVal)){cat('Missing values in Snowf \n')}
  if(any(PALSt[4:dim(PALSt)[1],16]==SprdMissingVal)){cat('Missing values in SnowfFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],17]==SprdMissingVal)){cat('Missing values in PSurf \n')}
  if(any(PALSt[4:dim(PALSt)[1],18]==SprdMissingVal)){cat('Missing values in PSurfFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],19]==SprdMissingVal)){cat('Missing values in CO2air \n')}
  if(any(PALSt[4:dim(PALSt)[1],20]==SprdMissingVal)){cat('Missing values in CO2airFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],21]==SprdMissingVal)){cat('Missing values in Rnet \n')}
  if(any(PALSt[4:dim(PALSt)[1],22]==SprdMissingVal)){cat('Missing values in RnetFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],23]==SprdMissingVal)){cat('Missing values in SWup \n')}
  if(any(PALSt[4:dim(PALSt)[1],23]<0 & PALSt[4:dim(PALSt)[1],23]!=SprdMissingVal)){cat('Negative values in SWup \n')}
  if(any(PALSt[4:dim(PALSt)[1],24]==SprdMissingVal)){cat('Missing values in SWupFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],25]==SprdMissingVal)){cat('Missing values in Qle \n')}
  if(any(PALSt[4:dim(PALSt)[1],26]==SprdMissingVal)){cat('Missing values in QleFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],27]==SprdMissingVal)){cat('Missing values in Qh \n')}
  if(any(PALSt[4:dim(PALSt)[1],28]==SprdMissingVal)){cat('Missing values in QhFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],29]==SprdMissingVal)){cat('Missing values in NEE \n')}
  if(any(PALSt[4:dim(PALSt)[1],30]==SprdMissingVal)){cat('Missing values in NEEFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],31]==SprdMissingVal)){cat('Missing values in GPP \n')}
  if(any(PALSt[4:dim(PALSt)[1],32]==SprdMissingVal)){cat('Missing values in GPPFlag \n')}
  if(any(PALSt[4:dim(PALSt)[1],33]==SprdMissingVal)){cat('Missing values in Qg \n')}
  if(any(PALSt[4:dim(PALSt)[1],34]==SprdMissingVal)){cat('Missing values in QgFlag \n')}
}

#-----------------------------------------------------------------------------

MeanValue = function(PALSt, var_idx){
  mean_val = mean(as.numeric(PALSt[4:length(PALSt[,1]),var_idx]), na.rm=TRUE)
  return(mean_val)
}

#-----------------------------------------------------------------------------

# Acceptable variable ranges - specific to flux tower data checking
GetVariableRanges = function(){
	vnames = c('SWdown','LWdown','Tair','Qair','Rainf','Snowf','PSurf','CO2air','Wind','Qle','Qh','Qg','NEE','GPP','SWup','Rnet')
	vars = GetVariableDetails(vnames)
	SWdown = vars[[1]]$range # surface incident shortwave rad [W/m^2]
	LWdown = vars[[2]]$range  # surface incident longwave rad [W/m^2]
	Tair = vars[[3]]$range  # near surface air temperature [K]
	Qair = vars[[4]]$range   # near surface specific humidity [kg/kg]
	Rainf = vars[[5]]$range  # rainfall rate [mm/s]
	Snowf = vars[[6]]$range  # snowfall rate [mm/s]
	PSurf = vars[[7]]$range # surface air pressure [Pa]
	CO2air = vars[[8]]$range # near surface CO2 concentration [ppmv]
	Wind = vars[[9]]$range     # scalar windspeed [m/s]
	Qle = vars[[10]]$range # latent heat flux [W/m^2]
	Qh = vars[[11]]$range # sensible heat flux [W/m^2]
	Qg = vars[[12]]$range # ground heat flux [W/m^2]
	NEE = vars[[13]]$range    # met ecosystem exchange CO2 [umol/m^2/s]
	GPP = vars[[14]]$range    # met ecosystem exchange CO2 [umol/m^2/s]
	SWup = vars[[15]]$range   # reflected SW rad [W/m^2]
	Rnet = vars[[16]]$range# net absorbed radiation [W/m^2]
	range = list(SWdown=SWdown,LWdown=LWdown,Tair=Tair,
		Qair=Qair,Rainf=Rainf,Snowf=Snowf,PSurf=PSurf,
		CO2air=CO2air,Wind=Wind,Qle=Qle,Qh=Qh,NEE=NEE,
		Rnet=Rnet)
	return(range)	
}
