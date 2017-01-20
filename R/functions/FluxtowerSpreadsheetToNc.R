# SpreadsheetToNc.R
#
# A collection of functions to convert flux tower
# data from spreadsheet to netcdf.
#
# Gab Abramowitz UNSW 2016 (palshelp at gmail dot com)
#

#-----------------------------------------------------------------------------


ReadTextFluxData = function(fileinname, vars, time_vars){
	# This function reads comma-delimited text files containing
	# met and flux data from Fluxnet data providers.
  
  
  ####### First read available variables, corresponding units and ranges ####
  
	# Get column names and classes:
  tcol <- findColIndices(fileinname=fileinname, 
                         var_names=vars$Fluxnet_variable, 
                         var_classes=vars$Fluxnet_class, 
                         essential_vars=vars$Essential_met,
                         preferred_vars=vars$Preferred_eval,
                         time_vars=time_vars)  
  
	# Read flux tower data (skips unwanted columns):
	FluxData = read.csv(file=fileinname, header=TRUE,	colClasses=tcol$classes)  
  
  #Sanity check, does variable order in file match that specified in tcols?
  if(any(colnames(FluxData) != tcol$all_names)) {
    ErrorCheck("Check variable ordering, variables don't match data retrieved from file")
  }
  
  
  #Split time variables from other variables
  #Extract time stamp data
  FluxTime <- FluxData[,which(colnames(FluxData)==time_vars)]
  #Remove time stamp variables from Data variable
  FluxData <- FluxData[,-which(colnames(FluxData)==time_vars)]
  
  
  #Remove faile
  
  #Retrieve original and target units for variables present:
  units <- retrieve_units(vars_present=tcol$names, all_vars=vars)
  
  #Retrieve acceptable variable ranges:
  var_ranges <- retrieve_ranges(vars_present=tcol$names, all_vars=vars)
  
  #Retrieve variable attributes (original Fluxnet name, long name and CF name):
  attributes <- retrieve_atts(vars_present=tcol$names, all_vars=vars)
  
  #Retrieve variable categories (met/eval):
  categories <- retrieve_categories(vars_present=tcol$names, all_vars=vars)
  
  #Retrieve names of ERAinterim variables
  era_vars <- retrieve_ERAvars(vars_present=tcol$names, all_vars=vars)
    
  #Change column names and tcol$names to match ALMA convention
  tcol$names         <- rename_vars(vars_present=tcol$names, all_vars=vars)
  colnames(FluxData) <- tcol$names
  
  
  ###### Get time step and date information #######
  
	# Note number of time steps in data:
	ntsteps = nrow(FluxTime)
  
	if(!(ntsteps>=12 && ntsteps < 1e9)){
		CheckError(paste('S5: Unable to determine number of time steps in:',
			stripFilename(fileinname)))
	}
  
	# and time step size:
  start <- strptime(FluxTime$TIMESTAMP_START[1], "%Y%m%d%H%M") #convert to date string
  end   <- strptime(FluxTime$TIMESTAMP_END[1], "%Y%m%d%H%M")
  
	timestepsize <- as.numeric(end) - as.numeric(start)
  
	if( !(timestepsize>=300 && timestepsize<=3600) ){
		CheckError(paste('S5: Unable to ascertain time step size in',
			stripFilename(fileinname)))
	}
  
	tstepinday=86400/timestepsize # time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set

  # Find starting date / time:
	sday = as.numeric(format(start, format="%d"))
	smonth = as.numeric(format(start, format="%m"))
  syear = as.numeric(format(start, format="%Y"))

	shod = as.numeric(format(start, format="%H")) # starting hour of day
	intyears = Yeardays(syear,ndays)

	# Collate start time variables:
	starttime=list(syear=syear,smonth=smonth,sday=sday,shod=shod)

  #Create list for function exit:
	filedata = list(data=FluxData, vars=tcol$names, era_vars=era_vars, 
                  attributes=attributes,
                  units=units, var_ranges=var_ranges, categories=categories,
                  time=FluxTime, ntsteps=ntsteps, starttime=starttime, 
                  timestepsize=timestepsize, daysPerYr=intyears$daysperyear,
                  ndays=ndays, whole=intyears$whole)

  return(filedata)

}

#-----------------------------------------------------------------------------

# This function creates a netcdf file for flux variables
CreateFluxNcFile = function(fluxfilename, datain, 
                           latitude, longitude, 
                           datasetname, datasetversion, 
                           timestepsize, starttime,
                           elevation=NA, measurementheight=NA, 
                           canopyheight=NA,	vegetationtype=NA, 
                           utcoffset=NA, avprecip=NA, avtemp=NA){
  
  # load netcdf library
  library(ncdf4) 
  
  # default missing value for all variables
  missing_value=NcMissingVal
  
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
  td = ncdim_def('time', unlim=TRUE, units=timeunits, vals=timedata)

  
  # VARIABLE DEFINITIONS ##############################################
  
  #Find met variable indices
  var_ind <- which(indata$categories=="Flux")
  
  #Create variable definitions for time series variables
  var_defs <- lapply(var_ind, function(x) ncvar_def(name=indata$vars[x],
                                                    units=indata$units$target_units[x], 
                                                    dim=list(xd,yd,zd,td), 
                                                    missval=missing_value, 
                                                    longname=indata$longnames[x]))
  
  
  # First necessary non-time variables:
  # Define latitude:
  latdim <- ncvar_def('latitude','degrees_north',dim=list(xd,yd),
                      missval=missing_value, longname='Latitude')  
  # Define longitude:
  londim <- ncvar_def('longitude','degrees_east',dim=list(xd,yd),
                      missval=missing_value,longname='Longitude')
  
  
  #Then optional non-time variables:
  opt_vars <- list()
  ctr <- 1
  # Define measurement height on tower:
  if(!is.na(measurementheight)){
    refheight=ncvar_def('reference_height','m',dim=list(xd,yd),
                        missval=missing_value,longname='Measurement height on flux tower')
    opt_vars[[ctr]] = refheight
    ctr = ctr + 1
  }
  # Define canopy height:
  if(!is.na(canopyheight)){
    canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
                        missval=missing_value,longname='Maximum height of vegetation')
    opt_vars[[ctr]] = canheight
    ctr = ctr + 1
  }
  # Define site time offset:
  if(!is.na(utcoffset)){
    timeoffset=ncvar_def('utc_offset','hours',dim=list(xd,yd),
                         missval=missing_value,longname='Local time difference from UTC')
    opt_vars[[ctr]] = timeoffset
    ctr = ctr + 1
  }
  # Define average precip:
  if(!is.na(avprecip)){
    averageprecip=ncvar_def('averagePrecip','mm',dim=list(xd,yd),
                            missval=missing_value,longname='Average annual precipitation')
    opt_vars[[ctr]] = averageprecip
    ctr = ctr + 1
  }
  # Define average temperature:
  if(!is.na(avtemp)){
    averagetemp=ncvar_def('averageTemp','K',dim=list(xd,yd),
                          missval=missing_value,longname='Average temperature')
    opt_vars[[ctr]] = averagetemp
    ctr = ctr + 1
  }
  
  # END VARIABLE DEFINITIONS #########################################
  
  
  # Create netcdf file:
  if(length(opt_vars)==0) {
    ncid = nc_create(fluxfilename, vars=append(var_defs, c(list(latdim), list(londim))))
  } else {
    ncid = nc_create(fluxfilename, vars=append(var_defs, c(list(latdim), list(londim), list(opt_vars))))
  }
  
  
  # Write global attributes:
  ncatt_put(ncid,varid=0,attname='Production_time',
            attval=as.character(Sys.time()))
  ncatt_put(ncid,varid=0,attname='Production_source',
            attval='PALS netcdf conversion')
  ncatt_put(ncid,varid=0,attname='site_name',
            attval=datasetname)
  ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
            attval=datasetversion)
  if(!is.na(vegetationtype)){
    ncatt_put(ncid,varid=0,attname='IGBP_vegetation_type',attval=vegetationtype)
  }
  ncatt_put(ncid,varid=0,attname='PALS contact',
            attval='palshelp@gmail.com')
  
  # Add variable data to file:
  ncvar_put(ncid, lat, vals=latitude)
  ncvar_put(ncid, lon, vals=longitude)
  
  # Optional meta data for each site:
  if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
  if(!is.na(measurementheight)) {ncvar_put(ncid,refheight,vals=measurementheight)}
  if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
  if(!is.na(utcoffset)) {ncvar_put(ncid,timeoffset,vals=utcoffset)}
  if(!is.na(avprecip)) {ncvar_put(ncid,averageprecip,vals=avprecip)}
  if(!is.na(avtemp)) {ncvar_put(ncid,averagetemp,vals=avtemp)}
  
  # Time dependent variables:
  lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid, 
                                                   varid=var_defs[[x]], 
                                                   vals=indata$data[,x]))
  
  
  ## NEEDS SORTING OUT, NOT SURE WHAT the attribute commands are about !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  #Add original Fluxnet variable name to file
  lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   att_name="Fluxnet_name", attval=  xxxx))  #complete !!!!!

  #Add CF-compliant name to file
  lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   att_name="CF_name", attval=  xxxx))  #complete !!!!!
  
  
  #	ncvar_put(ncid,SWdown,vals=datain$data$SWdown)
  #	ncatt_put(ncid,SWdown,attname='CF_name',attval='surface_downwelling_shortwave_flux_in_air')
  #	if(! found$PSurf){
  #		ncatt_put(ncid,PSurf,'source','Synthesized in PALS based on elevation and temperature')
  #	}
  #	if(! found$LWdown){
  #		ncatt_put(ncid,LWdown,'source',paste('Entirely synthesized in PALS using',defaultLWsynthesis))
  #	}else if(found$LWdown & !found$LWdown_all){
  #		ncatt_put(ncid,LWdown,'gapfill_technique',paste('Gap-filled in PALS using',defaultLWsynthesis))
  #		ncatt_put(ncid,LWdown,'gapfill_note','Fluxdata.org template has no QC flag for LWdown - data are assumed original.')
  #	}
  #
  
  
  # Close netcdf file:
  nc_close(ncid)
}


#-----------------------------------------------------------------------------

# This function creates a netcdf file for met variables
CreateMetNcFile = function(metfilename, datain, 
                           latitude, longitude, 
	                         datasetname, datasetversion, 
                           defaultLWsynthesis, 
                           timestepsize, starttime,
                           elevation=NA, measurementheight=NA, 
                           canopyheight=NA,	vegetationtype=NA, 
                           utcoffset=NA, avprecip=NA, avtemp=NA){
  
  # load netcdf library
	library(ncdf4) 
  
	# default missing value for all variables
	missing_value=NcMissingVal
  
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
	td = ncdim_def('time', unlim=TRUE, units=timeunits, vals=timedata)
  
	# VARIABLE DEFINITIONS ##############################################


  
  #Find met variable indices
  var_ind <- which(indata$categories=="Met")
  
  #Create variable definitions for time series variables
  var_defs <- lapply(var_ind, function(x) ncvar_def(name=indata$vars[x],
                                                    units=indata$units$target_units[x], 
                                                    dim=list(xd,yd,zd,td), 
                                                    missval=missing_value, 
                                                    longname=indata$longnames[x]))
  
  
	# First necessary non-time variables:
	# Define latitude:
	latdim <- ncvar_def('latitude','degrees_north',dim=list(xd,yd),
	                   missval=missing_value, longname='Latitude')  
	# Define longitude:
	londim <- ncvar_def('longitude','degrees_east',dim=list(xd,yd),
	                   missval=missing_value,longname='Longitude')
  
  
  #Then optional non-time variables:
  opt_vars <- list()
  ctr <- 1
	# Define measurement height on tower:
	if(!is.na(measurementheight)){
		refheight=ncvar_def('reference_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Measurement height on flux tower')
		opt_vars[[ctr]] = refheight
    ctr = ctr + 1
	}
	# Define canopy height:
	if(!is.na(canopyheight)){
		canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Maximum height of vegetation')
		opt_vars[[ctr]] = canheight
		ctr = ctr + 1
	}
	# Define site time offset:
	if(!is.na(utcoffset)){
		timeoffset=ncvar_def('utc_offset','hours',dim=list(xd,yd),
			missval=missing_value,longname='Local time difference from UTC')
		opt_vars[[ctr]] = timeoffset
		ctr = ctr + 1
	}
	# Define average precip:
	if(!is.na(avprecip)){
		averageprecip=ncvar_def('averagePrecip','mm',dim=list(xd,yd),
			missval=missing_value,longname='Average annual precipitation')
		opt_vars[[ctr]] = averageprecip
		ctr = ctr + 1
	}
	# Define average temperature:
	if(!is.na(avtemp)){
		averagetemp=ncvar_def('averageTemp','K',dim=list(xd,yd),
			missval=missing_value,longname='Average temperature')
		opt_vars[[ctr]] = averagetemp
		ctr = ctr + 1
	}

	# END VARIABLE DEFINITIONS #########################################

  
  # Create netcdf file:
	if(length(opt_vars)==0) {
    ncid = nc_create(metfilename, vars=append(var_defs, c(list(latdim), list(londim))))
	} else {
	  ncid = nc_create(metfilename, vars=append(var_defs, c(list(latdim), list(londim), list(opt_vars))))
	}
  
  
	# Write global attributes:
  ncatt_put(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	ncatt_put(ncid,varid=0,attname='Production_source',
		attval='PALS netcdf conversion')
	ncatt_put(ncid,varid=0,attname='site_name',
		attval=datasetname)
	ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
		attval=datasetversion)
	if(!is.na(vegetationtype)){
		ncatt_put(ncid,varid=0,attname='IGBP_vegetation_type',attval=vegetationtype)
	}
	ncatt_put(ncid,varid=0,attname='PALS contact',
		attval='palshelp@gmail.com')
  
	# Add variable data to file:
	ncvar_put(ncid, lat, vals=latitude)
	ncvar_put(ncid, lon, vals=longitude)
  
	# Optional meta data for each site:
	if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
	if(!is.na(measurementheight)) {ncvar_put(ncid,refheight,vals=measurementheight)}
	if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
	if(!is.na(utcoffset)) {ncvar_put(ncid,timeoffset,vals=utcoffset)}
	if(!is.na(avprecip)) {ncvar_put(ncid,averageprecip,vals=avprecip)}
	if(!is.na(avtemp)) {ncvar_put(ncid,averagetemp,vals=avtemp)}
  
	# Time dependent variables:
  lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid, 
                                                   varid=var_defs[[x]], 
                                                   vals=indata$data[,x]))
  
    
  ## NEEDS SORTING OUT, NOT SURE WHAT the attribute commands are about !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  	
	#Add original Fluxnet variable name to file
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   att_name="Fluxnet_name", attval=  xxxx))  #complete !!!!!
	
	#Add CF-compliant name to file
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   att_name="CF_name", attval=  xxxx))  #complete !!!!!
	
	
#	ncvar_put(ncid,SWdown,vals=datain$data$SWdown)
#	ncatt_put(ncid,SWdown,attname='CF_name',attval='surface_downwelling_shortwave_flux_in_air')
#	if(! found$PSurf){
#		ncatt_put(ncid,PSurf,'source','Synthesized in PALS based on elevation and temperature')
#	}
#	if(! found$LWdown){
#		ncatt_put(ncid,LWdown,'source',paste('Entirely synthesized in PALS using',defaultLWsynthesis))
#	}else if(found$LWdown & !found$LWdown_all){
#		ncatt_put(ncid,LWdown,'gapfill_technique',paste('Gap-filled in PALS using',defaultLWsynthesis))
#		ncatt_put(ncid,LWdown,'gapfill_note','Fluxdata.org template has no QC flag for LWdown - data are assumed original.')
#	}
#


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
