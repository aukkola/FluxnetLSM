# SpreadsheetToNc.R
#
# A collection of functions to convert flux tower
# data from spreadsheet to netcdf.
#
# Gab Abramowitz UNSW 2016 (palshelp at gmail dot com)
#
# TODO: Check and merge back in to palsR
#

#-----------------------------------------------------------------------------

# TODO: This function exists in palsR/Gab and has a different signature. Merge?
ReadTextFluxData <- function(fileinname, vars, time_vars){
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
	FluxData <- read.csv(file=fileinname, header=TRUE,	colClasses=tcol$classes)  
  

  #Sanity check, does variable order in file match that specified in tcols?
  #Ignore any possible duplicates in tcol$all_names before check
  if(any(colnames(FluxData) != unique(tcol$all_names))) {
    CheckError(paste("Check variable ordering, variables don't match data", 
               "retrieved from file [ function:", match.call()[[1]], "]"))
  }
  

  #Split time variables from other variables
  #Extract time stamp data
  FluxTime <- FluxData[,which(colnames(FluxData)==time_vars)]
  #Remove time stamp variables from Data variable
  FluxData <- FluxData[,-which(colnames(FluxData)==time_vars)]
  
  
  #Duplicate Fluxnet data column if the same variable needs to be
  #processed several times (e.g. RH converted to RH and Qair)
  if(ncol(FluxData) != length(tcol$names))
  {
    
    FluxData <- duplicate_columns(data=FluxData, vars=tcol$names)
    
    #Make sure FluxData now has correct no. of columns
    if(ncol(FluxData) != length(tcol$names)){
      CheckError(paste("Duplicate variable names exist but columns could", 
                 "not be be duplicated correctly [ function:", match.call()[[1]], "]"))
    }
    
  }
  
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
		CheckError(paste('Unable to determine number of time steps in:',
			                stripFilename(fileinname)))
	}
  
	# and time step size:
  start <- strptime(FluxTime$TIMESTAMP_START[1], "%Y%m%d%H%M") #convert to date string
  end   <- strptime(FluxTime$TIMESTAMP_END[1], "%Y%m%d%H%M")
  
	timestepsize <- as.numeric(end) - as.numeric(start)
  
	if( !(timestepsize>=300 && timestepsize<=3600) ){
		CheckError(paste('Unable to ascertain time step size in',
			               stripFilename(fileinname)))
	}
  
	tstepinday=86400/timestepsize # time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set

  
  # Find starting date / time:
  starttime <- findStartTime(start=start)
  
  intyears = Yeardays(starttime$syear,ndays)
  
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


# This function creates a netcdf file for met variables
# TODO: This function exists in palsR/Gab and has a different signature. Merge?
CreateFluxNcFile = function(fluxfilename, datain,                 #outfile file and data
                           latitude, longitude,                   #lat, lon
                           site_code, long_sitename,              #Fluxnet site code and full site name
                           datasetversion, github_rev,            #Dataset version and github revision
                           tier,                                  #Fluxnet site tier
                           ind_start, ind_end,                    #time period indices
                           starttime, timestepsize,               #timing info
                           flux_varname, cf_name,                 #Original Fluxnet variable names and CF_compliant names
                           elevation=NA, towerheight=NA,          #Site elevation and flux tower height
                           canopyheight=NA,                       #Canopy height
                           short_veg_type=NA, long_veg_type=NA){  #Long and short IGBP vegetation types
  
  # load netcdf library
  library(ncdf4) 
  
  
  #Extract time period to be written
  datain$data <- datain$data[ind_start:ind_end,]
  
  # default missing value for all variables
  missing_value=NcMissingVal
  
  # Define x, y and z dimensions
  xd = ncdim_def('x',vals=c(1),units='')	
  yd = ncdim_def('y',vals=c(1),units='')
  zd = ncdim_def('z',vals=c(1),units='')
  
  # Determine data start date and time:
  timeunits = CreateTimeunits(starttime)
  
  # Create time dimension variable:
  tt=c(0:(length(ind_start:ind_end)-1))
  timedata = as.double(tt*timestepsize)
  
  # Define time dimension:
  td = ncdim_def('time', unlim=TRUE, units=timeunits, vals=timedata)
  
  # VARIABLE DEFINITIONS ##############################################
  
  
  
  #Find met variable indices
  var_ind <- which(datain$categories=="Eval")
  
  #Create variable definitions for time series variables
  var_defs <- lapply(var_ind, function(x) ncvar_def(name=datain$vars[x],
                                                    units=datain$units$target_units[x], 
                                                    dim=list(xd,yd,zd,td), 
                                                    missval=missing_value, 
                                                    longname=datain$attributes[x,2]))
  
  
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
  if(!is.na(towerheight)){
    towheight=ncvar_def('tower_height','m',dim=list(xd,yd),
                        missval=missing_value,longname='Height of flux tower')
    opt_vars[[ctr]] = towheight
    ctr <- ctr + 1
  }  
  # Define site canopy height:
  if(!is.na(canopyheight)){
    canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
                        missval=missing_value,longname='Canopy height')
    opt_vars[[ctr]] = canheight
    ctr <- ctr + 1
  }
  #Define site elevation:
  if(!is.na(elevation)){
    elev=ncvar_def('elevation','m',dim=list(xd,yd),
                   missval=missing_value,longname='Site elevation')
    opt_vars[[ctr]] = elev
    ctr <- ctr + 1
  }
  # Define IGBP short vegetation type:
  if(!is.na(short_veg_type)){
    short_veg=ncvar_def('IGBP_veg_short','-',dim=list(xd,yd), missval=NULL,
                        longname='IGBP vegetation type (short)', prec="char")
    opt_vars[[ctr]] = short_veg
    ctr <- ctr + 1
  }
  # Define IGBP long vegetation type:
  if(!is.na(long_veg_type)){
    long_veg=ncvar_def('IGBP_veg_long','-',dim=list(xd,yd), missval=NULL,
                       longname='IGBP vegetation type (long)', prec="char")
    opt_vars[[ctr]] = long_veg
    ctr <- ctr + 1 
  }
  
  
  # END VARIABLE DEFINITIONS #########################################
  
  ### Create netcdf file ###
  if(length(opt_vars)==0) {
    ncid = nc_create(fluxfilename, vars=append(var_defs, c(list(latdim), list(londim))))
  } else {
    ncid = nc_create(fluxfilename, vars=append(var_defs, c(list(latdim), list(londim), opt_vars)))
  }
  
  
  #### Write global attributes ###
  ncatt_put(ncid,varid=0,attname='Production_time',
            attval=as.character(Sys.time()))
  ncatt_put(ncid,varid=0,attname='Github_revision',  
            attval=github_rev, prec="text")
  ncatt_put(ncid,varid=0,attname='site_code',
            attval=site_code, prec="text")
  ncatt_put(ncid,varid=0,attname='site_name',
            attval=as.character(long_sitename), prec="text")
  ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
            attval=datasetversion, prec="text")	  
  ncatt_put(ncid,varid=0,attname='Fluxnet site tier',
            attval=tier)
  ncatt_put(ncid,varid=0,attname='PALS contact',
            attval='palshelp@gmail.com')
  
  # Add variable data to file:
  ncvar_put(ncid, latdim, vals=latitude)
  ncvar_put(ncid, londim, vals=longitude)
  
  
  # Optional meta data for each site:
  if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
  if(!is.na(towerheight)) {ncvar_put(ncid,towheight,vals=towerheight)}
  if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
  if(!is.na(short_veg_type)) {ncvar_put(ncid,short_veg,vals=short_veg_type)}
  if(!is.na(long_veg_type)) {ncvar_put(ncid,long_veg,vals=long_veg_type)}
  
  
  
  # Time dependent variables:
  lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid, 
                                                   varid=var_defs[[x]], 
                                                   vals=datain$data[,var_ind[x]]))
  
  
  #Add original Fluxnet variable name to file
  lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   attname="Fluxnet_name", 
                                                   attval=datain$attributes[var_ind[x],1], 
                                                   prec="text"))  
  
  #Add CF-compliant name to file (if not missing)
  lapply(1:length(var_defs), function(x)  ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                    attname="CF_name", 
                                                    attval=datain$attributes[var_ind[x],3], 
                                                    prec="text"))
  
  
  
  # Close netcdf file:
  nc_close(ncid)
}


#-----------------------------------------------------------------------------

# This function creates a netcdf file for met variables
# TODO: This function exists in palsR/Gab and has a different signature. Merge?
CreateMetNcFile = function(metfilename, datain,                   #outfile file and data
                           latitude, longitude,                   #lat, lon
                           site_code, long_sitename,              #Fluxnet site code and full site name
                           datasetversion, github_rev,            #Dataset version and github revision
                           tier,                                  #Fluxnet site tier
                           ind_start, ind_end,                    #time period indices
                           starttime, timestepsize,               #timing info
                           flux_varname, cf_name,                 #Original Fluxnet variable names and CF_compliant names
                           elevation=NA, towerheight=NA,          #Site elevation and flux tower height
                           canopyheight=NA,                       #Canopy height
                           short_veg_type=NA, long_veg_type=NA){  #Long and short IGBP vegetation types
  
  # load netcdf library
	library(ncdf4) 
  

  #Extract time period to be written
  datain$data <- datain$data[ind_start:ind_end,]
  
	# default missing value for all variables
	missing_value=NcMissingVal
  
	# Define x, y and z dimensions
	xd = ncdim_def('x',vals=c(1),units='')	
	yd = ncdim_def('y',vals=c(1),units='')
	zd = ncdim_def('z',vals=c(1),units='')
  
	# Determine data start date and time:
	timeunits = CreateTimeunits(starttime)
  
	# Create time dimension variable:
	tt=c(0:(length(ind_start:ind_end)-1))
	timedata = as.double(tt*timestepsize)
  
	# Define time dimension:
	td = ncdim_def('time', unlim=TRUE, units=timeunits, vals=timedata)
  
	# VARIABLE DEFINITIONS ##############################################


  
  #Find met variable indices
  var_ind <- which(datain$categories=="Met")
  
  
  #Create variable definitions for time series variables
  var_defs <- lapply(var_ind, function(x) ncvar_def(name=datain$vars[x],
                                                    units=datain$units$target_units[x], 
                                                    dim=list(xd,yd,zd,td), 
                                                    missval=missing_value, 
                                                    longname=datain$attributes[x,2]))
  
  
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
	if(!is.na(towerheight)){
	  towheight=ncvar_def('tower_height','m',dim=list(xd,yd),
	                      missval=missing_value,longname='Height of flux tower')
	  opt_vars[[ctr]] = towheight
    ctr <- ctr + 1
	}  
  # Define site canopy height:
	if(!is.na(canopyheight)){
	  canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
	                      missval=missing_value,longname='Canopy height')
	  opt_vars[[ctr]] = canheight
	  ctr <- ctr + 1
	}
  #Define site elevation:
	if(!is.na(elevation)){
	  elev=ncvar_def('elevation','m',dim=list(xd,yd),
	                      missval=missing_value,longname='Site elevation')
	  opt_vars[[ctr]] = elev
	  ctr <- ctr + 1
	}
	# Define IGBP short vegetation type:
	if(!is.na(short_veg_type)){
	  short_veg=ncvar_def('IGBP_veg_short','-',dim=list(xd,yd), missval=NULL,
                        longname='IGBP vegetation type (short)', prec="char")
	  opt_vars[[ctr]] = short_veg
	  ctr <- ctr + 1
	}
	# Define IGBP long vegetation type:
	if(!is.na(long_veg_type)){
	  long_veg=ncvar_def('IGBP_veg_long','-',dim=list(xd,yd), missval=NULL,
                       longname='IGBP vegetation type (long)', prec="char")
	  opt_vars[[ctr]] = long_veg
	  ctr <- ctr + 1 
	}
	

	# END VARIABLE DEFINITIONS #########################################
  
  ### Create netcdf file ###
	if(length(opt_vars)==0) {
    ncid = nc_create(metfilename, vars=append(var_defs, c(list(latdim), list(londim))))
	} else {
	  ncid = nc_create(metfilename, vars=append(var_defs, c(list(latdim), list(londim), opt_vars)))
	}
  
  
	#### Write global attributes ###
  ncatt_put(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	ncatt_put(ncid,varid=0,attname='Github_revision',  
		attval=github_rev, prec="text")
	ncatt_put(ncid,varid=0,attname='site_code',
		attval=site_code, prec="text")
  ncatt_put(ncid,varid=0,attname='site_name',
          attval=as.character(long_sitename), prec="text")
  ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
		attval=datasetversion, prec="text")	  
  ncatt_put(ncid,varid=0,attname='Fluxnet site tier',
          attval=tier)
	ncatt_put(ncid,varid=0,attname='PALS contact',
		attval='palshelp@gmail.com')
  
	# Add variable data to file:
	ncvar_put(ncid, latdim, vals=latitude)
	ncvar_put(ncid, londim, vals=longitude)

  
	# Optional meta data for each site:
	if(!is.na(elevation)) {ncvar_put(ncid,elev,vals=elevation)}
	if(!is.na(towerheight)) {ncvar_put(ncid,towheight,vals=towerheight)}
  if(!is.na(canopyheight)) {ncvar_put(ncid,canheight,vals=canopyheight)}
	if(!is.na(short_veg_type)) {ncvar_put(ncid,short_veg,vals=short_veg_type)}
  if(!is.na(long_veg_type)) {ncvar_put(ncid,long_veg,vals=long_veg_type)}


 
	# Time dependent variables:
  lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid, 
                                                   varid=var_defs[[x]], 
                                                   vals=datain$data[,var_ind[x]]))
  
      	
	#Add original Fluxnet variable name to file
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], attname="Fluxnet_name", 
                                                   attval=datain$attributes[var_ind[x],1], prec="text"))  
	
	#Add CF-compliant name to file (if not missing)
	lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], 
                                                   attname="CF_name", 
                                                   attval=datain$attributes[var_ind[x],3], 
                                                   prec="text"))
	
	

	# Close netcdf file:
	nc_close(ncid)
}

