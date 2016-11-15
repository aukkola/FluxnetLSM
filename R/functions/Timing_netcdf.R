# Timing_netcdf.R
#
# Functions that help to read / write timing variables in netcdf files.
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
GetTimingNcfile = function(fid){
	# This function gets the time step size, number of timesteps
	# and start date and time details from a netcdf file.
	errtext='ok'
	# Get the name of the time variable in this file
	timevar = FindTimeVarName(fid)
	if(timevar$err){ # Report fatal error
		timing=list(errtext=timevar$errtext,err=TRUE)
		return(timing)
	}
	# Get time units:
	tunits = GetTimeUnits(fid,timevar$name)
	if(tunits$err){ # Report fatal error
		timing=list(errtext=tunits$errtext,err=TRUE)
		return(timing)
	}
	# Get number of time steps:
	ntsteps = GetNumberTimesteps(fid,timevar)
	
	# Get the time step size:
	tstep = GetTimestepSize(fid,timevar$name,tunits,ntsteps)
	if(tstep$err){ # Report fatal error
		timing=list(errtext=tstep$errtext,err=TRUE)
		return(timing)
	}
		
	# Create return list:
	timing = list(err=FALSE,errtext=errtext,tstepsize=tstep$size,tsteps=ntsteps,
		syear=tunits$syear,smonth=tunits$smonth,sdoy=tunits$sdoy,whole=tstep$wholeyear,interval=tstep$interval)
	return(timing)
}

FindTimeVarName = function(fid){
	# Finds the name of the time variable in an open netcdf file.
	errtext='ok' 
	exists = FALSE # initialise
	nvars = length(fid$var) # number of variables in netcdf file
	ndims = length(fid$dim) # number of dimensions in netcdf file
	for (v in 1:nvars){ # Search through all variables in netcdf file
		if((substr(fid$var[[v]]$name,1,6)=='t_ave_') | # i.e. ORCHIDEE file
			(substr(fid$var[[v]]$name,1,5)=='mscur') | 
			(fid$var[[v]]$name == 'time')){
			# i.e. 	ORCHIDEE file, CLM file or non-dimension variable named time, respectively
			exists = TRUE
			dimvar = FALSE # i.e. time variable is not a dimension variable
			timevarname = fid$var[[v]]$name
			timedimid = fid$var[[v]]$dimids
			break # leave for loop for variables
		}
	}
	if(!exists){ # i.e. none of the above time variables were found
		# Search for time as a dimension variable:
		for (d in 1:ndims){ # Search through all dimensions in netcdf file
			if(fid$dim[[d]]$name=='time' | fid$dim[[d]]$name=='t' |
				fid$dim[[d]]$name=='time_counter' | fid$dim[[d]]$name=='Time'){
				# Now check for time dimension variable:
				if(fid$dim[[d]]$dimvarid$id != -1){ # i.e. dim var exists	
					exists = TRUE # i.e. found time dimension variable
					dimvar = TRUE # time variable is a dimension variable
					timevarname = fid$dim[[d]]$name
					timedimid = fid$dim[[d]]$id
				}else{ # i.e. time dim exists but no dim var
					errtext = paste('T1: Cannot interpret timing in ',stripFilename(fid$filename),
						': time dimension exists but no dimension variable.',sep='')
					timevar = list(err=TRUE,errtext=errtext)
					return(timevar)
				}
				break	
			}
		}
		if(!exists){ # Still cannot identify time variable
			# Return to parent function with error:
			errtext = paste('T1: Unable to ascertain name of time variable in', stripFilename(fid$filename))
			timevar = list(err=TRUE,errtext=errtext)
			return(timevar)
		}
	}
	# Return result:
	timevar = list(err=FALSE, errtext=errtext,name=timevarname,dimid=timedimid,dimvar=dimvar)
	return(timevar)
}

GetTimeUnits = function(fid,timevarname){
	# Fetches and processes time units from a netcdf file.
	errtext = 'ok'
	if(substr(timevarname,1,5)=='mscur'){ # i.e. CLM file
		# Read date variable:
		date1=as.character(ncvar_get(fid,'mcdate',start=1,count=1))
		syear = as.numeric(substr(date1,1,4))
		smonth = as.numeric(substr(date1,5,6))
		sdoy = as.numeric(substr(date1,7,8))
		units = 'clm_blah'
	}else{
		units = ncatt_get(fid,timevarname,'units')
		if(! units$hasatt){
			errtext = paste('T1: Unable to find time units in', stripFilename(fid$filename))
			tunits = list(err=TRUE,errtext=errtext)
			return(tunits)	
		}
		if(substr(units$value,1,4)=='seco'){ # time units are seconds
			syear = as.numeric(substr(units$value,15,18))
			smonth = as.numeric(substr(units$value,20,21))
			sdoy = as.numeric(substr(units$value,23,24))
			units = 'seconds'
		}else if(substr(units$value,1,4)=='days'){ # time units are days
			syear = as.numeric(substr(units$value,11,14))
			smonth = as.numeric(substr(units$value,16,17))
			sdoy = as.numeric(substr(units$value,19,20))
			units = 'days'
		}else{
			errtext = paste('T1: Unable to interpret time units in', stripFilename(fid$filename))
			tunits = list(err=TRUE,errtext=errtext)
			return(tunits)	
		}
	}
	tunits = list(err=FALSE,errtext=errtext,syear=syear,smonth=smonth,sdoy=sdoy,units=units)
	return(tunits)	
}

GetTimestepSize = function(fid,timevarname,tunits,ntsteps){
	# Fetches time step size
	errtext = 'ok'
	# Read first 2 timesteps of time variable:
	time=ncvar_get(fid,timevarname,start=1,count=2)
	time_end=ncvar_get(fid,timevarname,start=ntsteps,count=1)
	# Define time step size:
	tsize=time[2]-time[1]
	tperiod=time_end - time[1] + tsize
	
	if(tunits$units == 'days'){
		if((tsize >359) && (tsize < 367)){
			interval = 'annual'
			wholeyear = TRUE
		}else if((tsize>27) && (tsize<32)){
			interval = 'monthly'
			if((ntsteps %% 12) == 0){
				wholeyear = TRUE	
			}else{
				wholesyear==FALSE
			}	
		}else if(tsize==1){
			interval = 'daily'
			intyear = Yeardays(tunits$syear,tperiod)
			wholeyear = intyear$whole	
		}else{
			errtext = paste('T1: Unable to interpret time step size in', stripFilename(fid$filename))
			tstep = list(err=TRUE,errtext=errtext)
			return(tstep)	
		}
	}else if(tunits$units == 'seconds'){
		if(tsize <= (3600*3)){ # i.e. less than 3-hourly
			interval = 'timestep'
			tstepinday=86400/tsize # time steps in a day
			ndays = ntsteps/tstepinday # number of days in file
			intyear = Yeardays(tunits$syear,ndays)
			wholeyear = intyear$whole
		}else if(tsize == (3600*24)){
			interval = 'daily'
			intyear = Yeardays(tunits$syear,(tperiod/3600/24))
			wholeyear = intyear$whole
		}else if( (tsize > (27*24*3600)) && (tsize < (32*24*3600)) ){
			interval = 'monthly'
			if((ntsteps %% 12) == 0){
				wholeyear = TRUE	
			}else{
				wholesyear==FALSE
			}	
		}else if( (tsize > (359*24*3600)) && (tsize < (367*24*3600)) ){
			interval = 'annual'
			wholeyear = TRUE
		}else{
			errtext = paste('T1: Unable to interpret time step size in', stripFilename(fid$filename))
			tstep = list(err=TRUE,errtext=errtext)
			return(tstep)	
		}
		
		
		
	}else{
		errtext = paste('T1: Unable to interpret time units in', stripFilename(fid$filename))
		tstep = list(err=TRUE,errtext=errtext)
		return(tstep)
	}
	tstep = list(err=FALSE,errtext=errtext,size=tsize,interval=interval,wholeyear=wholeyear)
	return(tstep)
}
			
GetNumberTimesteps = function(fid,timevar){		
	# Gets the number of time steps in a netcdf file
	ndims = length(fid$dim)
	# Find out how many time steps there are in the model file:
	for (d in 1:ndims){ # Search through all dimensions in netcdf file
		if(fid$dim[[d]]$id == timevar$dimid){ # i.e. this is the dim of the time variable
			ntsteps = fid$dim[[d]]$len
			break # stop searching for unlim dim
		}
	}
	return(ntsteps)
}

CheckTiming = function(timing1,timing2,benchmark_timing=FALSE){
	# Simply checks whether model and obs (and maybe benchmark) 
	# time step size and number of time steps are compatible.
	errtext = 'ok'
	err = FALSE
	# First check intervals are the same:
	if(timing1$interval != timing2$interval){
		if(benchmark_timing){
			errtext = paste('Time interval differs between',
				'observed data set and benchmark time series:',
				timing1$interval, timing2$interval)
			err=TRUE
		}else{
			errtext = paste('Time interval differs between',
				'observed data set and model output:',
				timing1$interval, timing2$interval)
			err=TRUE
		}
	}
	if(timing1$interval=='timestep'){
		if(timing1$tstepsize != timing2$tstepsize){
			if(benchmark_timing){
				errtext = paste('Time step size differs between',
					'observed data set and benchmark time series:',
					timing1$tstepsize, timing2$tstepsize)
				err=TRUE
			}else{
				errtext = paste('Time step size differs between',
					'observed data set and model output:',
					timing1$tstepsize, timing2$tstepsize)
				err=TRUE
			}
		}
	}
	if(timing1$tsteps != timing2$tsteps){
		if(benchmark_timing){
			errtext = paste('Number of time steps differs between',
				'observed data set and benchmark time series:',
				timing1$tsteps, timing2$tsteps)
			err=TRUE
		}else{
			errtext = paste('Number of time steps differs between',
				'observed data set and model output:',
				timing1$tsteps, timing2$tsteps)
			err=TRUE
		}
	}
	result = list(err=err,errtext=errtext)
	return(result)
}
CreateTimeunits = function(starttime) {
	# Determine data start date and time:
	shour = floor(starttime$shod)
	smin = floor((starttime$shod - shour)*60)
	ssec = floor(((starttime$shod - shour)*60 - smin)*60)
	start_hod = paste(Create2Uchar(shour),':',Create2Uchar(smin),':',Create2Uchar(ssec),sep='')
	timeunits=paste('seconds since ',as.character(starttime$syear),'-',
		Create2Uchar(starttime$smonth),'-',Create2Uchar(starttime$sday),' ',
		start_hod,sep='')
	return(timeunits)
}