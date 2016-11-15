# SynthesizeGapfill.R
#
# Functions for synthesizing or gapfilling timeseries.
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)

#-----------------------------------------------------------------------------

SynthesizeLWdown=function(TairK,RH,technique){
	if(technique=='Swinbank (1963)'){
		# Synthesise LW down from air temperature only:
		lwdown = 0.0000094*0.0000000567*TairK^6
	}else if(technique=='Brutsaert (1975)'){
		satvapres = 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
		vapres = pmax(5,RH)/100*satvapres
		emiss = 0.642*(vapres/TairK)^(1/7)
		lwdown = emiss*0.0000000567*TairK^4
	}else if(technique=='Abramowitz (2012)'){
		satvapres = 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
		vapres = pmax(5,RH)/100*satvapres
		lwdown = 2.648*TairK + 0.0346*vapres - 474
	}else{
		CheckError('S4: Unknown requested LWdown synthesis technique.')
	}
	return(lwdown)
}

#-----------------------------------------------------------------------------

SynthesizePSurf=function(TairK,elevation){
	# Synthesizes PSurf based on temperature and elevation
	PSurf = 101325 * (TairK / (TairK + 0.0065*elevation))^(9.80665/287.04/0.0065)
	return(PSurf)
}

#-----------------------------------------------------------------------------

############### NEED TO CHECK FLAGS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gapfillLWdown = function(LWdownIN,TairK,RH,technique){
	# Fills any gaps in LWdown time series using synthesis:
	LWdownOUT = c() # initialise
	LWflag = c() # initialise
	for(t in 1:length(LWdownIN)){
		if(LWdownIN[t]==SprdMissingVal){
			LWdownOUT[t] = SynthesizeLWdown(TairK[t],RH[t],technique)
			LWflag[t] = 3  
		}else{
			LWdownOUT[t] = LWdownIN[t]
			LWflag[t] = 0
		}
	}
	return(list(data=LWdownOUT,flag=LWflag))
}

#-----------------------------------------------------------------------------

######################################################
# Below are functions previously used for gapfilling Fluxnet formatted spreadhseets
# Does linear gap filling between met data and flux:
regressionfill = function(PALSt,varname,templateVersion,starttime,
	regThreshold=4,winsize=8760){
	tsteps = length(PALSt[,1]) - 3
	# Get index of variable in question:
	vidx = varIndex(varname,templateVersion)
	ingap = FALSE
	ctr = 0
	for(t in 4:(tsteps+3)){
		if(PALSt[t,vidx] == SprdMissingVal){ # i.e. missing data
			ingap = TRUE
			ctr = ctr + 1
			if(t==(tsteps+3)){ # i.e. data missing from last timestep
				if(ctr<=regThreshold){ # i.e. there are very few missing timesteps
					# Just make last few missing timesteps equal to last real value:
					PALSt[(t-ctr):t,vidx] = PALSt[(t-ctr-1),vidx]
					PALSt[(t-ctr):t,(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
						nowstart$year,'through to',nowend$time,'hr,',nowend$day,
						month.abb[nowend$month],nowend$year,':\n')
					cat('Filled timestep',t-ctr,'to',t,'(',ctr,'in total) of',varname,
						'with previous non-missing value:',PALSt[(t-ctr-1),vidx],' \n')
				}else{
					# Get regression parameters based on previous section of timeseries:
					rtrain = regtrain(PALSt,vidx,(t-ctr),t,'previous',templateVersion,winsize)
					# Use these to empirically gap-fill missing section:
					regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),t,templateVersion)
					PALSt[(t-ctr):t,vidx] = regresult
					PALSt[(t-ctr):t,(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
						nowstart$year,'through to',nowend$time,'hr,',nowend$day,
						month.abb[nowend$month],nowend$year,':\n')
					cat('Filled timestep',t-ctr,'to',t,'(',ctr,'in total) of',varname,
						'with regression based on met drivers. \n')
				}
			}
		}else{ # not missing data
			if(ingap){ # i.e. 1st timestep after gap
				if(ctr <= regThreshold){ # i.e. there are very few missing timesteps
					# Just use a linear fit between surrounding timesteps
					if((t-ctr) == 4){ # i.e. the first time step was missing
						# Just make first few missing timesteps equal first real value:
						PALSt[(t-ctr):(t-1),vidx] = PALSt[t,vidx]
						PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
						nowstart = dateFromTstep(starttime,t-ctr)
						nowend = dateFromTstep(starttime,t-1)
						cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
							nowstart$year,'through to',nowend$time,'hr,',nowend$day,
							month.abb[nowend$month],nowend$year,':\n')
						cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
							'with first non-missing value:',PALSt[t,vidx],' \n')
					}else{ # there is a variable value before and after gap 
						# Fill gap with linear approximation:
						fill_values = as.single(PALSt[(t-ctr-1),vidx]) + c(1:ctr) *
							(as.single(PALSt[t,vidx])-as.single(PALSt[(t-ctr-1),vidx]))/(ctr+1)
						PALSt[(t-ctr):(t-1),vidx] = fill_values
						# Note in variable flag that this is crude gap-filling:
						PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
						nowstart = dateFromTstep(starttime,t-ctr)
						nowend = dateFromTstep(starttime,t-1)
						cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
							nowstart$year,'through to',nowend$time,'hr,',nowend$day,
							month.abb[nowend$month],nowend$year,':\n')
						cat('Filled timestep',t-ctr,'to',t-1 ,'(',ctr,
							'in total) of',varname,'with linear map of surrounding timesteps. \n')
					}	
				}else if((t-ctr) <  ceiling(winsize/2)){ # Gap close to the start of the timeseries:
					# Get regression parameters based on previous section of timeseries:
					rtrain = regtrain(PALSt,vidx,(t-ctr),(t-1),'next',templateVersion,winsize)
					# Use these to empirically gap-fill missing section:
					regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),(t-1),templateVersion)
					PALSt[(t-ctr):(t-1),vidx] = regresult
					PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t-1)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],
						nowstart$year,'through to',nowend$time,'hr,',nowend$day,
						month.abb[nowend$month],nowend$year,':\n')
					cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
						'with regression based on met drivers. \n')
				}else if((t + ceiling(winsize/2)) > tsteps){# Gap close to the end of the timeseries:
					# Get regression parameters based on previous section of timeseries:
					rtrain = regtrain(PALSt,vidx,(t-ctr),(t-1),'previous',templateVersion,winsize)
					# Use these to empirically gap-fill missing section:
					regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),(t-1),templateVersion)
					PALSt[(t-ctr):(t-1),vidx] = regresult
					PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t-1)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
						'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
					cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
						'with regression based on met drivers. \n')
				}else{ # there is space to train with data before and after gap
					# Get regression parameters based on previous section of timeseries:
					rtrain = regtrain(PALSt,vidx,(t-ctr),(t-1),'around',templateVersion,winsize)
					# Use these to empirically gap-fill missing section:
					regresult = regpredict(rtrain,PALSt,vidx,(t-ctr),(t-1),templateVersion)
					PALSt[(t-ctr):(t-1),vidx] = regresult
					PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t-1)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
						'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
					cat('Filled timestep',t-ctr,'to',t-1,'(',ctr,'in total) of',varname,
						'with regression based on met drivers. \n')
				}
				# Reset gap timestep counter:
				ingap = FALSE
				ctr = 0
			}
		}		
	}
	return(PALSt)	
}

#-----------------------------------------------------------------------------

# Does linear gap filling between endpoints:
linearfill = function(PALSt,varname,templateVersion,starttime){
	tsteps = length(PALSt[,1]) - 3
	# Get index of variable in question:
	vidx = varIndex(varname,templateVersion)
	ingap = FALSE
	ctr = 0
	for(t in 4:(tsteps+3)){
		if(PALSt[t,vidx] == SprdMissingVal){ # i.e. missing data
			ingap = TRUE
			ctr = ctr + 1
			if(t==(tsteps+3)){ # i.e. data missing from last timestep
				# Just make last few missing timesteps equal to last real value:
				PALSt[(t-ctr):t,vidx] = PALSt[(t-ctr-1),vidx]
				PALSt[(t-ctr):t,(vidx+1)] = 0 # following fluxdata.org
				nowstart = dateFromTstep(starttime,t-ctr)
				nowend = dateFromTstep(starttime,t)
				cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
					'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
				cat('Filled last',ctr,'timesteps of',varname,
					'with previous non-missing value:',PALSt[(t-ctr-1),vidx],' \n')
			}
		}else{
			if(ingap){ # i.e. 1st timestep after gap
				if((t-ctr) == 4){ # i.e. the first time step was missing
					# Just make first few missing timesteps equal first real value:
					PALSt[(t-ctr):(t-1),vidx] = PALSt[t,vidx]
					PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t-1)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
						'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
					cat('Filled first',ctr,'timesteps of',varname,
						'with first non-missing value:',PALSt[t,vidx],' \n')
				}else{ # there is a variable value before and after gap 
					# Fill gap with linear approximation:
					fill_values = as.single(PALSt[(t-ctr-1),vidx]) + c(1:ctr) *
						(as.single(PALSt[t,vidx])-as.single(PALSt[(t-ctr-1),vidx]))/(ctr+1)
					PALSt[(t-ctr):(t-1),vidx] = fill_values
					# Note in variable flag that this is crude gap-filling:
					PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t-1)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
						'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
					cat('Filled timestep',t-ctr,'to',t-1 ,'(',ctr,
						'in total) of',varname,'with linear map of surrounding timesteps. \n')
				}
				# Reset gap timestep counter:
				ingap = FALSE
				ctr = 0
			}
		}		
	}
	return(PALSt)	
}

#-----------------------------------------------------------------------------

# Copies previous / future time period for gap filling:
copyfill = function(PALSt,varname,templateVersion,starttime){
	tsteps = length(PALSt[,1]) - 3
	# Get index of variable in question:
	vidx = varIndex(varname,templateVersion)
	ingap = FALSE
	ctr = 0
	for(t in 4:(tsteps+3)){
		if(PALSt[t,vidx] == SprdMissingVal){ # i.e. missing data
			ingap = TRUE
			ctr = ctr + 1
			if(t==(tsteps+3)){ # i.e. data missing from last timestep
				# Make last few missing timesteps equal to last real value period in diurnal cycle:
				PALSt[(t-ctr):t,vidx] = 
					PALSt[((t-ctr) - ceiling(ctr/48)*48) : (t - ceiling(ctr/48)*48),vidx]
				PALSt[(t-ctr):t,(vidx+1)] = 0 # following fluxdata.org
				nowstart = dateFromTstep(starttime,t-ctr)
				nowend = dateFromTstep(starttime,t)
				cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
					'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
				cat('Filled last',ctr,'timesteps of',varname,
						'with previous non-missing values at same point in diurnal cycle. \n')
			}
		}else{
			if(ingap){ # i.e. 1st timestep after gap
				if((t-ctr) == 4){ # i.e. the first time step was missing
					# Make first few missing timesteps equal first real value in similar diurnal cycle:
					PALSt[(t-ctr):(t-1),vidx] = 
						PALSt[((t-ctr) + ceiling(ctr/48)*48) : (t - 1 + ceiling(ctr/48)*48),vidx]
					PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
					nowstart = dateFromTstep(starttime,t-ctr)
					nowend = dateFromTstep(starttime,t-1)
					cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
						'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
					cat('Filled first',ctr,'timesteps of',varname,
						'with next non-missing values at same point in diurnal cycle. \n')
				}else{ # there is a variable value before and after gap
					if(t<tsteps/2){
						# Make missing timesteps equal next real value in similar diurnal cycle:
						PALSt[(t-ctr):(t-1),vidx] = 
							PALSt[((t-ctr) + ceiling(ctr/48)*48) : (t - 1 + ceiling(ctr/48)*48),vidx]
						nowstart = dateFromTstep(starttime,t-ctr)
						nowend = dateFromTstep(starttime,t-1)
						cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
							'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
						cat('Filled timestep',t-ctr,'to',t-1 ,'(',ctr,'in total) of',varname,
							'with next non-missing values at same point in diurnal cycle. \n')
					}else{
						# Make missing timesteps equal previous real value in similar diurnal cycle:
						PALSt[(t-ctr):(t-1),vidx] = 
							PALSt[((t-ctr) - ceiling(ctr/48)*48) : (t - 1 - ceiling(ctr/48)*48),vidx]
						nowstart = dateFromTstep(starttime,t-ctr)
						nowend = dateFromTstep(starttime,t-1)
						cat(nowstart$time,'hr,',nowstart$day,month.abb[nowstart$month],nowstart$year,
							'through to',nowend$time,'hr,',nowend$day,month.abb[nowend$month],nowend$year,':\n')
						cat('Filled timestep',t-ctr,'to',t-1 ,'(',ctr,'in total) of',varname,
							'with previous non-missing values at same point in diurnal cycle. \n')
					}
					# Note in variable flag that this is crude gap-filling:
					PALSt[(t-ctr):(t-1),(vidx+1)] = 0 # following fluxdata.org
				}
				# Reset gap timestep counter:
				ingap = FALSE
				ctr = 0
			}
		}		
	}
	return(PALSt)	
}

#-----------------------------------------------------------------------------

# Replaces negative values with 0:
posfill = function(PALSt,varname,templateVersion,starttime){
	tsteps = length(PALSt[,1]) - 3
	# Get index of variable in question:
	vidx = varIndex(varname,templateVersion)
	for(t in 4:(tsteps+3)){
		if(as.double(PALSt[t,vidx]) < 0){
			nowt = dateFromTstep(starttime,t)
			cat(nowt$time,'hr,',nowt$day,month.abb[nowt$month],nowt$year,
				': replaced',varname,'value:',PALSt[t,vidx],'with 0. \n')
			PALSt[t,vidx] = 0
		}
	}
	return(PALSt)	
}

#-----------------------------------------------------------------------------

# Replaces values above a cap value with that value:
capfill = function(PALSt,varname,templateVersion,starttime,capvalue){
	tsteps = length(PALSt[,1]) - 3
	# Get index of variable in question:
	vidx = varIndex(varname,templateVersion)
	for(t in 4:(tsteps+3)){
		if(as.double(PALSt[t,vidx]) > capvalue){
			nowt = dateFromTstep(starttime,t)
			cat(nowt$time,'hr,',nowt$day,month.abb[nowt$month],nowt$year,
				': replaced',varname,'value:',PALSt[t,vidx],'with',capvalue,'. \n')
			PALSt[t,vidx] = capvalue
		}
	}
	return(PALSt)	
}

#-----------------------------------------------------------------------------

# Replaces qc -9999 values with 1 => data okay:
qcfill = function(PALSt,varname,templateVersion,starttime){
	tsteps = length(PALSt[,1]) - 3
	# Get index of variable in question:
	vidx = varIndex(varname,templateVersion)
	for(t in 4:(tsteps+3)){
		if(PALSt[t,vidx] == SprdMissingVal){
			nowt = dateFromTstep(starttime,t)
			cat(nowt$time,'hr,',nowt$day,month.abb[nowt$month],nowt$year,
				': replaced',varname,'value:',PALSt[t,vidx],'with 1. \n')
			PALSt[t,vidx] = 1
		}
	}
	return(PALSt)	
}

#-----------------------------------------------------------------------------

# Train multiple linear regression:
regtrain = function(PALSt,vidx,startT,endT,sourceDirection,templateVersion,winsize=3000){
	swidx = varIndex('SWdown',templateVersion)
	tidx = varIndex('Tair',templateVersion)
	hidx = varIndex('Qair',templateVersion)
	if(sourceDirection=='previous'){
		trainStart = startT - winsize
		trainEnd = startT - 1
		trainY = PALSt[trainStart:trainEnd,vidx]
		trainX = matrix(NA,(trainEnd-trainStart+1),3)
		trainX[,1] = PALSt[trainStart:trainEnd,swidx]
		trainX[,2] = PALSt[trainStart:trainEnd,tidx]
		trainX[,3] = PALSt[trainStart:trainEnd,hidx]
	}else if(sourceDirection=='next'){
		trainStart = endT + 1
		trainEnd = endT + winsize
		trainY = PALSt[trainStart:trainEnd,vidx]
		trainX = matrix(NA,(trainEnd-trainStart+1),3)
		trainX[,1] = PALSt[trainStart:trainEnd,swidx]
		trainX[,2] = PALSt[trainStart:trainEnd,tidx]
		trainX[,3] = PALSt[trainStart:trainEnd,hidx]
	}else if(sourceDirection=='around'){
		trainStartpre = startT - ceiling(winsize/2)
		trainEndpre = startT - 1
		trainStartpost = endT + 1
		trainEndpost = endT + ceiling(winsize/2)
		trainY = c()
		trainY[1:ceiling(winsize/2)] = PALSt[trainStartpre:trainEndpre,vidx]
		trainY[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2)] = 
			PALSt[trainStartpost:trainEndpost,vidx]
		trainX = matrix(NA,(ceiling(winsize/2)*2),3)
		trainX[1:ceiling(winsize/2),1] = PALSt[trainStartpre:trainEndpre,swidx]
		trainX[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2),1] = 
			PALSt[trainStartpost:trainEndpost,swidx]
		trainX[1:ceiling(winsize/2),2] = PALSt[trainStartpre:trainEndpre,tidx]
		trainX[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2),2] = 
			PALSt[trainStartpost:trainEndpost,tidx]
		trainX[1:ceiling(winsize/2),3] = PALSt[trainStartpre:trainEndpre,hidx]
		trainX[(ceiling(winsize/2)+1):(ceiling(winsize/2)*2),3] = 
			PALSt[trainStartpost:trainEndpost,hidx]
	}else{
		stop('Unknown sourceDirection [regtrain]')
	}
	# Separate day and night:
	dayn = DayNight(as.double(trainX[,1]))
	trainYday = as.double(trainY[dayn])
	trainXday1 = as.double(trainX[dayn,1])
	trainXday2 = as.double(trainX[dayn,2])
	trainXday3 = as.double(trainX[dayn,3])
	trainYnight = as.double(trainY[!dayn])
	trainXnight1 = as.double(trainX[!dayn,1])
	trainXnight2 = as.double(trainX[!dayn,2])
	trainXnight3 = as.double(trainX[!dayn,3])
	# Convert missing values to NAs so regression copes:
	for(t in 1:length(trainYday)){
		if(trainYday[t] == SprdMissingVal){trainYday[t] = NA}
		if(trainXday1[t] == SprdMissingVal){trainXday1[t] = NA}
		if(trainXday2[t] == SprdMissingVal){trainXday2[t] = NA}
		if(trainXday3[t] == SprdMissingVal){trainXday3[t] = NA}
	}
	for(t in 1:length(trainYnight)){
		if(trainYnight[t] == SprdMissingVal){trainYnight[t] = NA}
		if(trainXnight1[t] == SprdMissingVal){trainXnight1[t] = NA}
		if(trainXnight2[t] == SprdMissingVal){trainXnight2[t] = NA}
		if(trainXnight3[t] == SprdMissingVal){trainXnight3[t] = NA}
	}
	
	# Train regression parameters:
	rgrp_day = lm(trainYday ~ trainXday1 + trainXday2 + 
		trainXday3,na.action=na.omit)
	rgrp_night = lm(trainYnight ~ trainXnight1 + 
		trainXnight2 + trainXnight3,na.action=na.omit)
	rgrp = list(day = rgrp_day, night = rgrp_night)
	return(rgrp)
}

#-----------------------------------------------------------------------------

# Use multiple linear regression parameters to predict time series:
regpredict = function(rgrp,PALSt,vidx,startT,endT,templateVersion){
	ntsteps = endT - startT + 1
	# Get indices of predictor variables in matrix:
	swidx = varIndex('SWdown',templateVersion)
	tidx = varIndex('Tair',templateVersion)
	hidx = varIndex('Qair',templateVersion)	
	testX = matrix(NA,ntsteps,3)
	# Write variables to predictor matrix:
	testX[,1] = as.double(PALSt[startT:endT,swidx])
	testX[,2] = as.double(PALSt[startT:endT,tidx])
	testX[,3] = as.double(PALSt[startT:endT,hidx])
	# Separate day and night:
	dayn = DayNight(as.double(testX[,1]))
	
	empflux = c()
	# Use existing parameters to make empirical prediction:
	daycoefs = coef(rgrp$day)
	nightcoefs = coef(rgrp$night)
	for(t in 1:ntsteps){
		if(dayn[t]){ # i.e. daytime time step
			empflux[t] = sum(daycoefs[2:4]*testX[t,]) + daycoefs[1]
		}else{
			empflux[t] = sum(nightcoefs[2:4]*testX[t,]) + nightcoefs[1]
		}
	}
	return(empflux)
}

