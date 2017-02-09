# AnalysisDiurnalCycle.R
# This function will plot the average diurnal cycle of a variable
# from netcdf model output and observed data, with one panel per
# season, over an entire, integer-year single-site data set. 
# Dataset **MUST START AT JAN 1**
#
# Gab Abramowitz CCRC, UNSW 2014 (palshelp at gmail dot com)
#
DiurnalCycle = function(obslabel,dcdata,varname,ytext,legendtext,
	timestepsize,whole,plotcolours,modlabel='no',vqcdata=matrix(-1,nrow=1,ncol=1)){
	errtext = 'ok'
	metrics = list()
	if(!whole){ # we need a whole number of years for this to run
		errtext = 'DS3: DiurnalCycle analysis requires a whole number of years of data.'
		result = list(errtext=errtext)
		return(result)
	}
	labels=c('DJF','MAM','JJA','SON') # for each season
	stid=c(1,60,152,244,335) # seasonal divisions in a year
	fnid=c(59,151,243,334,365) # seasonal divisions in a year
	ncurves = length(dcdata[1,]) # Number of curves in final plot:
	ntsteps = length(dcdata[,1]) # Number of timesteps in data:
	tstepinday=86400/timestepsize # number of time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	nyears=as.integer(ndays/365) # find # years in data set
	# Plot layout:
	par(mfcol=c(2,2),mar=c(4,4,3,0.5),oma=c(0,0,0,1),
		mgp=c(2.5,0.7,0),ps=16,tcl=-0.4)
	avday=array(0,dim=c(4,tstepinday,ncurves)) # initialise
	if(modlabel=='no'){
		alltitle=paste('Obs:',obslabel)
	}else{
		alltitle=paste('Obs:',obslabel,'  Model:',modlabel)
	}
	# For each curve (i.e. model, obs etc):
	pscoretotal = c();
	removefrac = c(0,0,0,0) # init
	if(vqcdata[1,1] != -1){ # Reshape qc data into column hours, day rows:
		qc_days = matrix(vqcdata[,1],ncol=tstepinday,byrow=TRUE)
	}
	for(p in 1:ncurves){
		# Reshape data into column hours, day rows:
		data_days = matrix(dcdata[,p],ncol=tstepinday,byrow=TRUE)
		# A count of excluded values - zero if not using gap-filling info:
		exclvals = matrix(0,4,tstepinday) # to count gap-filled data
		# First calculate values to be plotted:
		for(k in 1:4){# for each season (DJF, MAM etc)
			# Sum up variable over each year of data set for current season
			for(l in 1:nyears){
				if(vqcdata[1,1] != -1){
					# Make sure gap-filled data is not included:
					for(i in 1:tstepinday){ # Sum all values for each timestep:	
						thisyearsseason = data_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i]
						sumnotexist = is.na(sum(thisyearsseason[
							as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])]))
						# Note the number of excluded values from k, ith sum:
						exclvals[k,i] = exclvals[k,i] + 
							sum(!as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i]))
						# Allow NA value only if all years of season sum are NA:
						if(l==1){ # 1st year of sum					
							avday[k,i,p] = avday[k,i,p] + sum(thisyearsseason[
								as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])])
						}else{
							if((!sumnotexist) & (!is.na(avday[k,i,p]))){ 
								# i.e. sum exists and values for previous years exist
								avday[k,i,p] = avday[k,i,p] + sum(thisyearsseason[
									as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])])		
							}else if(!sumnotexist){ 
								# i.e. sum exists but previous years' sums are NA
								avday[k,i,p] = sum(thisyearsseason[
									as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])])
							}		
						}
					}
					if(k==1){ # i.e. DJF, which is split in any year
						# add Dec to Jan/Feb
						# this will never e the first attempt to add to this index of avday[k,i,p],
						# so there's no l==1 case here:					
						for(i in 1:tstepinday){
							thisyearsseason = data_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i]
							sumnotexist = is.na(sum(thisyearsseason[
								as.logical(qc_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i])]))
							# Note the number of excluded values from k, ith sum:
							exclvals[k,i] = exclvals[k,i] + 
								sum(!as.logical(qc_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i]))
							if((!sumnotexist) & (!is.na(avday[k,i,p]))){ 
								# i.e. sum exists and values for previous years/DJF portions exist
								avday[k,i,p] = avday[k,i,p] + sum(thisyearsseason[
									as.logical(qc_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i])])		
							}else if(!sumnotexist){ 
								# i.e. sum exists but previous years' sums are NA
								avday[k,i,p] = sum(thisyearsseason[
									as.logical(qc_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i])])
							}	
						}
					}
				}else{ # no gap-filling information - assume all data are useable:
					for(i in 1:tstepinday){
						# Sum all values for each timestep:
						avday[k,i,p]=avday[k,i,p] + 
							sum(data_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])
					}
					if(k==1){ # i.e. DJF, which is split in any year
						# add Dec to Jan/Feb
						for(i in 1:tstepinday){
							avday[k,i,p]=avday[k,i,p] + 
								sum(data_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i])
						}
					}
				} # use gap-filling info or not
			} # for each year in the data set
			
			# Then find the average of these values:
			if(k==1){ # i.e. DJF, which is split in any year
				avday[k,,p]=avday[k,,p]/(90*nyears - exclvals[k,])
				removefrac[k] = sum(exclvals[k,])/(90*nyears*tstepinday)
			}else{
				avday[k,,p]=avday[k,,p]/((fnid[k]-stid[k])*nyears - exclvals[k,])
				removefrac[k] =  sum(exclvals[k,]) / ((fnid[k]-stid[k])*nyears*tstepinday)
			}
		} # over each season
		if(p>1){
			# Calculate all-season score:
			pscoretotal[p-1] = sum(abs(as.vector(avday[,,p] - avday[,,1])),na.rm=TRUE) / 
				sum(abs(as.vector(mean(avday[,,1],na.rm=TRUE) - avday[,,1])),na.rm=TRUE)
			removefractotal = sum(exclvals) / ntsteps
		}
	} # for each curve (mod, obs, etc)
	
	# Report NME metric:
	if(ncurves==2){ # model only
		metrics[[1]] = list(name='NME',model_value=pscoretotal[1])	
	}else if(ncurves==3){
		metrics[[1]] = list(name='NME',model_value=pscoretotal[1],bench_value=list(bench1=pscoretotal[2]))	
	}else if(ncurves==4){
		metrics[[1]] = list(name='NME',model_value=pscoretotal[1],
			bench_value=list(bench1=pscoretotal[2],bench2=pscoretotal[3]))
	}else if(ncurves==5){
		metrics[[1]] = list(name='NME',model_value=pscoretotal[1],
			bench_value=list(bench1=pscoretotal[2],bench2=pscoretotal[3],bench3=pscoretotal[4]))
	}
	
	# Determine boundaries for plots:
	xloc=c(0:(tstepinday-1)) # set location of x-coords in plot
	yaxmin=min(avday) # y axis minimum in plot
	yaxmax=max(avday)+(max(avday)-yaxmin)*0.15 # y axis maximum in plot
	# Now plot each panel:
	for(k in 1:4){# for each season (DJF, MAM etc)
		# Plot obs data result:
		plot(xloc,avday[k,,1],type="l",xaxt="n",xlab=paste(labels[k],'hour of day'),
			ylab=ytext,lwd=4,col=plotcolours[1],ylim=c(yaxmin,yaxmax))
		# Then add other curves, if any:
		if(ncurves>1){
			pscore = matrix(NA,4,(ncurves-1))
			for(p in 2:ncurves){ # for each additional curve
				lines(xloc,avday[k,,p],lwd=3,col=plotcolours[p])
				# Score is normalised mean error:
				pscore[k,p-1] = sum(abs(avday[k,,p] - avday[k,,1]),na.rm=TRUE) /
					sum(abs(as.vector(mean(avday[k,,1],na.rm=TRUE) - avday[k,,1])),na.rm=TRUE)
			}	
		}	
		axis(1,at=c(0,6*tstepinday/24,12*tstepinday/24,18*tstepinday/24,
			23*tstepinday/24),labels=c('0','6','12','18','23'))
		title(alltitle) # add title
		if(k==1){
			# Position legend (and total score, if required):
			posctr = 1
			placeditemctr = 0
			npos = 4
			if(ncurves == 1) {
				nitems = 1 # i.e. just need to position the legend
			}else{
				nitems = 2 # i.e. need to position total score as well
			}
			ypos = c() # y positions of items to placed
			repeat{
				if(! any((avday[k,0:tstepinday/5,]>(yaxmin+(npos-posctr)*(yaxmax-yaxmin)/npos)) &
					(avday[k,0:tstepinday/5,]<(yaxmin+(npos-posctr + 1)*(yaxmax-yaxmin)/npos)))){
					# if not any data in this interval
					placeditemctr = placeditemctr + 1 # i.e. we've just placed something
					ypos[placeditemctr] = yaxmin+((npos-posctr+0.7)/npos)*(yaxmax-yaxmin)
					if(placeditemctr==nitems) {break} # i.e. we've positioned everything
				}else if (posctr == npos){
					# give up and place everything regularly
					for(i in 1:nitems){
						ypos[i] = yaxmin+(1.1 - 0.2*i)*(yaxmax-yaxmin)
					}
					break
				}
				posctr = posctr + 1
			}
			legend(-1,ypos[2],legendtext[1:ncurves],lty=1,col=plotcolours[1:ncurves],
				lwd=3,bty="n",yjust=0.5)
			if(ncurves>1){
				scorestring = paste(signif(pscoretotal,digits=2),collapse=', ')
				removestring = paste(signif(removefractotal*100,digits=2),collapse=', ')
				scoretext = paste('Total score: ',scorestring,'\n','(NME; ',
					removestring,'% data removed)',sep='')
				text(-1,yaxmax-(yaxmax-yaxmin)*0.07,scoretext,pos=4)
			}
		}else if(k==3){
			# Add note about removing gap-filled data:
			if(vqcdata[1,1] != -1){
				qctext = 'Gap-filled observed data removed\nfrom all plots and scores.'	
			}else{	
				qctext = 'All time steps of observed data used.'
			}
			text(-1,yaxmax-(yaxmax-yaxmin)*0.07,qctext,pos=4)
		}
		# Now position seasonal score:
		if(ncurves>1){
			scorestring = paste(signif(pscore[k,],digits=2),collapse=', ')
			removestring = paste(signif(removefrac[k]*100,digits=2),collapse=', ')
			scoretext = paste('Score: ',scorestring,'\n','(NME; ',
				removestring,'% data removed)',sep='')
			text((tstepinday/2),yaxmax-(yaxmax-yaxmin)*0.07,scoretext,pos=4)	
		}
	} # each plot / season
	result=list(err=FALSE,errtext=errtext,metrics=metrics)
	return(result)
} # End function DiurnalCycle
