# AnalysisAnnualCycle.R
#
# This function plots the monthly averages of a variable,
# for both modelled and observed. 
# For this to be successful, the dataset **MUST START AT JAN 1**
# AND be a integer number of years in length.
#
# Gab Abramowitz CCRC, UNSW 2014 (palshelp at gmail dot com)
#
AnnualCycle = function(obslabel,acdata,varname,ytext,legendtext,
	timestepsize,whole,plotcolours,modlabel='no',na.rm=FALSE){
	######
	errtext = 'ok'
	metrics = list()
	if(!whole){ # we need a whole number of years for this to run
		errtext = 'AnnualCycle analysis requires a whole number of years of data.'
		result = list(errtext=errtext,metrics=list(first=list(name='fail',model_value=NA)))
		return(result)
	}
	ncurves = length(acdata[1,]) # Number of curves in final plot:
	ntsteps = length(acdata[,1]) # Number of timesteps in data:
	tstepinday=86400/timestepsize # time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	nyears=as.integer(ndays/365) # find # years in data set
	month=getMonthDays(leap=FALSE) # get non-leap year month days (in PALSconstants)
	data_monthly=matrix(NA,12,ncurves) # initialise monthly averages
	# For each curve (i.e. model, obs etc):
	for(p in 1:ncurves){
		# Reshape into timestep columns, row days:
		data_days=matrix(acdata[,p],ncol=tstepinday,byrow=TRUE) 
		avday=c() # initialise
		# Transform data into daily averages:
		for(i in 1:ndays){
			avday[i]=mean(data_days[i,],na.rm=na.rm) # calc daily average value
		}	
		# Transform daily means into monthly means:
		for(m in 1:12){ # for each month
			data_month=0 # initialise
      days_missing=0 #initialise
			for(k in 1:nyears){ # for each year of data set
				# Add all daily averages for a given month
				# over all data set years:
				data_month = data_month + 
					sum(avday[(month$start[m]+(k-1)*365):
					(month$start[m+1]-1 +(k-1)*365) ], na.rm=na.rm)
        #Number of missing time steps
        days_missing = days_missing +
          sum(is.na(avday[(month$start[m]+(k-1)*365):
                               (month$start[m+1]-1 +(k-1)*365) ]))
			}
			# Then divide by the total number of days added above (minus missing time steps):
			data_monthly[m,p]=data_month/(month$length[m]*nyears-days_missing)
		}
	}
	xloc=c(1:12) # set location of x-coords
	# Plot model output result:
	yaxmin=min(data_monthly,na.rm=na.rm) # y axis minimum in plot
	yaxmax=max(data_monthly,na.rm=na.rm)+0.18*(max(data_monthly,na.rm=na.rm)-yaxmin) # y axis maximum in plot
	plot(xloc,data_monthly[,1],type="l",xaxt="n",xlab='Month',ylab=ytext,
		lwd=3,col=plotcolours[1],ylim=c(yaxmin,yaxmax),cex.lab=1.2,cex.axis=1.3,
		mgp = c(2.5,0.8,0))
	# Add other curves:
	if(ncurves>1){
		pscore = c()
		for(p in 2:ncurves){ # for each additional curve
			lines(xloc,data_monthly[,p],lwd=3,col=plotcolours[p])
			# Score is normalised mean error:
			pscore[p-1] = sum(abs(data_monthly[,p] - data_monthly[,1])) /
				sum(abs(mean(data_monthly[,1]) - data_monthly[,1]))
		}	
	}
	axis(1,at=c(2,4,6,8,10,12),labels=c('2','4','6','8','10','12'),cex.axis=1.3)
	if(modlabel=='no'){ # i.e. an obs analysis
		title(paste('Average monthly ',varname[1],':   Obs - ',obslabel,
			sep=''),cex.main=1.1) # add title
	}else{
		title(paste('Average monthly ',varname[1],':   Obs - ',obslabel,'   Model - ',
			modlabel,sep=''),cex.main=1.1) # add title
	}
	legend(1,max(data_monthly)+0.15*(max(data_monthly)-yaxmin),legendtext[1:ncurves],
		lty=1,col=plotcolours[1:ncurves],lwd=3,bty="n",yjust=0.8)
	if(ncurves>1){
		scorestring = paste(signif(pscore,digits=3),collapse=', ')
		scoretext = paste('Score: ',scorestring,'\n','(NME)',sep='')
		text(8,max(data_monthly)+0.1*(max(data_monthly)-yaxmin),scoretext,pos=4,offset=1)
		if(ncurves==2){ # model only
			metrics[[1]] = list(name='NME',model_value=pscore[1])	
		}else if(ncurves==3){
			metrics[[1]] = list(name='NME',model_value=pscore[1],bench_value=list(bench1=pscore[2]))	
		}else if(ncurves==4){
			metrics[[1]] = list(name='NME',model_value=pscore[1],
				bench_value=list(bench1=pscore[2],bench2=pscore[3]))
		}else if(ncurves==5){
			metrics[[1]] = list(name='NME',model_value=pscore[1],
				bench_value=list(bench1=pscore[2],bench2=pscore[3],bench3=pscore[4]))
		}
	}
  #Print percentage of data missing if na.rm=TRUE and some data missing
  if(na.rm){
    perc_missing = round(sapply(1:ncol(acdata), function(x) #round
      sum(is.na(acdata[,x]))/length(acdata[,x])), digits=3)      
    if(any(perc_missing > 0)){
      text(1,yaxmax, paste("(",paste(perc_missing,collapse=", "), ")% data missing", sep=""),
           pos=4,offset=1, col="red")
    }
  }
	result=list(err=FALSE,errtext=errtext,metrics=metrics)
	return(result)
} # End function AnnualCycle
