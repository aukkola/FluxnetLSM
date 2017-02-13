# AnalysisTimeseries.R
#
# Plots simple or smoothed timeseries of a variable
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)

Timeseries = function(obslabel,tsdata,varname,ytext,legendtext,
                      plotcex,timing,smoothed=FALSE,winsize=1,plotcolours,modlabel='no',
                      vqcdata=matrix(-1,nrow=1,ncol=1),na.rm=FALSE){
  #
  errtext = 'ok'
  metrics = list()
  ncurves = length(tsdata[1,]) # Number of curves in final plot:
  ntsteps = length(tsdata[,1]) # Number of timesteps in data:
  tstepinday=86400/timing$tstepsize # number of time steps in a day
  ndays = ntsteps/tstepinday # number of days in data set
  nyears=as.integer(ndays/365) # find # years in data set
  # x-axis labels:
  xxat=c()
  xxlab=c()
  data_smooth = matrix(NA,(ndays-winsize-1),ncurves) # init
  if(smoothed){
    for(p in 1:ncurves){
      # Reshape into column timesteps, row days:
      data_days=matrix(tsdata[,p],ncol=tstepinday,byrow=TRUE) 
      for(i in 1:(ndays-winsize-1)){
        # Find evaporative fraction using averaging window:
        data_smooth[i,p] = mean(data_days[i:(i+winsize-1),],na.rm=na.rm)
      }
      if(p==1){
        yvalmin = as.character(signif(min(tsdata[,p],na.rm=na.rm),3))
        yvalmax = as.character(signif(max(tsdata[,p],na.rm=na.rm),3))
        datamean = as.character(signif(mean(tsdata[,p],na.rm=na.rm),3))
        datasd = as.character(signif(sd(tsdata[,p],na.rm=na.rm),3))
        
      }else{
        yvalmin = paste(yvalmin,', ',as.character(signif(min(tsdata[,p],na.rm=na.rm),3)),sep='')
        yvalmax = paste(yvalmax,', ',as.character(signif(max(tsdata[,p],na.rm=na.rm),3)),sep='')
        datamean = paste(datamean,', ',as.character(signif(mean(tsdata[,p],na.rm=na.rm),3)),sep='')
        datasd = paste(datasd,', ',as.character(signif(sd(tsdata[,p]),3),na.rm=na.rm),sep='')
      }
    }
    ymin = signif(min(data_smooth,na.rm=na.rm),3)
    ymax = signif(max(data_smooth,na.rm=na.rm),3)
    # If we're adding a gap-filling QC line, make space for it:
    if(vqcdata[1,1] != -1) {
      ymin = ymin - (ymax-ymin)*0.06
    }
    #If ignoring NA, make space for printing % missing
    #Also shift other labels and legend down in this case
    y_adj=1
    if(na.rm){
      ymax=ymax*1.1
      y_adj = 0.94
    }
    xmin = 1
    xmax = length(data_smooth[,1])
    xloc=c(1:xmax)
    # Draw plot:
    plot(xloc,data_smooth[,1],type="l",ylab=ytext,lwd=3,
         col=plotcolours[1],ylim=c((ymin),(ymin + (ymax-ymin)*1.2)),
         xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='',mgp = c(2.5,1,0))
    # Calculate NME scores:
    if(ncurves>1){
      smoothscore = c()
      allscore = c()
      for(p in 2:ncurves){ # for each additional curve
        lines(data_smooth[,p],lwd=3,col=plotcolours[p])
        smoothscore[p-1] = sum(abs(data_smooth[,1] - data_smooth[,p]))/
          sum(abs(mean(data_smooth[,1]) - data_smooth[,1]))
        allscore[p-1] = sum(abs(tsdata[,1] - tsdata[,p]))/
          sum(abs(mean(tsdata[,1]) - tsdata[,1]))
      }
      # Report NME metric:
      metricname = paste('NME',winsize,'day',sep='')
      if(ncurves==2){ # model only
        metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE))	
        metrics[[2]] = list(name='NME',model_value=allscore[1])	
        metrics[[3]] = list(name=metricname,model_value=smoothscore[1])	
      }else if(ncurves==3){
        metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
                            bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE) ))
        metrics[[2]] = list(name='NME',model_value=allscore[1],bench_value=list(bench1=allscore[2]))	
        metrics[[3]] = list(name=metricname,model_value=smoothscore[1],
                            bench_value=list(bench1=smoothscore[2]))	
      }else if(ncurves==4){
        metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
                            bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE),
                                             bench2=mean(tsdata[,4]-tsdata[,1],na.rm=TRUE) ))
        metrics[[2]] = list(name='NME',model_value=allscore[1],
                            bench_value=list(bench1=allscore[2],bench2=allscore[3]))
        metrics[[3]] = list(name=metricname,model_value=smoothscore[1],
                            bench_value=list(bench1=smoothscore[2],bench2=smoothscore[3]))	
      }else if(ncurves==5){
        metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
                            bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE),
                                             bench2=mean(tsdata[,4]-tsdata[,1],na.rm=TRUE),
                                             bench3=mean(tsdata[,5]-tsdata[,1],na.rm=TRUE) ))
        metrics[[2]] = list(name='NME',model_value=allscore[1],
                            bench_value=list(bench1=allscore[2],bench2=allscore[3],bench3=allscore[4]))
        metrics[[3]] = list(name=metricname,model_value=smoothscore[1],
                            bench_value=list(bench1=smoothscore[2],bench2=smoothscore[3],bench3=smoothscore[4]))	
      }
    }
    for(l in 1:nyears){
      xxat[(2*l-1)] = (l-1)*365 + 1
      xxat[(2*l)] = (l-1)*365 + 152	
      xxlab[(2*l-1)]=paste('1 Jan',substr(as.character(timing$syear+l-1),3,4))
      xxlab[(2*l)]=paste('1 Jun',substr(as.character(timing$syear+l-1),3,4))
    }
    # place legend:
    legend(xmin-(xmax-xmin)*0.03,(ymin + (ymax-ymin)*(y_adj+0.24)),legend=legendtext[1:ncurves],lty=1,
           col=plotcolours[1:ncurves],lwd=3,bty="n",cex=max((plotcex*0.75),1))
    # Add title:
    if(modlabel=='no'){
      title(paste('Smoothed ',varname[1],': ',winsize,'-day running mean.   Obs - ',
                  obslabel,sep=''),cex.main=plotcex)
    }else{
      title(paste('Smoothed ',varname[1],': ',winsize,'-day running mean.   Obs - ',
                  obslabel,'   Model - ',modlabel,sep=''),cex.main=plotcex)
    }
    # Add Max/Min/Mean/SD numbers:
    text(x=(xmin+(xmax-xmin)*0.25),y=c(ymin + (ymax-ymin)*(y_adj+0.19),ymin + (ymax-ymin)*(y_adj+0.14)),
         labels=c(paste('Min = (',yvalmin,')',sep=''),
                  paste('Max = (',yvalmax,')',sep='')),
         cex=max((plotcex*0.75),1),pos=4)
    text(x=(xmin+(xmax-xmin)*0.25),y=c(ymin + (ymax-ymin)*(y_adj+0.09),ymin + (ymax-ymin)*(y_adj+0.04)),
         labels=c(paste('Mean = (',datamean,')',sep=''),paste('SD = (',datasd,')',sep='')),
         cex=max((plotcex*0.75),1),pos=4)
    # Add NME scores to plot (if there is at least one model output):
    if(ncurves>1){
      sscorestring = paste(signif(smoothscore,digits=3),collapse=', ')
      ascorestring = paste(signif(allscore,digits=3),collapse=', ')
      text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*(y_adj+0.18)),
           labels=paste('Score_smooth: ',sscorestring,sep=''),,pos=4)
      text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*(y_adj+0.12)),
           labels= paste('Score_all: ',ascorestring,sep=''),pos=4)
      text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*(y_adj+0.06)),
           labels=' (NME)',pos=4)
    }
    #Print percentage of data missing if na.rm=TRUE and some data missing
    if(na.rm){
      perc_missing = round(sapply(1:ncol(tsdata), function(x) 
        sum(is.na(tsdata[,x]))/length(tsdata[,x])), digits=3)      
      if(any(perc_missing > 0)){
        text(xmin-(xmax-xmin)*0.03,y=(ymin + (ymax-ymin)*(y_adj+0.24)),
             paste("(",paste(perc_missing,collapse=", "), ")% data missing", sep=""),
             pos=4,offset=1, col="red")
      }
    }
    # Calculate QC time series information, if it exists:
    if(vqcdata[1,1] != -1){
      qcliney = ymin - (ymax-ymin)*0.015# y-location of qc line
      qctexty = ymin + (ymax-ymin)*0.02 # y-location of qc text
      qcpc = signif((1-mean(vqcdata[,1]))*100,2) # % of data that's gapfilled
      # Construct line-plottable version of qc timeseries:
      origline =  qcliney/(vqcdata[,1]) # 0s will become 'Inf'
      gapline = (qcliney/(vqcdata[,1]-1))*-1 # 1s will become 'Inf'
      # Plot qc time series line:
      xloc_qc = c(1:length(origline))/length(origline) * length(xloc)
      lines(xloc_qc,origline,lwd=6,col='gray80')
      lines(xloc_qc,gapline,lwd=3,col='indianred')
      text(x=xmin,y=qctexty,cex=max((plotcex*0.75),0.85),pos=4,
           labels=paste(qcpc,'% of observed ',varname[1],' is gap-filled:',sep=''))
    }		
  }else{
    # this code not functioning but kept for future modification:
    yvalmin = signif(min(tsdata),3)
    yvalmax = signif(max(tsdata),3)
    datamean = signif(mean(tsdata[,1]),3)
    datasd = signif(sd(tsdata[,1]),3)
    ymin = yvalmin
    ymax = yvalmax
    xmin = 1
    xmax = ntsteps
    xloc=c(1:xmax)
    plot(xloc,tsdata[,1],type="l",ylab=ytext,lwd=3,
         col=plotcolours[1],ylim=c(ymin,(ymin + (ymax-ymin)*1.3)),
         xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='')
    # Add smoothed curve over whole timeseries:
    data_days=matrix(tsdata[,1],ncol=tstepinday,byrow=TRUE) 
    data_smooth = c()
    dayssmooth = 30
    for(i in 1:(ndays-dayssmooth-1)){
      # Find evaporative fraction using averaging window:
      data_smooth[i] = mean(data_days[i:(i+dayssmooth-1),])
    }
    xct = c(1:(ndays-dayssmooth-1))
    xsmooth = xct*tstepinday + (tstepinday*dayssmooth / 2 - tstepinday)
    lines(xsmooth,data_smooth,lwd=3,col='gray')
    
    if(ncurves>1){
      for(p in 2:ncurves){ # for each additional curve
        lines(tsdata[,p],lwd=3,col=plotcolours[p])
      }	
    }
    for(l in 1:nyears){
      xxat[(2*l-1)] = (l-1)*365*tstepinday + 1
      xxat[(2*l)] = (l-1)*365*tstepinday + 183*tstepinday
      xxlab[(2*l-1)]=paste('1 Jan',substr(as.character(timing$syear+l-1),3,4))
      xxlab[(2*l)]=paste('1 Jul',substr(as.character(timing$syear+l-1),3,4))
    }
    legend(0-(xmax-xmin)*0.05,(ymin + (ymax-ymin)*1.42),legend=legendtext[1:ncurves],lty=1,
           col=plotcolours[1:ncurves],lwd=3,bty="n",cex=max((plotcex*0.75),1))
    title(paste(obslabel,varname[1]),cex.main=plotcex)
    # Locations of max,min,mean,sd text:
    stattextx = c(xmin,xmin+(xmax-xmin)*0.5)
    stattexty = c(ymin + (ymax-ymin)*1.18,ymin + (ymax-ymin)*1.24)
    # Write max,min,mean,sd to plot in two lines:
    text(x=stattextx,y=stattexty[2],
         labels=c(paste('Min = ',ymin,sep=''),paste('Max = ',ymax,sep='')),
         cex=max((plotcex*0.75),1),pos=4)
    text(x=stattextx,y=stattexty[1],
         labels=c(paste('Mean = ',datamean,sep=''),paste('SD = ',datasd,sep='')),
         cex=max((plotcex*0.75),1),pos=4)
    # Calculate QC time series information, if it exists:
    if(vqcdata[1,1] != -1){
      qcliney = ymin + (ymax-ymin)*1.04 # y-location of qc line
      qctexty = ymin + (ymax-ymin)*1.09 # y-location of qc text
      qcpc = signif((1-mean(vqcdata[,1]))*100,2) # % of data that's gapfilled
      # Construct line-plottable version of qc timeseries:
      origline =  qcliney/(vqcdata[,1]) # 0s will become 'Inf'
      gapline = (qcliney/(vqcdata[,1]-1))*-1 # 1s will become 'Inf'
      # Plot qc time series line:
      lines(origline,lwd=5,col='gray80')
      lines(gapline,lwd=2,col='red')
      text(x=stattextx[1],y=qctexty,cex=max((plotcex*0.75),1),pos=4,
           labels=paste(qcpc,'% of time series is gap-filled:',sep=''))
    }
  }
  axis(1,at=xxat,labels=xxlab,cex.axis=plotcex)
  result = list(err=FALSE,errtext = errtext,metrics=metrics)
  return(result)
}


#######################################################################################################################

Timeseries_unc = function(model,obs,bench,variable,plottype,mask,cl,region,outfile){
  
  #### From DistributeSingleSiteAnalyses function
  varname = variable[['Name']][1]      # Name of the variable
  unitstxt = variable[['UnitsText']]   # Units expression
  longvarname = variable[['PlotName']] # Longer variable name for plots
  filestring = paste(getwd(),outfile,sep = "/")       # File name for graphics file
  outfiletype = paste(varname,tolower(plottype)) # Analysis identifier for javascript
  
  data = list(obs=obs, model=model, bench = bench) 
  bencherrtext = data$bench$errtext
  plotcex = 1.1 # plot text magnification factor
  ytext=bquote('Monthly mean'~.(tolower(longvarname)) ~ ' (' ~ .(unitstxt) ~ ')')
  
  # Check obs or model aren't missing variable data and and that their timing is compatible
  errcheck = CanAnalysisProceed(data$obs,data$model)
  if(errcheck$err){
    result = list(type=outfiletype,filename=filestring,mimetype="image/png",analysistype=Analysis$type,
                  error=errcheck$errtext,bencherror=data$bench$errtext,
                  metrics=list(first=list(name='failed',model_value=NA)),variablename=varname)
    return(result)
  }
  
  # Create data matrix to send to analysis function -> global monthly mean   
  adata = Prepare4DMatrix(data)
  
  # Calculate area weighted mean
  avg_data = AreaWeightedMean(lonlen=obs$grid$lonlen, lat=model$grid$lat, data=adata, mask=mask)
  
  # Dataset names for adding to plots
  obsname = data$obs$name
  moname = data$model$name
  benchnames = c()
  if(data$bench$exist){
    for(b in 1: (data$bench$howmany) ){
      benchnames[b] = data$bench[[ data$bench$index[b] ]]$name
    }
  }
  
  # Prepare legend text
  legendtext = LegendText(data,plotobs=TRUE)
  legendtext[3:(length(legendtext)+1)] = legendtext[2:length(legendtext)]
  legendtext[2] = paste(legendtext[1]," uncertainty",sep='')
  plotcolours = BenchmarkColours(data$bench,plotobs=TRUE)
  
  errtext = 'ok'
  metrics = list()
  dim_ts = dim(avg_data) # 5x24
  ncurves = dim_ts[1]+1 # Number of curves in final plot (+1 for +/- std)
  nmonths = data$obs$timing$tsteps # number of months in data set
  nyears = as.integer(nmonths/12) # find # years in data set
  
  # Create matrix with global mean values in it
  data_series = matrix(NA,ncurves,nmonths) # initialize
  data_series[1,]=avg_data[1,]             # obs
  data_series[2,]=avg_data[1,]+avg_data[2,]  # obs + std
  data_series[3,]=avg_data[1,]-avg_data[2,]  # obs - std
  data_series[4,]=avg_data[3,]             # model
  for(i in 1:(ncurves-4)){
    data_series[(4+i),]=avg_data[3+i,]      # bench
  }
  
  # Calculate minimum and maximum
  ymin = signif(min(data_series),3)
  ymax = signif(max(data_series),3)
  xmin = 1
  xmax = length(data_series[1,])
  xloc=c(1:xmax)
  
  # Plotting
  if(ncurves==5){
    plotcolours = c("black","grey90","grey90","salmon","skyblue2")
    linetype = c(1,1,1,1,2)
    linewidth = c(3,2,2,3,3)
  }else if(ncurves==6){
    plotcolours = c("black","grey90","grey90","salmon","skyblue2","palegreen2")
    linetype = c(1,1,1,1,2,2)
    linewidth = c(3,2,2,3,3,3)
  }else if(ncurves==7){
    plotcolours = c("black","grey90","grey90","salmon","skyblue2","palegreen2","lightgoldenrod")
    linetype = c(1,1,1,1,2,2,2)
    linewidth = c(3,2,2,3,3,3,3)
  }
  plot(xloc,data_series[1,],type="l",ylab=ytext,lwd=3,col=plotcolours[1],ylim=c((ymin),(ymin + (ymax-ymin)*1.2)), 
       xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='',mgp = c(2.5,1,0))
  polygon(c(1:nmonths,rev(1:nmonths)),c(data_series[3,], rev(data_series[2,])),col=adjustcolor("grey95",alpha.f=0.5),border =NA)
  for(p in 1:ncurves){ # for each additional curve
    lines(data_series[p,],lwd=linewidth[p],col=plotcolours[p],lty=linetype[p])
  }
  
  # x-axis labels
  xxat=c()
  xxlab=c()
  for(l in 1:nyears){
    xxat[(2*l-1)] = (l-1)*12 +1
    xxat[(2*l)] = (l-1)*12 + 6  
    xxlab[(2*l-1)]=paste('1 Jan',substr(as.character(data$obs$timing$syear[l]),3,4))
    xxlab[(2*l)]=paste('1 Jun',substr(as.character(data$obs$timing$syear[l]),3,4))
  }
  axis(1,at=xxat,labels=xxlab,cex.axis=plotcex)
  
  # Legend
  legend(xmin-(xmax-xmin)*0.03,(ymin + (ymax-ymin)*1.24),legend=legendtext,lty=linetype[-2],
         col=plotcolours[-2],lwd=3,bty="n",cex=max((plotcex*0.75),1))
  
  # Title
  if(moname=='no'){
    title(paste('Monthly mean ',varname[1],':   Obs - ',obsname,sep=''),cex.main=plotcex)
  }else{
    title(paste('Monthly mean ',varname[1],':   Obs - ',obsname,'   Model - ',moname,sep=''),cex.main=plotcex)
  }
  
  tsdata = t(avg_data[-2,]) # contains transposed time series of : obs, model, bench[1...]
  
  # Calculate NME scores
  allscore = c()
  for(p in 2:(ncurves-2)){ # for each additional curve
    allscore[p-1] = sum(abs(tsdata[,1] - tsdata[,p]))/sum(abs(mean(tsdata[,1]) - tsdata[,1]))
  }
  
  # Report NME metric:
  metricname = 'NMEmonth'
  if((ncurves-2)==2){ # model only
    metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE))  
    metrics[[2]] = list(name='NME',model_value=allscore[1])		
  }else if((ncurves-2)==3){
    metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
                        bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE) ))
    metrics[[2]] = list(name='NME',model_value=allscore[1],bench_value=list(bench1=allscore[2]))		
  }else if((ncurves-2)==4){
    metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
                        bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE),
                                         bench2=mean(tsdata[,4]-tsdata[,1],na.rm=TRUE) ))
    metrics[[2]] = list(name='NME',model_value=allscore[1],
                        bench_value=list(bench1=allscore[2],bench2=allscore[3]))	
  }else if((ncurves-2)==5){
    metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
                        bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE),
                                         bench2=mean(tsdata[,4]-tsdata[,1],na.rm=TRUE),
                                         bench3=mean(tsdata[,5]-tsdata[,1],na.rm=TRUE) ))
    metrics[[2]] = list(name='NME',model_value=allscore[1],
                        bench_value=list(bench1=allscore[2],bench2=allscore[3],bench3=allscore[4]))	
  }
  
  # Calculate Max/Min/Mean/SD numbers
  for(p in 1:(ncurves-2)){
    if(p==1){
      yvalmin = as.character(signif(min(tsdata[,p]),3))
      yvalmax = as.character(signif(max(tsdata[,p]),3))
      datamean = as.character(signif(mean(tsdata[,p]),3))
      datasd = as.character(signif(sd(tsdata[,p]),3))        
    }else{
      yvalmin = paste(yvalmin,', ',as.character(signif(min(tsdata[,p]),3)),sep='')
      yvalmax = paste(yvalmax,', ',as.character(signif(max(tsdata[,p]),3)),sep='')
      datamean = paste(datamean,', ',as.character(signif(mean(tsdata[,p]),3)),sep='')
      datasd = paste(datasd,', ',as.character(signif(sd(tsdata[,p]),3)),sep='')
    }
  }
  
  # Add Max/Min/Mean/SD numbers
  text(x=(xmin+(xmax-xmin)*0.32),y=c(ymin + (ymax-ymin)*1.19,ymin + (ymax-ymin)*1.14),
       labels=c(paste('Min = (',yvalmin,')',sep=''),
                paste('Max = (',yvalmax,')',sep='')),
       cex=max((plotcex*0.75),1),pos=4)
  text(x=(xmin+(xmax-xmin)*0.32),y=c(ymin + (ymax-ymin)*1.09,ymin + (ymax-ymin)*1.04),
       labels=c(paste('Mean = (',datamean,')',sep=''),paste('SD = (',datasd,')',sep='')),
       cex=max((plotcex*0.75),1),pos=4)
  
  # Add NME scores to plot (if there is at least one model output)
  if((ncurves-2)>1){
    ascorestring = paste(signif(allscore,digits=3),collapse=', ')
    text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*1.12),
         labels= paste('Score_all: ',ascorestring,sep=''),pos=4)
    text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*1.06),
         labels=' (NME)',pos=4)
  }
  
  
  result = list(err=FALSE,errtext = errtext,metrics=metrics)
  return(result)
}



#######################################################################################################################

Timeseries_MultiObs_unc = function(model,obs,bench,variable,plottype,mask,cl,region,outfile){
  
  varname = variable[['Name']][1]      # Name of the variable
  unitstxt = variable[['UnitsText']]   # Units expression
  longvarname = variable[['PlotName']] # Longer variable name for plots
  filestring = paste(getwd(),outfile,sep = "/")       # File name for graphics file
  outfiletype = paste(varname,tolower(plottype)) # Analysis identifier for javascript
  
  data = list(obs=obs, model=model, bench = bench) 
  bencherrtext = data$bench$errtext
  plotcex = 1.1 # plot text magnification factor
  ytext=bquote('Monthly mean'~.(tolower(longvarname)) ~ ' (' ~ .(unitstxt) ~ ')')
  
  # Create data matrix to send to analysis function -> global monthly mean   
  adata = Prepare4DMatrix_MultiObs(data)
  
  # Calculate area weighted mean
  avg_data = list()
  obsunc_cnt = 0
  for(i in 1:length(adata)){
    avg_data[[i]] = AreaWeightedMean_MultiObs(lonlen=adata[[i]]$lonlen, lat=adata[[i]]$lat, data=adata[[i]]$data, name=adata[[i]]$nm, mask, years = adata[[i]]$years)
    if(avg_data[[i]]$nm == "obs_unc"){
      obsunc_cnt = obsunc_cnt + 1         # count the number of obs. with uncertainty estimates
    }
  }
  
  # Dataset names for adding to plots
  obsname=c()
  for(i in 1:length(obs)){
    obsname[i] = data$obs[[i]]$name
  }
  moname = data$model$name
  benchnames = c()
  if(data$bench$exist){
    for(b in 1: (data$bench$howmany) ){
      benchnames[b] = data$bench[[ data$bench$index[b] ]]$name
    }
  }
  
  legendtext=c(moname,obsname,benchnames) # without unc.
  errtext = 'ok'
  metrics = list()
  dim_ts = length(avg_data) # 11
  ncurves = dim_ts[1] + obsunc_cnt # Number of curves in final plot (+obsunc_cnt for +/- std)
   
  # Plotting Colours  
  colors =  TimeSeriesColour()
  sequence = c()
  years = list()
  plotcolor = c()
  linetype = c()
  linewidth = c()
  add = 1
  for(i in 1:length(avg_data)){
    if((avg_data[[i]]$nm=="model") | (avg_data[[i]]$nm=="obs")){
      plotcolor[add] = colors[i]
      linetype[add] = 1 
      linewidth[add] = 3 
      sequence[add] = avg_data[[i]]$nm
      years[[add]] = avg_data[[i]]$yr
      add = add +1
    }else if(avg_data[[i]]$nm=="bench"){
      plotcolor[add] = colors[i]
      linetype[add] = 2 
      linewidth[add] = 3 
      sequence[add] = avg_data[[i]]$nm
      years[[add]] = avg_data[[i]]$yr
      add = add +1
    }else if(avg_data[[i]]$nm=="obs_unc"){
      for(m in 1:2){
        plotcolor[add] = colors[i-1]
        linetype[add] = 1 
        linewidth[add] = 0 
        sequence[add] = avg_data[[i]]$nm
        years[[add]] = avg_data[[i]]$yr
        add = add +1
      }
    }
  }
  
  # Minimum and Maximum years
  yrmin = min(unlist(years))
  yrmax = max(unlist(years))
  yrspan = c(yrmin:yrmax)
  nyears = length(yrspan)
  nmonths = nyears * 12
  yrloc = c(1:(length(yrspan)*12))
  
  # Year list to month list
  months = list()
  for(i in 1:length(years)){
    months[[i]] = c(((years[[i]][1]-yrmin)*12)+1,(years[[i]][1]-yrmin)*12 + ((years[[i]][2]-years[[i]][1])+1)*12)
  }
   
  # Create matrix with global mean values in it
  data_series = matrix(NA,ncurves,length(yrloc)) # initialize
  add=1
  for(i in 1:length(avg_data)){
    if(avg_data[[i]]$nm != "obs_unc"){
      data_series[add,c(months[[add]][1]:months[[add]][2])]=avg_data[[i]]$data                                  # model, obs, bench
      add = add + 1
    }else{
      data_series[add,c(months[[add]][1]:months[[add]][2])]=avg_data[[i-1]]$data - avg_data[[i]]$data           # obs - std
      data_series[add+1,c(months[[add+1]][1]:months[[add+1]][2])]=avg_data[[i-1]]$data + avg_data[[i]]$data     # obs + std
      add = add + 2
    }
  }
  
  # Calculate minimum and maximum
  ymin = signif(min(data_series,na.rm=T),3)
  ymax = signif(max(data_series,na.rm=T),3)
  xmin = 1
  xmax = length(data_series[1,])
  xloc = c(1:xmax)
  
  # Plot Timeseries
  plot(xloc,data_series[1,],type="l",ylab=ytext,lwd=3,col=plotcolor[1],ylim=c((ymin),(ymin + (ymax-ymin)*1.3)), 
       xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='',mgp = c(2.5,1,0))
  for(p in 1:ncurves){ # for each additional curve
    if(((p+1)<=ncurves) & (sequence[p]=="obs_unc" & sequence[p+1]=="obs_unc")){
      nona = which(!is.na(data_series[p,]))
      polygon(c(yrloc[nona],rev(yrloc[nona])),c(data_series[p+1,nona], rev(data_series[p,nona])),col=adjustcolor(plotcolor[p-1],alpha.f=0.3),border =NA)
    }
  }
  for(p in 1:ncurves){
    lines(data_series[p,],lwd=linewidth[p],col=plotcolor[p],lty=linetype[p])
  }
  
  # x-axis labels
  xxat=c()
  xxlab=c()
  for(l in 1:nyears){
    xxat[(2*l-1)] = (l-1)*12 +1
    xxat[(2*l)] = (l-1)*12 + 6  
    xxlab[(2*l-1)]=paste('1 Jan',substr(as.character(yrspan[l]),3,4))
    xxlab[(2*l)]=paste('1 Jun',substr(as.character(yrspan[l]),3,4))
  }
  axis(1,at=xxat,labels=xxlab,cex.axis=plotcex)
  
  # Legend
  legend(xmin+(xmax-xmin)*0.75,(ymin + (ymax-ymin)*1.34),legend=legendtext,lty=linetype[-which(sequence=="obs_unc")],
         col=plotcolor[-which(sequence=="obs_unc")],lwd=3,bty="n",cex=max((plotcex*0.5),1))
  
  # Title
  if(moname=='no'){
    title(paste('Monthly mean ',varname[1],':   Obs - ',obsname,sep=''),cex.main=plotcex)
  }else{
    title(paste('Monthly mean ',varname[1],':  Model - ',moname,sep=''),cex.main=plotcex)
  }
  
  tsdata = t(data_series[-which(sequence=="obs_unc"),]) # contains transposed time series of : obs, model, bench[1...]
  sequence_nounc = sequence[-which(sequence=="obs_unc")]
  
  # Calculate NME scores and Bias, compares model and bench to all the obs
  allscore = matrix(NA,length(which(sequence_nounc=="obs")),length(which(sequence_nounc!="obs"))) # obs x (model, #bench)
  bias = matrix(NA,length(which(sequence_nounc=="obs")),length(which(sequence_nounc!="obs")))
  icnt = 1
  pcnt = 1
  for(i in which(sequence_nounc=="obs")){
    for(p in which(sequence_nounc!="obs")){
      allscore[icnt,pcnt] = sum(abs(tsdata[,i] - tsdata[,p]),na.rm=T)/sum(abs(mean(tsdata[,i],na.rm=T) - tsdata[,i]),na.rm=T)
      bias[icnt,pcnt] = mean(tsdata[,p]-tsdata[,i],na.rm=T)
      pcnt = pcnt + 1
    }
    icnt = icnt + 1
    pcnt=1
  }
  
  # Report NME metric:  
  metricname = 'NMEmonth'
  if(length(sequence_nounc[-which(sequence_nounc=="obs")])==1){ # model only
    metrics[[1]] = list(name='Bias',model_value=bias[,1])  
    metrics[[2]] = list(name='NME',model_value=allscore[,1])  
  }else if(length(sequence_nounc[-which(sequence_nounc=="obs")])==2){ # 3 benchmarks
    metrics[[1]] = list(name='Bias',model_value=bias[,1], bench_value=list(bench1=bias[,2]))
    metrics[[2]] = list(name='NME',model_value=allscore[,1], bench_value=list(bench1=allscore[,2]))
  }else if(length(sequence_nounc[-which(sequence_nounc=="obs")])==3){ # 3 benchmarks
    metrics[[1]] = list(name='Bias',model_value=bias[,1], bench_value=list(bench1=bias[,2],bench2=bias[,3] ))
    metrics[[2]] = list(name='NME',model_value=allscore[,1], bench_value=list(bench1=allscore[,2],bench2=allscore[,3]))
  }else if(length(sequence_nounc[-which(sequence_nounc=="obs")])==4){ # 3 benchmarks
    metrics[[1]] = list(name='Bias',model_value=bias[,1], bench_value=list(bench1=bias[,2],bench2=bias[,3],bench3=bias[,4] ))
    metrics[[2]] = list(name='NME',model_value=allscore[,1], bench_value=list(bench1=allscore[,2],bench2=allscore[,3],bench3=allscore[,4]))
  }
  
  # Calculate Max/Min/Mean/SD numbers
  for(p in 1:(dim(tsdata)[2])){
    if(p==1){
      yvalmin = as.character(signif(min(tsdata[,p],na.rm=T),3))
      yvalmax = as.character(signif(max(tsdata[,p],na.rm=T),3))
      datamean = as.character(signif(mean(tsdata[,p],na.rm=T),3))
      datasd = as.character(signif(sd(tsdata[,p],na.rm=T),3))        
    }else{
      yvalmin = paste(yvalmin,', ',as.character(signif(min(tsdata[,p],na.rm=T),3)),sep='')
      yvalmax = paste(yvalmax,', ',as.character(signif(max(tsdata[,p],na.rm=T),3)),sep='')
      datamean = paste(datamean,', ',as.character(signif(mean(tsdata[,p],na.rm=T),3)),sep='')
      datasd = paste(datasd,', ',as.character(signif(sd(tsdata[,p],na.rm=T),3)),sep='')
    }
  }
  
  # Add Max/Min/Mean/SD numbers
  text(x=(xmin+(xmax-xmin)*0),y=c(ymin + (ymax-ymin)*1.29,ymin + (ymax-ymin)*1.24),
       labels=c(paste('Min = (',yvalmin,')',sep=''), paste('Max = (',yvalmax,')',sep='')),
       cex=max((plotcex*0.5),1),pos=4)
  text(x=(xmin+(xmax-xmin)*0),y=c(ymin + (ymax-ymin)*1.19,ymin + (ymax-ymin)*1.14),
       labels=c(paste('Mean = (',datamean,')',sep=''),paste('SD = (',datasd,')',sep='')),
       cex=max((plotcex*0.5),1),pos=4)
  
  # Add NME scores to plot (if there is at least one model output)
  text(x=c(xmin+(xmax-xmin)*0),y=(ymin + (ymax-ymin)*1.08), labels='NME Scores:',cex=max((plotcex*0.5),1),pos=4)
  ascorestring = list()
  for(i in 1:dim(allscore)[1]){
    ascorestring[i] = paste(signif(allscore[i,],digits=3),collapse=', ')
    text(x=c(xmin+(xmax-xmin)*0.14),y=(ymin + (ymax-ymin)*(1.08-(i-1)*0.05)), labels= paste(ascorestring[i],'  (',obsname[i],')',sep=''),
         cex=max((plotcex*0.5),1),pos=4)
  } 
  
  # Add image of mask
  text(x=(xmin+(xmax-xmin)*0.5),y=ymin + (ymax-ymin)*1.29, labels='Mask:',cex=max((plotcex*0.5),1),pos=4)
  mapwidth = ifelse(region=="Australia",0.18,0.28)
  yscaling = ifelse(region=="Australia",1.15,1.14)
  add.image(x=xmin+(xmax-xmin)*0.54,y=(ymin + (ymax-ymin)*yscaling),z=mask,col="gray30",image.width=mapwidth,image.height=mapwidth)
  
  result = list(err=FALSE,errtext = errtext,metrics=metrics)
  return(result)
}