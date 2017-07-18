# Plotting.R
#
# author: Anna Ukkola UNSW 2017
#

#' Plots standard analysis plots from netcdf data
#'
#' @param ncfile an open netcdf file
#' @param analysis_type vector of plot names: c("annual", "diurnal", "timeseries")
#' @param vars vector of variable names to plot
#' @param outfile output path prefix, including directory
#'
#' @export
#' 
plot_nc <- function(ncfile, analysis_type, vars, varnames, outfile){
  
  #Initialise warnings
  warnings <- ""
  
  #Separate data and QC variables
  #TODO: hard coding "_qc" here, need to change
  data_vars <- vars[!grepl("_qc", vars)]
  qc_vars   <- vars[grepl("_qc", vars)]
  
    
  #Load data variables and units from NetCDF file
  data        <- lapply(data_vars, ncvar_get, nc=ncfile)
  data_units  <- lapply(data_vars, function(x) ncatt_get(nc=ncfile, varid=x, 
                                                        attname="units")$value)
  #fluxnet_names
  fluxnet_names  <- lapply(data_vars, function(x) ncatt_get(nc=ncfile, varid=x, 
                                                         attname="Fluxnet_name")$value)
  names(data)       <- data_vars
  names(data_units) <- data_vars
  
  
  #Retrieve time variable and units
  time       <- ncvar_get(ncfile, "time")
  time_units <- ncatt_get(ncfile, "time", "units")$value
  
  #Find time attributes
  timing <- GetTimingNcfile(ncfile)  
  
  #Abort if time unit not in seconds
  if(!grepl("seconds since", time_units)){
    warn <- paste("Unknown time units, unable to produce",
                     "output plots. Expects time in seconds,",
                     "currently in units of", time_units)
    warnings <- append_and_warn(warn=warn, warnings)
    return(warnings)
  }
  
  
  #Time step size  
  timestepsize <- time[2] - time[1]
  
  #Find start year
  startdate <- as.Date(strsplit(time_units, "seconds since ")[[1]][2])
  syear     <- as.numeric(format(startdate, "%Y"))
  
  
  ## If rainfall and air temp being plotted, ##
  ## convert to units mm/timestepsize and deg C             ##
  if(any(fluxnet_names==varnames$precip)){
    ind <- which(fluxnet_names==varnames$precip)
    
    #If recognised units, convert to mm/timestep
    if(data_units[[ind]]=="mm/s" | data_units[[ind]]=="mm s-1" | 
       data_units[[ind]]=="kg/m2/s" | data_units[[ind]]=="kg m-2 s-1"){
      
      data[[ind]] <- data[[ind]] * timestepsize
      data_units[[ind]] <- paste("mm/", timestepsize/60, "min", sep="")
    }
  }
  if(any(fluxnet_names==varnames$tair)){
    ind <- which(fluxnet_names==varnames$tair)
    
    #If recognised units, convert to mm/timestep
    if(data_units[[ind]]=="K"){
      data[[ind]] <- data[[ind]] - 273.15
      data_units[[ind]] <- paste("C")
    }
  }
  
  
  ## Load qc variables if available ##
  if(length(qc_vars) > 0) {
    qc_data <- lapply(qc_vars, ncvar_get, nc=ncfile)
    names(qc_data) <- qc_vars  
  }

  
  ## Number of variables to plot ##
  no_vars <- length(data_vars)
 
  ### Loop through analysis types ###
  for(k in 1:length(analysis_type)){
    

    ##################
    ## Annual cycle ##
    ##################
    if(analysis_type[k]=="annual"){
      
      #Initialise file
      pdf(paste(outfile, "AnnualCycle.pdf", sep=""), height=no_vars*5,
            width=no_vars*5)

      
      par(mai=c(0.6+(no_vars/7),1+(no_vars/15),0.7,0.2))
      par(omi=c(0.8+(no_vars/10),0.5+(no_vars/10),0.2+(no_vars/10),0.1+(no_vars/10)))
      par(mfrow=c(ceiling(sqrt(no_vars)), ceiling(sqrt(no_vars))))
      
      #Plot
      for(n in 1:length(data)){
        
        AnnualCycle(obslabel="", acdata=as.matrix(data[[n]]),
                    varname=data_vars[n], 
                    ytext=paste(data_vars[n], " (", data_units[n], ")", sep=""), 
                    legendtext=data_vars[n], 
                    timestepsize=timestepsize,
                    whole=timing$whole, plotcolours="blue",
                    plot.cex=no_vars/2, na.rm=TRUE)  
      }
  
      #Close file
      dev.off()
      
      
      
      
      
    ###################
    ## Diurnal cycle ## 
    ###################
    } else if(analysis_type[k]=="diurnal"){
      
      #Initialise file
      #Each variable is plotted as a separate figure so dimensions handled differently
      pdf(paste(outfile, "DiurnalCycle.pdf", sep=""), height=10,
          width=10)
      
      par(mai=c(0.6,0.7,0.7,0.2))
      par(omi=c(0.8,0.5,0.2,0.1))
      par(mfrow=c(ceiling(sqrt(no_vars)), ceiling(sqrt(no_vars))))
      
      #Plot
      for(n in 1:length(data)){  
        
        #Find corresponding QC variable (if available)
        qc_ind <- which(qc_vars==paste(data_vars[n], "_qc", sep=""))
        
        #Extract QC data and replace all gap-filled values with 0
        # and measured with 1 (opposite to Fluxnet but what PALS expects)
        if(length(qc_ind) >0){
          
          var_qc <- qc_data[[qc_ind]]
          
          var_qc[var_qc > 0]  <- 2 #replace gap-filled values with a temporary value
          var_qc[var_qc == 0] <- 1 #set measured to 1
          var_qc[var_qc == 2] <- 0 #set gap-filled to 0
          
          #Else set to PALS option corresponding to no QC data
        } else {
          var_qc <- matrix(-1, nrow = 1, ncol = 1)
        }
        
        DiurnalCycle(obslabel=data_vars[n],dcdata=as.matrix(data[[n]]),
                     varname=data_vars[n], 
                     ytext=paste(data_vars[n], " (", data_units[n], ")", sep=""), 
                     legendtext=data_vars[n], timestepsize=timestepsize,
                     whole=timing$whole, plotcolours="blue",
                     #vqcdata=as.matrix(var_qc),
                     plot.cex=1, na.rm=TRUE)  
      }
      
      #Close file
      dev.off()
      
      
      
    ################################
    ## 14-day running time series ##
    ################################
    } else if(analysis_type[k]=="timeseries"){

      #Initialise file
      pdf(paste(outfile, "Timeseries.pdf", sep=""), height=no_vars*2.2*4, width=no_vars*1.4*7.5)
      
      par(mai=c(0.6+(no_vars/7),1+(no_vars/15),0.7,0.2))
      par(omi=c(0.8+(no_vars/10),0.5+(no_vars/10),0.2+(no_vars/10),0.1+(no_vars/10)))
      
      if(no_vars==1){
        par(mfrow=c(ceiling(no_vars/2), 1))
      } else {
        par(mfrow=c(ceiling(no_vars/2), 2))
      }
      

      #Plot
      for(n in 1:length(data)){
  
        
        #Find corresponding QC variable (if available)
        qc_ind <- which(qc_vars==paste(data_vars[n], "_qc", sep=""))
        
        #Extract QC data and replace all gap-filled values with 0
        # and measured with 1 (opposite to Fluxnet but what PALS expects)
        if(length(qc_ind) >0){
          
          var_qc <- qc_data[[qc_ind]]
          
          var_qc[var_qc > 0]  <- 2 #replace gap-filled values with a temporary value
          var_qc[var_qc == 0] <- 1 #set measured to 1
          var_qc[var_qc == 2] <- 0 #set gap-filled to 0
          
          #If first value missing, set to measured (to avoid an error when PALS
          #checks if first value -1)
          if(is.na(var_qc[1])){ var_qc[1] <- 0}
          
          
          #Else set to PALS option corresponding to no QC data
        } else {
          var_qc <- matrix(-1, nrow = 1, ncol = 1)
        }
        
        
        Timeseries(obslabel="", tsdata=as.matrix(data[[n]]), 
                   varname=data_vars[n],
                   ytext=paste(data_vars[n], " (", data_units[n], ")", sep=""), 
                   legendtext=data_vars[n],
                   plotcex=no_vars/2 + 1 , timing=timing, 
                   smoothed = FALSE, winsize = 1, 
                   plotcolours="blue", 
                   vqcdata = as.matrix(var_qc),
                   na.rm=TRUE)
      
      }

    dev.off()  
           
    
    ###################################################
    ## Else: Analysis type not known, return warning ##
    ###################################################
    } else {
      
      warn <- paste("Attempted to produce output plot but analysis
                    type not known. Accepted types are 'annual', 'diurnal'
                    and 'timeseries' but ", "'", analysis_type[k], "' was 
                    passed to function.", sep="")
      
      warnings <- append_and_warn(warn=warn, warnings)
    }
    
    
    
  } #analyses
  
  return(warnings)
  
} #function

#-----------------------------------------------------------------------------

### The following three functions are reproduced from PALS ###

#' Plots a diurnal cycle
#' @export
DiurnalCycle <- function(obslabel,dcdata,varname,ytext,legendtext,
                         timestepsize,whole,plotcolours,modlabel='no',
                         vqcdata=matrix(-1,nrow=1,ncol=1),
                         plot.cex, na.rm=FALSE){
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
  perc_missing = matrix(0,4,ncurves) #initialise, % data missing each season and model/obs
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
    #  A count of missing values for each season
    missing_vals = matrix(0, ncol=4)
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
                as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])],na.rm=na.rm)
              missing_vals[k] = missing_vals[k] + sum(is.na(thisyearsseason)) #count no. missing timesteps
            }else{
              if((!sumnotexist) & (!is.na(avday[k,i,p]))){ 
                # i.e. sum exists and values for previous years exist
                avday[k,i,p] = avday[k,i,p] + sum(thisyearsseason[
                  as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])],na.rm=na.rm)
                missing_vals[k] = missing_vals[k] + sum(is.na(thisyearsseason)) #count no. missing timesteps
              }else if(!sumnotexist){ 
                # i.e. sum exists but previous years' sums are NA
                avday[k,i,p] = sum(thisyearsseason[
                  as.logical(qc_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])],na.rm=na.rm)
                missing_vals[k] = missing_vals[k] + sum(is.na(thisyearsseason)) #count no. missing timesteps
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
                  as.logical(qc_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i])],na.rm=na.rm)
              }else if(!sumnotexist){ 
                # i.e. sum exists but previous years' sums are NA
                avday[k,i,p] = sum(thisyearsseason[
                  as.logical(qc_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i])],na.rm=na.rm)
              }	
              missing_vals[k] = missing_vals[k] + sum(is.na(thisyearsseason)) #count missing values
            }
          }
        }else{ # no gap-filling information - assume all data are useable:
          for(i in 1:tstepinday){
            # Sum all values for each timestep:
            if(all(is.na(data_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i]))){
              avday[k,i,p] <- NA
            } else{
              avday[k,i,p]=avday[k,i,p] + 
                sum(data_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i], na.rm=na.rm)
             }
            missing_vals[k] = missing_vals[k] + 
              sum(is.na(data_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])) #count missing values
          }
          if(k==1){ # i.e. DJF, which is split in any year
            # add Dec to Jan/Feb
            for(i in 1:tstepinday){
              if(all(is.na(data_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i]))){
                avday[k,i,p] <- NA
              } else{
                avday[k,i,p]=avday[k,i,p] + 
                  sum(data_days[(stid[k+4]+(l-1)*365):(fnid[k+4]+(l-1)*365),i], na.rm=na.rm)
              }
              missing_vals[k] = missing_vals[k] + 
                sum(is.na(data_days[(stid[k]+(l-1)*365):(fnid[k]+(l-1)*365),i])) #count missing values
            }
          }
        } # use gap-filling info or not
      } # for each year in the data set
      
      # Then find the average of these values:
      if(k==1){ # i.e. DJF, which is split in any year
        avday[k,,p]=avday[k,,p]/(90*nyears - exclvals[k,] - (missing_vals[k]/tstepinday))
        removefrac[k] = sum(exclvals[k,])/(90*nyears*tstepinday)
        perc_missing[k,p] <- missing_vals[k]/tstepinday/(90*nyears)*100
      }else{
        avday[k,,p]=avday[k,,p]/((fnid[k]-stid[k])*nyears - exclvals[k,] - (missing_vals[k]/tstepinday))
        removefrac[k] =  sum(exclvals[k,]) / ((fnid[k]-stid[k])*nyears*tstepinday)
        perc_missing[k,p] <- missing_vals[k]/tstepinday/((fnid[k]-stid[k])*nyears)*100
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
  # Now plot each panel:
  for(k in 1:4){# for each season (DJF, MAM etc)
    #All missing: plot empty
    if(all(is.na(avday[k,,1]))){
      plot(xloc, xloc, type="n",xaxt="n",xlab=paste(labels[k],'hour of day'),
           ylab=ytext,yaxt="n", cex.axis=plot.cex, cex.lab=plot.cex)
      mtext(side=3, "All values missing", col="red", line=-4)
    #Else plot
    }else{
      yaxmin=min(avday,na.rm=na.rm) # y axis minimum in plot
      yaxmax=max(avday,na.rm=na.rm)+(max(avday,na.rm=na.rm)-yaxmin)*0.15 # y axis maximum in plot
      # Plot obs data result:
      plot(xloc,avday[k,,1],type="l",xaxt="n",xlab=paste(labels[k],'hour of day'),
           ylab=ytext,lwd=4,col=plotcolours[1],ylim=c(yaxmin,yaxmax), 
           cex.axis=plot.cex, cex.lab=plot.cex)
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
                     (avday[k,0:tstepinday/5,]<(yaxmin+(npos-posctr + 1)*(yaxmax-yaxmin)/npos)), na.rm=TRUE)){
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
               lwd=3,bty="n",yjust=0.5, cex=plot.cex)
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
      #Print percentage of data missing if na.rm=TRUE and some data missing
      if(na.rm){
        if(!all(is.na(avday[k,,1])) & any(perc_missing[k,] > 0)){
          rounded=round(perc_missing[k,],digits=3)
          text(-1,yaxmax,paste("(",paste(rounded,collapse=", "), ")% data missing", sep=""),
               pos=4, col="red")
        }
      }
  }#all NA
    axis(1,at=c(0,6*tstepinday/24,12*tstepinday/24,18*tstepinday/24,
                23*tstepinday/24),labels=c('0','6','12','18','23'), 
                cex.axis=plot.cex, cex.lab=plot.cex)
    title(alltitle) # add title
  } # each plot / season
  result=list(err=FALSE,errtext=errtext,metrics=metrics)
  return(result)
} # End function DiurnalCycle

#-----------------------------------------------------------------------------

#' Plots an annual cycle
#' @export
AnnualCycle <- function(obslabel,acdata,varname,ytext,legendtext,timestepsize,
                        whole,plotcolours,modlabel='no',plot.cex,na.rm=FALSE){
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
  
  #If all missing, plot empty
  if(all(is.na(data_monthly[,1]))){
    plot(xloc,xloc,type="n",xaxt="n",xlab='Month',ylab=ytext,yaxt="n",
         cex,cex.axis=plot.cex*1.08,mgp = c(2.5+plot.cex*0.9,0.8,0))
    mtext(side=3, "All values missing", col="red", line=-4, cex=plot.cex)
  } else{
    # Plot model output result:
    yaxmin=min(data_monthly,na.rm=na.rm) # y axis minimum in plot
    yaxmax=max(data_monthly,na.rm=na.rm)+0.18*(max(data_monthly,na.rm=na.rm)-yaxmin) # y axis maximum in plot
    plot(xloc,data_monthly[,1],type="l",xaxt="n",xlab='Month',ylab=ytext,
         lwd=3,col=plotcolours[1],ylim=c(yaxmin,yaxmax),cex.lab=plot.cex,cex.axis=plot.cex*1.08,
         mgp = c(2.5+plot.cex*0.9,0.8,0))
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
    legend(1,max(data_monthly)+0.15*(max(data_monthly)-yaxmin),legendtext[1:ncurves],
           lty=1,col=plotcolours[1:ncurves],lwd=3,bty="n",yjust=0.8, cex=plot.cex)
    if(ncurves>1){
      scorestring = paste(signif(pscore,digits=3),collapse=', ')
      scoretext = paste('Score: ',scorestring,'\n','(NME)',sep='')
      text(8,max(data_monthly)+0.1*(max(data_monthly)-yaxmin),scoretext,pos=4,offset=1, cex=plot.cex)
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
  }
  axis(1,at=c(2,4,6,8,10,12),labels=c('2','4','6','8','10','12'),cex.axis=plot.cex,
       mgp = c(2.3,plot.cex*0.7,0))
  if(modlabel=='no'){ # i.e. an obs analysis
    title(paste('Average monthly ',varname[1],#':   Obs - ',obslabel,
                sep=''),cex.main=plot.cex) # add title
  }else{
    title(paste('Average monthly ',varname[1],#':   Obs - ',obslabel,'   Model - ',
                modlabel,sep=''),cex.main=plot.cex) # add title
  }
  #Print percentage of data missing if na.rm=TRUE and some data missing
  if(na.rm){
    perc_missing = round(sapply(1:ncol(acdata), function(x) #round
      sum(is.na(acdata[,x]))/length(acdata[,x])), digits=3)      
    if(!all(is.na(data_monthly[,1])) & any(perc_missing > 0)){
      text(1,yaxmax, paste(paste(perc_missing,collapse=","), "% data missing", sep=""),
           pos=4,offset=1, col="red", cex=plot.cex)
    }
  }
  result=list(err=FALSE,errtext=errtext,metrics=metrics)
  return(result)
} # End function AnnualCycle

#-----------------------------------------------------------------------------

#' Plots a smoothed and non-smoothed time seris
#' @export
Timeseries <- function(obslabel,tsdata,varname,ytext,legendtext,
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
    #All missing, plot empty
    if(all(is.na(data_smooth[,1]))){
      plot(xloc,xloc,type="n",ylab=ytext, xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='',mgp = c(2.5+plotcex*0.9,0.8,0))
      mtext(side=3, "All values missing", col="red", line=-4)
    } else {
      plot(xloc,data_smooth[,1],type="l",ylab=ytext,lwd=3,
           col=plotcolours[1],ylim=c((ymin),(ymin + (ymax-ymin)*1.2)),
           xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='',mgp = c(2.5+plotcex*0.9,0.8,0))
    }
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
                  obslabel,'	 Model - ',modlabel,sep=''),cex.main=plotcex)
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
      perc_missing = signif(sapply(1:ncol(tsdata), function(x) 
        sum(is.na(tsdata[,x]))/length(tsdata[,x])), digits=3)	   
      if(any(perc_missing > 0)){
        text(xmin-(xmax-xmin)*0.03,y=(ymin + (ymax-ymin)*(y_adj+0.24)),
             paste("(",paste(perc_missing,collapse=", "), ")% data missing", sep=""),
             pos=4,offset=1, col="red", cex=plotcex)
      }
    }
    # Calculate QC time series information, if it exists:
    if(vqcdata[1,1] != -1){
      qcliney = ymin - (ymax-ymin)*0.015# y-location of qc line
      qctexty = ymin + (ymax-ymin)*0.02 # y-location of qc text
      qcpc = signif((1-mean(vqcdata[,1]))*100,2) # % of data that's gapfilled
      # Construct line-plottable version of qc timeseries:
      origline =	qcliney/(vqcdata[,1]) # 0s will become 'Inf'
      gapline = (qcliney/(vqcdata[,1]-1))*-1 # 1s will become 'Inf'
      # Plot qc time series line:
      xloc_qc = c(1:length(origline))/length(origline) * length(xloc)
      lines(xloc_qc,origline,lwd=6,col='gray80')
      lines(xloc_qc,gapline,lwd=3,col='indianred')
      text(x=xmin,y=qctexty,cex=max((plotcex*0.75),0.85),pos=4,
           labels=paste(qcpc,'% of observed ',varname[1],' is gap-filled:',sep=''))
    }	
    
  #Not smoothed
  }else{
    xmin = 1
    xmax = ntsteps
    xloc=c(1:xmax)
    y_adj=1

    #All missing
    if(all(is.na(tsdata[,1]))){
      plot(xloc,xloc,type="n",ylab=ytext,lwd=3,
           yaxt="n", xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='')
      mtext(side=3, "All values missing", col="red", line=-4,mgp = c(2.5+plotcex*0.9,0.8,0))
    #Else plot
    } else {
      # this code not functioning but kept for future modification:
      yvalmin = signif(min(tsdata, na.rm=na.rm),3)
      yvalmax = signif(max(tsdata, na.rm=na.rm),3)
      datamean = signif(mean(tsdata[,1], na.rm=na.rm),3)
      datasd = signif(sd(tsdata[,1], na.rm=na.rm),3)
      ymin = yvalmin
      ymax = yvalmax
      
      #If ignoring NA, make space for printing % missing
      #Also shift other labels and legend down in this case
      if(na.rm){
        ymax=ymax*1.1
        y_adj = 0.94
      }
      
      plot(xloc,tsdata[,1],type="l",ylab=ytext,lwd=3,
           col=plotcolours[1],ylim=c(ymin,(ymin + (ymax-ymin)*1.3)),
           xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='',mgp = c(2.5+plotcex*0.9,0.8,0))
      # Add smoothed curve over whole timeseries:
      data_days=matrix(tsdata[,1],ncol=tstepinday,byrow=TRUE) 
      data_smooth = c()
      dayssmooth = 30
      for(i in 1:(ndays-dayssmooth-1)){
        # Find evaporative fraction using averaging window:
        data_smooth[i] = mean(data_days[i:(i+dayssmooth-1),], na.rm=na.rm)
      }
      xct = c(1:(ndays-dayssmooth-1))
      xsmooth = xct*tstepinday + (tstepinday*dayssmooth / 2 - tstepinday)
      lines(xsmooth,data_smooth,lwd=3,col='gray')
      
      if(ncurves>1){
        for(p in 2:ncurves){ # for each additional curve
          lines(tsdata[,p],lwd=3,col=plotcolours[p])
        }  
      }
      
      legend(0-(xmax-xmin)*0.05,(ymin + (ymax-ymin)*(y_adj+0.42)),legend=legendtext[1:ncurves],lty=1,
             col=plotcolours[1:ncurves],lwd=3,bty="n",cex=max((plotcex*0.75),1))
      # Locations of max,min,mean,sd text:
      stattextx = c(xmin,xmin+(xmax-xmin)*0.5)
      stattexty = c(ymin + (ymax-ymin)*(y_adj+0.18),ymin + (ymax-ymin)*(y_adj+0.24))
      # Write max,min,mean,sd to plot in two lines:
      text(x=stattextx,y=stattexty[2],
           labels=c(paste('Min = ',ymin,sep=''),paste('Max = ',ymax,sep='')),
           cex=max((plotcex*0.75),1),pos=4)
      text(x=stattextx,y=stattexty[1],
           labels=c(paste('Mean = ',datamean,sep=''),paste('SD = ',datasd,sep='')),
           cex=max((plotcex*0.75),1),pos=4)
      #Print percentage of data missing if na.rm=TRUE and some data missing
      if(na.rm){
        perc_missing = signif(sapply(1:ncol(tsdata), function(x) 
          sum(is.na(tsdata[,x]))/length(tsdata[,x])), digits=3)     
        if(any(perc_missing > 0)){
          text((xmax-xmin)*0.5,y=(ymin + (ymax-ymin)*(y_adj+0.42)),
               paste("(",paste(perc_missing,collapse=", "), ")% data missing", sep=""),
               pos=1,offset=1, col="red",cex=plotcex)
        }
        # Calculate QC time series information, if it exists:
        if(vqcdata[1,1] != -1){
          qcliney = ymin + (ymax-ymin)*(y_adj+0.04) # y-location of qc line
          qctexty = ymin + (ymax-ymin)*(y_adj+0.09) # y-location of qc text
          qcpc = signif((1-mean(vqcdata[,1], na.rm=TRUE))*100,2) # % of data that's gapfilled
          # Construct line-plottable version of qc timeseries:
          origline =	qcliney/(vqcdata[,1]) # 0s will become 'Inf'
          gapline = (qcliney/(vqcdata[,1]-1))*-1 # 1s will become 'Inf'
          # Plot qc time series line:
          lines(origline,lwd=5,col='gray80')
          lines(gapline,lwd=2,col='red')
          text(x=stattextx[1],y=qctexty,cex=max((plotcex*0.75),1),pos=4,
               labels=paste(qcpc,'% of time series is gap-filled:',sep=''))
        }
      }
    } #all NA?
    for(l in 1:nyears){
      xxat[(2*l-1)] = (l-1)*365*tstepinday + 1
      xxat[(2*l)] = (l-1)*365*tstepinday + 183*tstepinday
      xxlab[(2*l-1)]=paste('1 Jan',substr(as.character(timing$syear+l-1),3,4))
      xxlab[(2*l)]=paste('1 Jul',substr(as.character(timing$syear+l-1),3,4))
    }
    title(paste(obslabel,varname[1]),cex.main=plotcex)
    axis(1,at=xxat,labels=xxlab,cex.axis=plotcex,mgp = c(2.3,plotcex*0.7,0))
    result = list(err=FALSE,errtext = errtext,metrics=metrics)
    return(result)
  }
}





