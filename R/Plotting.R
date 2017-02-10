#
#
#
# ADD COMMENTS
#
#
#


plot_nc <- function(ncfile, analysis_type, vars, outfile){
  
  
  #Separate data and QC variables
  data_vars <- vars[!grepl("_qc", vars)]
  qc_vars   <- vars[grepl("_qc", vars)]
  
    
  #Load data variables and units from NetCDF file
  data        <- lapply(data_vars, ncvar_get, nc=ncfile)
  data_units  <- lapply(data_vars, function(x) ncatt_get(nc=ncfile, varid=x, 
                                                        attname="units")$value)
  names(data)       <- data_vars
  names(data_units) <- data_vars
  
  
  #Retrieve time variable and units
  time       <- ncvar_get(ncfile, "time")
  time_units <- ncatt_get(ncfile, "time", "units")$value
  
    
  
  #Abort if time unit not in seconds
  if(!grepl("seconds since", time_units)){
    CheckError(paste("Unknown time units, unable to produce",
                     "output plots. Expects time in seconds,",
                     "currently in units of", time_units))
  }
  
  
  #Time step size  
  timestepsize <- time[2] - time[1]
  
  #Find start year
  startdate <- as.Date(strsplit(time_units, "seconds since ")[[1]][2])
  syear     <- as.numeric(format(startdate, "%Y"))
  
  
  #Load qc variables if available
  if(length(qc_vars) > 0) {
    qc_data <- lapply(qc_vars, ncvar_get, nc=ncfile)
    names(qc_data) <- qc_vars  
  }

  
  #Number of variables to plot
  no_vars <- length(data_vars)
 
  
  ### Loop through analysis types ###
  for(k in 1:length(analysis_type)){
    
    
    
    ##################
    ## Annual cycle ##
    ##################
    if(analysis_type[k]=="annual"){
      
      #Initialise file
      pdf(paste(outfile, "AnnualCycle.pdf", sep=""), height=no_vars,
          width=no_vars)
      
      par(mai=c(0.6,0.7,0.7,0.2))
      par(omi=c(0.2,0.2,0.2,0.1))
      par(mfrow=c(ceiling(sqrt(no_vars)), ceiling(sqrt(no_vars))))
      
      #Plot
      for(n in 1:length(data)){   #NEED TO GET whole ARGUMENT FROM DATA, NOT SET TO TRUE IN FUNCTION !!!!!!!!!!!
        
        AnnualCycle(obslabel="", acdata=as.matrix(data[[n]]),
                    varname=data_vars[n], 
                    ytext=paste(data_vars[n], " (", data_units[n], ")", sep=""), 
                    legendtext=data_vars[n], 
                    timestepsize=timestepsize,
                    whole=TRUE, plotcolours="blue",
                    na.rm=TRUE)  
      }
  
      #Close file
      dev.off()
      
      
      
    ###################
    ## Diurnal cycle ##      FIX???? SAVES EACH VARIABLE IN SEPARATE FIGURE due to multiple mfrow !!!!!!!!
    ###################
    } else if(analysis_type[k]=="diurnal"){
      
      #Initialise file
      pdf(paste(outfile, "DiurnalCycle.pdf", sep=""), height=no_vars,
          width=no_vars)
      
      par(mai=c(0.6,0.7,0.7,0.2))
      par(omi=c(0.2,0.2,0.2,0.1))
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
                     whole=TRUE, plotcolours="red",
                     vqcdata=as.matrix(var_qc),
                     na.rm=TRUE)  
      }
      
      #Close file
      dev.off()
      
      
      
    ################################
    ## 14-day running time series ##   COMPLETE !!!!!!!!!!!!!!!
    ################################
    } else if(analysis_type[k]=="timeseries"){
    
      
      #List time variables (required in this format)
      timing <- list(timestepsize, syear)
      names(timing) <- c("tstepsize", "syear")
     
      
      #Initialise file
      pdf(paste(outfile, "Timeseries.pdf", sep=""), height=no_vars*2.2, width=no_vars*1.4)
      
      par(mai=c(0.6,0.6,0.4,0.2))
      par(omi=c(0.2,0.2,0.2,0.1))
      par(mfrow=c(ceiling(no_vars/2), 2))
      

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
        
        
        Timeseries(obslabel="", tsdata=as.matrix(data[[n]]), 
                   varname=data_vars[n],
                   ytext=paste(data_vars[n], " (", data_units[n], ")", sep=""), 
                   legendtext=data_vars[n],
                   plotcex=1.5, timing=timing, 
                   smoothed = TRUE, winsize = 14, 
                   plotcolours="blue", 
                   vqcdata = as.matrix(var_qc),
                   na.rm=TRUE)
      
      }

    dev.off()  
      
      
    
    ###################################################
    ## Else: Analysis type not known, return warning ##
    ###################################################
    } else {
      warning(paste("Attempted to produce output plot but analysis
              type not known. Accepted types are 'annual', 'diurnal'
              and 'timeseries' but ", "'", analysis_type[k], "' was 
              passed to function.", sep=""))
    }
    
    
    

    
  } #analyses
    
}