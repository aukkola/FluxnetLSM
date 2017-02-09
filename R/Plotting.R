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
  
  
  #Retrieve time variable
  time <- ncvar_get(ncfile, "time")
  
  
  #Load qc variables if available
  if(length(qc_vars) > 0) {
    qc_data <- lapply(qc_vars, ncvar_get, nc=ncfile)
    names(qc_data) <- qc_vars  
  }

  
  #Number of variables to plot
  no_vars <- length(data_vars)
 
  for(k in 1:length(analysis_type)){
    
    
    ## Annual cycle ##
    if(analysis_type[k]=="annual"){
      
      #Initialise file
      pdf(paste(outfile, "AnnualCycle.pdf", sep=""), height=no_vars,
          width=no_vars)
      
      par(mai=c(0.6,0.7,0.7,0.2))
      par(omi=c(0.2,0.2,0.2,0.1))
      par(mfrow=c(ceiling(sqrt(no_vars)), ceiling(sqrt(no_vars))))
      
      #Plot
      for(n in 1:length(data)){
        AnnualCycle(obslabel="",acdata=as.matrix(data[[n]]),varname=data_vars[n],
                    ytext=paste(data_vars[[n]], " (", data_units[[n]], ")", sep=""), 
                    legendtext=data_vars[n], timestepsize=time[2]-time[1],
                    whole=TRUE, plotcolours="blue")  
      }
  
      #Close file
      dev.off()
      
      
      
      
    ## Diurnal cycle ##      FIX ONLY SAVES LAST VARIABLES due to multiple mfrow !!!!!!!!
    } else if(analysis_type[k]=="diurnal"){
      
      #Initialise file
      pdf(paste(outfile, "DiurnalCycle.pdf", sep=""), height=no_vars,
          width=no_vars)
      
      par(mai=c(0.6,0.7,0.7,0.2))
      par(omi=c(0.2,0.2,0.2,0.1))
      par(mfrow=c(ceiling(sqrt(no_vars)), ceiling(sqrt(no_vars))))
      
      #Plot
      for(n in 1:length(data)){
        
        DiurnalCycle(obslabel="",dcdata=as.matrix(data[[n]]),varname=data_vars[n],
                    ytext=paste(data_vars[[n]], " (", data_units[[n]], ")", sep=""), 
                    legendtext=data_vars[n], timestepsize=time[2]-time[1],
                    whole=TRUE, plotcolours="red")  
      }
      
      #Close file
      dev.off()
      
      

    ## 14-day running time series ##   COMPLETE !!!!!!!!!!!!!!!
    } else if(analysis_type[k]=="timeseries"){
    
    
      Timeseries(obslabel="", tsdata=as.matrix(data[[n]]), varname=data_vars[n],
                 ytext=paste(data_vars[[n]], " (", data_units[[n]], ")", sep=""), 
                 legendtext=data_vars[n],
                 
                 plotcex, 
      timing, smoothed = TRUE, winsize = 14, plotcolours="blue", 
      vqcdata = matrix(-1, nrow = 1, ncol = 1)
      
      
      
      
      
      
    ## Analysis type not known, return warning ##
    } else {
      warning(paste("Attempted to produce output plot but analysis
              type not known. Accepted types are 'annual', 'diurnal'
              and 'timeseries' but ", "'", analysis_type[k], "' was 
              passed to function.", sep=""))
    }
    
    
    

    
  } #analyses
    
}