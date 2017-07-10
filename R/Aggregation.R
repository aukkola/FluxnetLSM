# Aggregation.R
# 
# Functions to aggregate data to a coarser temporal resolution
#
# Author: Anna Ukkola UNSW 2017


#' Aggregates original time resolution to a coarser time step
#' @export
aggregate_tsteps <- function(datain, new_tstep, qc_flags, qc_name){
  
  #Number of time steps to aggregate
  ntsteps <- (new_tstep * 60*60) / datain$timestepsize 
  
  #Variable names
  vars <- datain$vars
  
  #Initialise new data.frame
  new_data <- matrix(Sprd_MissingVal, ncol=ncol(datain$data), nrow=nrow(datain$data)/ntsteps)
  colnames(new_data) <- colnames(datain$data)
  new_data <- as.data.frame(new_data)
  
  #Indices for aggregating
  seq <- seq(from=1, by=ntsteps, length.out=nrow(new_data))
  
  #Flags for observed 
  good_data <- c(qc_flags$QC_measured)
  
  #Loop through variables
  for(k in 1:length(vars)){
    
    method <- datain$aggr_method[vars[k]]
    
    #QC variable: calculate fraction observed & good quality gapfilling
    if(grepl(qc_name, substr(vars[k], nchar(vars[k])-(nchar(qc_name)-1), nchar(vars[k])))){
      
      if(is.na(method)){
        
        #Calculate fraction
        new_data[,vars[k]] <- sapply(seq, function(x) qc_frac(datain$data[x:(x+ntsteps-1),
                                                                          vars[k]], good_data))
      } else {
        
        stop(paste("Aggregation method for QC variable", vars[k], "not set",
                   "correctly. Method must be set to NA for QC variables,",
                   "please amend output variable file."))
      }
      
      
      #Other variables: average or sum up  
    } else {
      
      #Set missing values to NA before aggregating
      aggr_data <- datain$data[,vars[k]]
      aggr_data[aggr_data==Sprd_MissingVal] <- NA
      
      if(method=="mean"){
        
        aggr_data <- sapply(seq, function(x) mean(aggr_data[x:(x+ntsteps-1)], na.rm=FALSE))
        
      } else if(method=="sum"){
        
        aggr_data <- sapply(seq, function(x) sum(aggr_data[x:(x+ntsteps-1)], na.rm=FALSE))    
        
      } else {
        
        stop(paste("Aggregation method for variable", vars[k], "not recognised.",
                   "Method must be set to 'mean' or 'sum', please amend output variable file."))
      }
      
      
      #Set missing values back to Sprd_missingval
      aggr_data[is.na(aggr_data)] <- Sprd_MissingVal
      
      #Write to data frame
      new_data[,vars[k]] <- aggr_data
      
    }  
  } #vars
  
  
  #Finally, extract correct time steps
  
  new_start <- datain$time[seq,1]
  new_end   <- datain$time[seq+(ntsteps-1),2]
  
  #Collate to new dataframe
  new_time <- cbind(new_start, new_end)
  colnames(new_time) <- colnames(datain$time)
  
  
  #Replace data and time step info
  datain$data <- new_data
  datain$time <- new_time

  datain$ntsteps <- nrow(datain$time)
  datain$timestepsize <- datain$timestepsize * ntsteps
  
  
  #New QC flag descriptions
  
  qc_flags$qc_info <- "Fraction (0-1) of aggregated time steps that were observed"
  
  #Collate
  outs <- list(data=datain, qc_flags=qc_flags)
  
  return(outs)
  
}

#-----------------------------------------------------------------------------

#' Calculates fraction of good quality gapfilled and observed data
#' for aggregated time steps
#' @export
qc_frac <- function(data, good_data){
  
  good_frac <- which(sapply(good_data, function(x) data==x))
  good_frac <- length(good_frac) / length(data)
  
  #If any data missing, set QC flag to missing
  if(any(data==Sprd_MissingVal)) good_frac <- Sprd_MissingVal
  
  return(good_frac)
}




