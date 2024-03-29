# Constants.R
#
# Functions to find variables and their attributes
# from Fluxnet csv-file
#
# author: Anna Ukkola UNSW 2017
#
# TODO: Check and merge back in to palsR

#-----------------------------------------------------------------------------

#' Finds variables present in input file
#' @return list of variables and their attributes
# Variable names in spreadsheet to be processed:
findColIndices <-  function(fileinname, var_names, var_classes, 
                            essential_vars, preferred_vars,
                            time_vars, dset_vars, site_log, datasetname, ...) {
  
  #CSV files in Fluxnet2015 Nov '16 release do not follow a set template
  #and not all files include all variables
  #This function finds which desired variables are present in file
  #and creates a column name and class vector for reading in data
   
  #Read headers (variable names) of CSV/NetCDF file
  
  #NetCDF file
  if (datasetname == "OzFlux") {
  
    library(ncdf4) 
    
    #Open file handle
    nc <- nc_open(fileinname)
    
    #Get variable names
    headers <- c(names(nc$var), names(nc$dim))
    
    nc_close(nc)
    
  #CSV file
  } else {
    headers <- read.csv(fileinname, header=FALSE, nrows=1)
  }
  
  #Find file header indices corresponding to desired variables
  #(returns an empty integer if cannot find variable)
  ind <- sapply(as.character(var_names), function(x) which(headers==x))
  
  #List variables that could not be found in file (instances where ind length equals 0)
  #and remove failed variables from var_name and var_classes vectors
  failed_ind  <- which(sapply(ind, length)==0)
  
  #List failed variables
  failed_vars <- var_names[failed_ind]
  
  
  # #Add an exception for VPD and relative humidity
  # #Some sites have one or the other available
  # #Missing RH
  # if (any(failed_vars %in% dset_vars$relhumidity)) {
  #   
  #   #Check if VPD available, if so don't worry about missing RH
  #   if (any(as.matrix(headers) %in% dset_vars$vpd)) {
  #     
  #     #Remove RH from failed vars list
  #     ind <- which(failed_vars %in% dset_vars$relhumidity)
  #     
  #     failed_ind <- failed_ind[-ind]
  #     failed_vars <- failed_vars[-ind]
  #     
  #   }
  #   
  # }
  # #Missing VPD
  # if (any(failed_vars %in% dset_vars$vpd)) {
  #   #Check if relative humidity available
  #   
  #   #Check if RH available, if so don't worry about missing VPD
  #   if (any(as.matrix(headers) %in% dset_vars$relhumidity)) {
  #     
  #     #Remove VPD from failed vars list
  #     ind <- which(failed_vars %in% dset_vars$vpd)
  #     
  #     failed_ind  <- failed_ind[-ind]
  #     failed_vars <- failed_vars[-ind]
  #     
  #   }
  #   
  # }
  # 
  
  #If found variables not present in file
  if(length(failed_ind) > 0){

    #Check if any essential meteorological variables missing, abort if so
    if(any(!is.na(essential_vars[failed_ind]))){
      
      #Add an exception for VPD and relative humidity
      #Some sites have one or the other available, can use either to derive Qair
      
      essential_failed <- var_names[failed_ind[which(!is.na(essential_vars[failed_ind]))]]
      
      stop <- TRUE
      
      #If RH only missing variable: Check if VPD available, if so don't worry about missing RH
      if (any(failed_vars %in% dset_vars$relhumidity) & length(essential_failed) == 1) {
        #If found VPD, don't stop
        if (any(as.matrix(headers) %in% dset_vars$vpd)) stop <- FALSE
        
      }
      
      
      #If VPD only missing variable: Check if RH available, if so don't worry about missing VPD
      if (any(failed_vars %in% dset_vars$vpd) & length(essential_failed) == 1) {
        #If found RH, don't stop
        if (any(as.matrix(headers) %in% dset_vars$relhumidity)) stop <- FALSE
        
      }
      
      
      if (stop) {
        error <- paste("Cannot find all essential variables in input file (missing: ", 
                       paste(essential_failed, collapse=","),
                       "), aborting", sep="")
        stop_and_log(error, site_log)
        
      }
    }
    
    #Check if no desired evaluation variables present, abort if so
    if(all(preferred_vars[failed_ind]==TRUE)){
      error <- paste("Cannot find any evaluation variables in input file (missing: ", 
                     paste(var_names[failed_ind[which(!is.na(essential_vars[failed_ind]))]], collapse=","),
                     "), aborting", sep="")
      stop_and_log(error, site_log)
    }

    
    #Remove variables that are not present from variable list
    var_names   <- var_names[-failed_ind]
    var_classes <- var_classes[-failed_ind]
    
  }
  
  
  #Find time information
  #Returns time variables and column classes
  time_info <- findTimeInfo(time_vars, headers, site_log, datasetname)
  
  #Check that column indices for time and other variables don't overlap
  if(length(intersect(time_info$ind, ind)) > 0){
    error <- paste("Error determining column indices for time and other", 
                   "variables, two or more variables overlap [ function:", 
                   match.call()[[1]], "]")
    stop_and_log(error, site_log)
  }
  
  #Combine column classes for time and other variables
  #If colClass is set to NULL, R won't read column unnecessarily
  columnClasses <- time_info$classes
  columnClasses[unlist(ind)] <- var_classes

  
  #Combine names of time and other variables and reorder to match CSV file
  #(used later to check that file was read correctly)
  all_vars <- c(var_names, time_info$names)
  all_vars <- all_vars[order(c(unlist(ind), time_info$ind), all_vars)]
   
  #Reorder variable names so matches the order in CSV file
  var_names <- var_names[order(unlist(ind), var_names)]  
  
  #List outputs
  tcols <- list(names=var_names, time_names=time_info$names, 
                all_names=all_vars, classes=columnClasses, 
                failed_vars=failed_vars)
  
  return(tcols)

}

#-----------------------------------------------------------------------------

#' Extract time stamp information
#' @return time stamp variables
findTimeInfo <- function(time_vars, headers, site_log, datasetname){
    
  #Find index of time variables
  ind <- sapply(time_vars, function(x) which(headers==x))
  
  #Check that found correct variables
  if(length(ind)!=length(time_vars)) {
    error <- paste("Cannot find time stamp variables in input file.",
                    "Looking for variables", paste(time_vars, collapse=" and "))
    stop_and_log(error, site_log)
  }
  
  #Initialise column class vector (sets class to NULL unless overwritten below)
  #If colClass is set to NULL, R won't read column
  columnclasses <- rep(list(NULL), length(headers))
  
  #Replace with correct variable class if found variables
  
  if(datasetname=="LaThuile"){
    columnclasses[unlist(ind)] <- "numeric"
  } else {
    columnclasses[unlist(ind)] <- "character"
  }
  
  #Reorder var_names and var_classes so matches the order in CSV file
  time_vars <- time_vars[order(unlist(ind), time_vars)]
  
  #List outputs
  tcols <- list(names=time_vars, classes=columnclasses, ind=ind)
  
  return(tcols)
}

#-----------------------------------------------------------------------------

#' Retrieves variable information
#' @return variable information
retrieve_varinfo <- function(vars_present, all_vars, attribute){
  
  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  #Check for duplicates (if Fluxnet variable being processed more than once)
  if(any(duplicated(ind_present))){
    ind_present <- unlist(remove_duplicates(ind_present))
  } 
  
  col_ind <- which(colnames(all_vars)==attribute)
  var_info <- all_vars[ind_present, col_ind]
  names(var_info) <- all_vars$Fluxnet_variable[ind_present]  
  
  return(var_info)
}

#-----------------------------------------------------------------------------

#' Retrieves original and target variable units
#' @return original and target units
retrieve_units <- function(vars_present, all_vars){
  
  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  #Check for duplicates (if Fluxnet variable being processed more than once)
  if(any(duplicated(ind_present))){
    ind_present <- unlist(remove_duplicates(ind_present))
  } 
  
  #Retrieve original and taret units, and list
  original_units <- all_vars$Fluxnet_unit[ind_present]
  target_units   <- all_vars$Output_unit[ind_present]
  
  #Set names
  names(original_units) <- all_vars$Fluxnet_variable[ind_present]  
  names(target_units) <- all_vars$Fluxnet_variable[ind_present]  
  
  units <- list(original_units=original_units, target_units=target_units)
  
  return(units)
}

#-----------------------------------------------------------------------------

#' Retrieve variables longnames to be written in NetCDF
#' @return Long name attributes
retrieve_atts <- function(vars_present, all_vars){
  
  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  #Check for duplicates (if Fluxnet variable being processed more than once)
  if(any(duplicated(ind_present))){
    ind_present <- unlist(remove_duplicates(ind_present))
  } 
  
  attributes <- cbind(all_vars$Fluxnet_variable, 
                      all_vars$Longname,
                      all_vars$Standard_name,
                      all_vars$CMIP_short_name)[ind_present,]
      
  colnames(attributes) <- c("Fluxnet_variable", "Longname", "CF_name",
                            "CMIP_name")
  
  return(attributes)
}


#-----------------------------------------------------------------------------

#' Retrieves acceptable variable ranges
#' @return variable ranges
retrieve_ranges <- function(vars_present, all_vars){

  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  #Check for duplicates (if Fluxnet variable being processed more than once)
  if(any(duplicated(ind_present))){
    ind_present <- unlist(remove_duplicates(ind_present))
  } 

  #Read min and max values
  range_min <- all_vars$Data_min[ind_present] 
  range_max <- all_vars$Data_max[ind_present] 
  
  #Combine
  var_ranges <- rbind(range_min, range_max)
  colnames(var_ranges) <- all_vars$Fluxnet_variable[ind_present]  
  
  return(var_ranges)
}


#-----------------------------------------------------------------------------

#' Removes duplicate indices if a Fluxnet variable is processed more than once
#' @return duplicate indices
remove_duplicates <- function(indices){
  
  #Determine how many variables duplicated
  vars_duplicated <- unique(names(indices)[duplicated(indices)])

  for(k in 1:length(vars_duplicated)){
    
    #Find indices for duplicated variable
    ind <- which(names(indices)==vars_duplicated[k])
    
    #Take the n-th element for each occurrence
    for(n in 1:length(ind)) indices[ind[n]][[1]] <- indices[ind[n]][[1]][n]
    
  }
  
  return(indices)
  
}


#-----------------------------------------------------------------------------

#' Duplicates columns in Fluxnet data if a variable is being processes multiple times
#' @return data with duplicated columns
duplicate_columns <- function(data, vars){
  
  #Find variables that are duplicated
  ind_duplicate <- which(duplicated(vars))
  
  for(k in ind_duplicate){
    
    #Find corresponding column (only use first instance)
    ind_column <- which(colnames(data)==vars[k])[1]
    
    #Add column to dataframe and rearrange column order
    #drop=False ensures original column name is kept
    #although R automatically renames duplicate column names from e.g. RH to RH.1
    data <- cbind(data, data[,ind_column, drop=FALSE])[, append(1:ncol(data), ncol(data) + 1, after=k-1)]
    
    }
  
  return(data)
 
}








