# Constants.R
#
# Functions to find variables and their attributes
# from Fluxnet csv-file
#
# author: Anna Ukkola UNSW 2017
#
# TODO: Check and merge back in to palsR

Sprd_MissingVal = -9999 # missing value in spreadsheet
Nc_MissingVal = -9999 # missing value in created netcdf files
QC_measured = 0
QC_gapfilled = c(1, 2, 3, 4)  #1: good quality gapfill, 2: medium, 3: poor, 4: ERA gapfilled



#Initialise site log
site_log <- vector(length=8)
names(site_log) <- c("Site_code", "Processed", "Errors", 
                     "Warnings", "No_files", "Met_files", "Flux_files", 
                     "Excluded_eval")

site_log["Site_code"] <- site_code
site_log["Warnings"]  <- ''
site_log[c(3, 5:8)]  <- NA


#-----------------------------------------------------------------------------

#' Finds variables present in input file
#' @return list of variables and their attributes
#' @export
# Variable names in spreadsheet to be processed:
findColIndices = function(fileinname, var_names, var_classes, 
                          essential_vars, preferred_vars,
                          time_vars) {
  
  #CSV files in Fluxnet2015 Nov '16 release do not follow a set template
  #and not all files include all variables
  #This function finds which desired variables are present in file
  #and creates a column name and class vector for reading in data
   
  #Read headers (variable names) of CSV file
  headers <- read.csv(fileinname, header=FALSE, nrows=1)
  
  #Find file header indices corresponding to desired variables
  #(returns an empty integer if cannot find variable)
  ind <- sapply(var_names, function(x) which(headers==x))
  
  #List variables that could not be found in file (instances where ind length equals 0)
  #and remove failed variables from var_name and var_classes vectors
  failed_ind  <- which(sapply(ind, length)==0)
  failed_vars <- var_names[failed_ind]
  

  #Check if any essential meteorological variables missing, abort if so
  if(any(essential_vars[failed_ind])){
    CheckError(paste("Cannot find all essential variables in input file (missing: ", 
                     paste(var_names[failed_ind[which(essential_vars[failed_ind])]], collapse=","),
                     "), aborting", sep=""))
  }
  
  #Check if no desired evaluation variables present, abort if so
  if(all(preferred_vars[failed_ind])){
    CheckError(paste("Cannot find any evaluation variables in input file (missing: ", 
                     paste(var_names[failed_ind[which(essential_vars[failed_ind])]], collapse=","),
                     "), aborting", sep=""))
  }
  
  
  #Remove variables that are not present from variable list
  if(length(failed_ind) > 0){
    var_names   <- var_names[-failed_ind]
    var_classes <- var_classes[-failed_ind]
  }

  
  #Find time information
  #Returns time variables and column classes
  time_info <- findTimeInfo(time_vars, headers)
  
  #Check that column indices for time and other variables don't overlap
  if(length(intersect(time_info$ind, ind)) > 0){
    CheckError(paste("Error determining column indices for time and other", 
               "variables, two or more variables overlap [ function:", match.call()[[1]], "]"))
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
#' @export
findTimeInfo <- function(time_vars, headers){
    
  ind <- sapply(time_vars, function(x) which(headers==x))
  
  if(length(ind)!=2) {
    CheckError(paste("Cannot find time stamp variables in input file. Looking for variables", 
                     paste(time_vars, collapse=" and ")))
  }
  
  #Initialise column class vector (sets class to NULL unless overwritten below)
  #If colClass is set to NULL, R won't read column
  columnclasses <- rep(list(NULL), length(headers))
  
  #Replace with correct variable class if found variables
  columnclasses[unlist(ind)] <- "character"
  
  #Reorder var_names and var_classes so matches the order in CSV file
  time_vars <- time_vars[order(unlist(ind), time_vars)]
  
  #List outputs
  tcols <- list(names=time_vars, classes=columnclasses, ind=ind)
  
  return(tcols)
}

#-----------------------------------------------------------------------------

#' Retrieves variable information
#' @return variable information
#' @export
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
#' @export
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
  
  units <- list(original_units=original_units, target_units=target_units)
  
  return(units)
}

#-----------------------------------------------------------------------------

#' Retrieve variables longnames to be written in NetCDF
#' @return Long name attributes
#' @export
retrieve_atts <- function(vars_present, all_vars){
  
  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  #Check for duplicates (if Fluxnet variable being processed more than once)
  if(any(duplicated(ind_present))){
    ind_present <- unlist(remove_duplicates(ind_present))
  } 
  
  attributes <- cbind(all_vars$Fluxnet_variable, 
                      all_vars$Longname,
                      all_vars$Standard_name)[ind_present,]
      
  colnames(attributes) <- c("Fluxnet_variable", "Longname", "CF_name")
  
  return(attributes)
}


#-----------------------------------------------------------------------------

#' Retrieves acceptable variable ranges
#' @return variable ranges
#' @export
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
#' @export
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
#' @export

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








