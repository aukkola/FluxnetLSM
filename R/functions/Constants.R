# Constants.R
#
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)
#
zeroC = 273.15
SprdMissingVal = -9999 # missing value in spreadsheet
NcMissingVal = -9999 # missing value in created netcdf files
QCmeasured = 0
QCnotmeasured = c(1, 2, 3)  #1: good quality gapfill, 2: medium, 3: poor
QCmissing = NcMissingVal


#-----------------------------------------------------------------------------

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
    CheckError(paste("Cannot find all essential variables (missing: ", 
                     paste(var_names[failed_ind[which(essential_vars[failed_ind])]], collapse=","),
                     "), aborting", sep=""))
  }
  
  #Check if no desired evaluation variables present, abort if so
  if(all(preferred_vars[failed_ind])){
    CheckError(paste("Cannot find any evaluation variables (missing: ", 
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
    CheckError("Error determining column indices for time and other variables, two or more variables overlap")
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
                all_names=all_vars, classes=columnclasses, 
                failed_vars=failed_vars)
  
  return(tcols)

}

#-----------------------------------------------------------------------------

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

# Gets index of a variable 
varIndex = function(varname){
	idx = 0
	for(v in 1:length(tcols$names)){
		if(varname == tcols$names[v]){
			idx = v	
		}
	}
	return(idx)	
}

#-----------------------------------------------------------------------------

# #Rename Fluxnet variables to ALMA convention
rename_vars <- function(vars_present, all_vars){

  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  #Replace names with corresponding ALMA variable names
  renamed_vars <- all_vars$ALMA_variable[ind_present]

  return(renamed_vars)
  
}

#-----------------------------------------------------------------------------

#Retrieves original and target variable units
retrieve_units <- function(vars_present, all_vars){
  
  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  original_units <- all_vars$Fluxnet_unit[ind_present]
  target_units   <- all_vars$ALMA_unit[ind_present]
  
  units <- list(original_units=original_units, target_units=target_units)
  
  return(units)
}

#-----------------------------------------------------------------------------

#Retrieve variable longnames to be written in NetCDF
retrieve_atts <- function(vars_present, all_vars){
  
  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  attributes <- cbind(all_vars$Fluxnet_variable, 
                      all_vars$Longname,
                      all_vars$CF_name)[ind_present,]
      
  colnames(attributes) <- c("Fluxnet_variable", "Longname", "CF_name")
  
  return(attributes)
}

#-----------------------------------------------------------------------------

#Retrieve variable categories to divide into met and flux data
retrieve_categories <- function(vars_present, all_vars){
  
  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  cat <- all_vars$Category[ind_present]
  names(cat) <- all_vars$ALMA_variable[ind_present]
  
  return(cat)
}

#-----------------------------------------------------------------------------

#Retrieve acceptable variable ranges
retrieve_ranges <- function(vars_present, all_vars){

  #Find index for fluxnet variables present in file
  ind_present <- sapply(vars_present, function(x) which(all_vars$Fluxnet_variable==x))
  
  #Read min and max values
  range_min <- all_vars$Data_min[ind_present] 
  range_max <- all_vars$Data_max[ind_present] 
  
  #Combine
  var_ranges <- rbind(range_min, range_max)
  colnames(var_ranges) <- vars$ALMA_variable[ind_present]  
  
  return(var_ranges)
}






