# Constants.R
#
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)
#
zeroC = 273.15
SprdMissingVal = -9999 # missing value in spreadsheet
NcMissingVal = -9999 # missing value in created netcdf files
QCmeasured = 1
QCnotmeasured = 0
QCmissing = NcMissingVal


#-----------------------------------------------------------------------------

# Variable names in spreadsheet to be processed:
findColIndices = function(file) {
  
  #CSV files in Fluxnet2015 Nov '16 release do not follow a set template
  #and not all files include all variables
  #This function finds which desired variables are present in file
  #and creates a column name and class vector for reading in data
  
  #Desired variables (refer to Fluxnet2015 documentation for full variable descriptions; 
  #http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/)
  var_names   <- c("TIMESTAMP_START", "TIMESTAMP_END",  #time start and end
                   "TA_F_MDS", "TA_F_MDS_QC",           #air temp and qc
                   "SW_IN_F_MDS", "SW_IN_F_MDS_QC",     #swdown and qc
                   "LW_IN_F_MDS", "LW_IN_F_MDS_QC",     #lwdown and qc
                   "PA",                                #atmospheric pressure
                   "P",                                 #precipitation
                   "WS",                                #wind speed
                   "NETRAD", "NETRAD_QC",               #net radiation and qc
                   "CO2_F_MDS", "CO2_F_MDS_QC",         #CO2 concentration and qc
                   "G_F_MDS", "G_F_MDS_QC",             #ground heat flux and qc      
                   "LE_F_MDS", "LE_F_MDS_QC",           #latent heat and qc
                   "LE_CORR", "LE_CORR_JOINTUNC",       #corrected latent heat and uncertainty
                   "H_F_MDS", "H_F_MDS_QC",             #sensible heat and qc
                   "H_CORR", "H_CORR_JOINTUNC",         #corrected sensible heat and uncertainty
                   "NEE_CUT_REF", "NEE_CUT_REF_QC",     #NEE constant ustar threshold, qc 
                   "NEE_CUT_REF_JOINTUNC",              #and uncertainty
                   "NEE_VUT_REF", "NEE_VUT_REF_QC",     #NEE variable ustar threshold, qc
                   "NEE_VUT_REF_JOINTUNC",              #and uncertainty
                   "GPP_DT_CUT_REF", "GPP_DT_CUT_SE",   #GPP daytime constant ustar threshold and std error
                   "GPP_DT_VUT_REF", "GPP_DT_VUT_SE")   #GPP daytime variable ustar threshold and std error
  
  #Variable classes to be read from CSV
 	var_classes<- c('character', 'character',       #time start and end
                  'numeric', 'integer',           #air temp and qc
 		              'numeric', 'integer',           #swdown and qc
 		              'numeric', 'integer',           #lwdown and qc
                  'numeric',                      #atmospheric pressure
 		              'numeric',                      #precipitation
 		              'numeric',                      #wind speed
 		              'numeric', 'integer',           #net radiation and qc
 		              'numeric', 'integer',           #CO2 concentration and qc
 		              'numeric', 'numeric',           #ground heat flux and qc    CANNOT READ IN as integer, not sure why !!!!!
 		              'numeric', 'integer',           #latent heat and qc
 		              'numeric', 'numeric',           #corrected latent heat and uncertainty
 		              'numeric', 'integer',           #sensible heat and qc
 		              'numeric', 'numeric',           #corrected sensible heat and uncertainty
 		              'numeric', 'integer',           #NEE constant ustar threshold, qc
 		              'numeric',                      #and uncertainty
 		              'numeric', 'integer',           #NEE variable ustar threshold, qc
 		              'numeric',                      #and uncertainty
 		              'numeric', 'numeric',           #GPP daytime constant ustar threshold and std error
 		              'numeric', 'numeric')           #GPP daytime variable ustar threshold and std error
 		              
  

  #Read headers (variable names) of CSV file
  headers <- read.csv(file, header=FALSE, nrows=1)

  #Find file header indices corresponding to desired variables
  #(returns an empty integer if cannot find variable)
  ind <- sapply(var_names, function(x) which(headers==x))

  #List variables that could not be found in file (instances where ind length equals 0)
  #and remove failed variables from var_name and var_classes vectors
  failed_ind  <- which(sapply(ind, length)==0)
  failed_vars <- var_names[failed_ind]
  
  if(length(failed_ind) > 0){
    var_names   <- var_names[-failed_ind]
    var_classes <- var_classes[-failed_ind]
  }

  #Initialise column class vector (sets class to NULL unless overwritten below)
  #If colClass is set to NULL, R won't read column
  columnclasses <- rep(list(NULL), length(headers))

  #Replace with correct variable class found variables
  columnclasses[unlist(ind)] <- var_classes
  
  #Reorder var_names and var_classes so matches the order in CSV file
  var_names <- var_names[order(unlist(ind), var_names)]
  #var_classes <- var_classes[order(unlist(ind), var_classes)]
  
  #List outputs
  tcols <- list(names=var_names, classes=columnclasses, failed_vars=failed_vars)
  
  return(tcols)

}

#-----------------------------------------------------------------------------

#Reads CSV file, skips unwanted columns
read_flux_csv <- function(file, tcols=tcols)
{
  
  data <- read.csv(file, colClasses=tcols$classes, header=TRUE)
  
  #Sanity check, does variable order in file match that specified in tcols?
  if(any(colnames(data)!=tcols$names)) stop("Something wrong with variable ordering (in Constants.R)")

  return(data)
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




