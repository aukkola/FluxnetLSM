# Model_initialisations.R
#
# Functions to initialise model attributes to
# be written to NetCDF files
#
# TODO: add more models and attributes, including
# CABLE patchfrac

#' Finds model-specific parameters to be written 
#' to output NetCDF files 
initialise_model <- function(model, site_info){
  
  if(is.na(model)){
    
    outs <- NA
    
  } else if(model=="CABLE"){
    
    #Variable: PFT (set NetCDF variable name (varname), value to read from site_info (varvalue),
    #long variable name (longname), and units (units))
    pft        <- list(varname  = "iveg",
                       varvalue = site_info[paste(model, "_PFT", sep="")],
                       longname = "CABLE veg type",
                       units    = "-"
                       )
    
    #Collate to list (this will allow more params to be added later)
    outs <- list(pft)
    
    
  ### Template for adding a new model ###
  #} else if(model=="My_model"){
    
  #     #PFT (specify the variable name as set in Site_metadata.csv,
  #     #and the variable name you wish to be written in the NetCDF files)
  #     pft        <- list(varname  = "model_variable_name",
  #                        varvalue = site_info[paste(model, "_PFT", sep="")],
  #                        longname = "variable long name",
  #                        units    = "variable units"
  #                        )
  #
  #     #You can add multiple parameters in this fashion
  #     #Collate these to this list and they will be automatically
  #     #written to the output file:
  #     outs <- list(pft)
  
    
  #Could not recognise model name    
  } else {
    
    stop(paste("Cannot recognise model name: ", model, 
               ". Please modify. See code at R/Model_initialisations.R",
               " for available options.", sep=""))
  }
  
  return(outs)
  
}