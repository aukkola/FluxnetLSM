# Model_initialisations.R
#
# Functions to initialise model attributes to
# be written to NetCDF files
#
# TODO: add more models and attributes, including
# CABLE patchfrac

#' Finds model-specific parameters to be written 
#' to output NetCDF files 
#' @export
initialise_model <- function(model, site_info){
  
  if(is.na(model)){
    
    outs <- NA
    
  } else if(model=="CABLE"){
    
    #PFT (set NetCDF variable name, here iver)
    pft        <- site_info$out[paste(model, "_PFT", sep="")]
    names(pft) <- "iveg"
    
    #Collate to list (this will allow more params to be added later)
    outs <- list(pft)
    
    
  ### Template for adding a new model ###
  #} else if(model=="My_model"){
    
  #     #PFT (set NetCDF variable name, here iver)
  #     pft        <- site_info$out[paste(model, "_PFT", sep="")]
  #     names(pft) <- "my_pft_variable_name"
  #     
  #     #Collate to list (this will allow more params to be added later)
  #     outs <- list(pft)
  
    
  #Could not recognise model name    
  } else {
    
    stop(paste("Cannot recognise model name: ", model, 
               ". Please modify. See code at R/Model_initialisations.R",
               " for available options.", sep=""))
  }
  
  return(outs)
  
}