#
# 
#




initialise_model <- function(model){
  
  if(model=="CABLE"){
    
    pft_name="iveg"
    

  ### Template for adding a new model ###
  #} else if(model=="My_model"){
    
    #pft_name <- "my_pft"  
    
    
    
  #Could not recognise model name    
  } else {
    
    stop(paste("Cannot recognise model name: ", model, 
               ". Please modify. See code at R/Model_initialisations.R",
               " for available options.", sep=""))
  }
  
  
  
  
  return(pft_name)
  
  
  
}