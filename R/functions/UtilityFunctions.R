# UtilityFuntions.R
#
# Utility functions for PALS R package
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#

# Function for crashing semi-gracefully:
CheckError = function(errtext,errcode='U1:'){
	if(errtext != 'ok'){
    		# Additionally report command line call
		calltext = paste(commandArgs(),collapse=' ')
		alltext = paste(errtext,calltext)
		# If error, write to std error
		stop(alltext, call. = FALSE)
	}
}


#-----------------------------------------------------------------------------


# Strips path from filename: 
stripFilename = function(fpath) {
  fsplit = strsplit(fpath,'/')
  fcharvec = as.character(fsplit[[1]])
  fname = fcharvec[length(fcharvec)]
  return(fname)
}



