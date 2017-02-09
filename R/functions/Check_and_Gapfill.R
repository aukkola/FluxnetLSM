#
#
# Needs comments
#
# TODO: Check and merge back in to palsR
#

# Check the existence of missing values:
CheckDataGaps <- function(datain, missing_val=SprdMissingVal,
threshold, min_yrs,
essential_met, preferred_eval){
    
    #Checks the existence of data gaps and determines which
    #years should be outputted depending on the percentage of data gaps
    #(set by thereshold) and the number of consecutive years available
    #(at least number set by min_yrs)
    
    library(R.utils)
    
    gaps_found <- apply(datain$data, MARGIN=2, function(x) any(x==missing_val))
    
    #Find indices for each start and end of year
    secs_per_day   <- 60*60*24
    tsteps_per_day <- secs_per_day/datain$timestepsize
    tsteps_per_yr  <- datain$daysPerYr * tsteps_per_day
    
    end   <- cumsum(tsteps_per_yr)
    start <- end - tsteps_per_yr + 1
    
    
    gap_length <- list()
    #Check how many missing values per year, per variable
    for(k in 1:length(gaps_found)){
        
        data <- datain$data[,k]
        
        #Calculate the percentage of data missing each year
        gap_length[[k]] <- sapply(1:length(start), function(x)
        length( which(data[start[x]:end[x]] == missing_val)) /
        length(start[x]:end[x]) * 100)
    }
    
    names(gap_length) <- names(gaps_found)
    
    
    ### Check that essential variables have at least one common year of data
    ### without too many gaps
    ### and the year has one or more evaluation variables available
    
    essential_ind <- sapply(essential_met, function(x) which(names(gap_length) == x))
    preferred_ind <- sapply(preferred_eval, function(x) which(names(gap_length) == x))
    
    yr_keep <- vector()
    
    #Loop through years
    for(k in 1:length(start)){
        
        #Extract gap lengths for the year
        gaps <- sapply(gap_length, function(x) x[k])
        
        #If any essential variables, or all evaluation variables,
        #have too many missing values, skip year
        if(any(gaps[essential_ind] > threshold) | all(gaps[preferred_ind] > threshold))
        {
            yr_keep[k] <- FALSE
            
            #Else, process year
        } else {
            yr_keep[k] <- TRUE
        }
        
    }
    
    
    #Indices of year(s) to keep
    yr_ind <- which(yr_keep)
    
    
    ### If no years fulfilling criteria, abort. ###
    if(all(!yr_keep) | length(yr_ind) < min_yrs){
        CheckError("No years to process, too many gaps present or available time period too short. Aborting.")
    }
    
    
    #Are all years consecutive? If not, need to split site to
    #multiple files. Determine which years are consecutive
    #and how many files need to create (variables 'consec'
    #tells which years should go in which file)
    
    ## only one year
    if(length(yr_ind)==1){
        
        consec <- 1
        
        #determine start and end of time series
        tstart <- start[yr_ind]
        tend   <- start[yr_ind]
        
        ## two or more years that are not consecutive
    } else if (any(diff(yr_ind) > 1)) {
        
        #Find non-consecutive instances
        breaks <- seqToIntervals(yr_ind)
        
        #Create an index vector for grouping years
        consec <- vector()
        tstart <- vector()
        tend   <- vector()
        
        for(c in 1:(nrow(breaks))){
            
            ind_consec <- rep(c, times=breaks[c,2] - breaks[c,1] + 1)
            
            
            #If number of consecutive years less than min_yrs
            if(length(ind_consec) < min_yrs){
                
                #remove these years from yr_ind
                yr_ind <- yr_ind[-(which(yr_ind==breaks[c,1]):which(yr_ind==breaks[c,2]))]
                
                
                #If all years removed because all available periods shorter than min_yrs, abort
                if(length(yr_ind)==0){
                    CheckError(paste("No years to process, all available time period too short (as set by min_yrs).
                    Aborting [ function:", match.call()[[1]], "]"))
                }
                
                
                #At least min_yrs number of available years
            } else {
                
                consec <- append(consec, ind_consec)
                
                #determine start and end of time series
                tstart <- append(tstart, start[breaks[c,1]])
                tend   <- append(tend, end[breaks[c,2]])
            }
        }
        
        
        ## multiple years but all consecutive
    } else {
        
        consec <- rep(1, length(yr_ind))
        
        #determine start and end of time series
        tstart <- start[yr_ind[1]]
        tend   <- start[yr_ind[length(yr_ind)]]
        
    }
    
    
    
    return(list(gap_length=gap_length, yr_keep=yr_ind, consec=consec, tseries_start=tstart, tseries_end=tend))
}



#-----------------------------------------------------------------------------

GapfillMet <- function(datain, era_data, era_vars,
tair_units, vpd_units,
missing_val){
    
    #ERAinterim estimates are provided for TA, SW_in,
    #LW_IN, VPD, PA, P and WS
    #Gapfill observed met data using these estimates
    #And create QC variables if not available to
    #save information about which time steps ERA-gapfilled
    
    #Check that Fluxnet and ERA data dimensions agree
    if(nrow(datain) != nrow(era_data)) {
        CheckError(paste("Observed flux data and ERAinterim data dimensions
        do not match, aborting [ function:", match.call()[[1]], "]"))
    }
    
    
    #List available ERA variables
    avail_era  <- era_vars[which(!is.na(era_vars))]
    avail_flux <- names(avail_era)
    
    
    #Initialise list for new QC variables
    #created if no existing QC flags for a variable
    new_qc   <- vector()
    qc_names <- vector()
    
    #Loop through available variables
    for(k in 1:length(avail_era)){
        
        #Find flux data column index and ERAinterim column index
        #for variable being processed
        flx_col <- which(colnames(datain)==avail_flux[k])
        
        era_name <- era_vars[which(names(era_vars)==avail_flux[k])] #corresponding ERA variable name
        era_col <- which(colnames(era_data)==era_name)
        
        #If gaps in met data variable, gapfill
        if(any(datain[,flx_col]==missing_val)){
            
            #Find missing values to fill
            missing <- which(datain[,flx_col]==missing_val)
            
            
            ### Relative humidity ###
            #If Flux variable relative humidity, but ERA variable VPD, convert
            if(avail_flux[k] == "RelH" & era_name=="VPD_ERA"){
                
                era_tair_col <- which(colnames(era_data)=="TA_ERA")
                
                if(length(era_tair_col) == 0){
                    CheckError("Cannot find ERAinterim air temperature data.
                    Cannot convert ERA VPD to relative humidity")
                }
                
                # Convert ERAinterim VPD to relative humidity
                # Assuming that ERA vpd and tair units the same as observed units
                era_rh <-  VPD2RelHum(VPD=era_data[,era_col], airtemp=era_data[,era_tair_col],
                vpd_units=vpd_units, tair_units=tair_units)
                
                #Gapfill
                datain[missing,flx_col] <- era_rh[missing]
                
                
                ### Other variables ###
                #ERAinterim equivalent should exist, use that directly
            } else {
                
                #Gapfill
                datain[missing, flx_col] <- era_data[missing, era_col]
                
            }
            
            
            ## Set QC flags to "4" for time steps filled with ERA data ##
            #Find corresponding qc variable, if available
            qc_col <- which(colnames(datain)==paste(avail_flux[k], "_qc", sep=""))
            
            
            #Replace era gap-filled time steps with "4"
            if(length(qc_col) > 0){
                datain[missing,qc_col] <- 4
                
            } else {
                message(paste("Could not find QC flag for variable", 
                avail_flux[k], "gap-filled with ERA data. Creating QC flag."))
                
                #Initialise QC flag with zeros and replace with "4" where gap-filled
                qc_var <- rep(0, length(datain[,flx_col]))
                qc_var[missing] <- 4
                
                #Create name for QC variable and save data and name to data.frame
                qc_names <- cbind(qc_names, paste(avail_flux[k],
                "_qc", sep=""))
                new_qc <- cbind(new_qc, qc_var)
                
            }
            
            
        } #if
    } #vars
    
    #Assign new QC variable names to data frame column names
    if(length(new_qc) > 0){ colnames(new_qc) <- qc_names}
    
    
    return(list(datain=datain, new_qc=new_qc))
    
} #function


#-----------------------------------------------------------------------------

# TODO: This function exists in palsR/Gab in pals/R/FluxtowerSpreadsheetToNc.R and has a different signature. Merge?
CheckTextDataRanges = function(datain, missingval){
    
    #Checks that variables are within acceptable ranges
    # as set in the "variables" auxiliary file
    
    #Loop through variables
    for(k in 1:length(datain$vars)){
        
        #First mask out missing values so not included in
        #determination of data range
        data <- datain$data[[k]]
        data[data==missingval] <- NA
        data_range <- range(data, na.rm=TRUE)
        
        # Get acceptable ranges for variables:
        valid_range <- datain$var_ranges[,k]
        
        
        #Return error if variable outside specified range
        if(data_range[1] < valid_range[1] | data_range[2] > valid_range[2]){
            
            CheckError(paste("Variable outside expected ranges. Check variable ",
            datain$vars[k], "; data range is [", data_range[1], ", ", data_range[2],
            "], valid range is [", valid_range[1], ", ", valid_range[2],
            "]. Check data or change data range in variables auxiliary file",
            sep=""))
        }
        
        
        
    } #variables
} #function


#-----------------------------------------------------------------------------

create_qc_var <- function(datain, qc_name){
    
    #Fill all variable categories in indata
    #In some cases, need to add a column name as not set automatically
    
    #vars
    datain$vars <- append(datain$vars, qc_name)
    
    #era_vars
    datain$era_vars <- append(datain$era_vars, NA)
    names(datain$era_vars)[length(datain$era_vars)] <- qc_name
    
    #attributes
    Fluxnet_var <- NULL
    Longname <- paste(strsplit(qc_name, "_qc"), "quality control flag")
    CF_name <- NULL
    datain$attributes <- rbind(datain$attributes, 
    c(Fluxnet_var, Longname, CF_name))  
    
    
    #units
    datain$units$original_units <- append(datain$units$original_units, "-")
    datain$units$target_units   <- append(datain$units$target_units, "-")
    
    #var_ranges
    datain$var_ranges <- cbind(datain$var_ranges, rbind(0,4))
    colnames(datain$var_ranges)[ncol(datain$var_ranges)] <- qc_name
    
    #categories
    datain$categories <- append(datain$categories, "Met")
    names(datain$categories)[length(datain$categories)] <- qc_name
    
    
    return(datain)
}






