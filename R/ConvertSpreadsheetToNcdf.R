#' ConvertSpreadsheetToNcdf.R
#'
#' Converts data from a FLUXNET2015 formatted spreadsheet to
#' NetCDF.
#'
#' See ?get_default_conversion_options for all conversion options.
#' 
#' Gapfilling options:
#'
#' EraInterim:
#' Downscaled ERAInterim estimates are provided for all meteorological variables
#' in the FLUXNET2015 release. Any missing time steps are replaced with the corresponding
#' ERAInterim estimates.
#'
#' Statistical:
#' Met variables gapfilled with a combination of linear interpolation (for short gaps) and
#' copyfill (for longer gaps). Air pressure and incoming longwave radiation are synthesised
#' from air temperature, elevation and relative humidity.
#' 
#' Flux variables gapfilled using a combination of linear interpolation (for short gaps) and
#' and a linear regression against met variables (incoming shortwave, and air temperature and 
#' relative humidity, if available.). If regfill is set to NA, copyfill is used instead.
#'
#'
#' author: Anna Ukkola UNSW 2017.
#' Main function to convert Fluxnet2015 CSV-files to NetCDF
#'
#'
#' @param site_code Fluxnet site code e.g. "AU-How"
#' @param infile input filename,
#'        e.g. "FULLSET/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv".
#'        La Thuile data is expected to be in the format sitecode.year.xxxx.csv,
#'        e.g. "AU-How.2001.synth.hourly.allvars.csv".
#' @param era_file ERA-Interim input file (needed if using ERAinterim to gapfill met variables)
#'        e.g. "FULLSET/FLX_AU-How_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv"
#' @param out_path output path e.g. "./FLUXNET2016_processing/"
#' @param conv_opts options for the conversion.
#'        See \code{\link{get_default_conversion_options}}.
#' @param plot Should annual, diurnal and/or 14-day running mean plots be produced?
#'        Set to NA if not required.
#'
#' @export
#'
#'
convert_fluxnet_to_netcdf <- function(site_code, infile, era_file=NA, out_path,
                                      conv_opts=get_default_conversion_options(),
                                      plot=c("annual", "diurnal", "timeseries"),
                                      ...) {
  
  # We allow options to be passed directly into the function, to override conv_opts
  opt_args <- list(...)
  for (o in names(opt_args)) {
      conv_opts[o] = opt_args[o]
  }

  # Add QC variables for each of the required variables
  if(!is.na(conv_opts$limit_vars[1])){
    conv_opts$limit_vars <- c(conv_opts$limit_vars, paste0(conv_opts$limit_vars, '_qc'))
  }

  
  library(R.utils)  
  
  ### Create sub-folders for outputs ###
  out_paths <- create_outdir(out_path, site_code, plot)
  
  
  ### Initialise site log ###
  site_log <- initialise_sitelog(site_code, out_paths)
  
  
  ### Set expected values for missing values, QC flags and time stamps ###
  
  #First check that fluxnet2015 version specified correctly, if using it
  check_flx2015_version(conv_opts$datasetname, conv_opts$flx2015_version)
  
  qc_flags <- get_qc_flags(conv_opts$datasetname, conv_opts$flx2015_version)
  
  Sprd_MissingVal <<- -9999 # missing value in spreadsheet
  Nc_MissingVal   <<- -9999 # missing value in created netcdf files
  
  
  #Name of time stamp and QC variables
  if(conv_opts$datasetname=="LaThuile"){
    time_vars <- c("Year", "DoY", "Time", "DTIME")
    qc_name <- "qc"
    
  } else if (conv_opts$datasetname=="OzFlux") { 
    
    time_vars <- c("time")
    qc_name <- "_QCFlag"
    
  } else {
    time_vars <- c("TIMESTAMP_START", "TIMESTAMP_END")
    qc_name <- "_QC"
    
  }
  
  
  #Do some initial checks that arguments set correctly
  InitialChecks(conv_opts, era_file)
  
  
  #Get variable names specific for the dataset (fluxnet2015, lathuile)
  #used for data conversions etc.
  dataset_vars <- get_varnames(conv_opts$datasetname, conv_opts$flx2015_version)
  
  
  
  ################################
  ###--- Read variable data ---###
  ################################
  
  #File contains desired variables (refer to Fluxnet2015 documentation for full variable descriptions;
  #http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/)
  #Separate file available for La Thuile synthesis as variable names differ 
  #(see http://fluxnet.fluxdata.org/data/la-thuile-dataset/)
  
  #Find variable file path (not using data() command directly because reads a CSV with a
  #semicolon separator and this leads to incorrect table headers)
  
  #La Thuile or OzFlux
  if(conv_opts$datasetname %in% c("LaThuile", "OzFlux")){
    
    var_file     <- system.file("data", paste0("Output_variables_", conv_opts$datasetname, 
                                               ".csv"), package="FluxnetLSM")
    
    #Fair use information for La Thuile
    if(conv_opts$datasetname == "LaThuile") {
      
      fair_use_file          <- system.file("data","LaThuile_site_policy.csv", package="FluxnetLSM")
      fair_use_vec           <- read.csv(fair_use_file, header=TRUE, check.names=FALSE)
      conv_opts$fair_use_vec <- fair_use_vec[fair_use_vec$site==site_code,]
      
      }

  #Fluxnet2015
  } else {
    if(conv_opts$flx2015_version=="SUBSET"){
      var_file <- system.file("data","Output_variables_FLUXNET2015_SUBSET.csv", package="FluxnetLSM")
    } else {
      var_file <- system.file("data","Output_variables_FLUXNET2015_FULLSET.csv", package="FluxnetLSM")
    }
  }
  
  
  vars_csv <- read.csv(var_file, header=TRUE,
                       colClasses=c(
                                    "character",  # Fluxnet_variable
                                    "character",  # Fluxnet_unit
                                    "character",  # Fluxnet_class
                                    "character",  # Output_variable
                                    "character",  # Output_unit
                                    "character",  # Longname
                                    "character",  # Standard_name
                                    "numeric",    # Data_min
                                    "numeric",    # Data_max
                                    "logical",    # Essential_met
                                    "logical",    # Preferred_eval
                                    "character",  # Category
                                    "character",  # ERAinterim_variable
                                    "character"   # Aggregate_method
                                    ))


  #Read site information (lon, lat, elevation)
  if (conv_opts$metadata_source == 'all') {
      site_info <- get_site_metadata(site_code, model=conv_opts$model)
  } else if (conv_opts$metadata_source == 'csv') {
      site_info <- get_site_metadata_from_CSV(site_code, model=conv_opts$model)
  } else if (conv_opts$metadata_source == 'web') {
      
      #Stop if using this option and trying to pass model information
      if(!is.na(conv_opts$model)) { stop(paste0("Cannot read model parameters when ",
                                                "metadata source set to web. Use csv or all ",
                                                "for conv_opts$metadata_source"))}
      site_info <- get_site_metadata_web(site_code)
      
  } else {
      stop("Unknown metadata source '", conv_opts$metadata_source, "'")
  }
  
  #Log possible warnings and remove warnings from output var
  site_log  <- log_warning(warn=site_info$warn, site_log)
  site_info <- site_info$out
  
  
  #Should site be excluded? If so, abort and print reason.
  #This option is set in the site info file (inside data folder)
  #Mainly excludes sites with mean annual ET excluding P, implying
  #irrigation or other additional water source.
  if(!is.null(site_info$Exclude) & site_info$Exclude){
    
    error <- paste("Site not processed. Reason:", site_info$Exclude_reason,
                   ". This is set in site info file, change >Exclude< options",
                   "in the file to process site")
    stop_and_log(error, site_log)
    
  }
  
  
  ### Find model-specific parameters ###
  model_params <- initialise_model(conv_opts$model, site_info)
  
  
  # Read text file containing flux data
  DataFromText <- ReadCSVFluxData(fileinname=infile, vars=vars_csv, 
                                  datasetname=conv_opts$datasetname,
                                  time_vars=time_vars, site_log,
                                  fair_usage=conv_opts$fair_use,
                                  fair_usage_vec=conv_opts$fair_use_vec,
                                  min_yrs=conv_opts$min_yrs,
                                  site_code=site_code)
  
  
  # Make sure whole number of days in dataset
  CheckCSVTiming(DataFromText, site_log)
  
  
  #Adding temporary La Thuile fix here (Only works if output variables called NEE_qc and GPP_qc !!!!)
  #La Thuile has the same QC flag for NEE and GPP (NEE_GPP_qc),
  #Need to rename it here or causes an error later on and change values 
  #(which range 1-6, not 0-3 like the rest of La Thuile QC flags)

  if (conv_opts$datasetname == "LaThuile") {
    
    if(any(DataFromText$vars == "NEE_GPP_qc")) {
      
      #Variables to check
      check_vars <- c("NEE_qc", "GPP_qc")
      
      #New variable names
      new_varnames <- c("NEE_fqc", "GPP_fqc")
        
      #Get indices
      ind_nee_gpp <-lapply(check_vars, function(x) which(DataFromText$out_vars == x))
      
      #Loop through vars
      for(v in 1:length(check_vars)) {
        
        #If found this variable
        if(length(ind_nee_gpp[[v]]) > 0) { 
          
          #Replace name
          DataFromText$vars[ind_nee_gpp[[v]]] <- new_varnames[v]
          
          #Repalce column name in data
          colnames(DataFromText$data)[ind_nee_gpp[[v]]] <- new_varnames[v]
          
          #Finally need to fix QC values to match other QC flags
          temp_data <- DataFromText$data[ind_nee_gpp[[v]]]
          
          #Set values 1-2 to 0 (observed)
          temp_data[temp_data %in% c(1,2)] <- 0
          
          #SEt values 3-4 to 1 (good gapfilling)
          temp_data[temp_data %in% c(3,4)] <- 1
          
          #Set values 5-6 to 3 (poor gapfilling)
          temp_data[temp_data %in% c(5,6)] <- 3
          
          #Replace with new values
          DataFromText$data[ind_nee_gpp[[v]]] <- temp_data
        }
      }
    }
  }
  
  
  #Replace vars with those found in file
  vars <- DataFromText$vars
  
  
  #Check for missing values in QC flags when data available
  #In FLUXNET2015 Nov 16 release, QC flags are missing in some
  #cases even when data is available. This is because the flags 
  #apply to both LE_F_MDS and LE_CORR (and similarly H) but are
  #only reported when LE_CORR is available
  #(pers. comm. with D. Papale, Fluxnet)
  
  #Set these time steps to 3 (poor gap-filling)
  DataFromText <- FillQCvarMissing(datain=DataFromText, 
                                   gapfillVal=qc_flags$QC_gapfilled, 
                                   qc_name=qc_name)
    
  
  ##############################################
  ###--- Gapfill meteorological variables ---###
  ##############################################
  
  if(!is.na(conv_opts$met_gapfill)){
    
    #Gapfill using statistical methods
    if(conv_opts$met_gapfill == "statistical") {
      
      gapfilled_met <- GapfillMet_statistical(datain=DataFromText, qc_name=qc_name, 
                                              qc_flags=qc_flags, copyfill=conv_opts$copyfill,
                                              linfill=conv_opts$linfill, 
                                              lwdown_method=conv_opts$lwdown_method,
                                              elevation=site_info$SiteElevation,
                                              varnames=dataset_vars, conv_opts$limit_vars,
                                              site_log=site_log)
      
      DataFromText <- gapfilled_met$data
      site_log     <- gapfilled_met$site_log        
      
      # Gapfill using ERA-interim data provided as part of FLUXNET2015      
    } else if(conv_opts$met_gapfill == "ERAinterim") {
      
      #Gapfill with ERAinterim
      DataFromText <- GapfillMet_with_ERA(DataFromText, era_file,
                                          qc_name, dataset_vars,
                                          qc_flags=qc_flags,
                                          site_log=site_log)
      # Not sure if we need to return the site_log from this function? Seems to stop on every error anyway..
      
      #Cannot recognise method, stop
    } else {
      
      stop(paste("Cannot ascertain met_gapfill method. Choose one of",
                 "'ERAinterim' and 'statistical' or set to NA if not desired"))  
    }
  }
  
  
  ####################################
  ###--- Gapfill flux variables ---###
  ####################################
  
  #Gapfill flux variables using statistical methods
  if(!is.na(conv_opts$flux_gapfill)){
    
    gapfilled_flux <- GapfillFlux(DataFromText, qc_name, qc_flags,
                                  conv_opts$regfill, conv_opts$linfill, conv_opts$copyfill,
                                  varnames=dataset_vars, site_log)      
    
    DataFromText <- gapfilled_flux$dataout
    site_log     <- gapfilled_flux$site_log
  }
  
  
  ############################################
  ### Aggregate data to a longer time step ###
  ############################################
  
  if(!is.na(conv_opts$aggregate)){
    
    # Aggregate to a coarser time step as set by argument aggregate
    # QC flags are set to a fraction measured+good gapfilling
    # (as per FLUXNET2015 convention for aggregated data)
    
    aggregated_data <- aggregate_tsteps(datain=DataFromText, new_tstep=conv_opts$aggregate,
                                        qc_flags=qc_flags, qc_name=qc_name)
    
    #update QC flag info
    DataFromText <- aggregated_data$data
    qc_flags     <- aggregated_data$qc_flags
    
  }
  
  
  #######################
  ### Check data gaps ###
  #######################
  
  #Check gaps after gapfilling to determine output data periods
  #according to user-defined thresholds

  gaps  <- CheckDataGaps(datain=DataFromText, qc_flags=qc_flags, 
                         missing=conv_opts$missing, 
                         gapfill_all=conv_opts$gapfill_all,
                         gapfill_good=conv_opts$gapfill_good, 
                         gapfill_med=conv_opts$gapfill_med,
                         gapfill_poor=conv_opts$gapfill_poor,
                         gapfill_era=conv_opts$gapfill_era,
                         gapfill_stat=conv_opts$gapfill_stat,                         
                         min_yrs=conv_opts$min_yrs,
                         qc_name=qc_name, 
                         aggregate=conv_opts$aggregate, 
                         site_log=site_log)
  
  #Log possible warnings and remove warnings from output var
  site_log <- log_warning(warn=gaps$warn, site_log)
  gaps     <- gaps$out
  

  
  ## Check that gap check found whole years ##
  IsWholeYrs(datain=DataFromText, gaps, site_log)
  
  
  ### Save info on which evaluation variables have all values missing ###
  
  #These are excluded when writing NetCDF file
  #Find variables with all values missing
  all_missing <- lapply(gaps$total_missing, function(x) names(which(x==100)))
  
  exclude_eval <- rep(NA, length(all_missing))
  
  if(any(sapply(all_missing, length) > 0) | !conv_opts$include_all_eval){
    
    #Find variables to exclude
    exclude_eval <- FindExcludeEval(datain=DataFromText, all_missing=all_missing, 
                                    gaps=gaps, include_all=conv_opts$include_all_eval,
                                    qc_name=qc_name)
    
  }
   
  
  
  ############################################################
  ### Calculate average annual precip if outputting precip ###
  ############################################################
  
  # Written as an attribute to file. Calculated before converting
  # data units, i.e. assumes rainfall units mm/timestep
  
  if(any(DataFromText$attributes[,1] %in% dataset_vars$precip)){
    
    #Check that units in mm
    if(DataFromText$units$original_units[names(DataFromText$units$original_units)
                                         %in% dataset_vars$precip] == "mm"){
      
      av_precip <- calc_avPrecip(datain=DataFromText, gaps=gaps)
      
    } else{
      warn = paste("Cannot ascertain P units, expecting 'mm'.", 
                   "Not outputting average precip as a netcdf attribute")
      site_log <- warn_and_log(warn, site_log)
    }
    
  } else {
    #Set to NA, repeat to match no. of output files 
    av_precip=rep(NA, length(unique(gaps$consec)))
  }
  
  
  ###########################################
  ### Convert units and check data ranges ###
  ###########################################
  
  
  # Convert data units from original Fluxnet units
  # to desired units as set in variables.csv
  ConvertedData <- ChangeUnits(DataFromText, dataset_vars, site_log)
  
  
  # Check that data are within acceptable ranges: 
  site_log <- CheckDataRanges(ConvertedData, site_log, conv_opts$check_range_action)
  
  
  #Replace original data with converted data
  DataFromText <- ConvertedData
  
  
  #Determine number of files to be written 
  no_files <- length(unique(gaps$consec))
  
  
  
  ###########################
  ### Get OzFlux metadata ###
  ###########################
  
  
  #Original OzFlux files have lots of useful metadata in NC files
  #Copy these to the new files
  
  if (conv_opts$datasetname == "OzFlux") {
    
    #Open file handle
    nc_oz <- nc_open(infile)
    
    #Get global attributes
    global_atts <- ncatt_get(nc_oz, varid=0)
    
    #Close file
    nc_close(nc_oz)
    
  } else {
    global_atts <- NA
  }
  
  
  
  
  ####################################################
  ###--- Write output met and flux NetCDF files ---###
  ####################################################
  
  browser()
  #Set all NA values to Nc missing value
  DataFromText$data[is.na(DataFromText$data)] <- Nc_MissingVal


  #Gather argument info to save as metadata in Nc file
  arg_info <- append(list(infile=infile, era_file=era_file),
                     conv_opts)


  #Initialise variables to save output file names (used to write log and for plotting)
  met_files  <- vector()
  flux_files <- vector()
  start_yr   <- vector()
  end_yr     <- vector()
  flux_ind   <- list()
  
  #save to loops so no_files can be amended below
  loops <- no_files
  
  for(k in 1:loops){        
    
    ### First check that there are evalution variables to output ##
    
    #If not, skip this file
    #Find eval variable indices
    flux_ind[[k]] <- FindFluxInd(datain=DataFromText,
                                 exclude_eval=exclude_eval[[k]], 
                                 k, site_log)
    
    #Log possible warnings and remove warnings from output var
    site_log <- log_warning(warn=flux_ind[[k]]$warn, site_log)
    flux_ind[[k]] <- flux_ind[[k]]$out
    
    #If no eval vars for any time period, abort        
    if(k==no_files & all(sapply(flux_ind, length)==0)){   
      
      error <- paste("No evaluation variables to process for any output",
                     "time periods. Site not processed.")       
      stop_and_log(error, site_log)
      
      #If no eval variables for this loop, skip to next
    } else if(length(flux_ind[[k]]) == 0) {
      no_files <- no_files-1
      next
    }
    
    
    
    ### Find start and end time ###
    
    #Find start year, day and hour
    nc_starttime <- findStartTime(start = strptime(DataFromText$time[gaps$tseries_start[k],1], "%Y%m%d%H%M"))
    
    #Extract start and end years
    start_yr[k] <- substring(DataFromText$time[gaps$tseries_start[k],1], 1, 4)
    end_yr[k]   <- substring(DataFromText$time[gaps$tseries_end[k],1], 1, 4)
    
    
    
    ### Create output file names ###
    metfilename  <- paste(out_paths$nc_met, "/", site_code, "_", start_yr[k], 
                          "-", end_yr[k], "_", conv_opts$datasetname, "_Met.nc", sep="")
    fluxfilename <- paste(out_paths$nc_flx, "/", site_code, "_", start_yr[k], 
                          "-", end_yr[k], "_", conv_opts$datasetname, "_Flux.nc", sep="")

    
    #Save file names
    met_files[k]  <- metfilename
    flux_files[k] <- fluxfilename
    
    
    ###--- Create netcdf met driving file ---###
    
    #Find met variable indices
    met_ind <- which(DataFromText$categories=="Met")

    # And limit them, if necessary
    if (!is.na(conv_opts$limit_vars[1])) {
        met_limit_ind <- sapply(met_ind, function(x) {
            if (DataFromText$out_vars[x] %in% conv_opts$limit_vars) {x} else {NA}
        })
        met_limit_ind <- met_limit_ind[!is.na(met_limit_ind)]
    } else {
        met_limit_ind <- met_ind
    }

    
    #Write met file
    CreateMetNetcdfFile(metfilename=metfilename, 
                        datain=DataFromText,
                        site_code=site_code,
                        siteInfo=site_info,
                        ind_start=gaps$tseries_start[k],
                        ind_end=gaps$tseries_end[k],
                        starttime=nc_starttime,
                        av_precip=av_precip[[k]],
                        total_missing=gaps$total_missing[[k]][met_limit_ind],
                        total_gapfilled=gaps$total_gapfilled[[k]][met_limit_ind],
                        qcInfo=qc_flags$qc_info,
                        arg_info=arg_info,
                        var_ind=met_limit_ind,
                        varnames=dataset_vars,
                        modelInfo=model_params,
                        global_atts=global_atts)
    
    
    
    ###--- Create netcdf flux data file ---###

    # Limit Flux output vars, if necessary
    if (!is.na(conv_opts$limit_vars[1])) {
        flux_limit_ind <- sapply(flux_ind[[k]], function(x) {
            if (DataFromText$out_vars[x] %in% conv_opts$limit_vars) {x} else {NA}
        })
        flux_limit_ind <- flux_limit_ind[!is.na(flux_limit_ind)]
    } else {
        flux_limit_ind <- flux_ind[[k]]
    }

    #Write flux file
    CreateFluxNetcdfFile(fluxfilename=fluxfilename, datain=DataFromText,
                         site_code=site_code,
                         siteInfo=site_info,
                         ind_start=gaps$tseries_start[k],
                         ind_end=gaps$tseries_end[k],
                         starttime=nc_starttime,
                         total_missing=gaps$total_missing[[k]][flux_limit_ind],
                         total_gapfilled=gaps$total_gapfilled[[k]][flux_limit_ind],
                         qcInfo=qc_flags$qc_info,
                         arg_info=arg_info,
                         var_ind=flux_limit_ind,
                         varnames=dataset_vars,
                         modelInfo=model_params,
                         global_atts=global_atts)
    
  }
  
  
  #############################
  ### Plot analysis outputs ###
  ############################# 
  
  #Plots annual and diurnal cycle plots, as well
  #as a 14-day running mean time series depending on
  #analysis choices (separate figures for Met and Flux vars)
  
  if(!any(is.na(plot))){
    
    #Loop through output periods
    for(k in 1:length(met_files)){
      
      #Open met and flux NetCDF file handles
      nc_met <- nc_open(met_files[k])
      nc_flux <- nc_open(flux_files[k])
      
      #Initialise output file names (completed in plotting code)
      outfile_met  <- paste(out_paths$plot, "/", site_code, "_", start_yr[k], "_", end_yr[k], "_plot_Met_", sep="")
      outfile_flux <- paste(out_paths$plot, "/", site_code, "_", start_yr[k], "_", end_yr[k], "_plot_Flux_", sep="")
      
      
      ## Plotting ##
      if(any(plot=="annual" | plot=="diurnal" | plot=="timeseries")){
        
        out1 <- plot_nc(ncfile=nc_met, analysis_type=plot, 
                        vars=DataFromText$out_vars[DataFromText$categories=="Met"],
                        varnames=dataset_vars, outfile=outfile_met)      
        
        out2 <- plot_nc(ncfile=nc_flux, analysis_type=plot,
                        vars=DataFromText$out_vars[flux_ind[[k]]],
                        varnames=dataset_vars, outfile=outfile_flux)
        
        #Log possible warnings
        site_log <- log_warning(warn=out1, site_log)
        site_log <- log_warning(warn=out2, site_log)
        
        
        #Analysis type doesn't match options, return warning
      } else {
        warn <- paste("Could not produce output plots. Analysis type not",
                      "recognised, choose all or any of 'annual',", 
                      "'diurnal' and 'timeseries'.")
        site_log <- warn_and_log(warn, site_log)
      }
      
      
      #Close file handles
      nc_close(nc_met)
      nc_close(nc_flux)  
      
    }
    
  } #plotting
  
  
  
  #################################################
  ### Collate processing information into a log ###
  #################################################
  
  if(no_error(site_log)){
    
    site_log["Processed"]     <- "TRUE"
    site_log["No_files"]      <- no_files
    site_log["Met_files"]     <- paste(met_files, collapse=", ")
    site_log["Flux_files"]    <- paste(flux_files, collapse=", ")
    site_log["Excluded_eval"] <- paste(sapply(1:length(exclude_eval), function(x)
      paste("File ", x, ": ", paste(exclude_eval[[x]], 
                                    collapse=","), sep="")), collapse="; ")    
  }
  
  #Write log to file
  write_log(site_log)
  
  return(cat("Site", site_code, "processed successfully. Refer to log file for details.\n"))
  
} #function

#-----------------------------------------------------------------------------

#' Default options for a dataset conversion
#'
#' @return options list, with values:
#' 
#' - datasetname: Name of the dataset, e.g. FLUXNET2015 or La Thuile. Defaults to FLUXNET2015,
#'        and thus must be set if processing a dataset not compliant with FLUXNET2015 format.
#'
#' - datasetversion: Version of the dataset, e.g. "1-3"
#' 
#' - flx2015_version: Version of FLUXNET2015 data product being used, i.e. "FULLSET" or "SUBSET".
#'        Required to set QC flags correctly.
#'        
#' - fair_use: La Thuile Fair Use policy that data should comply with, e.g. "LaThuile" or "Fair_Use" (default).
#'        Can be a single entry or a vector of several policies. If this is set, code will only extract
#'        years that comply with the required policy/policies. Must provide fair_use_vec to use this
#'        functionality.
#'        
#' - fair_use_vec: A vector of Data Use policy for each year in the data file, e.g. "LaThuile" or "Fair_Use".
#'        Should have years as vector column names.
#'        
#' - met_gapfill: Method to use for gap-filling meteorological data. Set to one of
#'        "ERAinterim", "statistical" or NA (default; no gap-filling).
#'        
#' - flux_gapfill: Method to use for gap-filling flux data. Set to one of
#'        "statistical" or NA (default; no gap-filling).
#'        
#' - missing: Maximum percentage of time steps allowed to be missing in any given year
#' 
#' - gapfill_all: Maximum percentage of time steps allowed to be gap-filled
#'        (any quality) in any given year. Note if gapfill_all is set, any thresholds
#'        for gapfill_good, gapfill_med or gapfill_poor are ignored. Set to NA if not required.
#'        
#' - gapfill_good: Maximum percentage of time steps allowed to be good-quality gap-filled
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#'        
#' - gapfill_med: Maximum percentage of time steps allowed to be medium-quality gap-filled
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#'        
#' - gapfill_poor: Maximum percentage of time steps allowed to be poor-quality gap-filled
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#'     
#' - gapfill_era: Maximum percentage of time steps allowed to be ERA-Interim gap-filled
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#'     
#' - gapfill_stat: Maximum percentage of time steps allowed to be statistically gap-filled
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#'                   
#' - min_yrs: Minimum number of consecutive years to process
#' 
#' - linfill: Maximum consecutive length of time (in hours) to be gap-filled
#'        using linear interpolation. Used for all variables except rainfall. Defaults to 4 hours.
#'        
#' - copyfill: Maximum consecutive length of time (in number of days) to be gap-filled using
#'        copyfill. Defaults to 10 days.
#'        
#' - regfill: Maximum consecutive length of time (in number of days) to be gap-filled
#'        using multiple linear regression. Defaults to 30 days. Default method used to gapfill flux variables.
#'        If gapfilling by copyfill is preferred, set regfill to NA.
#'        
#' - lwdown_method: Method used to synthesize incoming longwave radiation.
#'        One of "Abramowitz_2012" (default), "Swinbank_1963" or "Brutsaert_1975".
#'        
#' - check_range_action: Action to take when input data falls outside of valid ranges
#'        (as defined in data/Output_variables_*.csv).
#'        One of "stop" (log error and stop processing, default),
#'        "warn" (log error and continue), "ignore" (continue), or
#'        "truncate" (set values outside the valid range to the range bounds).
#'        
#' - include_all_eval: Should all evaluation values be included, regardless of data gaps?
#'        If set to FALSE, any evaluation variables with missing or gap-filled values in
#'        excess of the thresholds will be discarded.
#'        
#' - aggregate: Time step (in hours) that the data is aggregated to. Must be divisible by 24 and can be set to
#'        a maximum 24 hours (daily). Defaults to NA (no aggregation).
#'        
#' - model: Name of land surface model. Used to retrieve model specific attributes, such as site
#'        plant functional type.
#'
#' - metadata_source: Sources to check for metadata. One of 'all', 'csv', or 'web'.
#'
#' @export
#'
get_default_conversion_options <- function() {
    conv_opts <- list(
        datasetname = "FLUXNET2015",
        datasetversion = "n/a",
        flx2015_version = "FULLSET",
        fair_use = "Fair_Use",
        fair_use_vec = NA,
        met_gapfill = NA,
        flux_gapfill = NA,
        missing = 15,
        gapfill_all = 20,
        gapfill_good = NA,
        gapfill_med = NA,
        gapfill_poor = NA,
        gapfill_era = NA,
        gapfill_stat = NA,        
        min_yrs = 2,
        linfill = 4,
        copyfill = 10,
        regfill = 30,
        lwdown_method = "Abramowitz_2012",
        check_range_action = "stop",
        include_all_eval = TRUE,
        aggregate = NA,
        model = NA,
        limit_vars = NA,
        metadata_source = 'all'
        )

    return(conv_opts)
}
