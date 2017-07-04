#' ConvertSpreadsheetToNcdf.R
#'
#' Converts data from a FLUXNET2015 formatted spreadsheet to
#' NetCDF.
#'
#' Gapfilling options:
#' 
#' EraInterim:
#' Downscaled ERAInterim estimates are provided for all meteorological variables
#' in the FLUXNET2015 release. Any missing time steps are replaced with the corresponding
#' ERAInterim estimates. 
#'
#' Statistical:
#' 
#' 
#' 
#' 
#' 
#' 
#' author: Anna Ukkola UNSW 2017.
#' Main function to convert Fluxnet2015 CSV-files to NetCDF
#'
#'
#' @param infile input filename,
#'        e.g. "FULLSET/FLX_AU-How_FLUXNET2015_FULLSET_HH_2001-2014_1-3.csv". 
#'        La Thuile data is expected to be in the format sitecode.year.xxxx.csv,
#'        e.g. "AU-How.2001.synth.hourly.allvars.csv".
#' @param site_code Fluxnet site code e.g. "AU-How"
#' @param out_path output path e.g. "./FLUXNET2016_processing/"
#' @param datasetname Name of the dataset, e.g. FLUXNET2015 or La Thuile. Defaults to FLUXNET2015,
#'        and thus must be set if processing a dataset not compliant with FLUXNET2015 format.
#' @param datasetversion Version of the dataset, e.g. "1-3"
#' @param flx2015_version Version of FLUXNET2015 data product being used, i.e. "FULLSET" or "SUBSET". 
#'        Required to set QC flags correctly.
#' @param fair_use La Thuile Fair Use policy that data should comply with, e.g. "LaThuile" or "Fair_Use" (default). 
#'        Can be a single entry or a vector of several policies. If this is set, code will only extract 
#'        years that comply with the required policy/policies. Must provide fair_use_vec to use this 
#'        functionality. 
#' @param fair_use_vec A vector of Data Use policy for each year in the data file, e.g. "LaThuile" or "Fair_Use". 
#'        Should have years as vector column names.
#' @param model Name of land surface model. Used to retrieve model specific attributes, such as site
#'        plant functional type.
#' @param missing Maximum percentage of time steps allowed to be missing in any given year
#' @param gapfill_all Maximum percentage of time steps allowed to be gap-filled 
#'        (any quality) in any given year. Note if gapfill_all is set, any thresholds
#'        for gapfill_good, gapfill_med or gapfill_poor are ignored. Set to NA if not required.
#' @param gapfill_good Maximum percentage of time steps allowed to be good-quality gap-filled 
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#' @param gapfill_med Maximum percentage of time steps allowed to be medium-quality gap-filled 
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#' @param gapfill_poor Maximum percentage of time steps allowed to be poor-quality gap-filled 
#'        in any given year. Refer to package documentation for information on QC flags.
#'        Set to NA if not required (default).
#' @param min_yrs Minimum number of consecutive years to process 
#' @param met_gapfill Method to use for gap-filling meteorological data. Set to one of 
#'        "ERAinterim", "statistical" or NA (default; no gap-filling).        
#' @param flux_gapfill Method to use for gap-filling flux data. Set to one of
#'        "statistical" or NA (default; no gap-filling).
#' @param era_file ERA input file (needed if using ERAinterim to gapfill met variables)
#'        e.g. "FULLSET/FLX_AU-How_FLUXNET2015_ERAI_HH_1989-2014_1-3.csv" 
#' @param lwdown_method Method used to synthesize incoming longwave radiation. 
#'        One of "Abramowitz_2012" (default), "Swinbank_1963" or "Brutsaert_1975".
#' @param regfill Maximum consecutive length of time (in number of days) to be gap-filled 
#'        using multiple linear regression. Defaults to 30 days. Used to gapfill flux variables.
#' @param regr_window Length of time period (in number of days) used to train multiple linear regression
#'        used for gap-filling flux variables. Defaults to 180 days.
#' @param linfill Maximum consecutive length of time (in hours) to be gap-filled 
#'        using linear interpolation. Used for all variables except rainfall. Defaults to 4 hours. 
#' @param copyfill Maximum consecutive length of time (in number of days) to be gap-filled using
#'        copyfill. Defaults to 10 days.
#' @param include_all_eval Should all evaluation values be included, regardless of data gaps? 
#'        If set to FALSE, any evaluation variables with missing or gap-filled values in
#'        excess of the thresholds will be discarded.
#' @param plot Should annual, diurnal and/or 14-day running mean plots be produced? 
#'        Set to NA if not required.
#' 
#' @export
#'
#'
convert_fluxnet_to_netcdf <- function(infile, site_code, out_path,                                   
                                      datasetname="FLUXNET2015", datasetversion="n/a",
                                      flx2015_version="FULLSET",
                                      fair_use="Fair_Use", fair_use_vec=NA,
                                      aggregate=NA,
                                      met_gapfill=NA, 
                                      flux_gapfill=NA,
                                      era_file=NA,
                                      missing = 15, gapfill_all=20,
                                      gapfill_good=NA, gapfill_med=NA,
                                      gapfill_poor=NA, min_yrs=2,
                                      linfill=4, copyfill=10,
                                      regfill=30, regwindow=180,
                                      lwdown_method="Abramowitz_2012",
                                      include_all_eval=TRUE,
                                      model=NA, 
                                      plot=c("annual", "diurnal", "timeseries")) {
    

    library(R.utils)  
    
    ### Create sub-folders for outputs ###
    outlog <- create_outdir(out_path, site_code, plot)
        
    
    ### Initialise site log ###
    site_log <- initialise_sitelog(site_code, outlog)
    
    
    ### Set expected values for missing values, QC flags and time stamps ###
    
    #First check that fluxnet2015 version specified correctly, if using it
    check_flx2015_version(datasetname, flx2015_version)
    
    qc_flags <- get_qc_flags(datasetname, flx2015_version)
    
    Sprd_MissingVal <- -9999 # missing value in spreadsheet
    Nc_MissingVal   <- -9999 # missing value in created netcdf files
       
    
    #Name of time stamp and QC variables
    if(datasetname=="LaThuile"){
      time_vars <- c("Year", "DoY", "Time", "DTIME")
      qc_name <- "qc"

    } else {
      time_vars <- c("TIMESTAMP_START", "TIMESTAMP_END")
      qc_name <- "_QC"
      
    }
     
    
    #Do some initial checks that arguments set correctly
    InitialChecks(met_gapfill, era_file, missing, aggregate,
                  datasetname, flx2015_version)
    
    
    
    ################################
    ###--- Read variable data ---###
    ################################
    
    #File contains desired variables (refer to Fluxnet2015 documentation for full variable descriptions;
    #http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/)
    #Separate file available for La Thuile synthesis as variable names differ 
    #(see http://fluxnet.fluxdata.org/data/la-thuile-dataset/)
    
    #Find variable file path (not using data() command directly because reads a CSV with a
    #semicolon separator and this leads to incorrect table headers)
    
    if(datasetname=="LaThuile"){
      var_file <- system.file("data","Output_variables_LaThuile.csv", package="FluxnetLSM")
    } else {
      var_file <- system.file("data","Output_variables_FLUXNET2015.csv", package="FluxnetLSM")
    }
    
    
    
    ### REMOVE THIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    var_file <- "~/Documents/FLUXNET2016_processing/scripts/data//Output_variables_FLUXNET2015.csv"
    
    vars <- read.csv(var_file, header=TRUE,
                     colClasses=c("character", "character", "character",
                     "character", "character", "character",
                     "character", "numeric", "numeric",
                     "logical", "logical", "character"))
    

    
    #Read site information (lon, lat, elevation)
    site_info <- get_site_metadata(site_code)
    
    #Log possible warnings and remove warnings from output var
    site_log  <- log_warning(warn=site_info$warn, site_log)
    site_info <- site_info$out
    
    
    #Should site be excluded? If so, abort and print reason.
    #This option is set in the site info file (inside data folder)
    #Mainly excludes sites with mean annual ET excluding P, implying
    #irrigation or other additional water source.
    if(site_info$Exclude){

        error <- paste("Site not processed. Reason:", site_info$Exclude_reason,
                       ". This is set in site info file, change >Exclude< options",
                       "in the file to process site")
        stop_and_log(error, site_log)
        
    }
        

    ### Find model-specific parameters ###
    model_params <- initialise_model(model, site_info)      
    
    
    # Read text file containing flux data
    DataFromText <- ReadCSVFluxData(fileinname=infile, vars=vars, 
                                    datasetname=datasetname,
                                    time_vars=time_vars, site_log,
                                    fair_usage=fair_use, 
                                    fair_usage_vec=fair_use_vec,
                                    site_code=site_code)
    
    
    # Make sure whole number of days in dataset
    CheckCSVTiming(DataFromText, site_log)
    
    
    #Replace vars with those found in file
    vars <- DataFromText$vars
    
    
    #Check for missing values in QC flags when data available
    #In FLUXNET2015 Nov 16 release, QC flags are missing in some
    #cases even when data is available. This is because the flags 
    #apply to both LE_F_MDS and LE_CORR (and similarly H) but are
    #only reported when LE_CORR is available
    #(pers. comm. with D. Papale, Fluxnet)
    
    #Set these time steps to 3 (poor gap-filling)
    DataFromText <- FillQCvarMissing(datain=DataFromText, missingVal=Sprd_MissingVal,
                                       gapfillVal=qc_flags$QC_gapfilled, qc_name=qc_name)
    
    
    # Check if variables have gaps in the time series and determine what years to output:
    gaps  <- CheckDataGaps(datain = DataFromText, missing_val = Sprd_MissingVal,
                           QCmeasured=qc_flags$QC_measured, 
                           QCgapfilled=qc_flags$QC_gapfilled,
                           missing = missing, gapfill_all=gapfill_all,
                           gapfill_good=gapfill_good, gapfill_med=gapfill_med,
                           gapfill_poor=gapfill_poor, min_yrs=min_yrs,
                           essential_met = vars[which(DataFromText$essential_met)], 
                           preferred_eval = vars[which(DataFromText$preferred_eval)],
                           all_eval = vars[which(DataFromText$categories=="Eval")],
                           qc_name=qc_name, site_log)
 
    
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

    if(any(sapply(all_missing, length) > 0) | !include_all_eval){
     
      #Find variables to exclude
      exclude_eval <- FindExcludeEval(datain=DataFromText, all_missing=all_missing, 
                                        gaps=gaps, include_all=include_all_eval)
      
    }
    
    
    ##############################################
    ###--- Gapfill meteorological variables ---###
    ##############################################
    
    if(!is.na(met_gapfill)){
      
      #Gapfill using statistical methods
      if(met_gapfill == "statistical") {
        
        
        DataFromText <- GapfillMet_statistical(datain=DataFromText, qc_name=qc_name, 
                                               qc_flags=qc_flags, copyfill=copyfill, 
                                               linfill=linfill, lwdown_method=lwdown_method,
                                               elevation=site_info$SiteElevation,
                                               gaps=gaps, site_log=site_log)
        
        
        # gapfill using ERA-interim data provided as part of FLUXNET2015      
      } else if(met_gapfill == "ERAinterim") {
        
        #Gapfill with ERAinterim
        DataFromText <- GapfillMet_with_ERA(DataFromText, ERA_file, 
                                            qc_name, qc_flags)
        
        #Cannot recognise method, stop
      } else {
        
        stop(paste("Cannot ascertain met_gapfill method. Choose one of",
                   "'ERAinterim' and 'statistical' or set to NA if not desired"))  
      }
      
      
      
      
      
      #COMPLETE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      #doesn't stop if can't gapfill all values
    
      #COMPLETE !!!!!!!!!!!!!!!!!
      #Need to update gaps
      
      #Update gaps after performing gapfilling
      
      gaps <- update_gaps(gaps, qc_info, DataFromText)
      
      
      
      
      
    }
       
    
    
    
    ####################################
    ###--- Gapfill flux variables ---###
    ####################################
    
    #Gapfill flux variables using statistical methods
    if(gapfill_flux){
      
      DataFromText <- GapfillFlux(DataFromText, qc_name, qc_flags,
                                  regfill, regwindow, linfill)
      
      
      #COMPLETE !!!!!!!!!!!!!!!!!
      
      #Need to update gaps
      
      #gaps <- update_gaps(gaps, DataFromText)
      
      
      
      
      
    }
    
    
    
    ############################################################
    ### Calculate average annual precip if outputting precip ###
    ############################################################
    
    # Written as an attribute to file. Calculated before converting
    # data units, i.e. assumes rainfall units mm/timestep
        
    if(any(DataFromText$attributes[,1]=="P")){
      
      #Check that units in mm
      if(DataFromText$units$original_units["P"] == "mm"){
        
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
        
    
    
    ############################################
    ### Aggregate data to a longer time step ###
    ############################################

    if(!is.na(aggregate)){
      
      if(aggregate)
      
      
      
      
    }
    
    
    
    
    
    
    
    
    
    ###########################################
    ### Convert units and check data ranges ###
    ###########################################
    
    
    # Convert data units from original Fluxnet units
    # to desired units as set in variables.csv
    ConvertedData <- ChangeUnits(DataFromText, site_log)
    

    # Check that data are within acceptable ranges: 
    CheckDataRanges(ConvertedData, missingval=Nc_MissingVal, site_log)
    
    
    #Replace original data with converted data
    DataFromText <- ConvertedData
    
    
    #Determine number of files to be written 
    no_files <- length(unique(gaps$consec))
    
    
        
    ####################################################
    ###--- Write output met and flux NetCDF files ---###
    ####################################################
    
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
        
        #If only one year, only write start year, else write time period
        if(start_yr[k]==end_yr[k]){
            metfilename  <- paste(outpath_nc, "/", site_code, "_", start_yr[k], 
                                  "_", datasetname, "_Met.nc", sep="")
            fluxfilename <- paste(outpath_nc, "/", site_code, "_", start_yr[k], 
                                  "_", datasetname, "_Flux.nc", sep="")
            
        } else {
            metfilename  <- paste(outpath_nc, "/", site_code, "_", start_yr[k], 
                                  "-", end_yr[k], "_", datasetname, "_Met.nc", sep="")
            fluxfilename <- paste(outpath_nc, "/", site_code, "_", start_yr[k], 
                                  "-", end_yr[k], "_", datasetname, "_Flux.nc", sep="")           
        }
        
        #Save file names
        met_files[k]  <- metfilename
        flux_files[k] <- fluxfilename
                  
        
        ###--- Create netcdf met driving file ---###
        
        #Find met variable indices
        met_ind <- which(DataFromText$categories=="Met")
        
        #Write met file
        CreateMetNetcdfFile(metfilename=metfilename, 
                            datain=DataFromText,
                            site_code=site_code,
                            siteInfo=site_info,
                            datasetversion=datasetversion,
                            ind_start=gaps$tseries_start[k],
                            ind_end=gaps$tseries_end[k],
                            starttime=nc_starttime,
                            timestepsize=DataFromText$timestepsize,
                            av_precip=av_precip[[k]],
                            missing=missing, gapfill_all=gapfill_all, 
                            gapfill_good=gapfill_good, gapfill_med=gapfill_med, 
                            gapfill_poor=gapfill_poor, min_yrs=min_yrs,
                            total_missing=gaps$total_missing[[k]][met_ind],
                            total_gapfilled=gaps$total_gapfilled[[k]][met_ind],
                            qcInfo=qc_flags$qc_info,
                            ERA_gapfill=ERA_gapfill,
                            infile=infile,
                            var_ind=met_ind,
                            modelInfo=model_params)
        
        
        
        ###--- Create netcdf flux data file ---###
        
        #Write flux file
        CreateFluxNetcdfFile(fluxfilename=fluxfilename, datain=DataFromText,
                             site_code=site_code,
                             siteInfo=site_info,
                             datasetversion=datasetversion,
                             ind_start=gaps$tseries_start[k],
                             ind_end=gaps$tseries_end[k],
                             starttime=nc_starttime,
                             timestepsize=DataFromText$timestepsize,
                             missing=missing, gapfill_all=gapfill_all, 
                             gapfill_good=gapfill_good, gapfill_med=gapfill_med, 
                             gapfill_poor=gapfill_poor, min_yrs=min_yrs,
                             total_missing=gaps$total_missing[[k]][flux_ind[[k]]],
                             total_gapfilled=gaps$total_gapfilled[[k]][flux_ind[[k]]],
                             qcInfo=qc_flags$qc_info,
                             infile=infile,
                             var_ind=flux_ind[[k]],
                             modelInfo=model_params)
        
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
        outfile_met  <- paste(outpath_plot, "/", site_code, "_", start_yr[k], "_", end_yr[k], "_plot_Met_", sep="")
        outfile_flux <- paste(outpath_plot, "/", site_code, "_", start_yr[k], "_", end_yr[k], "_plot_Flux_", sep="")
        
        
        ## Plotting ##
        if(any(plot=="annual" | plot=="diurnal" | plot=="timeseries")){
          
          out1 <- plot_nc(ncfile=nc_met, analysis_type=plot, 
                         vars=DataFromText$out_vars[DataFromText$categories=="Met"],
                         outfile=outfile_met)      
          
          out2 <- plot_nc(ncfile=nc_flux, analysis_type=plot,
                         vars=DataFromText$out_vars[flux_ind[[k]]],
                         outfile=outfile_flux)
          
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
    
    return(cat("Site", site_code, "processed successfully. Refer to log file for details"))
    
} #function
