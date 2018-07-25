#' Gets a Fluxnet file template (or full file name if site codes and years are specified)
#'
#' @export
get_fluxnet_file_template <- function(site_code = "[A-Z]{2}-[A-Za-z0-9]{3}",
                                      datasetname = "FLUXNET2015",
                                      subset = "FULLSET",
                                      resolution = "[A-Z]{2}",
                                      flx2015_years = "[0-9]{4}-[0-9]{4}",
                                      lathuile_year="[0-9]{4}",
                                      datasetversion = "[0-9]{1}-[0-9]{1}",
                                      extension=".csv") {
  
    if(datasetname=="LaThuile"){
      file_template <- paste(site_code, lathuile_year, sep=".")
    } else{
      version <- gsub("\\.", "-", datasetversion)
      if (is.character(resolution) & nchar(resolution) > 0) {
        file_template <- paste("FLX", site_code, datasetname, subset, resolution,
                               flx2015_years, version, sep = "_")
        file_template <- paste0(file_template, extension)
      } else {
        file_template <- paste("FLX", site_code, datasetname, subset, flx2015_years,
                               version, sep = "_")
        file_template <- paste0(file_template, extension)
      }      
    }

    return(file_template)
}

#-----------------------------------------------------------------------------

#' Gets a Fluxnet ERA interim file template (or full file name if site codes and years are specified)
#'
#' @export
get_fluxnet_erai_template <- function(site_code, ...) {
    return(get_fluxnet_file_template(site_code = site_code, subset = "ERAI", ...))
}

#-----------------------------------------------------------------------------

#' Gets Fluxnet file(s) available at a given path
#'
#' @export
get_fluxnet_files <- function(path, site_code = "[A-Z]{2}-[A-Za-z0-9]{3}", ...) {
    template <- get_fluxnet_file_template(site_code = site_code, ...)
    files <- list.files(path, template, full.names = TRUE, ignore.case=TRUE)

    return(files)
}

#-----------------------------------------------------------------------------

#' Gets Fluxnet ERA Interim file(s) available at a given path
#'
#' @export
get_fluxnet_erai_files <- function(path, site_code = "[A-Z]{2}-[A-Za-z0-9]{3}", ...) {
  template <- get_fluxnet_erai_template(site_code = site_code, ...)
  files <- list.files(path, template, full.names = TRUE, ignore.case=TRUE)
  
  return(files)
}

#-----------------------------------------------------------------------------

#' Gets Fluxnet dataset version from Fluxnet file
#'
#' @export
get_fluxnet_version_no <- function(file) {
  #assumes a FLUXNET2015 file, e.g FLX_US-Ha1_FLUXNET2015_FULLSET_HR_1991-2012_1-3.csv
  version <- substr(file, start=nchar(file)-6, stop=nchar(file)-4) 
  return(version)
}

#-----------------------------------------------------------------------------

#' Gets Fluxnet site_code from Fluxnet file.
#' Useful when processing multiple files
#' @export
get_path_site_code <- function(path) {
  #assumes a FLUXNET2015 file, e.g FLX_US-Ha1_FLUXNET2015_FULLSET_HR_1991-2012_1-3.csv
  filename  <- basename(path)
  if (substring(filename[1], 1, nchar("FLX")) == "FLX"){     #better implementation but only for R>3.3: startsWith(filename, "FLX_")) {
    site_code <- substr(filename, start = 5, stop = 10)
  } else {
    site_code <- substr(filename, start=1, stop = 6)
  }
  if (!grepl("^[A-Z]{2}-[A-Za-z0-9]{3}$", site_code[1])) {
    stop("Site code not found in file name")
  }

  return(site_code)
}

#-----------------------------------------------------------------------------

#' Gets La Thuile data policy for each site year
#' @export
get_lathuile_datapolicy <- function(site_code, site_use){
  
  #Read data policy file (must use check.names so R doesn't add "X" in front of colnames)
  use_policy <- read.csv(sites_use, header=TRUE, check.names=FALSE)
  
  #Extract site
  site_policy <- use_policy[use_policy$site==site_code,]

  return(site_policy)
}


#-----------------------------------------------------------------------------

#' Pre-processes OzFlux files that have missing time steps or incomplete years
#' @export
preprocess_OzFlux <- function(infile, outpath) {
  
  library(ncdf4)
  library(tools)
  
  #Open file handle
  nc <- nc_open(infile)
  
  
  #Flag for re-writing data
  rewrite_data <- FALSE
  
  
  ### Get timing information ###
  
  #Read time data and origin
  time_var <- ncvar_get(nc, "time")
  
  time_origin <- strsplit(ncatt_get(nc, "time")$units, "days since ")[[1]][2]
  
  #Convert to Y-M-D h-m-s
  time_date <- as.POSIXct(time_var * 24*60*60,  origin=time_origin, tz="GMT")
  
  
  
  ### Get all data variables with a time dimension ###
  
  #Get variable names
  vars <- names(nc$var)
  
  #Get dimensions for each variable
  dims <- lapply(vars, function(x) sapply(nc[["var"]][[x]][["dim"]], function(dim) dim[["name"]]))

  #Find which variables are time-varying
  var_inds <- which(sapply(dims, function(x) any(x == "time")))
  
  #Load variable data
  var_data <- lapply(vars[var_inds], function(x) ncvar_get(nc, x))
  
  #Set names
  names(var_data) <- vars[var_inds]  
  
  
  ### First check if missing first time step ###
  
  start_time <- format(time_date[1], "%H%M")
  
  #First time step missing
  if (start_time == "0030") {
    
    #Fix missing time step by duplicating first available time step
    
    #First fix time variable (round first available time step to the nearest whole number, i.e. 00:00 start time)
    time_var <- append(floor(time_var[1]), time_var)
    
    #Convert new time vector to Y-M-D h-m-s
    time_date <- as.POSIXct(time_var * 24*60*60,  origin=time_origin, tz="GMT")

    
    #For each time-varying variable, copy first time step
    var_data <- lapply(var_data, function(x) append(x[1], x))
    
    #Data amended, set rewrite flag to TRUE
    rewrite_data <- TRUE
    
      
  #Else if not midnight, stop to decide what to do
  } else if (start_time != "0000") {
    
    stop("Start time unknown, decide on action")
  }
  
  
  
  ### Then check if have incomplete years ###
  
  day_month <- format(time_date, "%m%d")
  
  #Get start date
  start_day <- day_month[1]
  
  #Get end date
  end_day <- day_month[length(day_month)]
  
  
  #If not full years, remove incomplete years
  if (start_day != "0101" | end_day != "1231") {
    

    #Find first instance of Jan 1
    start_ind <- which(day_month == "0101")[1]
    
    #Find last instance of Dec 31
    end_ind <- tail(which(day_month == "1231"), n=1)
    
    
    #Check that have a whole number of days
    
    #No. of time steps per day
    tsteps_per_day <- (24*60) / as.numeric(time_date[2]-time_date[1])
    
    #Total no. of time tsteps
    no_tsteps <- length(start_ind:end_ind)
    
    #Check for an even no. of time steps
    if ( !round(no_tsteps / tsteps_per_day) == (no_tsteps / tsteps_per_day)) {
      stop("Incomplete days present!")
    }
    
    
    #Change size of data variables and time vector
    var_data <- lapply(var_data, function(x) x[start_ind:end_ind])
    
    time_var <- time_var[start_ind:end_ind]
    
    
    #Data amended, set rewrite flag to TRUE
    rewrite_data <- TRUE
    
  }

  
  
  
  ### Write output file ###
  
  
  #Create output directory
  dir.create(outpath, recursive = TRUE)
  
  #Create output filename
  outfile <- paste0(outpath, "/", file_path_sans_ext(basename(infile)), 
                    "_preprocessed.nc")
  
  
  #If nothing amended, copy input file
  if (!rewrite_data) {
    
    #Copy original file
    file.copy(infile, outfile)
    
    
  #If data amended, rewrite time info and time-varying variabless
  } else {
    
    
    ### Set dimensions ###
    
    #Get dimensions from input file
    new_dims <- nc$dim
    
    #Change time dimensions
    #Change values, then length    
    new_dims$time$vals <- time_var
    new_dims$time$len <- length(time_var)
    
    
    ### Define variables ###
    
    #Get variables from input file
    new_vars <- nc$var
    
    #Change dimensions and values for time-varying data
    for (v in 1:length(var_inds)) {
      
      if(new_vars[[var_inds[v]]]$name != names(var_data)[v]) {
        stop("Wrong variable names")
      }
      
      #Change time dimension
      new_vars[[var_inds[v]]]$varsize[3] <- length(time_var)
      
      #Change time values
      new_vars[[var_inds[v]]]$dim[[3]]$vals <- time_var

      #Change time size
      new_vars[[var_inds[v]]]$dim[[3]]$len <- length(time_var)
      
      #Change length
      new_vars[[var_inds[v]]]$len <- length(time_var)
      
      #Change values
      new_vars[[var_inds[v]]]$vals <- var_data[[v]]
      
      #Change chunk size (no idea what this is but produces an error otherwise
      #during nc_create)
      new_vars[[var_inds[v]]]$chunksizes <- NA
    }
    
    
    #Fix missing values (some are text and cause an error)
    for (v in 1:length(new_vars)) {
      
      if(!is.na(new_vars[[v]]$missval)) {
        new_vars[[v]]$missval <- -9999
      }

    }

    
    #Read global attributes from input file
    new_atts <- ncatt_get(nc, varid=0)
    
    
    #Close input file
    nc_close(nc)
    
    
    
    ### Create output NC file ###
    
    out_nc <- nc_create(outfile, vars=new_vars)
    
    
    #Write global attributes to output file
    
    #For some reason this crashes if using lapply, loop works ok-ish    
    for(a in 1:length(new_atts)){
      ncatt_put(out_nc, varid=0, attname=names(new_atts[a]), 
                attval=unlist(new_atts[a]))
    }
    
    
    #Write variables to output file
    for (v in 1:length(new_vars)) {
      
      #CRS returns an error, skip
      if (new_vars[[v]]$name != "crs") {
        
        ncvar_put(nc=out_nc, varid=new_vars[[v]],
                  vals=new_vars[[v]]$vals)
      }
    }

  
    
    #Close output file
    nc_close(out_nc)
    
    
  }
  
  
  

  
}


