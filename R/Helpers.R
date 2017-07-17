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



