#' Gets a Fluxnet file template (or full file name if site codes and years are specified)
#'
#' @export
get_fluxnet_file_template <- function(site_code = "??-???", datasetname = "FLUXNET2015",
                                     subset = "FULLSET", resolution = "[A-Z]{2}",
                                     years = "[0-9]{4}-[0-9]{4}", datasetversion = "[0-9]{1}-[0-9]{1}",
                                     extension=".csv") {
    version <- gsub("\\.", "-", datasetversion)
    if (is.character(resolution) & nchar(resolution) > 0) {
        file_template <- paste("FLX", site_code, datasetname, subset, resolution,
                               years, version, sep = "_")
        file_template <- paste0(file_template, extension)
    } else {
        file_template <- paste("FLX", site_code, datasetname, subset, years,
                               version, sep = "_")
        file_template <- paste0(file_template, extension)
    }

    return(file_template)
}


#' Gets a Fluxnet ERA interim file template (or full file name if site codes and years are specified)
#'
#' @export
get_fluxnet_erai_template <- function(site_code = "??-???", ...) {
    return(get_fluxnet_file_template(site_code = site_code, subset = "ERAI", ...))
}


#' Gets Fluxnet file(s) available at a given path
#'
#' @export
get_fluxnet_files <- function(path, site_code = "??-???", ...) {
    template <- get_fluxnet_file_template(site_code = site_code, ...)
    files <- list.files(path, template, full.names = TRUE)

    return(files)
}


#' Gets Fluxnet ERA Interim file(s) available at a given path
#'
#' @export
get_fluxnet_erai_files <- function(path, site_code = "??-???", ...) {
    template <- get_fluxnet_erai_template(site_code = site_code, ...)
    files <- list.files(path, template, full.names = TRUE)

    return(files)
}



#' Gets Fluxnet dataset version from Fluxnet file
#'
#' @export
get_fluxnet_version_no <- function(file, ...) {
  #assumes a FLUXNET2015 file, e.g FLX_US-Ha1_FLUXNET2015_FULLSET_HR_1991-2012_1-3.csv
  version <- substr(file, start=nchar(file)-6, stop=nchar(file)-4) 
  return(version)
}
