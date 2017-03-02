# Author: ned haughton

# Uses rvest library for read_html, html_node, html_attr, html_table


#################################################
# General metadata functions
################################################

#' Empty site metadata template, including only the site code
#'
#' @return metadata list
#' @export
site_metadata_template <- function(site_code) {
    metadata <- list(
        SiteCode = site_code,
        Fullname = NA,
        SiteLatitude = NA,
        SiteLongitude = NA,
        SiteElevation = NA,
        IGBP_vegetation_short = NA,
        IGBP_vegetation_long = NA,
        TowerHeight = NA,
        CanopyHeight = NA,
        Tier = NA,
        Exclude = FALSE,
        Exclude_reason = NA
    )

    return(metadata)
}


#' Get site name from metadata
#' @export
get_site_code <- function(metadata){
    return(metadata[["SiteCode"]])
}


#' Gets the git version from the installed package
#' See src/zzz.R for how git revision is discovered
get_git_version <- function() {
    desc <- read.dcf(system.file("DESCRIPTION", package = "FluxnetLSM"))
    if ("git_revision" %in% colnames(desc)) {
        git_rev <- desc[1, "git_revision"]
    } else if ("RemoteSha" %in% colnames(desc)) {
        git_rev <- desc[1, "RemoteSha"]
    } else {
        git_rev <- "UNKNOWN"
                
        warning_message <- paste("Unknown git revision of FluxnetLSM. Please",
                                 "visit https://github.com/aukkola/FluxnetLSM and",
                                 "review the installation procedure")
        site_log["Warnings"] <- paste(site_log["Warnings"], warning_message, sep=" ##### ")
        
        warning(warning_message)
    }
    return(git_rev)
}


#' Adds processor metadata, including processor version
#'
#' @return metadata list
#' @export
add_processing_metadata <- function(metadata) {
    metadata$Processing <- list(
        processor = "FluxnetLSM",
        URL = "https://github.com/aukkola/FluxnetLSM",
        version = packageVersion("FluxnetLSM"),
        git_rev = get_git_version()
    )

    return(metadata)
}


#' Updates old metadata with new metadata, ignoring NAs, and warning on differences
#'
#' @return metadata list
#' @export
update_metadata <- function(metadata, new_metadata, overwrite=TRUE) {
    for (n in names(new_metadata)) {
        if (!is.na(new_metadata[[n]])) {
            if (n %in% names(metadata) && !is.na(metadata[[n]]) &&
                new_metadata[[n]] != metadata[[n]]) {
                overwrite_text = if (overwrite) "Overwriting" else "Not overwriting"
                
                warning_message <- paste("New metadata for ", n, " has different values! ",
                                         overwrite_text, ".\n", "  old: ", metadata[n], 
                                         ", new: ", new_metadata[n], sep="")
                #Append to log
                site_log["Warnings"] <- paste(site_log["Warnings"], warning_message, sep=" ##### ")                
                message(warning_message)
            }
            metadata[n] <- new_metadata[n]
        }
    }

    return(metadata)
}


################################################
# CSV-stored metadata
################################################

# TODO: use system.file("help", "aliases.rds", package="FluxnetLSM")
# when this is a proper package.
# https://stackoverflow.com/questions/3433603/parsing-command-line-arguments-in-r-scripts

#Find site info file path (not using data() command directly because reads a CSV with a
#semicolon separator and this leads to incorrect table headers)

site_csv_file <- system.file("data", "Site_metadata.csv", package = "FluxnetLSM")

#' Tries to gather metadata from the included site CSV
#'
#' @return metadata list
#' @export
get_site_metadata_CSV <- function(metadata) {

    site_code <- get_site_code(metadata)

    message("Loading metadata for ", site_code, " from csv cache (", site_csv_file, ")")

    csv <- read.csv(site_csv_file, header = TRUE,
                    stringsAsFactors = FALSE, row.names = 1)

    if (site_code %in% row.names(csv)) {
        csv_row <- as.list(csv[site_code, ])
        metadata = update_metadata(metadata, csv_row)
    } else {
        message("    ", site_code, " not found in CSV file")
    }

    return(metadata)
}


#' Writes metadata to CSV, only updating non-NA data
#' @export
save_metadata_to_csv <- function(metadata) {
    save_metadata_list_to_csv(list(metadata))
}


#' Write multiple site metadata to list at once
#' @export
save_metadata_list_to_csv <- function(metadata_lists) {

    to_save <- list("SiteCode", "Fullname", "Description", "TowerStatus",
                    "Country", "SiteLatitude", "SiteLongitude", "SiteElevation",
                    "IGBP_vegetation_short", "IGBP_vegetation_long",
                    "TowerHeight", "CanopyHeight", "Tier"
                    )

    csv_data <- read.csv(site_csv_file, header = TRUE,
                         stringsAsFactors = FALSE,
                         row.names = 1)

    for (metadata in metadata_lists) {
        site_code <- metadata$SiteCode
        for (v in to_save) {
            if (v %in% names(metadata) & !is.na(metadata[[v]])) {
                csv_data[site_code, v] <- metadata[[v]]
            }
        }
    }

    write.csv(csv_data, site_csv_file)
}


#' Reads all ORNL data into the CSV file
#' @export
update_csv_from_ornl <- function() {
    ornl_site_codes <- get_ornl_site_codes()
    ornl_url_list <- get_ornl_site_url_list(ornl_site_codes)
    message(length(ornl_url_list), " sites found.")

    metadata_lists <- list()
    for (site_code in ornl_site_codes) {
        metadata_lists[[site_code]] <- get_ornl_site_metadata(site_metadata_template(site_code),
                                                              ornl_url_list[[site_code]])
    }

    message("Saving ORNL metadata to ", site_csv_file)
    save_metadata_list_to_csv(metadata_lists)
}

################################################
# Web-based metadata
################################################

### fluxnet.ornl.gov ###

#' Get all available site codes from site_status table
#' @export
get_ornl_site_codes <- function() {
  
    library(rvest)
    status_table_url <- "https://fluxnet.ornl.gov/site_status"

    page_html <- read_html(status_table_url)

    table <- page_html %>% html_node("#historical_site_list") %>% html_table()

    site_codes <- sort(table[["FLUXNET ID"]])

    return(site_codes)

}


#' Get a single ORNL site URL from site_status table
#' @export
get_site_ornl_url <- function(site_code) {
    ornl_url <- get_ornl_site_url_list(list(site_code))[[site_code]]
    return(ornl_url)
}


#' Get a list of ORNL site URLs from site_status table
#' @export
get_ornl_site_url_list <- function(site_code_list) {
 
    library(rvest)
    status_table_url <- "https://fluxnet.ornl.gov/site_status"

    page_html <- read_html(status_table_url)

    ornl_url_list <- list()

    for (site_code in site_code_list) {
        # looks for table cell with site code as contents, then looks up the parent
        # row, and finds the href of the first link.
        xpath =  paste0("//td[text()='", site_code, "']/..")
        ornl_rel_url <- page_html %>% html_node(xpath = xpath) %>%
            html_node("a") %>%
            html_attr("href")

        ornl_url_list[[site_code]] <- paste0("https://fluxnet.ornl.gov/", ornl_rel_url)
    }

    return(ornl_url_list)
}


#' Get metadata from ORNL
#'
#' @return metadata list
#' @export
get_ornl_site_metadata <- function(metadata, site_url=NULL, overwrite=TRUE) {
  
    library(rvest)
    site_code <- get_site_code(metadata)

    if (is.null(site_url)) {
        site_url <- get_site_ornl_url(site_code)
        metadata$ORNL_URL <- site_url
    }

    message("Trying to load metadata for ", site_code, " from ORNL (", site_url, ")")

    page_html <- read_html(site_url)

    new_metadata = list()
    # General info
    table <- page_html %>% html_node("table#fluxnet_site_information") %>% html_table()
    new_metadata$Fullname <- table[table[1] == "Site Name:"][2]
    new_metadata$Description <- table[table[1] == "Description:"][2]
    new_metadata$TowerStatus <- table[table[1] == "Tower Status:"][2]

    # Location Information
    table <- page_html %>% html_node("table#fluxnet_site_location_information") %>% html_table()
    new_metadata$Country <- table[table[1] == "Country:"][2]
    lat_lon <- strsplit(table[table[1] == "Coordinates:(Lat, Long)"][2], ", ")[[1]]
    new_metadata$SiteLatitude <- as.numeric(lat_lon[1])
    new_metadata$SiteLongitude <- as.numeric(lat_lon[2])

    # Site Characteristics
    tryCatch({
        table <- page_html %>% html_node("table#fluxnet_site_characteristics") %>% html_table()
        elevation_text <- table[table[1] == "GTOPO30 Elevation:"][2]
        new_metadata$SiteElevation <- as.numeric(gsub("m", "", elevation_text))
        new_metadata$IGBP_vegetation_long <- table[table[1] == "IGBP Land Cover:"][2]
    }, error = function(cond) {
        message(site_code, " doesn't have a Site Characteristics table at ", site_url)
    })

    # ORNL doesn't have any of these:
    #    metadata$IGBP_vegetation_short = NULL,
    #    metadata$TowerHeight = NaN,
    #    metadata$CanopyHeight = NaN,
    #    metadata$Tier = NaN,
    #    metadata$Exclude = FALSE,
    #    metadata$Exclude_reason = NULL

    # TODO: ORNL has other potentially useful site information, affiliation info,
    # and investigator info. Should we use some?

    metadata = update_metadata(metadata, new_metadata, overwrite=overwrite)

    return(metadata)
}


### Fluxdata.org ###

#' Get a single fluxdata_org site URL from site_status table
#' @export
get_site_fluxdata_org_url <- function(site_code) {
    fluxdata_org_url <- paste0("http://sites.fluxdata.org/", site_code, "/")
    return(fluxdata_org_url)
}


#' Get metadata from Fluxdata.org
#'
#' @return metadata list
#' @export
get_fluxdata_org_site_metadata <- function(metadata, site_url=NULL) {
    library(rvest)

    site_code <- get_site_code(metadata)

    new_metadata = list()
    
    if (is.null(site_url)) {
        site_url <- get_site_fluxdata_org_url(site_code)
        new_metadata$fluxdata_org_URL <- site_url
    }

    message("Trying to load new_metadata for ", site_code, " from Fluxdata.org (", site_url, ")")

    page_html <- read_html(site_url)

    # General info
    table <- page_html %>% html_node("table.maininfo") %>% html_table()
    new_metadata$Fullname <- table[table[1] == "Site Name:"][2]
    new_metadata$SiteLatitude <- as.numeric(table[table[1] == "Latitude:"][2])
    new_metadata$SiteLongitude <- as.numeric(table[table[1] == "Longitude:"][2])

    elevation_text <- table[table[1] == "Elevation (m):"][2]
    elevation <- as.numeric(gsub("m", "", elevation_text))
    if (!is.na(elevation)) {  # lots of Fluxdata.org elevtions are missing, don't overwrite
    new_metadata$SiteElevation <- elevation
    }

    IGBP_text = strsplit(gsub("\\)", "", table[table[1] == "IGBP:"][2]), " \\(")[[1]]
    new_metadata$IGBP_vegetation_short <- IGBP_text[1]
    new_metadata$IGBP_vegetation_long <- IGBP_text[2]

    # Fluxdata.org doesn't have any of these:
    #    metadata$Description
    #    metadata$TowerStatus
    #    metadata$Country
    #    metadata$TowerHeight
    #    metadata$CanopyHeight
    #    metadata$Tier

    # TODO: fluxdata_org has other potentially useful site information, affiliation info,
    # and investigator info. Should we use some?

    metadata = update_metadata(metadata, new_metadata)

    return(metadata)
}


#' Tries to load metadata from known Fluxnet info sources on the 'web
#'
#' @return metadata list
#' @export
get_site_metadata_web <- function(metadata) {
    metadata <- get_fluxdata_org_site_metadata(metadata)

    if (any(check_missing(metadata))) {
        metadata <- get_ornl_site_metadata(metadata, overwrite=FALSE)
    }

    # TODO: Add loaders for OzFlux, AmeriFlux, etc.

    return(metadata)
}


################################################
# metadata checks
################################################

#' Checks which metadata are missing (correcting for OK NAs)
#'
#' @return boolean metadata availability vector
#' @export
check_missing <- function(metadata) {
    key_data = c("SiteCode", "Fullname",
                 "SiteLatitude", "SiteLongitude", "SiteElevation",
                 "IGBP_vegetation_short", "IGBP_vegetation_long",
                 "TowerHeight", "CanopyHeight", "Tier")

    missing_data <- is.na(metadata[key_data])

    return(missing_data)
}


#' Warns about missing metadata for the site
#' @export
warn_missing_metadata <- function(metadata) {
    missing_data <- check_missing(metadata)

    if (any(missing_data)) {
        message("Missing metadata for site ", metadata$SiteCode, ":")
        message("   ", paste(names(metadata)[missing_data], collapse = ", "))
    }
}


################################################
# Main metadata functions
################################################

#' Get site metadata. Tries multiple methods to retrieve full metadata
#'
#' @return metadata list
#' @export
get_site_metadata <- function(site_code, incl_processing=TRUE,
                              use_csv=TRUE, update_csv=FALSE) {
    metadata <- site_metadata_template(site_code)

    if (use_csv) {
        metadata <- get_site_metadata_CSV(metadata)
    }

    if (any(check_missing(metadata))) {
        metadata <- get_site_metadata_web(metadata)
    }

    warn_missing_metadata(metadata)

    if (incl_processing) {
        metadata <- add_processing_metadata(metadata)
    }

    if (update_csv) {
        save_metadata_to_csv(metadata)
    }

    return(metadata)
}
