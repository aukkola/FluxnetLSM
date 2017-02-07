#' Author: ned haughton
#' Date:
#' --------------
#' Author:
#' Date:
#' Modification:
#' --------------

library(rvest)  # read_html, html_node, html_attr, html_table


#################################################
# General metadata functions
################################################

#' Empty site metadata template, including only the site code
#'
#' @return metadata list
site_metadata_template <- function(site_code) {
    metadata <- list(
        SiteCode = site_code,
        Fullname = NULL,
        SiteLatitude = NaN,
        SiteLongitude = NaN,
        SiteElevation = NaN,
        IGBP_vegetation_short = NULL,
        IGBP_vegetation_long = NULL,
        TowerHeight = NaN,
        CanopyHeight = NaN,
        Tier = NaN,
        Exclude = FALSE,
        Exclude_reason = NULL
    )

    return(metadata)
}


#' Get site name from metadata
get_site_code <- function(metadata){
    return(metadata[["SiteCode"]])
}


#' Adds processor metadata, including processor version
#'
#' @return metadata list
add_processing_metadata <- function(metadata) {
    metadata$Processing <- list(
        processor = "FluxnetProcessing",
        URL = "https://github.com/aukkola/FLUXNET2015_processing",
        # TODO: add git tags if we start using them.
        git_rev = system("git rev-parse --verify HEAD", intern = TRUE)
    )

    return(metadata)
}


################################################
# CSV-stored metadata
################################################

#' Tries to gather metadata from the included site CSV
#'
#' @return metadata list
get_site_metadata_CSV <- function(metadata) {

    # TODO: use system.file("help", "aliases.rds", package="FLUXNETProcessing")
    # when this is a proper package.
    # https://stackoverflow.com/questions/3433603/parsing-command-line-arguments-in-r-scripts
    site_csv_file <- "./R/auxiliary_data/Site_info_tier1_only.csv"

    cat(paste0("Trying to load metadata from csv cache (", site_csv_file, ")"), "\n")

    site_code <- get_site_code(metadata)

    csv_row <- as.list(read.csv(site_csv_file, header = TRUE,
                                stringsAsFactors = FALSE,
                                row.names = 1)[site_code, ])

    for (n in names(csv_row)) {
        if (!is.na(csv_row[n])) {
            metadata[n] <- csv_row[n]
        }
    }

    return(metadata)
}


################################################
# Web-based metadata
################################################

#' Get all available site codes from site_status table
get_ornl_site_codes <- function() {
    status_table_url <- "https://fluxnet.ornl.gov/site_status"

    page_html <- read_html(status_table_url)

    table <- page_html %>% html_node("#historical_site_list") %>% html_table()

    site_codes = table["FLUXNET ID"]

    return(site_codes)

}


#' Get ORNL site URL from site_status table
get_site_ornl_url <- function(site_code) {
    status_table_url <- "https://fluxnet.ornl.gov/site_status"

    page_html <- read_html(status_table_url)

    # looks for table cell with site code as contents, then looks up the parent
    # row, and finds the href of the first link.
    xpath =  paste0("//td[text()='", site_code, "']/..")
    ornl_rel_url <- page_html %>% html_node(xpath = xpath) %>%
        html_node("a") %>%
        html_attr("href")

    ornl_url <- paste0("https://fluxnet.ornl.gov/", ornl_rel_url)

    return(ornl_url)

}


#' Get metadata from ORNL
#'
#' @return metadata list
get_site_metadata_ornl <- function(metadata) {
    site_code <- get_site_code(metadata)

    site_url <- get_site_ornl_url(site_code)
    metadata$ORNL_URL <- site_url

    cat(paste0("Trying to load metadata from ORNL (", site_url, ")"), "\n")

    page_html <- read_html(site_url)

    # General info
    table <- page_html %>% html_node("table#fluxnet_site_information") %>% html_table()
    metadata$Fullname <- table[table[1] == "Site Name:"][2]
    metadata$Description <- table[table[1] == "Description:"][2]
    metadata$TowerStatus <- table[table[1] == "Tower Status:"][2]

    # Location Information
    table <- page_html %>% html_node("table#fluxnet_site_location_information") %>% html_table()
    metadata$Country <- table[table[1] == "Country:"][2]
    lat_lon <- strsplit(table[table[1] == "Coordinates:(Lat, Long)"][2], ", ")[[1]]
    metadata$SiteLatitude <- as.numeric(lat_lon[1])
    metadata$SiteLongitude <- as.numeric(lat_lon[1])

    # Site Characteristics
    table <- page_html %>% html_node("table#fluxnet_site_characteristics") %>% html_table()
    metadata$SiteElevation <- table[table[1] == "GTOPO30 Elevation:"][2]
    metadata$IGBP_vegetation_long <- table[table[1] == "IGBP Land Cover:"][2]
    # ORNL doesn't have any of these:
    #    metadata$IGBP_vegetation_short = NULL,
    #    metadata$TowerHeight = NaN,
    #    metadata$CanopyHeight = NaN,
    #    metadata$Tier = NaN,
    #    metadata$Exclude = FALSE,
    #    metadata$Exclude_reason = NULL

    # TODO: ORNL has other potentially useful site information, affiliation info,
    # and investigator info. Should we use some?

    return(metadata)
}


#' Tries to load metadata from known Fluxnet info sources on the 'web
#'
#' @return metadata list
get_site_metadata_web <- function(metadata) {
    metadata <- get_site_metadata_ornl(metadata)

    # TODO: Add loaders for OzFlux, AmeriFlux, etc.

    return(metadata)
}


################################################
# metadata checks
################################################

#' Checks which metadata are missing (correcting for OK NAs)
#'
#' @return boolean metadata availability vector
check_missing <- function(metadata) {
    missing_data <- is.na(metadata)
    if (missing_data["Exclude_reason"] & metadata$Exclude) {
        missing_data["Exclude_reason"] <- FALSE
    }

    return(missing_data)
}


#' Warns about missing metadata for the site
warn_missing_metadata <- function(metadata) {
    missing_data <- check_missing(metadata)

    if (any(missing_data)) {
        cat(paste("Missing metadata for site ", metadata$SiteCode, ":"), "\n")
        cat("   ", paste(names(metadata)[missing_data], collapse = ", "), "\n")
    }
}


################################################
# Main metadata functions
################################################

#' Get site metadata. Tries multiple methods to retrieve full metadata
#'
#' @return metadata list
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
        # TODO: metadata_to_csv(metadata)
    }

    return(metadata)
}
