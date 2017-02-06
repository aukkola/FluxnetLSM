#' Author: ned haughton
#' Date:
#' --------------
#' Author:
#' Date:
#' Modification:
#' --------------


get_metadata_template <- function(site_code) {
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
}


add_processing_metadata <- function(metadata) {
    metadata["processing"] <- list(
        processor = "FluxnetProcessing",
        URL = "https://github.com/aukkola/FLUXNET2015_processing",
        version = system("git rev-parse --verify HEAD", intern = TRUE,
                         show.output.on.console = FALSE)
    )

    return(metadata)
}


get_site_metadata_CSV <- function(metadata) {

    # TODO: use system.file("help", "aliases.rds", package="FLUXNETProcessing")
    # when this is a proper package.
    # https://stackoverflow.com/questions/3433603/parsing-command-line-arguments-in-r-scripts
    site_csv_file <- "./R/auxiliary_data/Site_info_tier1_only.csv"

    print(paste("Trying to load metadata from csv cache (", site_csv_file, ")"))

    site_code <- metadata[["SiteCode"]]

    csv_data <- as.list(read.csv(site_csv_file, header = TRUE,
                                 stringsAsFactors = FALSE,
                                 row.names = 1)[site_code, ])

    for (n in names(csv_data)) {
        if (!is.na(csv_data[n])) {
            metadata[n] <- csv_data[n]
        }
    }

    return(metadata)
}


check_missing <- function(metadata) {
    missing_data <- is.na(metadata)
    if (missing_data$Exclude_reason & metadata$Exclude) {
        missing_data$Exclude_reason <- FALSE
    }

    return(missing_data)
}


warn_missing <- function(metadata) {
    missing_data <- check_missing(metadata)

    if (any(missing_data)) {
        print(paste("Missing metadata for site ", metadata$SiteCode, ":"))
        print(paste("    ", paste(names(metadata)[missing_data]), collapse = ", "))
    }
}


get_site_metadata_web <- function(metadata) {
    # TODO: stub
    return(metadata)
}


get_site_metadata <- function(site_code, incl_processing=TRUE) {
    metadata <- get_metadata_template(site_code)
    metadata <- get_site_metadata_CSV(metadata)
    if (any(check_missing(metadata))) {
        metadata <- get_site_metadata_web(metadata)
    }

    warn_missing_metadata(metadata)

    if (incl_processing) {
        metadata <- add_processing_metadata(metadata)
    }

    return(metadata)
}
