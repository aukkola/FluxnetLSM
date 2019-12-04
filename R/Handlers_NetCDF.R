# Handlers_NetCDF.R
#
# A collection of functions to read and convert
# flux tower data to netcdf.
#
# author: Anna Ukkola UNSW 2017
#


#' Creates a netcdf file for flux variables
CreateFluxNetcdfFile = function(fluxfilename, datain,            # outfile file and data
                                site_code,                       # Fluxnet site code
                                siteInfo,                        # Site attributes
                                ind_start, ind_end,              # time period indices
                                starttime,                       # timing info
                                flux_varname, cf_name,           # Original Fluxnet variable names and CF_compliant names
                                total_missing, total_gapfilled,  # Percentage missing and gap-filled
                                qcInfo,                          # QC flag values
                                arg_info,                        # Processing information
                                var_ind,                         # Indices to extract variables to be written
                                varnames,                        # Original FLUXNET names corresponding to dataset
                                modelInfo,                       # Model parameters
                                global_atts){                    # Global attributes from original OzFlux nc-files


    # load netcdf library
    library(ncdf4)
    library(lutz) #to get time zone
  
    # Time step size
    timestepsize <- datain$timestepsize

    # Extract time period to be written
    datain$data <- datain$data[ind_start:ind_end,]

    # Define x, y and z dimensions
    xd = ncdim_def('x',vals=c(1),units='')
    yd = ncdim_def('y',vals=c(1),units='')
    dimnchar = ncdim_def("nchar", "", 1:200, create_dimvar=FALSE)

    # Determine data start date and time:
    timeunits = CreateTimeunits(starttime)

    # Create time dimension variable:
    tt=c(0:(length(ind_start:ind_end)-1))
    timedata = as.double(tt*timestepsize)

    # Define time dimension:
    td = ncdim_def('time', unlim=TRUE, units=timeunits, 
                   vals=timedata, calendar="standard")

    
    # VARIABLE DEFINITIONS ##############################################

    # Create variable definitions for time series variables
    var_defs <- lapply(var_ind, function(x) ncvar_def(name=datain$out_vars[x],
                                                      units=datain$units$target_units[x],
                                                      dim=list(xd,yd,td),
                                                      missval=Nc_MissingVal,
                                                      longname=datain$attributes[x,2]))


    # Create model variable definitions if applicable
    if(any(!is.na(modelInfo))){
        model_defs <- define_model_params(modelInfo, list(xd,yd))
    }


    # First necessary non-time variables:
    # Define latitude:
    latdim <- ncvar_def('latitude','degrees_north',dim=list(xd,yd),
                        missval=Nc_MissingVal, longname='Latitude')
    # Define longitude:
    londim <- ncvar_def('longitude','degrees_east',dim=list(xd,yd),
                        missval=Nc_MissingVal,longname='Longitude')


    # Then optional non-time variables:
    opt_vars <- list()
    ctr <- 1
    # Define reference height of tower:
    # Use measurement height if available, else take tower height
    if (!is.na(siteInfo$MeasurementHeight) | !is.na(siteInfo$TowerHeight)) {
        refheight=ncvar_def('reference_height','m',dim=list(xd,yd),
                            missval=Nc_MissingVal,longname='Reference height of flux tower')
        opt_vars[[ctr]] = refheight
        ctr <- ctr + 1
    } 
    # Define site canopy height:
    if(!is.na(siteInfo$CanopyHeight)){
        canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
                            missval=Nc_MissingVal,longname='Canopy height')
        opt_vars[[ctr]] = canheight
        ctr <- ctr + 1
    }
    # Define site elevation:
    if(!is.na(siteInfo$SiteElevation)){
        elev=ncvar_def('elevation','m',dim=list(xd,yd),
                       missval=Nc_MissingVal,longname='Site elevation')
        opt_vars[[ctr]] = elev
        ctr <- ctr + 1
    }
    # Define IGBP short vegetation type:
    if(!is.na(siteInfo$IGBP_vegetation_short)){
        short_veg=ncvar_def('IGBP_veg_short','-',dim=list(dimnchar), missval=NULL,
                            longname='IGBP vegetation type (short)', prec="char")
        opt_vars[[ctr]] = short_veg
        ctr <- ctr + 1
    }
    # Define IGBP long vegetation type:
    if(!is.na(siteInfo$IGBP_vegetation_long)){
        long_veg=ncvar_def('IGBP_veg_long','-',dim=list(dimnchar), missval=NULL,
                           longname='IGBP vegetation type (long)', prec="char")
        opt_vars[[ctr]] = long_veg
        ctr <- ctr + 1
    }
    

    # END VARIABLE DEFINITIONS #########################################

    # Collate variables
    all_vars <- append(var_defs, c(list(latdim), list(londim)))

    if(length(opt_vars)>0) { all_vars <- append(all_vars, opt_vars) }
    if(any(!is.na(modelInfo)))  { all_vars <- append(all_vars, model_defs$nc_vars) }

    # Create
    ncid <- nc_create(fluxfilename, vars=all_vars)


    #### Write global attributes ###
    
    ncatt_put(ncid,varid=0,attname='Production_time',
              attval=as.character(Sys.time()))
    ncatt_put(ncid,varid=0,attname='Github_revision',
              attval=siteInfo$Processing$git_rev, prec="text")
    ncatt_put(ncid,varid=0,attname='site_code',
              attval=site_code, prec="text")
    ncatt_put(ncid,varid=0,attname='site_name',
              attval=as.character(siteInfo$Fullname), prec="text")
    ncatt_put(ncid,varid=0,attname='country',
              attval=as.character(siteInfo$Country), prec="text")
    ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
              attval=arg_info$datasetversion, prec="text")
    if(!is.na(siteInfo$Tier)) {
              ncatt_put(ncid,varid=0,attname='Fluxnet site tier',
              attval=siteInfo$Tier)}
    if(!is.na(siteInfo$Description)) { 
              ncatt_put(ncid,varid=0,attname='site_description',
              attval=as.character(siteInfo$Description), prec="text") }
    if(!is.na(siteInfo$VegetationDescription)) { 
              ncatt_put(ncid,varid=0,attname='vegetation_description',
              attval=as.character(siteInfo$VegetationDescription), prec="text") }
    if(!is.na(siteInfo$SoilType)) { 
              ncatt_put(ncid,varid=0,attname='soil_type',
              attval=as.character(siteInfo$SoilType), prec="text") }
    if(!is.na(siteInfo$Disturbance)) { 
              ncatt_put(ncid,varid=0,attname='disturbance',
              attval=as.character(siteInfo$Disturbance), prec="text") }
    if(!is.na(siteInfo$CropDescription)) { 
              ncatt_put(ncid,varid=0,attname='crop_description',
              attval=as.character(siteInfo$CropDescription), prec="text") }
    if(!is.na(siteInfo$Irrigation)) { 
              ncatt_put(ncid,varid=0,attname='irrigation',
              attval=as.character(siteInfo$Irrigation), prec="text") }
    if(!is.na(siteInfo$TowerStatus)) { 
      ncatt_put(ncid,varid=0,attname='tower_status',
                attval=as.character(siteInfo$TowerStatus), prec="text") }
    
    #Qc flag descriptions
    ncatt_put(ncid,varid=0,attname='QC_flag_descriptions',
              attval=qcInfo, prec="text")

    #Add info for time stamp and time zone
    ncatt_put(ncid,varid="time",attname='info',
              attval="Time stamp indicates start time", prec="text")
    
    ncatt_put(ncid,varid="time",attname='time_zone',
              attval=tz_lookup_coords(siteInfo$SiteLatitude, 
              siteInfo$SiteLongitude, method="accurate"), prec="text")
    
    
    # args info
    add_processing_info(ncid, arg_info, datain, cat="Flux")


    # contact info
    ncatt_put(ncid,varid=0,attname='Package contact',
              attval='a.ukkola@unsw.edu.au')


    # Add variable data to file:
    ncvar_put(ncid, latdim, vals=siteInfo$SiteLatitude)
    ncvar_put(ncid, londim, vals=siteInfo$SiteLongitude)


    # Optional meta data for each site:
    if(!is.na(siteInfo$SiteElevation)) {
        ncvar_put(ncid,elev,vals=siteInfo$SiteElevation)}
    if(!is.na(siteInfo$CanopyHeight)) {
        ncvar_put(ncid,canheight,vals=siteInfo$CanopyHeight)}
    if(!is.na(siteInfo$IGBP_vegetation_short)) {
        ncvar_put(ncid,short_veg,vals=sprintf("%-200s", siteInfo$IGBP_vegetation_short))}
    if(!is.na(siteInfo$IGBP_vegetation_long)) {
        ncvar_put(ncid,long_veg,vals=sprintf("%-200s", siteInfo$IGBP_vegetation_long))}
    
    #Reference height, use measurement if available, else tower height
    if (!is.na(siteInfo$MeasurementHeight)) { 
      ncvar_put(ncid,refheight,vals=siteInfo$MeasurementHeight)
      #also add source of data as an attribute
      ncatt_put(nc=ncid, varid=refheight, attname="Source",
                attval="measurement height", prec="text")
    } else if (!is.na(siteInfo$TowerHeight)) {
      ncvar_put(ncid,refheight,vals=siteInfo$TowerHeight)
      #also add source of data as an attribute
      ncatt_put(nc=ncid, varid=refheight, attname="Source",
                attval="tower height", prec="text")
    }

    # Time dependent variables:
    lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid,
                                                     varid=var_defs[[x]],
                                                     vals=datain$data[,var_ind[x]]))


    # Add original Fluxnet variable name to file
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="Fluxnet_name",
                                                     attval=datain$attributes[var_ind[x],1],
                                                     prec="text"))

    # Add CF-compliant name to file (if not missing)
    lapply(1:length(var_defs), function(x)  ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                      attname="standard_name",
                                                      attval=datain$attributes[var_ind[x],3],
                                                      prec="text"))

    #Also add this for time, lat and lon
    ncatt_put(nc=ncid, varid="time", attname="standard_name",       #time
              attval="time", prec="text")
    ncatt_put(nc=ncid, varid="latitude", attname="standard_name",   #lat
              attval="latitude", prec="text")
    ncatt_put(nc=ncid, varid="longitude", attname="standard_name",  #lon
              attval="longitude", prec="text")
    
    
    # Add CMIP name to file (if not missing)
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="CMIP_short_name",
                                                     attval=datain$attributes[var_ind[x],4],
                                                     prec="text"))
    
    # Add missing percentage to file
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="Missing_%",
                                                     attval=round(total_missing[x],1)))

    # Add gap-filled percentage to file
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="Gap-filled_%",
                                                     attval=round(total_gapfilled[x],1)))

    
    # Add variable-specific gap-filling methods to file
    if(!is.na(arg_info$flux_gapfill)){

        gap_methods <- datain$gapfill_flux[which(!is.na(datain$gapfill_flux))]
        gap_vars <- names(gap_methods)
        lapply(1:length(var_defs), function(x) if(any(gap_vars==names(var_defs[[x]]$name))){
               ncatt_put(nc=ncid, varid=var_defs[[x]],
                         attname="Gapfilling_method",
                         attval=gap_methods[names(var_defs[[x]]$name)],
                         prec="text")} )
    }


    # Add model parameters
    if(any(!is.na(modelInfo))){

        # If created new variables, add to file
        if(length(model_defs$nc_vars) > 0){
            lapply(1:length(model_defs$nc_vars), function(x) ncvar_put(nc=ncid,
                                                                       varid=model_defs$nc_vars[[x]],
                                                                       vals=model_defs$vals[[x]]))
        }
    }


    # Add OzFlux global attributes if using this dataset
    if (!is.na(global_atts[1])) {
      
      for(n in 1:length(global_atts)) {
        ncatt_put(nc=ncid, varid=0, attname=names(global_atts)[n],
                  attval=global_atts[[n]])
      }
    }

    
    
    # Close netcdf file:
    nc_close(ncid)
}


#-----------------------------------------------------------------------------


# TODO: This function exists in palsR/Gab and has a different signature. Merge?
#' Creates a netcdf file for met variables
CreateMetNetcdfFile = function(metfilename, datain,             # outfile file and data
                               site_code,                       # Fluxnet site code
                               siteInfo,                        # Site attributes
                               ind_start, ind_end,              # time period indices
                               starttime,                       # timing info
                               flux_varname, cf_name,           # Original Fluxnet variable names and CF_compliant names
                               av_precip=NA,                    # average annual rainfall
                               total_missing, total_gapfilled,  # Percentage missing and gap-filled
                               qcInfo,                          # QC flag values
                               qc_name,                         # Name of qc flag in original dataset
                               arg_info,                        # Arguments passed to main function
                               var_ind,                         # Indices to extract variables to be written
                               varnames,                        # Original FLUXNET names corresponding to dataset
                               modelInfo,                       # Model parameters
                               global_atts){                    # Global attributes from original OzFlux nc-files

    # load netcdf library
    library(ncdf4)
    library(lutz) #to get time zone
  
  
    # Time step size
    timestepsize <- datain$timestepsize

    # Extract time period to be written
    datain$data <- datain$data[ind_start:ind_end,]

    # Define x, y and z dimensions
    xd = ncdim_def('x',vals=c(1),units='')
    yd = ncdim_def('y',vals=c(1),units='')
    #zd = ncdim_def('z',vals=c(1),units='')
    dimnchar = ncdim_def("nchar", "", 1:200, create_dimvar=FALSE )

    # Determine data start date and time:
    timeunits = CreateTimeunits(starttime)

    # Create time dimension variable:
    tt=c(0:(length(ind_start:ind_end)-1))
    timedata = as.double(tt*timestepsize)

    # Define time dimension:
    td = ncdim_def('time', unlim=TRUE, units=timeunits, 
                   vals=timedata, calendar="standard")

    
    # VARIABLE DEFINITIONS ##############################################

    # #First set correct dimensions (Tair, Qair, CO2air and Wind need an extra z-dimension, as well
    # #as their QC flags)
    # N.B. not using this anymore as ALMA convention specifies these as simple x-y variables
    # ind_dim <- which(names(datain$out_vars) %in% c(varnames$tair, paste0(varnames$tair, qc_name),
    #                                                varnames$relhumidity, paste0(varnames$relhumidity, qc_name),
    #                                                varnames$wind, paste0(varnames$wind, qc_name),
    #                                                varnames$co2, paste0(varnames$co2, qc_name)))
    # 
    # dims    <- lapply(var_ind, function(x) if(x %in% ind_dim) list(xd,yd,zd,td) else list(xd,yd,td))

    dims    <- lapply(var_ind, function(x) list(xd,yd,td))
    
    
    # Create variable definitions for time series variables
    var_defs <- mapply(function(i, dim) ncvar_def(name=datain$out_vars[i],
                                                  units=datain$units$target_units[i],
                                                  dim=dim, missval=Nc_MissingVal,
                                                  longname=datain$attributes[i,2]), 
                       i=var_ind, dim=dims, SIMPLIFY=FALSE)

    # Create model variable definitions if applicable
    if(any(!is.na(modelInfo))){
        model_defs <- define_model_params(modelInfo, list(xd,yd))
    }


    # First necessary non-time variables:
    # Define latitude:
    latdim <- ncvar_def('latitude','degrees_north',dim=list(xd,yd),
                        missval=Nc_MissingVal, longname='Latitude')
    # Define longitude:
    londim <- ncvar_def('longitude','degrees_east',dim=list(xd,yd),
                        missval=Nc_MissingVal,longname='Longitude')


    # Then optional non-time variables:
    opt_vars <- list()
    ctr <- 1
    # Define reference height of tower:
    # Use measurement height if available, else take tower height
    if (!is.na(siteInfo$MeasurementHeight) | !is.na(siteInfo$TowerHeight)) {
      refheight=ncvar_def('reference_height','m',dim=list(xd,yd),
                          missval=Nc_MissingVal,longname='Reference height of flux tower')
      opt_vars[[ctr]] = refheight
      ctr <- ctr + 1
    } 
    # Define site canopy height:
    if(!is.na(siteInfo$CanopyHeight)){
      canheight=ncvar_def('canopy_height','m',dim=list(xd,yd),
                          missval=Nc_MissingVal,longname='Canopy height')
      opt_vars[[ctr]] = canheight
      ctr <- ctr + 1
    }
    # Define site elevation:
    if(!is.na(siteInfo$SiteElevation)){
      elev=ncvar_def('elevation','m',dim=list(xd,yd),
                     missval=Nc_MissingVal,longname='Site elevation')
      opt_vars[[ctr]] = elev
      ctr <- ctr + 1
    }
    # Define IGBP short vegetation type:
    if(!is.na(siteInfo$IGBP_vegetation_short)){
      short_veg=ncvar_def('IGBP_veg_short','-',dim=list(dimnchar), missval=NULL,
                          longname='IGBP vegetation type (short)', prec="char")
      opt_vars[[ctr]] = short_veg
      ctr <- ctr + 1
    }
    # Define IGBP long vegetation type:
    if(!is.na(siteInfo$IGBP_vegetation_long)){
      long_veg=ncvar_def('IGBP_veg_long','-',dim=list(dimnchar), missval=NULL,
                         longname='IGBP vegetation type (long)', prec="char")
      opt_vars[[ctr]] = long_veg
      ctr <- ctr + 1
    }
    

    # END VARIABLE DEFINITIONS #########################################

    ### Create netcdf file ###

    # Collate variables
    all_vars <- append(var_defs, c(list(latdim), list(londim)))

    if(length(opt_vars)>0) { all_vars <- append(all_vars, opt_vars) }
    if(any(!is.na(modelInfo)))  { all_vars <- append(all_vars, model_defs$nc_vars) }

    # Create
    ncid <- nc_create(metfilename, vars=all_vars)


    #### Write global attributes ###
    
    ncatt_put(ncid,varid=0,attname='Production_time',
              attval=as.character(Sys.time()))
    ncatt_put(ncid,varid=0,attname='Github_revision',
              attval=siteInfo$Processing$git_rev, prec="text")
    ncatt_put(ncid,varid=0,attname='site_code',
              attval=site_code, prec="text")
    ncatt_put(ncid,varid=0,attname='site_name',
              attval=as.character(siteInfo$Fullname), prec="text")
    ncatt_put(ncid,varid=0,attname='country',
              attval=as.character(siteInfo$Country), prec="text")
    ncatt_put(ncid,varid=0,attname='Fluxnet_dataset_version',
              attval=arg_info$datasetversion, prec="text")
    if(!is.na(siteInfo$Tier)) {
      ncatt_put(ncid,varid=0,attname='Fluxnet site tier',
                attval=siteInfo$Tier)}
    if(!is.na(siteInfo$Description)) { 
      ncatt_put(ncid,varid=0,attname='site_description',
                attval=as.character(siteInfo$Description), prec="text") }
    if(!is.na(siteInfo$VegetationDescription)) { 
      ncatt_put(ncid,varid=0,attname='vegetation_description',
                attval=as.character(siteInfo$VegetationDescription), prec="text") }
    if(!is.na(siteInfo$SoilType)) { 
      ncatt_put(ncid,varid=0,attname='soil_type',
                attval=as.character(siteInfo$SoilType), prec="text") }
    if(!is.na(siteInfo$Disturbance)) { 
      ncatt_put(ncid,varid=0,attname='disturbance',
                attval=as.character(siteInfo$Disturbance), prec="text") }
    if(!is.na(siteInfo$CropDescription)) { 
      ncatt_put(ncid,varid=0,attname='crop_description',
                attval=as.character(siteInfo$CropDescription), prec="text") }
    if(!is.na(siteInfo$Irrigation)) { 
      ncatt_put(ncid,varid=0,attname='irrigation',
                attval=as.character(siteInfo$Irrigation), prec="text") }
    if(!is.na(siteInfo$TowerStatus)) { 
      ncatt_put(ncid,varid=0,attname='tower_status',
                attval=as.character(siteInfo$TowerStatus), prec="text") }
    
    #Qc flag descriptions
    ncatt_put(ncid,varid=0,attname='QC_flag_descriptions',
              attval=qcInfo, prec="text")
    
    
    #Add info for time stamp and time zone
    ncatt_put(ncid,varid="time",attname='info',
              attval="Time stamp indicates start time", prec="text")
    
    ncatt_put(ncid,varid="time",attname='time_zone',
              attval=tz_lookup_coords(siteInfo$SiteLatitude, 
              siteInfo$SiteLongitude, method="accurate"), prec="text")
    
    
    # args info
    add_processing_info(ncid, arg_info, datain, cat="Met")
    
    
    # contact info
    ncatt_put(ncid,varid=0,attname='Package contact',
              attval='a.ukkola@unsw.edu.au')
    
    
    # Add variable data to file:
    ncvar_put(ncid, latdim, vals=siteInfo$SiteLatitude)
    ncvar_put(ncid, londim, vals=siteInfo$SiteLongitude)
    
    
    # Optional meta data for each site:
    if(!is.na(siteInfo$SiteElevation)) {
      ncvar_put(ncid,elev,vals=siteInfo$SiteElevation)}
    if(!is.na(siteInfo$CanopyHeight)) {
      ncvar_put(ncid,canheight,vals=siteInfo$CanopyHeight)}
    if(!is.na(siteInfo$IGBP_vegetation_short)) {
      ncvar_put(ncid,short_veg,vals=sprintf("%-200s", siteInfo$IGBP_vegetation_short))}
    if(!is.na(siteInfo$IGBP_vegetation_long)) {
      ncvar_put(ncid,long_veg,vals=sprintf("%-200s", siteInfo$IGBP_vegetation_long))}
    
    #Reference height, use measurement if available, else tower height
    if (!is.na(siteInfo$MeasurementHeight)) { 
      ncvar_put(ncid,refheight,vals=siteInfo$MeasurementHeight)
      #also add source of data as an attribute
      ncatt_put(nc=ncid, varid=refheight, attname="Source",
                attval="measurement height", prec="text")
    } else if (!is.na(siteInfo$TowerHeight)) {
      ncvar_put(ncid,refheight,vals=siteInfo$TowerHeight)
      #also add source of data as an attribute
      ncatt_put(nc=ncid, varid=refheight, attname="Source",
                attval="tower height", prec="text")
    }
    

    # Time dependent variables:
    lapply(1:length(var_defs), function(x) ncvar_put(nc=ncid,
                                                     varid=var_defs[[x]],
                                                     vals=datain$data[,var_ind[x]]))


    # Add original Fluxnet variable name to file
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]], attname="Fluxnet_name",
                                                     attval=datain$attributes[var_ind[x],1], prec="text"))

    # Add CF-compliant name to file (if not missing)
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="standard_name",
                                                     attval=datain$attributes[var_ind[x],3],
                                                     prec="text"))
  
    #Also add this for time, lat and lon
    ncatt_put(nc=ncid, varid="time", attname="standard_name",       #time
              attval="time", prec="text")
    ncatt_put(nc=ncid, varid="latitude", attname="standard_name",   #lat
              attval="latitude", prec="text")
    ncatt_put(nc=ncid, varid="longitude", attname="standard_name",  #lon
              attval="longitude", prec="text")
    
    
    
    # Add CMIP name to file (if not missing)
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="CMIP_short_name",
                                                     attval=datain$attributes[var_ind[x],4],
                                                     prec="text"))
    

    # Add ERA-Interim name to file when available (if used)
    if(!is.na(arg_info$met_gapfill) & (arg_info$met_gapfill=="ERAinterim")){
        lapply(1:length(var_defs), function(x) if(!is.na(datain$era_vars[var_ind[x]])){
               ncatt_put(nc=ncid, varid=var_defs[[x]],
                         attname="ERA-Interim variable used in gapfilling",
                         attval=datain$era_vars[var_ind[x]],
                         prec="text")})
    }

    # Add missing percentage to file
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="Missing_%",
                                                     attval=round(total_missing[x],1)))

    # Add gap-filled percentage to file
    lapply(1:length(var_defs), function(x) ncatt_put(nc=ncid, varid=var_defs[[x]],
                                                     attname="Gap-filled_%",
                                                     attval=round(total_gapfilled[x],1)))


    # Add variable-specific gap-filling methods to file
    if(!is.na(arg_info$met_gapfill)){

        gap_methods <- datain$gapfill_met[which(!is.na(datain$gapfill_met))]
        gap_vars <- names(gap_methods)
        lapply(1:length(var_defs), function(x) if(any(gap_vars==names(var_defs[[x]]$name))){
               ncatt_put(nc=ncid, varid=var_defs[[x]],
                         attname="Gapfilling_method",
                         attval=gap_methods[names(var_defs[[x]]$name)],
                         prec="text")} )
    }


    # Add model parameters
    if(any(!is.na(modelInfo))){

        # If created new variables, add to file
        if(length(model_defs$nc_vars) > 0){
            lapply(1:length(model_defs$nc_vars), function(x) ncvar_put(nc=ncid,
                                                                       varid=model_defs$nc_vars[[x]],
                                                                       vals=model_defs$vals[[x]]))
        }
    }

    
    # Add OzFlux global attributes if using this dataset
    if (!is.na(global_atts[1])) {
      
      for(n in 1:length(global_atts)) {
        ncatt_put(nc=ncid, varid=0, attname=names(global_atts)[n],
                  attval=global_atts[[n]])
      }
    }
    
    
    # Close netcdf file:
    nc_close(ncid)
}

#-----------------------------------------------------------------------------

#' Writes model parameters as global attribute to NetCDF file
define_model_params <- function(modelInfo, dims){

    nc_vars <- list()
    vals    <- list()

    # Loop through model variables
    for(k in 1:length(modelInfo)){

        val <- modelInfo[[k]]$varvalue

        # Check that value is not missing
        if(val != Nc_MissingVal & val != Sprd_MissingVal & !is.na(val)){

            # Append to netcdf variables
            nc_vars <- append(nc_vars, list(ncvar_def(name=modelInfo[[k]]$varname,
                                                      units=modelInfo[[k]]$units,
                                                      dim=dims,
                                                      missval=Nc_MissingVal,
                                                      longname=modelInfo[[k]]$longname)))

            # Save value
            vals <- append(vals, list(modelInfo[[k]]$varvalue))
        }
    }


    # Collate var definitions and values
    outs <- list(nc_vars=nc_vars, vals=vals)

    return(outs)
}

#-----------------------------------------------------------------------------

#' Writes attributes common to met and flux NC files
add_processing_info <- function(ncid, arg_info, datain, cat){

    # Input file
    ncatt_put(ncid,varid=0,attname='Input_file',
              attval=arg_info$infile, prec="text")


    # Processing thresholds
    ncatt_put(ncid,varid=0,attname='Processing_thresholds(%)',
              attval=paste("missing: ", arg_info$missing,
                           ", gapfill_all: ", arg_info$gapfill_all,
                           ", gapfill_good: ", arg_info$gapfill_good,
                           ", gapfill_med: ", arg_info$gapfill_med,
                           ", gapfill_poor: ", arg_info$gapfill_poor,
                           ", min_yrs: ", arg_info$min_yrs,
                           sep=""), prec="text")

    # Aggregation
    if(!is.na(arg_info$aggregate)){
        ncatt_put(ncid,varid=0,attname='Timestep_aggregation',
                  attval=paste("Aggregated from", (datain$original_timestepsize/60/60),
                               "hours to", datain$timestepsize/60/60, "hours"))
    }


    # Gapfilling info
    # Met data
    if(cat=="Met" & !is.na(arg_info$met_gapfill)){

        ncatt_put(ncid,varid=0,attname='Gapfilling_method',
                  attval=arg_info$met_gapfill, prec="text")

        if(arg_info$met_gapfill=="statistical") {

            ncatt_put(ncid,varid=0,attname='Gapfilling_thresholds',
                      attval=paste("linfill: ", arg_info$linfill,", copyfill: ", arg_info$copyfill,
                                   ", lwdown_method: ", arg_info$lwdown_method, sep=""),
                      prec="text")

        } else if(arg_info$met_gapfill=="ERAinterim"){

            ncatt_put(ncid,varid=0,attname='ERAinterim_file',
                      attval=arg_info$era_file, prec="text")
        }


    # Flux data
    } else if(cat=="Flux" & !is.na(arg_info$flux_gapfill)){

        ncatt_put(ncid,varid=0,attname='Gapfilling_method',
                  attval=arg_info$flux_gapfill, prec="text")

        ncatt_put(ncid,varid=0,attname='Gapfilling_thresholds',
                  attval=paste("linfill: ", arg_info$linfill,", copyfill: ", arg_info$copyfill,
                               ", regfill: ", arg_info$regfill, sep=""),
                  prec="text")
    }


    # La Thuile fair use
    if(arg_info$datasetname=="LaThuile"){
        ncatt_put(ncid,varid=0,attname="LaThuile_fair_use_policies",
                  attval=arg_info$fair_use, prec="text")
    }

    # FLUXNET2015 subset
    if(arg_info$datasetname=="FLUXNET2015"){
        ncatt_put(ncid,varid=0,attname='FLUXNET2015_version',
                  attval=arg_info$flx2015_version, prec="text")
    }


}
