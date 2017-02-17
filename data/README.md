README for auxiliary data files
===============================


Fluxnet_site_info.csv
------------------------

SiteCode
: Short site ID code. As provided at http://fluxnet.fluxdata.org/sites/site-list-and-pages/.

Fullname
: Full site name. As provided at http://fluxnet.fluxdata.org/sites/site-list-and-pages/.

SiteLatitude
: Site latitude in degrees north. As provided at http://fluxnet.fluxdata.org/sites/site-list-and-pages/.

SiteLongitude
: Site longitude in degrees east. As provided at http://fluxnet.fluxdata.org/sites/site-list-and-pages/.

SiteElevation
: Elevation in meters. Retrieved from http://fluxnet.fluxdata.org/sites/site-list-and-pages/ where available. Otherwise retrieved from https://fluxnet.ornl.gov/ or individual flux tower network websites (such as http://ozflux.org.au/). If elevation could not be found in any of the former sources, it was extracted from the approximately 1km spatial resolution Global 30 arc-sec Elevation (GTOPO30) elevation data set (https://lta.cr.usgs.gov/GTOPO30) using site coordinates.

SiteVegetation
: IGBP site vegetation type. As provided at http://fluxnet.fluxdata.org/sites/site-list-and-pages/.

TowerHeight
: Height of flux tower. As reported at https://fluxnet.ornl.gov/ (under “tower height”) or individual flux tower network websites (such as http://ozflux.org.au/).

CanopyHeight
: Average or maximum canopy height at site. Should be treated STRICTLY AS INDICATIVE only. Some sites report the maximum height and others average height; height measurement is not standardised across sites. Canopy height information was retrieved from site descriptions (or site vegetation descriptions) at https://fluxnet.ornl.gov/ or individual flux tower network websites (such as http://ozflux.org.au/ or http://ameriflux.lbl.gov/).

Tier
: Tier of flux tower site (refer to http://fluxnet.fluxdata.org/data/data-policy/ for data policy). Only Tier 1 sites of Fluxnet2015 November 2016 release currently provided in this file.


Fluxnet_variables.csv
-------------

NB. Variables can be added and removed as per user requirements as long as all fields are provided. Fluxnet variables can be duplicated as per example file (e.g. RH, outputted as two ALMA variables, RH and Qair). However, the following options will not work unless required variables are present in variables.csv:

LWdown synthesis/gap_filling
: The code will attempt to synthesise and/or gap-fill LWdown if “LWdown_fill” flag is set to TRUE and the time series is not complete. The LW_IN_F_MDS variable is not required to be present in the Fluxnet2015 data file, but the variable and the corresponding quality control flag (LW_IN_F_MDS and LW_IN_F_MDS_QC, respectively) must be present in the variable.csv file to perform the gap-filling.

Fluxnet_variable
: Name of data variable in the Fluxnet2015 dataset. See http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/ for full variable descriptions.

Fluxnet_unit
: Units of the data variable in the Fluxnet2015 dataset. See http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/fullset-data-product/ for variable units. The code attempts to convert Fluxnet units into ALMA convention for commonly required variables if units differ. If conversion is not available, check data units and compare to those used in Conversions.R. If conversion is not available, recommend converting to required units prior to processing files.

Fluxnet_class
: Specifies whether a column should be read in as “integer” or “numeric” (generally set to “integer” for quality control flags and “numeric” otherwise). If unsure, set to “numeric”.

ALMA_variable
: Name of variable to be written in output NetCDF file. Follows ALMA convention (http://www.lmd.jussieu.fr/~polcher/ALMA/convention_output_3.html) where possible.

ALMA_unit
: Required output unit. If different from input units (i.e. Fluxnet_unit), the code will attempt to convert the units. If conversion is not available, an error will be produced. Recommend converting units before or after processing data in this case.

Longname
: Long variable name to be written in output NetCDF file.

Data_min and Data_max
: Minimum and maximum allowable values for a variable. The code checks that the variables to be written into the output NetCDF are within the specified range.

Essential_met
: TRUE or FALSE. Specifies if a meteorological variable is essential to be processed. Code will abort if a variable is set as TRUE but is not present in the file.

Preferred_eval
: TRUE or FALSE. Specifies which evaluation (flux) variables variables are preferred. If none of the preferred variables are present in the file, code will abort.

Category
: Met or Flux. Determines if a variable is written into the met or flux NetCDF output file.

