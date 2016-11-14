# ConvertSpreadsheetToNcdf.R
#
# Converts data from a PALS formatted spreadhseet to
# netcdf.
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)


defaultLWsynthesis = 'Abramowitz (2012)' # 'Brutsaert (1975)' or 'Swinbank (1963)' or 'Abramowitz (2012)'

# Read text file containing flux data:
DataFromText = ReadTextFluxData(fileinname)

# Make sure whole number of days in dataset:
CheckSpreadsheetTiming(DataFromText)

# Check which variables are actually present:
found = CheckTextDataVars(DataFromText) # list of variables, TRUE or FALSE

if(!found$LWdown){ # no LWdown found
	DataFromText$data$LWdown = SynthesizeLWdown((DataFromText$data$Tair+zeroC),
		DataFromText$data$Qair,defaultLWsynthesis)
	DataFromText$data$LWdownFlag = 0
	found$LWdown_qc=TRUE
}else if(found$LWdown & !found$LWdown_all){ # only some LWdown found
	filledLWdown = gapfillLWdown(DataFromText$data$LWdown,
		(DataFromText$data$Tair+zeroC),DataFromText$data$Qair,
		defaultLWsynthesis)
	DataFromText$data$LWdown = filledLWdown$data
	DataFromText$data$LWdownFlag = filledLWdown$flag
	found$LWdown_qc=TRUE
}

# Change units:
ConvertedData = ChangeMetUnits(DataFromText,found,elevation)

# Run preliminary tests on data:
CheckTextDataRanges(ConvertedData,found)

# Create netcdf met driving file:
CreateMetNcFile(metfilename,ConvertedData,latitude,longitude,
	DataFromText$timestepsize,sitename,datasetversion,defaultLWsynthesis,
	found,DataFromText$starttime,DataFromText$templateVersion,
	elevation=elevation,measurementheight=measurementheight,
	canopyheight=canopyheight,vegetationtype=vegetationtype,
	utcoffset=utcoffset,avprecip=avprecip,avtemp=avtemp)

# Create netcdf flux data file:
CreateFluxNcFile(fluxfilename, ConvertedData,latitude,longitude,
	DataFromText$timestepsize,sitename,datasetversion,
	found,DataFromText$starttime,DataFromText$templateVersion,
	elevation=elevation,measurementheight=measurementheight,
	canopyheight=canopyheight,vegetationtype=vegetationtype,
	utcoffset=utcoffset,avprecip=avprecip,avtemp=avtemp)
