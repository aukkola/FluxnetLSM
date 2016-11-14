# ConvertOzFlux2PALS.R
#
# Converts data from several OzFlux formatted spreadsheets
# a single PALS template spreadsheet.
#
# Gab Abramowitz, Nadja Herger UNSW 2016 (palshelp at gmail dot com)

rm(list=ls()) # clear all variables
library(gdata)
library(pals)
library(stringr)
library(ncdf4)


basedir = '~/data/'
scriptdir = '~/Documents/admin/PALS/scripts/palsR/scripts/'

SaveToLOG = TRUE
source = 'DINGO' # DINGO (spreadsheets) or OzFlux (netcdf)?

# Site specific information
#--------------------------------------
name='Yanco'
#--------------------------------------
source(paste0(scriptdir,'FluxTowerSiteInfo.R'))

FluxTemplateVersion = '1.0.3'
PALSversion = '1.4'

qcScript = paste(basedir,'flux_tower/PALS/ObsUpload/OzFlux/',
                 sitename,'QC.R',sep='')
csvfile = paste(basedir,'flux_tower/PALS/ObsUpload/OzFlux',
                '/',sitename,'Conversion',FluxTemplateVersion,'.csv',sep='')

metfilename = paste0(basedir,'flux_tower/PALS/ObsNc/',sitename,source,PALSversion,'met.nc')
fluxfilename = paste0(basedir,'flux_tower/PALS/ObsNc/',sitename,source,PALSversion,'flux.nc')

# Start writing to an output file
if(SaveToLOG){
  sink(paste0(basedir,'flux_tower/PALS/ObsUpload/OzFlux/',name,source,'LOG.txt'))
}

if(source == 'DINGO'){
	sourcedir = paste0(basedir,'flux_tower/OzFlux/DINGO')
}else if(source == 'OzFlux'){ # Use OzFlux nc data
	sourcedir = paste0(basedir,'flux_tower/OzFlux/',name,'/')
}

# Save all Fluxnet file names and list to screen:
fnet = list.files(path = sourcedir,pattern=sitename,full.names=TRUE)
cat('Found',length(fnet),'file(s) for site:',name,' \n')

# Load PALS template spreadsheet (4x34):
PALSinit = as.matrix(read.xls(
  paste(basedir,'flux_tower/PALS/ObsUpload/PALSFluxTowerTemplate',
        FluxTemplateVersion,'.xls',sep='')))

ncols = 34	# is the number of variables + 2 (date and hour)

# Initialise time step counter, knowing header has to replicate PALS template
tctr = 4 

cat('Loading:',fnet,'\n')
# Read Fluxnet file:	
fdata = as.matrix(read.csv(fnet))	 
time = as.double(substr(fdata[,1],12,13))
months = as.integer(substr(fdata[,1],6,7))

# Site specific adjustments: Set appropriate start and end dates
# Last date: fdata[dim(fdata)[1],1]
# First date: fdata[1,1] 
if(name=='Tumbarumba'){
  # First date: "2001-01-01 00:00:00"
  # Last date: "2013-12-31 22:00:00" -> "2013-12-31 23:00:00"
  # Last hour is missing; Simply copy the last timestep
  fdata = rbind(fdata, fdata[(dim(fdata)[1]),])
  fdata[ (dim(fdata)[1]), 1] = "2013-12-31 23:00:00"  
  fdata[dim(fdata)[1],50] = "23"
}else if(name=='AdelaideRiver'){
  # First date: "2007-01-01 00:00:00"
  # Last date: "2009-05-24 06:00:00" -> "2009-05-23 23:30:00" -> "2008-12-31 23:30:00"
  idx = which(time==23 & months==12)
  fdata = fdata[1:idx[length(idx)],] 
}else if(name=='AliceSprings'){
  # Last date: "2015-03-02 01:30:00" -> "2015-03-01 23:30:00" -> "2014-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2010-09-03 00:00:00" -> "2011-01-01 00:00:00"
  idx_start = which(months==1)
  fdata = fdata[idx_start[1]:idx_end[length(idx_end)],]  
}else if(name=='Cumberland'){
  # Last date: "2013-12-31 00:00:00" -> "2013-12-30 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2012-01-01 00:30:00" -> "2012-01-01 00:00:00"
  # Duplicate first date to create "2012-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2012-01-01 00:00:00"
  fdata[1,56] = " 0.0"
  # Repeat Dec 30 to make up Dec 31
  fdata = rbind(fdata,fdata[34993:35040,]) # before: 35040, new: 35088
  substr(fdata[35041:35088,1],9,10) = "31"
}else if(name=='Daintree'){
  # Last date: "2014-06-27 19:30:00" -> "2014-06-26 23:30:00" -> "2013-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2011-01-01 00:30:00" -> "2011-01-01 00:00:00"
  # Duplicate first date to create "2011-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2011-01-01 00:00:00"
  fdata[1,55] = " 0.0"
}else if(name=='DalyPasture'){
  # First date: "2007-01-01 00:00:00"
  # Last date: "2013-09-08 13:00:00" -> "2013-09-07 23:30:00" -> "2012-12-31 23:30:00"
  idx = which(time==23 & months==12)
  fdata = fdata[1:idx[length(idx)],]
}else if(name=='DalyRegrowth'){
  # First date: "2007-01-01 00:00:00"
  # Last date: "2010-05-10 11:30:00" -> "2010-05-09 23:30:00" -> "2009-12-31 23:30:00"
  idx = which(time==23 & months==12)
  fdata = fdata[1:idx[length(idx)],]
}else if(name=='DalyUncleared'){
  # First date: "2007-01-01 00:00:00"
  # Last date: "2014-10-13 16:30:00" -> "2014-10-12 23:30:00" -> "2013-12-31 23:30:00"
  idx = which(time==23 & months==12)
  fdata = fdata[1:idx[length(idx)],]
}else if(name=='Dargo'){
  # Last date: "2013-12-31 23:30:00"
  # First date: "2007-01-01 00:30:00" -> "2007-01-01 00:00:00"
  # Duplicate first date to create "2007-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata)
  fdata[1,1] = "2007-01-01 00:00:00"
  fdata[1,68] = " 0.0"
}else if(name=='DryRiver'){
  # First date: "2008-01-01 00:00:00"
  # Last date: "2014-10-13 11:00:00" -> "2014-10-12 23:30:00" -> "2013-12-31 23:30:00"
  idx = which(time==23 & months==12)
  fdata = fdata[1:idx[length(idx)],]
}else if(name=='FoggDam'){
  # Last date: "2008-12-31 23:30:00"
  # First date: "2006-02-07 00:00:00" -> "2007-01-01 00:00:00"
  # Start in "2007-01-01 00:00:00"
  idx = which(months==1)
  fdata = fdata[idx[1]:dim(fdata)[1],]
}else if(name=='Gingin'){
  # Last date: "2013-11-30 23:30:00" -> "2012-12-31 23:30:00" 
  idx_end = which(time==23 & months==12)
  # First date: "2011-01-01 00:30:00" -> "2011-01-01 00:00:00"
  # Duplicate first date to create "2011-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2011-01-01 00:00:00"
  fdata[1,174] = " 0.0"
}else if(name=='GWW'){
  # Last date: "2014-07-30 00:00:00" -> "2014-07-29 23:30:00" -> "2013-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2013-01-01 00:30:00" -> "2013-01-01 00:00:00"
  # Duplicate first date to create "2013-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2013-01-01 00:00:00"
  fdata[1,56] = " 0.0"
}else if(name=='HowardSprings'){
  # Last date: "2014-12-31 23:30:00"
  # First date: "2001-01-01 00:30:00" -> "2001-01-01 00:00:00"
  # Duplicate first date to create "2001-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata)
  fdata[1,1] = "2001-01-01 00:00:00"
  fdata[1,74] = " 0.0"
}else if(name=='Nimmo'){
  # Last date: "2013-12-31 23:30:00"
  # First date: "2007-01-01 00:30:00" -> "2007-01-01 00:00:00"
  # Duplicate first date to create "2007-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata)
  fdata[1,1] = "2007-01-01 00:00:00"
  fdata[1,57] = " 0.0"
}else if(name=='RDMF'){
  # Last date: "2013-07-21 23:30:00" -> "2012-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2011-09-23 09:00:00" -> "2012-01-01 00:00:00"
  # Start in "2012-01-01 00:00:00"
  idx = which(months==1)
  fdata = fdata[idx[1]:idx_end[length(idx_end)],]
}else if(name=='Riggs'){
  # Last date: "2015-03-13 07:00:00" -> "2015-03-12 23:30:00" -> "2014-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2011-01-01 00:30:00" -> "2011-01-01 00:00:00"
  # Duplicate first date to create "2011-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2011-01-01 00:00:00"
  fdata[1,147] = " 0.0"
}else if(name=='RobsonCreek'){
  # First date: "2013-01-01 00:00:00"
  # Last date: "2014-06-28 18:00:00" -> "2014-06-27 23:30:00" -> "2013-12-31 23:30:00"
  idx = which(time==23 & months==12)
  fdata = fdata[1:idx[length(idx)],]
}else if(name=='Samford'){
  # Last date: "2014-07-01 00:00:00" -> "2014-06-30 23:30:00" -> "2013-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2011-01-01 00:30:00" -> "2011-01-01 00:00:00"
  # Duplicate first date to create "2011-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2011-01-01 00:00:00"
  fdata[1,48] = " 0.0"
}else if(name=='SturtPlains'){
  # First date: "2008-01-01 00:00:00"
  # Last date: "2015-03-18 06:30:00" -> "2015-03-17 23:30:00" -> "2014-12-31 23:30:00"
  idx = which(time==23 & months==12)
  fdata = fdata[1:idx[length(idx)],]
}else if(name=='TiTree'){
  # Last date: "2015-03-09 09:00:00" -> "2015-03-08 23:30:00" -> "2014-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2012-07-18 00:00:00" -> "2013-01-01 00:00:00"
  idx_start = which(months==1)
  fdata = fdata[idx_start[1]:idx_end[length(idx_end)],] 
}else if(name=='Wallaby'){
  # Last date: "2013-12-31 23:30:00"
  # First date: "2005-01-01 00:30:00" -> "2005-01-01 00:00:00"
  # Duplicate first date to create "2005-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata)
  fdata[1,1] = "2005-01-01 00:00:00"
  fdata[1,190] = " 0.0"
}else if(name=='Warra'){
  # First date: "2013-03-05 15:00:00" -> "2014-01-01 00:00:00"
  idx_start = which(months==1)
  # Last date: "2014-12-31 23:00:00" -> "2014-12-31 23:30:00"
  # Duplicate last date to create "2014-12-31 23:30:00"
  fdata = rbind(fdata[idx_start[1]:dim(fdata)[1],],fdata[dim(fdata)[1],])
  fdata[dim(fdata)[1],1] = "2014-12-31 23:30:00"
  fdata[dim(fdata)[1],181] = "23.5"  
}else if(name=='Whroo'){
  # Last date: "2014-12-31 23:30:00" 
  # First date: "2011-12-01 13:30:00" -> "2012-01-01 00:00:00"
  # Start in "2012-01-01 00:00:00"
  idx = which(months==1)
  fdata = fdata[idx[1]:dim(fdata)[1],]
}else if(name=='Wombat'){
  # Last date: "2015-03-04 07:00:00" -> "2015-03-03 23:30:00" -> "2014-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2010-01-01 00:30:00" -> "2010-01-01 00:00:00"
  # Duplicate first date to create "2010-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2010-01-01 00:00:00"
  fdata[1,193] = " 0.0"
}else if(name=='Yanco_JAXA'){
  # Last date: "2014-03-02 13:00:00" -> "2014-03-01 23:30:00" -> "2013-12-31 23:30:00"
  idx_end = which(time==23 & months==12)
  # First date: "2012-01-01 00:30:00" -> "2012-01-01 00:00:00"
  # Duplicate first date to create "2012-01-01 00:00:00"
  fdata = rbind(fdata[1,], fdata[1:idx_end[length(idx_end)],])
  fdata[1,1] = "2012-01-01 00:00:00"
  fdata[1,62] = " 0.0"
}

time = as.double(substr(fdata[,1],12,13))
unitime = unique(time)

# Check if the spreadsheet data contains a whole number of days
if(time[length(time)] != unitime[length(unitime)]){
  cat('Spreadsheet data does not contain a whole number of days. \n')
}

# Check if spreadsheet data starts with 1st of January
if(substr(fdata[1,1],6,10) != "01-01"){
  cat('Spreadsheet data does not begin on January 1st. \n')
}

# Read time info
tstepsize = as.numeric(difftime(fdata[2,1],fdata[1,1],units="hours")) # Time, stepsize in hours
years = as.integer(substr(fdata[,1],1,4)) 
uniyear = unique(years) 
days = as.integer(substr(fdata[,1],9,10))
months = as.integer(substr(fdata[,1],6,7))

# Determine total length of file (number of timesteps)
tlength = dim(fdata)[1]	

PALSt = matrix(NA,(tlength+3),ncols) # init (3 is the header) 
PALSt[1:3,1:ncols] = PALSinit[1:3,1:ncols] # write header from template

# Start of data:      
starttime = list(time=as.double(substr(fdata[1,1],12,13)),day=days[1],
                 month=months[1],year=uniyear[1],timestepsize=tstepsize)
tsteps = length(fdata[,1]) # number of timesteps in 1 file
cat('Found',tsteps,'time steps for',length(fdata[1,]),'variables. \n')
variables = attributes(fdata[1,])$names
# Write date data:
for(t in 1:tsteps){     
  PALSt[(t+tctr-1),1] = paste(as.character(days[t]),'/',as.character(months[t]),
                              '/',as.character(years[t]),sep='') # write data in format day/month/year
}
PALSt[tctr:(tctr+tsteps-1),2] = NA2MissVal(as.double(fdata[,which(variables=='Hdh')])) # Hour-of-day data: Hdh
PALSt[tctr:(tctr+tsteps-1),3] = NA2MissVal(as.double(fdata[,which(variables=='Fsd_Con')])) # SWdown, Fsd_Con
PALSt[tctr:(tctr+tsteps-1),4] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fsd_Con_QCFlag')])) # SWdownFLAG, Fsd_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),5] = NA2MissVal(as.double(fdata[,which(variables=='Fld_Con')])) # LWdown, Fld_Con
PALSt[tctr:(tctr+tsteps-1),6] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fld_Con_QCFlag')])) # LWdownFLAG, Fld_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),7] = NA2MissVal(as.double(fdata[,which(variables=='Ta_Con')])) # Tair, Ta_Con
PALSt[tctr:(tctr+tsteps-1),8] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Ta_Con_QCFlag')])) # TairFLAG, Ta_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),9] = NA2MissVal(as.double(fdata[,which(variables=='Ah_Con')])) # Rel H --> Qair, Ah_Con  <-- Abs. instead of specific humidity
PALSt[tctr:(tctr+tsteps-1),10] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Ah_Con_QCFlag')])) # RHFLAG <-- QairFLAG, Ah_Con_QCFlag
if(name=='AdelaideRiver' | name=='Cumberland' | name=='DalyPasture' | name=='DalyRegrowth' | name=='DalyUncleared' | name=='DryRiver' | name=='FoggDam' | name=='Gingin' | name=='HowardSprings' | name=='RDMF' | name=='Riggs' | name=='SturtPlains' | name=='TiTree' | name=='Wallaby' | name=='Whroo' | name=='Wombat' | name=='Yanco_JAXA'){
  PALSt[tctr:(tctr+tsteps-1),11] = NA2MissVal(as.double(fdata[,which(variables=='Ws_CSAT_Con')])) # Windspeed, Ws_CSATCon
  PALSt[tctr:(tctr+tsteps-1),12] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Ws_CSAT_Con_QCFlag')])) # WSFLAG, Ws_CSAT_Con_QCFlag
}else{
  PALSt[tctr:(tctr+tsteps-1),11] = NA2MissVal(as.double(fdata[,which(variables=='Ws_Con')])) # Windspeed, Ws_Con
  PALSt[tctr:(tctr+tsteps-1),12] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Ws_Con_QCFlag')])) # WSFLAG, Ws_Con_QCFlag
}
PALSt[tctr:(tctr+tsteps-1),13] = NA2MissVal(as.double(fdata[,which(variables=='Precip_Con')])) # Rainfall, Precip_Con (mm/30min)
PALSt[tctr:(tctr+tsteps-1),14] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Precip_Con_QCFlag')])) # RainfFLAG, Precip_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),15] = -9999 # snowf
PALSt[tctr:(tctr+tsteps-1),16] = -9999 # snowfFLAG
PALSt[tctr:(tctr+tsteps-1),17] = NA2MissVal(as.double(fdata[,which(variables=='ps_Con')])) # pressure, ps_Con (convert from kPa to Pa)
PALSt[tctr:(tctr+tsteps-1),18] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='ps_Con_QCFlag')])) # pressureFLAG, ps_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),19] = NA2MissVal(as.double(fdata[,which(variables=='Cc')])) # CO2, Cc (CO2 conc., umol/mol = ppmv)
PALSt[tctr:(tctr+tsteps-1),20] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Cc_QCFlag')])) # CO2flag, Cc_QCFlag 
PALSt[tctr:(tctr+tsteps-1),21] = NA2MissVal(as.double(fdata[,which(variables=='Fn_Con')])) # Rnet, Fn_Con
PALSt[tctr:(tctr+tsteps-1),22] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fn_Con_QCFlag')])) # RnetFLAG, Fn_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),23] = NA2MissVal(as.double(fdata[,which(variables=='Fsu_Con')])) # SWup, Fsu_Con
PALSt[tctr:(tctr+tsteps-1),24] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fsu_Con_QCFlag')])) # SWupFLAG, Fsu_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),25] = NA2MissVal(as.double(fdata[,which(variables=='Fe_Con')])) # LE (latent heat), Fe_Con
PALSt[tctr:(tctr+tsteps-1),26] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fe_Con_QCFlag')])) # LEFLAG, Fe_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),27] = NA2MissVal(as.double(fdata[,which(variables=='Fh_Con')])) # H, Fh_Con
PALSt[tctr:(tctr+tsteps-1),28] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fh_Con_QCFlag')])) # HFLAG, Fh_Con_QCFlag
PALSt[tctr:(tctr+tsteps-1),29] = NA2MissVal(as.double(fdata[,which(variables=='Fc_ustar')])) # NEE, Fc_ustar (Net ecosystem exchange CO2)
PALSt[tctr:(tctr+tsteps-1),30] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fc_Con_QCFlag')])) # NEEFLAG, Fc_Con_QCFlag 
PALSt[tctr:(tctr+tsteps-1),31] = NA2MissVal(as.double(fdata[,which(variables=='GPP_Con')])) # GPP, GPP_Con
PALSt[tctr:(tctr+tsteps-1),32] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fc_Con_QCFlag')])) # GPPFLAG, same as NEEFlag
PALSt[tctr:(tctr+tsteps-1),33] = NA2MissVal(as.double(fdata[,which(variables=='Fg_Con')])) # G (soil heat flux), Fg_Con (ground heat flux)
PALSt[tctr:(tctr+tsteps-1),34] = OzFlux2PALSQCFlag(as.double(fdata[,which(variables=='Fg_Con_QCFlag')])) # GFLAG, Fg_Con_QCFlag 

# Increment time step counter
tctr = tctr + tsteps

# Calculate mean temperature
meanT = MeanValue(PALSt, 7)
if(SaveToLOG == 'no'){
  cat('The average annual temperature is',meanT,'°C \n')
  cat('Station data ranges from',PALSt[4,1],'(',PALSt[4,2],') to',PALSt[dim(PALSt)[1],1],'(',PALSt[dim(PALSt)[1],2],') \n')
}

# Find Missing Values
if(SaveToLOG == 'no'){
  FindMissingValue(PALSt,-9999)
}

# Perform QC for this data set if required:
if(qcScript != ''){
  cat('Calling QC for this dataset... \n')
  source(qcScript)	
  PALSt = datasetqc(PALSt,templateVersion=FluxTemplateVersion,starttime)
}

if(SaveToLOG == 'no'){
  meanT = MeanValue(PALSt, 7)
  cat('The average annual temperature is',meanT,'°C \n')
}
cat('Station data ranges from',PALSt[4,1],'(',PALSt[4,2],') to',PALSt[dim(PALSt)[1],1],'(',PALSt[dim(PALSt)[1],2],') \n')

# Write csv file:
cat('Writing OzFlux data to PALS template file... \n')
write.csv(as.data.frame(PALSt,optional=TRUE),file=csvfile,
          row.names=FALSE,quote=FALSE)

# Stop writing to the file
if(SaveToLOG){
  sink()
}

# Create variables needed by ConvertSpreadsheetToNcdf
fileinname = csvfile

# Create met and flux NetCDF files
source('/media/nadja/Documents/CCRC/palsR/scripts/ConvertSpreadsheetToNcdf.R') # write NetCDF files

cresult = readline(prompt = 'Conversion okay?')
if(cresult != ''){stop()}

# Open graphics device:
QCplotfile = paste(basedir,'flux_tower/PALS/ObsUpload/OzFlux/',sitename,'QCplot.png',sep='')
png(filename=QCplotfile, width=1200, height=2200, pointsize=24) # creates empty png file

# Generate and view QC plots:
source('/media/nadja/Documents/CCRC/palsR/scripts/QCplotsSpreadsheet.R') # QC plots
dev.off()
system(paste('eog',QCplotfile)) 




