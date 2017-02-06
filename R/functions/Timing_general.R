# Timing_general.R
#
# Functions that assess or convert timing variables.
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#

#-----------------------------------------------------------------------------

Yeardays = function(startyear,ndays) {
	# Returns: an integer vector of possible number of days in each year of a 
	# dataset, and whether it contains a whole number of years
	if(ndays<365){
		whole=FALSE
		daysperyear = ndays
	}
	daysperyear = c()
	ctr=0 # initialise
	year = startyear # initialise
	days=ndays # initialise
	lpyrs = 0
	# Incrementally remove year of days from total number of days:
	repeat {
		ctr = ctr + 1
		if(is.leap(year)){	
			days = days - 366
			daysperyear[ctr] = 366
			lpyrs = lpyrs + 1
		}else{
			days = days - 365
			daysperyear[ctr] = 365
		}
		year = year + 1
		if(days<365){
			if(days>0 && days!=(365-lpyrs)){ # ie. after removing whole years, days are left over
				daysperyear[ctr+1] = days
				whole=FALSE
			}else if(days==(365-lpyrs)){ # i.e. non leap year data set
				daysperyear[ctr+1] = days
				whole=TRUE
			}else{ # =0
				whole=TRUE
			}
			break
		}
	}
	# Create return list:
	yeardays = list(daysperyear=daysperyear,whole=whole)
	return(yeardays)
}

#-----------------------------------------------------------------------------

is.leap = function(year){
	if((((year %% 4)==0) & ((year %% 100)!=0)) || 
		(((year %% 4)==0) & ((year %% 400)==0))){
		leap=TRUE	
	}else{
		leap=FALSE
	}
	return(leap)
}

#-----------------------------------------------------------------------------

getMonthDays = function(leap=FALSE) {
	# The days on which each month begins:
    if (leap) {    #  J   F   M   A    M    J    J    A    S    O    N    D    J
        month_start=c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367)
        feb = 29
    } else {
        month_start=c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
        feb = 28
    }
	month_length=c(31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

	return(list(start=month_start,length=month_length))
}

#-----------------------------------------------------------------------------

doydate = function(doy,leap=FALSE){
	# Doydate returns the day of month and month, given day of year:
	month=getMonthDays(leap)
	# Find month of this doy
	for(m in 1:12){
		if(doy >= month$start[m] && doy < month$start[m+1]){
			doymonth = m
			doyday = doy - month$start[m] + 1
		}
	}
	date = list(month = doymonth, day = doyday)
	return(date)
}

#-----------------------------------------------------------------------------

# Provides time and date for a given time step:
dateFromTstep = function(starttime,tstep){
	ntstepsinday = (1 / starttime$timestepsize) * 24 # assume in hours
	nowtstep = tstep # initialise current tstep
	nowday = 1 + tstep / ntstepsinday # initialise number of days
	nowyear = starttime$year # initialise year
	repeat { # for each year that has passed before this tstep
		mdays = getMonthDays(is.leap(nowyear)) # get ndays in year
		if(nowday >= mdays$start[13]){ # number of days in year
			nowtstep = nowtstep - mdays$start[13]*ntstepsinday # reduce remaining
			nowday = nowday - mdays$start[13] + 1 # reduce remaining days
			nowyear = nowyear + 1 # increment year
		}else{ # # outstanding days < a year
			realdate = doydate(nowday,is.leap(nowyear))
			realtime = (tstep %% ntstepsinday) * starttime$timestepsize
			break
		}
	}
	nowdate = list(time = realtime, day = as.integer(realdate$day), 
		month = realdate$month, year = nowyear)
	return(nowdate)
	
}



findStartTime <- function(start){
  
  # Find starting date / time:
  sday = as.numeric(format(start, format="%d"))
  smonth = as.numeric(format(start, format="%m"))
  syear = as.numeric(format(start, format="%Y"))
  
  shod = as.numeric(format(start, format="%H")) # starting hour of day
  
  # Collate start time variables:
  starttime=list(syear=syear,smonth=smonth,sday=sday,shod=shod)
  
  
  return(starttime)

}


