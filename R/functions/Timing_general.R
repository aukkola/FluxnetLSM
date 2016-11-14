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

#-----------------------------------------------------------------------------

Create2Uchar = function(intin){
	# Creates string of length 2 from integer of length 1 or 2
	if(intin<10){
		temp=as.character(intin)
		charout=paste('0',temp,sep='')
	}else if(intin>99){
		charout='NA'
		CheckError('I3: Character variable too long in function Create2Uchar.')
	}else{
		charout=as.character(intin)	
	}
	return(charout)
}

#-----------------------------------------------------------------------------

DayNight = function(SWdown,threshold = 5){
	# Returns a logical day/night time series based on a SW threshold:
	daynotnight = c()
	daynotnight[1:length(SWdown)] = FALSE
	for(t in 1:length(SWdown)){
		if(SWdown[t]>threshold){
			daynotnight[t]=TRUE
		}
	}
	return(daynotnight)
}

#-----------------------------------------------------------------------------

DailyToMonthly = function(dailydata,startyear,ndays){
	# Converts daily to monthly data
	# assumes data begins at the start of the year
	# assumes whole number of years
	# assumes time dim is 3rd dim
	yds = Yeardays(startyear,ndays)
	nyears = length(yds$daysperyear)
	monthlydata = array(NA,dim=c(length(dailydata[,1,1]),length(dailydata[1,,1]), (nyears*12) ))
	
	ydaysum = 0
	ymonthsum = 0
	for(y in 1:nyears){
		days = getMonthDays(is.leap(startyear+y-1))
		for(m in 1:12){
			monthlydata[,,(ymonthsum+m)] = aperm( 
				apply(dailydata[,,(ydaysum+days$start[m]):(ydaysum+days$start[m+1]-1)],1,rowSums)
				) / (days$start[m+1] - days$start[m])
		}
		ydaysum = ydaysum + yds$yeardays[y]
		ymonthsum = ymonthsum + 12
	}
	return(monthlydata)
}



