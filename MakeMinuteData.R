
MakeMinuteData <- function(indata)
	{
	#indata <- C2AS2_orig
	indata$date <- as.POSIXct(indata$date, tz = 'UTC')
	indata$date1 <- as.Date(indata$date)
	
	outdata <- indata

	indata$date <- as.POSIXct(indata$date)
	

	start_date <- indata$date1[!is.na(indata$date1)]
	end_date <- as.POSIXct(tail(start_date,1) + 1)
	start_date <- as.POSIXct(head(start_date,1) - 1)
	
	seq_dates <- seq(start_date, end_date, by = 60)
	seq_dates <- seq_dates[seq_dates > head(indata$date,1) & seq_dates < tail(indata$date,1)]
	
	
	
	banana <- with(indata, approxfun(date, date))
	a = banana(seq_dates)
	a = as.POSIXct(a, origin = '1970-01-01')
	#a = as.POSIXct(a[!is.na(a)], origin = '1970-01-01')
	outdata <- data.frame(a)
	names(outdata) <- 'date'

	outdata$date1 <- as.Date(outdata$date)

	stopifnot(length(seq_dates) == dim(outdata)[1])

	
	banana <- with(indata, approxfun(date, DO_persat))
	a = banana(seq_dates)
	outdata$DO_persat <- a

	banana <- with(indata, approxfun(date, Temp))
	a = banana(seq_dates)
	outdata$Temp <- a

	banana <- with(indata, approxfun(date, PARraw))
	a = banana(seq_dates)
	outdata$PARraw <- a

	banana <- with(indata, approxfun(date, DO_mol))
	a = banana(seq_dates)
	outdata$DO_mol <- a


	banana <- with(indata, approxfun(date, CSat_mol))
	a = banana(seq_dates)
	outdata$CSat_mol <- a
	
	
	return(outdata)
	}


#library(rovelli); library(st); data(st)
#x1 = 'C2GN1'; do.call(data, list (x1, package = 'rovelli.data')); x = get(x1); y = MakeMinuteData(x)
#c_file <- paste(x1,'_60_orig', sep = ''); assign(c_file, y)
#save(list = c_file, file = paste(dirdmp, c_file, '.rda', sep = ''))



#x1 = 'C3AS1'; do.call(data, list (x1, package = 'rovelli.data')); x = get(x1); y = MakeMinuteData(x)
#c_file <- paste(x1,'_60_orig', sep = ''); assign(c_file, y)
#save(list = c_file, file = paste(dirdmp, c_file, '.rda', sep = ''))




