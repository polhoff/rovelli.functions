

ProcessRovelliDataOrigBST <- function ( infile, c_library = 'rovelli.data', outname, lon, lat)
	{

	#lon = LonLatSite ('CE1')['lon'] ; lat = LonLatSite ('CE1')['lat']
	#infile = 'Ebble_CE1_2013_04_25_orig'; c_library = 'rovelli'; outname = 'Ebble_CE1_2013_04_25';
	

	library (BADC)
	library(mdot)
	library (O2)

	#library (parker)

	library(PenmanMonteith)
	#library(st)
	library (sun)

	library (TTR)


	data (BADC_hourly_Atm)
	data (O2_sol)

	data(st)
	
	n_moveaverage = 15
	
	#load original data set from library
	#do.call ( data, list (infile))
	do.call ( data, list (infile))
	indata <- get (infile)
	indata$date.character <- paste(indata$date, 'BST')
	indata_orig <- indata

	#indata$date <- strptime(indata$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE)
	#indata$date <- strptime(indata$date, format = "%Y-%m-%d %H:%M:%S", tz = "BST")


	#indata$date <- as.POSIXct (indata$date)
	#indata$date <- as.POSIXct (indata$date,tz = 'UTC')
	#indata$date <- as.POSIXct (indata$date,tz = 'BST'); attr(indata$date, "tzone") <- "Europe/London";
	#indata$date <- as.POSIXct (indata$date, tz = 'GMT'); attr(indata$date, "tzone") <- "Europe/London";
	indata$date.BST <- as.POSIXlt (indata$date, tz = 'BST'); attr(indata$date, "tzone") <- "Europe/London";
	indata$date.UTC <- as.POSIXct (indata$date)
	
	#=================BUT..............................
	#the time zone of the readings I think is BST, so that means take off one hour
	#this is a bodge. I don't know how to do it properly
	indata$date.UTC <- indata$date.UTC - 60*60
	
		
	head(indata$date)
	head(indata$date.BST)
	head(indata$date.UTC)

	indata$date <- indata$date.UTC
	
	
	indata$date <- as.POSIXct (indata$date)
	indata$date1 <- as.Date (indata$date)

	head(indata$date)


	dates_range <- unique (indata$date1)
	dates_range <- c (head (dates_range) - 1, dates_range, tail (dates_range) + 1)

	#indata$DO <- indata$DO_mol / (10^6) * 32 * 1000
	#.......which is:
	indata$DO <- (indata$DO_mol * 32) / (10^3)
	indata$CSat_mol_To_mg <- (indata$CSat_mol * 32) / (10^3)



	x22 <- as.numeric (indata$date)
	x23 <- c (x22[-1], NA)
	print ( table (x23-x22))


	indata$TimeDiff <- c (NA, (x23 - x22)[-1])


	BADC_hourly_Atm_sub <- BADC_hourly_Atm [BADC_hourly_Atm$date1 %in% dates_range, ]
	plot ( BADC_hourly_Atm_sub$AbsPres_kPa)


	banana <- approxfun ( BADC_hourly_Atm_sub$date, BADC_hourly_Atm_sub$AbsPres_kPa)
	squashed <- banana ( indata$date )


	indata$AbsPres_kPa <- squashed


	input <- indata[, c ('date','Temp', 'AbsPres_kPa')]

	indata$CSat <- CalcCsat ( input )
	indata$DO15 <- MoveAv ( indata$DO, n_moveaverage )
	indata$Temp15 <- MoveAv ( indata$Temp, n_moveaverage )
	indata$AbsPres_kPa15 <- MoveAv ( indata$AbsPres_kPa, n_moveaverage )


	input <- indata[, c ('date', 'Temp15', 'AbsPres_kPa15')]
	#change names because required by function
	names(input) <- c ('date', 'Temp', 'AbsPres_kPa')
	indata$CSat15 <- CalcCsat ( input )

	rm(input)
















































	#differences for regression analysis
	indata$DO_deficit = with ( indata, CSat - DO )
	indata$DO_diff = c (NA, diff (indata$DO))
	indata$DO_diff_lag = c ( diff (indata$DO), NA)
	indata$DO_diff_mean =  ( indata$DO_diff + indata$DO_diff_lag ) / 2
	indata$CSat_diff =  c (NA, diff (indata$CSat))
	indata$Temp_diff =  c (NA, diff (indata$Temp))
	#DO_diff = c (diff (DO_change$DO), NA)



	indata$DO_diff_diff = c(NA, diff(indata$DO_diff))
	indata$DO_diff_diff_lag = c ( diff (indata$DO), NA)




	#differences for regression analysis
	indata$DO15_deficit = with ( indata, CSat15 - DO15 )
	indata$DO15_diff = c (NA, diff (indata$DO15))
	indata$DO15_diff_lag = c ( diff (indata$DO15), NA)
	indata$DO15_diff_mean =  ( indata$DO15_diff + indata$DO15_diff_lag ) / 2
	indata$CSat15_diff =  c (NA, diff (indata$CSat15))
	#DO15_diff = c (diff (DO15_change$DO15), NA)


	indata$DO15_diff_diff = c(NA, diff(indata$DO15_diff))
	indata$DO15_diff_diff_lag = c ( diff (indata$DO15), NA)


	indata$mv_avg_diff <- with ( indata, filter (DO_diff, rep(1/10,10), sides=2))

	#calc Ks
	#indata$Ks_2 <- with ( indata, (DO_diff_diff / (CSat_diff - DO_diff)))
	indata$Ks_2 <- with ( indata, (DO15_diff_diff / (CSat15_diff - DO15_diff)))

	
	#nice
	#nice
	#nice
	DO_vectored <- embed(indata$DO, 10)
	mv_SD <- apply ( DO_vectored, 1, sd )
	indata$st.dev <- try(runSD(indata$DO,10))





















































































	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset


	indata$Sunrise <- Sunrise ( indata$date, lon, lat, uniq = FALSE )
	indata$Sunset <- Sunset ( indata$date, lon, lat, uniq = FALSE )
	indata$SolarNoon <- with (indata, (Sunset - Sunrise)/2 + Sunrise )

	indata$Midnight <- with (indata, (SolarNoon - 60*60*12))
	indata$TimeAfterMidnight <- with (indata, difftime(date,Midnight, units = 'hours'))
	indata$TimeAfterMidnight <- round(as.numeric(indata$TimeAfterMidnight) %% 24,1)

	#indata$TimeAfterSolarNoon <- with (indata, difftime(date,SolarNoon, units = 'mins'))
	#indata$TimeAfterSolarNoon <- round(as.numeric(indata$TimeAfterSolarNoon) %% 24*60,0)
	indata$TimeAfterSolarNoon <- with (indata, difftime(date,SolarNoon, units = 'hours'))
	indata$TimeAfterSolarNoon <- round(as.numeric(indata$TimeAfterSolarNoon) %% 24,1)

	indata$TimeAfterSunrise <- with (indata, difftime(date,Sunrise, units = 'hours'))
	indata$TimeAfterSunrise <- round(as.numeric(indata$TimeAfterSunrise) %% 24,1)

	indata$daynight <- 'night'
	indata$daynight[indata$date > indata$Sunrise & indata$date < indata$Sunset] <- 'day'



	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset























































	dates <- unique ( indata$date1 )
	dates <- c ( dates[1] - 1, dates, tail (dates,1) + 1 )
	dates_days <- dates


	dates <- as.POSIXct (paste (as.character (dates), '12:00:00'), tz = 'UTC')
	c_labels <- Labels1()[1:length(dates)]


	dates_days <- as.POSIXct (paste (as.character (dates_days), '02:00:00'), tz = 'UTC')
	#c_labels1 <- Labels1()[1:length(dates_days)]



	indata$daynight1 <- indata$daynight
	for ( i in 1:length(dates))
		{
		#nights
		x_ndx_night <- indata$date > dates[i] & indata$daynight == 'night'
		indata$daynight1[x_ndx_night] <- paste ('night', c_labels[i], sep = '')
		
		#days
		x_ndx_day <- indata$date > dates_days[i] & indata$daynight == 'day'
		indata$daynight1[x_ndx_day] <- paste ('day', c_labels[i], sep = '')
		}




	assign ( outname, indata )

	setwd (dirdmp)
	save ( list = outname, file =  paste ( outname, '.rda', sep = ''))
	return(indata)
	}


#library(avon); library(rovelli)
#lon = LonLatSite ('CE1')['lon'] ; lat = LonLatSite ('CE1')['lat']
#ProcessRovelliDataOrigBST ( infile = 'Ebble_CE1_2013_04_25_orig', c_library = 'rovelli', outname = 'Ebble_CE1_2013_04_25', lon, lat )


#ProcessRovelliDataOrig ( infile = 'C3CE1_orig', c_library = 'rovelli', outname = 'C3CE1', lon, lat )


#lon = LonLatSite ('CE1')['lon'] ; lat = LonLatSite ('CE1')['lat']
#ProcessRovelliDataOrig ( infile = 'Ebble_CE1_2013_08_08_orig', c_library = 'rovelli', outname = 'Ebble_CE1_2013_08_08', lon, lat )



#lon = LonLatSite ('GA2')['lon'] ; lat = LonLatSite ('GA2')['lat']
#ProcessRovelliDataOrig ( infile = 'Avon_GA2_2013_04_28_orig', c_library = 'rovelli.data', outname = 'Avon_GA2_2013_04_28', lon, lat )



#c_site <- 'AS1'
#c_prog <- 'C1'
#lon = LonLatSite (c_site)['lon'] ; lat = LonLatSite (c_site)['lat']

#c_in <- paste (c_prog, c_site, '_orig', sep = '')
#c_out <- paste (c_prog, c_site, sep = '')

#ProcessRovelliDataOrig ( infile = c_in, c_library = 'rovelli.data', outname = c_out, lon, lat )





















































































































































ProcessRovelliDataOrig <- function ( infile, c_library = 'rovelli.data', outname, lon, lat)
	{

	#lon = LonLatSite ('CE1')['lon'] ; lat = LonLatSite ('CE1')['lat']
	#infile = 'Ebble_CE1_2013_04_25_orig'; c_library = 'rovelli'; outname = 'Ebble_CE1_2013_04_25';
	

	library (BADC)
	library(mdot)
	library (O2)

	#library (parker)

	library(PenmanMonteith)
	#library(st)
	library (sun)

	library (TTR)


	data (BADC_hourly_Atm)
	data (O2_sol)

	data(st)
	
	n_moveaverage = 15
	
	#load original data set from library
	#do.call ( data, list (infile))
	do.call ( data, list (infile))
	indata <- get (infile)
	

	indata$date <- as.POSIXct (indata$date)
	indata$date1 <- as.Date (indata$date)


	dates_range <- unique (indata$date1)
	dates_range <- c (head (dates_range) - 1, dates_range, tail (dates_range) + 1)

	#indata$DO <- indata$DO_mol / (10^6) * 32 * 1000
	#.......which is:
	indata$DO <- (indata$DO_mol * 32) / (10^3)
	indata$CSat_mol_To_mg <- (indata$CSat_mol * 32) / (10^3)



	x22 <- as.numeric (indata$date)
	x23 <- c (x22[-1], NA)
	print ( table (x23-x22))


	indata$TimeDiff <- c (NA, (x23 - x22)[-1])


	BADC_hourly_Atm_sub <- BADC_hourly_Atm [BADC_hourly_Atm$date1 %in% dates_range, ]
	plot ( BADC_hourly_Atm_sub$AbsPres_kPa)


	banana <- approxfun ( BADC_hourly_Atm_sub$date, BADC_hourly_Atm_sub$AbsPres_kPa)
	squashed <- banana ( indata$date )


	indata$AbsPres_kPa <- squashed


	input <- indata[, c ('date','Temp', 'AbsPres_kPa')]

	indata$CSat <- CalcCsat ( input )
	indata$DO15 <- MoveAv ( indata$DO, n_moveaverage )
	indata$Temp15 <- MoveAv ( indata$Temp, n_moveaverage )
	indata$AbsPres_kPa15 <- MoveAv ( indata$AbsPres_kPa, n_moveaverage )


	input <- indata[, c ('date', 'Temp15', 'AbsPres_kPa15')]
	#change names because required by function
	names(input) <- c ('date', 'Temp', 'AbsPres_kPa')
	indata$CSat15 <- CalcCsat ( input )

	rm(input)
















































	#differences for regression analysis
	indata$DO_deficit = with ( indata, CSat - DO )
	indata$DO_diff = c (NA, diff (indata$DO))
	indata$DO_diff_lag = c ( diff (indata$DO), NA)
	indata$DO_diff_mean =  ( indata$DO_diff + indata$DO_diff_lag ) / 2
	indata$CSat_diff =  c (NA, diff (indata$CSat))
	indata$Temp_diff =  c (NA, diff (indata$Temp))
	#DO_diff = c (diff (DO_change$DO), NA)



	indata$DO_diff_diff = c(NA, diff(indata$DO_diff))
	indata$DO_diff_diff_lag = c ( diff (indata$DO), NA)




	#differences for regression analysis
	indata$DO15_deficit = with ( indata, CSat15 - DO15 )
	indata$DO15_diff = c (NA, diff (indata$DO15))
	indata$DO15_diff_lag = c ( diff (indata$DO15), NA)
	indata$DO15_diff_mean =  ( indata$DO15_diff + indata$DO15_diff_lag ) / 2
	indata$CSat15_diff =  c (NA, diff (indata$CSat15))
	#DO15_diff = c (diff (DO15_change$DO15), NA)


	indata$DO15_diff_diff = c(NA, diff(indata$DO15_diff))
	indata$DO15_diff_diff_lag = c ( diff (indata$DO15), NA)


	indata$mv_avg_diff <- with ( indata, filter (DO_diff, rep(1/10,10), sides=2))

	#calc Ks
	#indata$Ks_2 <- with ( indata, (DO_diff_diff / (CSat_diff - DO_diff)))
	indata$Ks_2 <- with ( indata, (DO15_diff_diff / (CSat15_diff - DO15_diff)))

	
	#nice
	#nice
	#nice
	DO_vectored <- embed(indata$DO, 10)
	mv_SD <- apply ( DO_vectored, 1, sd )
	indata$st.dev <- try(runSD(indata$DO,10))





















































































	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset


	indata$Sunrise <- Sunrise ( indata$date, lon, lat, uniq = FALSE )
	indata$Sunset <- Sunset ( indata$date, lon, lat, uniq = FALSE )
	indata$SolarNoon <- with (indata, (Sunset - Sunrise)/2 + Sunrise )

	indata$Midnight <- with (indata, (SolarNoon - 60*60*12))
	indata$TimeAfterMidnight <- with (indata, difftime(date,Midnight, units = 'hours'))
	indata$TimeAfterMidnight <- round(as.numeric(indata$TimeAfterMidnight) %% 24,1)

	#indata$TimeAfterSolarNoon <- with (indata, difftime(date,SolarNoon, units = 'mins'))
	#indata$TimeAfterSolarNoon <- round(as.numeric(indata$TimeAfterSolarNoon) %% 24*60,0)
	indata$TimeAfterSolarNoon <- with (indata, difftime(date,SolarNoon, units = 'hours'))
	indata$TimeAfterSolarNoon <- round(as.numeric(indata$TimeAfterSolarNoon) %% 24,1)

	indata$TimeAfterSunrise <- with (indata, difftime(date,Sunrise, units = 'hours'))
	indata$TimeAfterSunrise <- round(as.numeric(indata$TimeAfterSunrise) %% 24,1)

	indata$daynight <- 'night'
	indata$daynight[indata$date > indata$Sunrise & indata$date < indata$Sunset] <- 'day'



	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset























































	dates <- unique ( indata$date1 )
	dates <- c ( dates[1] - 1, dates, tail (dates,1) + 1 )
	dates_days <- dates


	dates <- as.POSIXct (paste (as.character (dates), '12:00:00'), tz = 'UTC')
	c_labels <- Labels1()[1:length(dates)]


	dates_days <- as.POSIXct (paste (as.character (dates_days), '02:00:00'), tz = 'UTC')
	#c_labels1 <- Labels1()[1:length(dates_days)]



	indata$daynight1 <- indata$daynight
	for ( i in 1:length(dates))
		{
		#nights
		x_ndx_night <- indata$date > dates[i] & indata$daynight == 'night'
		indata$daynight1[x_ndx_night] <- paste ('night', c_labels[i], sep = '')
		
		#days
		x_ndx_day <- indata$date > dates_days[i] & indata$daynight == 'day'
		indata$daynight1[x_ndx_day] <- paste ('day', c_labels[i], sep = '')
		}




	assign ( outname, indata )

	setwd (dirdmp)
	save ( list = outname, file =  paste ( outname, '.rda', sep = ''))
	return(indata)
	}


#library(avon)
#library(rovelli)

#lon = LonLatSite ('CE1')['lon'] ; lat = LonLatSite ('CE1')['lat']
#ProcessRovelliDataOrig ( infile = 'Ebble_CE1_2013_04_25_orig', c_library = 'rovelli', outname = 'Ebble_CE1_2013_04_25', lon, lat )


#ProcessRovelliDataOrig ( infile = 'C3CE1_orig', c_library = 'rovelli', outname = 'C3CE1', lon, lat )


#lon = LonLatSite ('CE1')['lon'] ; lat = LonLatSite ('CE1')['lat']
#ProcessRovelliDataOrig ( infile = 'Ebble_CE1_2013_08_08_orig', c_library = 'rovelli', outname = 'Ebble_CE1_2013_08_08', lon, lat )

#lon = LonLatSite ('GA2')['lon'] ; lat = LonLatSite ('GA2')['lat']
#ProcessRovelliDataOrig ( infile = 'Avon_GA2_2013_04_28_orig', c_library = 'rovelli.data', outname = 'Avon_GA2_2013_04_28', lon, lat )



#c_site <- 'AS1'
#c_prog <- 'C1'
#lon = LonLatSite (c_site)['lon'] ; lat = LonLatSite (c_site)['lat']

#c_in <- paste (c_prog, c_site, '_orig', sep = '')
#c_out <- paste (c_prog, c_site, sep = '')

#ProcessRovelliDataOrig ( infile = c_in, c_library = 'rovelli.data', outname = c_out, lon, lat )



