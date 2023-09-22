
PlotWindspeed <- function (c_rda =  'Ebble_windspeed', c_indata1 =  'Ebble_windspeed_2013_04', l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'windspeed.png' )
	{
	
	x = l_png
	y = l_tif1
	#library (rovelli)
	#indata <- get (data (DO_obs_sim_Rovelli_Ebble2013))
	library (sun)
	library (parker)
	library (O2)
	library (avon)
	data (SiteLocations)
	data (st)
	
	do.call ( data, list (c_rda))
	
	indata =  get ( c_indata1 ) 

	lon <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lon']
	lat <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lat']

	max_y <- max ( indata$windspeed_mpersec, na.rm = T )
	min_y <- min ( indata$windspeed_mpersec, na.rm = T )

	max_y <- ceiling (max_y)
	min_y <- floor (min_y)

	#max_y_moles <- max ( indata$obs_simDO_mol, na.rm = T )
	#min_y_moles <- min ( indata$obs_sim$DO_mol, na.rm = T )

	#max_y_moles <- ceiling (max_y_moles)
	#min_y_moles <- floor (min_y_moles)

	pngfilename <- outfile
	
	Png ( pick1 = x, pick2 = y, outfile =  'windspeed', n_height = n_height)
		par ( mar = c ( 7,7,7,4))
		
		plot ( indata$date, indata$windspeed_mpersec, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = 'Date (2013-04-25 to 2013-04-27)', ylab = 'Windspeed (metres per second)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)

		points ( indata$date, indata$windspeed_mpersec, type = 'p')
		
	Pnh( off = (x | y))
	}

#PlotWindspeed ( )
































































PlotPmaxByHourSegment <- function ( c_indata1, c_indata2, c_library, n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'PmaxByHour.png' )
	{
	
	library (parker)
	data(st)
	
	x = l_png
	y = l_tif1

	data01 <- PreProcessDOPhot (c_indata1, c_indata2, c_library)
	data02 <- CutDataDataSetByTime (indata = data01, timeinterval, n_date,  lon, lat)
	data03 <- CalcDOPhot (data02)

	n_cuts <- unique ( data03$splitpoint )

	max_y <- max (data03$DO_Phot, na.rm = TRUE)
	min_y <- min (data03$DO_Phot, na.rm = TRUE)

	max_x <- max (data03$PARraw, na.rm = TRUE)
	min_x <- min (data03$PARraw, na.rm = TRUE)
	
	x = l_png
	y = l_tif1

	pngfilename <- outfile
	
	Png ( pick1 = x, pick2 = y, outfile =  'PmaxByHour', n_height = n_height)
		
		par (mfrow = c ( 3,3))
		par(pty="s")
		for ( i in 1:length (n_cuts))
			{
			data_sub <- data03[data03$splitpoint == n_cuts[i],]
			with ( data_sub, plot ( PARraw, DO_Phot, ylim = c (min_y, max_y), xlim = c ( min_x, max_x), col = 'gray30', pch = 1, type = 'p', xlab = expression (paste ('PAR ', mu, 'moles photons ',  m^{-2}, s^{-1})), ylab = 'Pmax', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6))
 			}

		par(pty="m")
		par (mfrow = c ( 1,1))
		
	Pnh( off = (x | y))
	}



#PlotPmaxByHourSegment ( c_indata1 = 'Ebble_Eddy', c_indata2 = 'Ebble_ER_Ks_timeseries', c_library = 'rovelli', n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'PmaxByHour.png' )























































































PlotDayNight <- function ( c_indata )
	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	print ( head (indata))

	l_png = F
	l_png = T


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_y <- max ( indata$DO, na.rm = T )
	min_y <- min ( indata$DO, na.rm = T )


	max_y <- ceiling (max_y) + 1
	min_y <- floor (min_y)


	max_y_moles <- max ( indata$DO_mol, na.rm = T )
	min_y_moles <- min ( indata$DO_mol, na.rm = T )


	max_y_moles <- ceiling (max_y_moles)
	min_y_moles <- floor (min_y_moles)






	pngfilename =  paste ( c_indata, '_daynight.png', sep = '')

	PngOn()
		par ( mar = c ( 7,7,7,4))

		plot ( indata$date, indata$DO, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = 'Date (2013-04-25 to 2013-04-27)', ylab = 'DO (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		points ( indata$date, indata$DO, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.4, 'DO concentration ')
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.2, 'at saturation ')
		points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		
		
		text ( as.POSIXct ( '2013-04-26 03:30:00'), 9.6, 'Night 1', cex = cex_lab)
		text ( as.POSIXct ( '2013-04-27 03:30:00'), 9.8, 'Night 2', cex = cex_lab)
		
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))

		
	PngOff()
	}


#PlotDayNight ( 'Ebble_CE1_2013_08_08' )
#PlotDayNight ( 'Ebble_CE1_2013_04_25' )













































































PlotDayNightDataSet <- function ( indata )
	{

	library (parker)
	data (parker)
	
	print ( head (indata))

	l_png = F
	l_png = T


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_y <- max ( indata$DO, na.rm = T )
	min_y <- min ( indata$DO, na.rm = T )


	max_y <- ceiling (max_y) + 1
	min_y <- floor (min_y)


	max_y_moles <- max ( indata$DO_mol, na.rm = T )
	min_y_moles <- min ( indata$DO_mol, na.rm = T )


	max_y_moles <- ceiling (max_y_moles)
	min_y_moles <- floor (min_y_moles)







	PngOn()
		par ( mar = c ( 7,7,7,4))

		plot ( indata$date, indata$DO, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = 'Date (2013-04-25 to 2013-04-27)', ylab = 'DO (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		points ( indata$date, indata$DO, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.4, 'DO concentration ')
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.2, 'at saturation ')
		points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		
		
		text ( as.POSIXct ( '2013-04-26 03:30:00'), 9.6, 'Night 1', cex = cex_lab)
		text ( as.POSIXct ( '2013-04-27 03:30:00'), 9.8, 'Night 2', cex = cex_lab)
		
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))

		
	PngOff()
	}


#PlotDayNightDataSet ( indata )






























































































PlotDayNightMol <- function ( c_indata )
	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	print ( head (indata))

	l_png = F
	l_png = T


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_y <- max ( indata$DO, na.rm = T )
	min_y <- min ( indata$DO, na.rm = T )


	max_y <- ceiling (max_y) + 1
	min_y <- floor (min_y)


	max_y_moles <- max ( indata$DO_mol, na.rm = T )
	min_y_moles <- min ( indata$DO_mol, na.rm = T )


	max_y_moles <- ceiling (max_y_moles)
	min_y_moles <- floor (min_y_moles)






	pngfilename =  paste ( c_indata, '_daynight.png', sep = '')

	PngOn()
		par ( mar = c ( 7,7,7,4))

		plot ( indata$date, indata$DO_mol, ylim = c ( min_y_moles, max_y_moles), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'DO (micromoles per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		points ( indata$date, indata$DO_mol, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.4, 'DO concentration ')
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.2, 'at saturation ')
		points ( indata$date, indata$CSat_mol, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_mol, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_mol[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		
		
		text ( as.POSIXct ( '2013-04-26 03:30:00'), 9.6, 'Night 1', cex = cex_lab)
		text ( as.POSIXct ( '2013-04-27 03:30:00'), 9.8, 'Night 2', cex = cex_lab)
		
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))

		
	PngOff()
	}


#PlotDayNight ( 'Ebble_CE1_2013_08_08' )
#PlotDayNight ( 'Ebble_CE1_2013_04_25' )
















































PlotDayNightMolDataSet <- function ( indata )
	{

	library (parker)
	data (parker)
	
	print ( head (indata))

	l_png = F
	l_png = T


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_y <- max ( indata$DO, na.rm = T )
	min_y <- min ( indata$DO, na.rm = T )


	max_y <- ceiling (max_y) + 1
	min_y <- floor (min_y)


	max_y_moles <- max ( indata$DO_mol, na.rm = T )
	min_y_moles <- min ( indata$DO_mol, na.rm = T )


	max_y_moles <- ceiling (max_y_moles)
	min_y_moles <- floor (min_y_moles)







	PngOn()
		par ( mar = c ( 7,7,7,4))

		plot ( indata$date, indata$DO_mol, ylim = c ( min_y_moles, max_y_moles), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'DO (micromoles per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		points ( indata$date, indata$DO_mol, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.4, 'DO concentration ')
		text ( as.POSIXct ( '2013-04-26 02:30:00'), 11.2, 'at saturation ')
		points ( indata$date, indata$CSat_mol, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_mol, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_mol[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		
		
		text ( as.POSIXct ( '2013-04-26 03:30:00'), 9.6, 'Night 1', cex = cex_lab)
		text ( as.POSIXct ( '2013-04-27 03:30:00'), 9.8, 'Night 2', cex = cex_lab)
		
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))

		
	PngOff()
	}


#PlotDayNightMolDataSet ( indata )






















































































































PlotDeficit_DODiff <- function ( c_indata )
	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	print ( head (indata))

	l_png = F
	l_png = T


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )



	pngfilename = 'Lorenzo_DO_diff_DO_deficit_2pane.png'
	PngOn()

		par ( mfrow = c ( 2,1))
		par ( mar = c ( 4,7,3,4))


		max_y <- max ( indata$DO_diff, na.rm = T )
		min_y <- min ( indata$DO_diff, na.rm = T )


		plot ( indata$date, indata$DO_diff, col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'Change in DO (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6, ylim = c ( min_y, max_y))
		points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		
		text ( as.POSIXct ( '2013-04-27 17:00:00'), 0.05, 'A', cex = cex_lab * 1.2)
		
		#points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_diff, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_diff[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))
		abline (0,0)
		

		max_y <- max ( indata$DO_deficit, na.rm = T )
		min_y <- min ( indata$DO_deficit, na.rm = T )


		#plot ( indata$date, indata$Cnow, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = 'Date (2013-04-25 to 2013-04-27)', ylab = 'DO (mg per litre)', main = 'Dissolved oxygen concentrations observed in river Ebble \n based on eddy correlation', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		plot ( indata$date, indata$DO_deficit, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'DO deficit (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		
		text ( as.POSIXct ( '2013-04-27 17:00:00'), 3, 'B', cex = cex_lab * 1.2)
		
		#points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_deficit, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_deficit[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))
		abline (0,0)
		
		par ( mfrow = c ( 1,1))
		
	PngOff()
	
	par ( mfrow = c ( 1,1))
	
	}


#PlotDeficit_DODiff ( 'Ebble_CE1_2013_08_08' )
#PlotDeficit_DODiff ( 'Ebble_CE1_2013_04_25' )














































































PlotDeficit_DODiffDataSet <- function ( indata )
	{

	library (parker)
	data (parker)
	

	l_png = F
	l_png = T


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )



	PngOn()

		par ( mfrow = c ( 2,1))
		par ( mar = c ( 4,7,3,4))


		max_y <- max ( indata$DO_diff, na.rm = T )
		min_y <- min ( indata$DO_diff, na.rm = T )


		plot ( indata$date, indata$DO_diff, col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'Change in DO (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6, ylim = c ( min_y, max_y))
		points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		
		text ( as.POSIXct ( '2013-04-27 17:00:00'), 0.05, 'A', cex = cex_lab * 1.2)
		
		#points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_diff, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_diff[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))
		abline (0,0)
		

		max_y <- max ( indata$DO_deficit, na.rm = T )
		min_y <- min ( indata$DO_deficit, na.rm = T )


		#plot ( indata$date, indata$Cnow, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = 'Date (2013-04-25 to 2013-04-27)', ylab = 'DO (mg per litre)', main = 'Dissolved oxygen concentrations observed in river Ebble \n based on eddy correlation', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		plot ( indata$date, indata$DO_deficit, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'DO deficit (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		
		text ( as.POSIXct ( '2013-04-27 17:00:00'), 3, 'B', cex = cex_lab * 1.2)
		
		#points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_deficit, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_deficit[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))
		abline (0,0)
		
		par ( mfrow = c ( 1,1))
		
	PngOff()
	
	par ( mfrow = c ( 1,1))
	
	}


#PlotDeficit_DODiffDataSet ( indata )




































































PlotDeficit_DODiffMol <- function ( c_indata )
	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	indata$DO_diff_mol <- indata$DO_diff / 32 *1e6
	indata$DO_deficit_mol <- indata$DO_deficit / 32 *1e6
	
	print ( head (indata))

	l_png = F
	l_png = T


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )



	pngfilename = 'Lorenzo_DO_diff_DO_deficit_2pane.png'
	PngOn()

		par ( mfrow = c ( 2,1))
		par ( mar = c ( 4,7,3,4))


		max_y <- max ( indata$DO_diff_mol, na.rm = T )
		min_y <- min ( indata$DO_diff_mol, na.rm = T )


		plot ( indata$date, indata$DO_diff_mol, col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'Change in DO (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6, ylim = c ( min_y, max_y))
		points ( indata$date, indata$CSat_mol, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		
		text ( as.POSIXct ( '2013-04-27 17:00:00'), 0.05, 'A', cex = cex_lab * 1.2)
		
		#points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_diff_mol, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_diff_mol[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))
		abline (0,0)
		

		max_y <- max ( indata$DO_deficit_mol, na.rm = T )
		min_y <- min ( indata$DO_deficit_mol, na.rm = T )


		#plot ( indata$date, indata$Cnow, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = 'Date (2013-04-25 to 2013-04-27)', ylab = 'DO (mg per litre)', main = 'Dissolved oxygen concentrations observed in river Ebble \n based on eddy correlation', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		plot ( indata$date, indata$DO_deficit_mol, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_labeldate, ylab = 'DO deficit (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		points ( indata$date, indata$CSat_mol, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		
		text ( as.POSIXct ( '2013-04-27 17:00:00'), 3, 'B', cex = cex_lab * 1.2)
		
		#points ( indata$date, indata$CSat, type = 'b', lty = 2,  col = 'gray85' ,lwd = 1, pch = 4, cex = 0.2  )
		points ( indata$date, indata$DO_deficit, type = 'b')
		with ( indata, points ( date[daynight == 'night'], DO_deficit[daynight == 'night'], type = 'p', col = 'grey70'))
		#points ( indata$date, indata$Cnow, type = 'l', lty = 3,  col = 'gray70' ,lwd = 2, pch = 2, cex = 0.8  )
		#legend ( 'topleft', c ( 'Observed','Simulated', 'DO concentration at saturation'), pch = c ( 1,2 ), lty = c ( 1,2), col = c ( 'black','grey60' ))
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA))
		abline (0,0)
		
		par ( mfrow = c ( 1,1))
		
	PngOff()
	
	par ( mfrow = c ( 1,1))
	
	}


#PlotDeficit_DODiff ( 'Ebble_CE1_2013_08_08' )
#PlotDeficit_DODiff ( 'Ebble_CE1_2013_04_25' )























































































































































































PlotRegressionER_Ks <- function ( c_indata, c_library, n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'regression_Ebble_April2013_2pane.png' )

	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_x <- max ( indata$DO_deficit, na.rm = T )
	min_x <- min ( indata$DO_deficit, na.rm = T )

	max_x <- ceiling (max_x) + 1
	min_x <- floor (min_x)




	max_y <- max ( indata$DO_diff, na.rm = T )
	min_y <- min ( indata$DO_diff, na.rm = T )

	max_y <- ceiling (max_y) + 1
	min_y <- floor (min_y)


	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )


	pngfilename =  outfile
	PngOn( n_height )

		par ( mfrow = c ( 2,1))
		par ( mar = c ( 5,10,4,4))

		
		for ( i in 1:length (x_nights) )
			{
			x_ndx = x_nights[i]
			
			#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			
			data_sub_ndx = which ( indata$daynight1 == x_ndx )
			data_sub = indata[data_sub_ndx, ]
			x_ndx1 = which ( !is.na (data_sub$DO_deficit))
			data_sub = data_sub[x_ndx1, ]


			#with ( data_sub, plot ( DO_deficit, DO_change ))
			#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
			
			try (plot (data_sub$DO_deficit, data_sub$DO_diff, ylim = c ( min_y, max_y), xlim = c ( min_x, max_x), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))
			fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		
			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])


			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			
			abline (fit)


			#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

			mtext (side = 3, line = 1, at = 0.05, paste ("Night", i), cex = cex_lab )
			
			#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			text ( 2,1.5, paste ( 'Date: ',x_ndx, sep = ''))
			
			}

	PngOff()

	par (xpd = FALSE)
	par ( mfrow = c ( 1,1))

	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}


#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_08_08' )
#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_04_25' )







































































PlotRegressionER_Ks_01 <- function ( c_indata, c_library, n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'regression_Ebble_April2013_2pane.png' )

	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )


	pngfilename =  outfile
	PngOn( n_height )

		par ( mfrow = c ( 2,1))
		par ( mar = c ( 5,10,4,4))

		
		for ( i in 1:length (x_nights) )
			{
			x_ndx = x_nights[i]
			
			#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			
			data_sub_ndx = which ( indata$daynight1 == x_ndx )
			data_sub = indata[data_sub_ndx, ]
			x_ndx1 = which ( !is.na (data_sub$DO_deficit))
			data_sub = data_sub[x_ndx1, ]


			#with ( data_sub, plot ( DO_deficit, DO_change ))
			#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
			
			try (plot (data_sub$DO_deficit, data_sub$DO_diff, xlim = c ( -0.5, 2), ylim = c ( -0.04, 0.04), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))
			fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		
			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])


			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			
			abline (fit)


			#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

			mtext (side = 3, line = 1, at = -0.4, paste ("Night", i), cex = cex_lab )
			
			#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			text ( 2,1.5, paste ( 'Date: ',x_ndx, sep = ''))
			
			}

	PngOff()

	par (xpd = FALSE)
	par ( mfrow = c ( 1,1))

	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}


#PlotRegressionER_Ks_01 ( c_indata = 'Ebble_CE1_2013_08_08' )
#PlotRegressionER_Ks_01 ( c_indata = 'Ebble_CE1_2013_04_25' )



























































PlotRegressionER_Ks_02 <- function ( c_indata, c_library, n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'regression_Ebble_April2013_2pane.png' )

	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_x <- max ( indata$DO_deficit[indata$daynight == 'night'], na.rm = T )
	min_x <- min ( indata$DO_deficit[indata$daynight == 'night'], na.rm = T )

	#max_x <- ceiling (max_x) + 1
	#min_x <- floor (min_x)


	max_y <- max ( indata$DO_diff[indata$daynight == 'night'], na.rm = T )
	min_y <- min ( indata$DO_diff[indata$daynight == 'night'], na.rm = T )

	#max_y <- ceiling (max_y) + 1
	#min_y <- floor (min_y)


	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )


	pngfilename =  outfile
	PngOn( n_height )

		par ( mfrow = c ( 2,1))
		par ( mar = c ( 5,10,4,4))

		
		for ( i in 1:length (x_nights) )
			{
			x_ndx = x_nights[i]
			
			#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			
			data_sub_ndx = which ( indata$daynight1 == x_ndx )
			data_sub = indata[data_sub_ndx, ]
			x_ndx1 = which ( !is.na (data_sub$DO_deficit))
			data_sub = data_sub[x_ndx1, ]


			#with ( data_sub, plot ( DO_deficit, DO_change ))
			#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
			
			try (plot (data_sub$DO_deficit, data_sub$DO_diff, ylim = c ( min_y, max_y), xlim = c ( min_x, max_x), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))
			fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		
			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])


			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			
			abline (fit)


			#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

			mtext (side = 3, line = 1, at = 0.05, paste ("Night", i), cex = cex_lab )
			
			#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			text ( 2,1.5, paste ( 'Date: ',x_ndx, sep = ''))
			
			}
	PngOff()

	par (xpd = FALSE)
	par ( mfrow = c ( 1,1))

	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}


#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_08_08' )
#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_04_25' )





















































































PlotRegressionER_Ks_03 <- function ( c_indata, c_library, n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'regression_Ebble_April2013_2pane.png' )

	{

	ThisEnv <- new.env()
	
	ThisEnv$l_png <- l_png
	ThisEnv$PngOn <- PngOn
	ThisEnv$PngOff <- PngOff
	

	library (parker)
	data (parker)
	
	
	do.call ( library, list ( c_library ))
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)
	print ( head (indata))

	if ( with (indata, exists ('qualityFilter'))){	indata <- indata[indata$qualityFilter > 2,] }

	
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_x <- max ( indata$DO_deficit[indata$daynight == 'night'], na.rm = TRUE )
	min_x <- min ( indata$DO_deficit[indata$daynight == 'night'], na.rm = TRUE )

	#max_x <- ceiling (max_x) + 1
	#min_x <- floor (min_x)
	


	max_y <- max ( indata$DO_diff[indata$daynight == 'night'], na.rm = TRUE )
	min_y <- min ( indata$DO_diff[indata$daynight == 'night'], na.rm = TRUE )

	#max_y <- ceiling (max_y) + 1
	#min_y <- floor (min_y)


	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]
	print (x_nights)

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )



	par ( mfrow = c ( 2,1))
	par ( mar = c ( 5,10,4,4))


	for ( i in 1:length (x_nights) )
		{
		x_ndx = x_nights[i]
		
		#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		n_date <- as.Date (data_sub$date)[1]



		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))
		ER_Ks.res = resid(fit) 
	
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope


		#with ( data_sub, plot ( DO_deficit, DO_change ))
		#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		

		max_x <- max ( data_sub$DO_deficit[data_sub$daynight == 'night'], na.rm = TRUE )
		min_x <- min ( data_sub$DO_deficit[data_sub$daynight == 'night'], na.rm = TRUE )

		max_y <- max ( data_sub$DO_diff[data_sub$daynight == 'night'], na.rm = TRUE )
		min_y <- min ( data_sub$DO_diff[data_sub$daynight == 'night'], na.rm = TRUE )
			
		ThisEnv$PngOn( n_height = 600, x = ThisEnv$l_png, outfile =  x_ndx)

			par ( mar = c ( 5,10,4,4))
			try (plot (data_sub$DO_deficit, data_sub$DO_diff, ylim = c ( min_y, max_y), xlim = c ( min_x, max_x), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))

			abline (fit, col = 'grey40', lwd = 2)


			#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

			mtext (side = 3, line = 1, at = 0, paste ("Night", i), cex = cex_lab )
			
			#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			text ( 0.1 * (max_x - min_x) + min_x, 0.9 * max_y, paste ( 'Date: ', n_date, sep = ''), cex = 1.6)
			
		ThisEnv$PngOff(x = ThisEnv$l_png)
		}

	par (xpd = FALSE)
	par (mfrow = c ( 1,1))


	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}


#ListData ('mdot.data')
#PlotRegressionER_Ks_03 ( c_indata = 'Ebble_CE1_2014-08-21_md', c_library = 'mdot.data', l_png = TRUE )
#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_08_08' )
#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_04_25' )
#PlotRegressionER_Ks_03 ( c_indata = 'Ebble_CE1_2014-08-21_md', c_library = 'mdot.data', l_png = TRUE, outfile = 'Ebble_CE1_2014-08-21_md' )

























































































PlotRegressionER_Ks_04 <- function ( c_indata, c_library, n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1')

	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)
	indata <- indata[indata$qualityFilter > 2,]

	
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	

	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )


		for ( i in 1:length (x_nights) )
			{
			
			outfile <- paste ( x_nights[i], '.png', sep = '' )
			
			
			pngfilename =  outfile
			PngOn( n_height )
			
			
			par ( mfrow = c ( 2,1))
			par ( mar = c ( 5,10,4,4))
			par (ask = TRUE)


			x_ndx = x_nights[i]
			
			#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			
			data_sub_ndx = which ( indata$daynight1 == x_ndx )
			data_sub = indata[data_sub_ndx, ]


			max_x <- max ( data_sub$DO_deficit, na.rm = T )
			min_x <- min ( data_sub$DO_deficit, na.rm = T )


			max_y <- max ( data_sub$DO_diff, na.rm = T )
			min_y <- min ( data_sub$DO_diff, na.rm = T )


			x_ndx1 = which ( !is.na (data_sub$DO_deficit))
			data_sub = data_sub[x_ndx1, ]


			#with ( data_sub, plot ( DO_deficit, DO_change ))
			#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
			
			try (plot (data_sub$DO_deficit, data_sub$DO_diff, ylim = c ( min_y, max_y), xlim = c ( min_x, max_x), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))
			fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))
			ER_Ks.res = resid(fit) 
		
			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])


			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			
			abline (fit)


			#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

			mtext (side = 3, line = 1, at = 0.05, paste ("Night", i), cex = cex_lab )
			
			#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			text ( 2,1.5, paste ( 'Date: ',x_ndx, sep = ''))
			
			PngOff()
			}


	par (xpd = FALSE)
	par ( mfrow = c ( 1,1))

	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}


#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_08_08' )
#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_04_25' )

























































































PlotRegressionER_Ks_Res <- function ( c_indata, c_library, n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode = 'CE1', outfile = 'regression_Ebble_April2013_2pane.png' )

	{

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)
	indata <- indata[indata$qualityFilter > 2,]

	
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_x <- max ( indata$DO_deficit[indata$daynight == 'night'], na.rm = T )
	min_x <- min ( indata$DO_deficit[indata$daynight == 'night'], na.rm = T )

	#max_x <- ceiling (max_x) + 1
	#min_x <- floor (min_x)



	max_y <- max ( indata$DO_diff[indata$daynight == 'night'], na.rm = T )
	min_y <- min ( indata$DO_diff[indata$daynight == 'night'], na.rm = T )

	#max_y <- ceiling (max_y) + 1
	#min_y <- floor (min_y)


	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )


	pngfilename =  outfile
	PngOn( n_height )

		par ( mfrow = c ( 2,1))
		par ( mar = c ( 5,10,4,4))

		
		for ( i in 1:length (x_nights) )
			{
			x_ndx = x_nights[i]
			
			#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			
			data_sub_ndx = which ( indata$daynight1 == x_ndx )
			data_sub = indata[data_sub_ndx, ]
			x_ndx1 = which ( !is.na (data_sub$DO_deficit))
			data_sub = data_sub[x_ndx1, ]


			#with ( data_sub, plot ( DO_deficit, DO_change ))
			#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
			
			
			fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))
			ER_Ks.res = resid(fit) 
		
			
			try (plot (data_sub$DO_deficit, ER_Ks.res, ylim = c ( min_y, max_y), xlim = c ( min_x, max_x), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))
			
			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])


			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			
			abline (0,0)


			#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

			mtext (side = 3, line = 1, at = 0.1, paste ('Night', i, 'residual plot' ), cex = cex_lab )
			
			#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			text ( 2,1.5, paste ( 'Date: ',x_ndx, sep = ''))
			
			}

	PngOff()

	par (xpd = FALSE)
	par ( mfrow = c ( 1,1))

	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}


#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_08_08' )
#PlotRegressionER_Ks ( c_indata = 'Ebble_CE1_2013_04_25' )




















































































































PlotRegressionER_KsDataSet <- function ( indata, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6 )

	{

	library (parker)
	data (parker)
	

	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )

	max_x <- max ( indata$DO_deficit, na.rm = T )
	min_x <- min ( indata$DO_deficit, na.rm = T )

	max_x <- ceiling (max_x) + 1
	min_x <- floor (min_x)




	max_y <- max ( indata$DO_diff, na.rm = T )
	min_y <- min ( indata$DO_diff, na.rm = T )

	max_y <- median ( indata$DO_diff, na.rm = T ) * 20
	min_y <- -median ( indata$DO_diff, na.rm = T ) * 10


	#max_y <- ceiling (max_y) + 1
	#min_y <- floor (min_y)


	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )

	par ( mfrow = c ( 2,1))
	par ( mar = c ( 5,10,4,4))

	
	for ( i in 1:length (x_nights) )
		{
		x_ndx = x_nights[i]
		
		#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]


		#with ( data_sub, plot ( DO_deficit, DO_change ))
		#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		
		try (plot (data_sub$DO_deficit, data_sub$DO_diff, ylim = c ( min_y, max_y), xlim = c ( min_x, max_x), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))
		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

	
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		
		abline (fit)


		#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

		mtext (side = 3, line = 1, at = 0.05, paste ("Night", i), cex = cex_lab )
		
		#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
		text ( 2,1.5, paste ( 'Date: ',x_ndx, sep = ''))
		
		}



	par (xpd = FALSE)
	par ( mfrow = c ( 1,1))

	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}



#PlotRegressionER_KsDataSet ( indata )




























































































































PlotRegressionER_Ks_02DataSet <- function ( indata, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6)

	{

	max_x <- max ( indata$DO_deficit[indata$daynight == 'night'], na.rm = T )
	min_x <- min ( indata$DO_deficit[indata$daynight == 'night'], na.rm = T )

	#max_x <- ceiling (max_x) + 1
	#min_x <- floor (min_x)


	max_y <- max ( indata$DO_diff[indata$daynight == 'night'], na.rm = T )
	min_y <- min ( indata$DO_diff[indata$daynight == 'night'], na.rm = T )

	#max_y <- ceiling (max_y) + 1
	#min_y <- floor (min_y)


	x_date = unique ( as.Date (indata$date1))

	x_nights = unique ( indata$daynight1)
	x_nights = x_nights[grep ('night', x_nights)]

	m_coeffs = matrix (nrow = length (x_nights), ncol = 2 )



		par ( mfrow = c ( 2,1))
		par ( mar = c ( 5,10,4,4))

		
		for ( i in 1:length (x_nights) )
			{
			x_ndx = x_nights[i]
			
			#text ( 1,1, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			
			data_sub_ndx = which ( indata$daynight1 == x_ndx )
			data_sub = indata[data_sub_ndx, ]
			x_ndx1 = which ( !is.na (data_sub$DO_deficit))
			data_sub = data_sub[x_ndx1, ]


			#with ( data_sub, plot ( DO_deficit, DO_change ))
			#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
			
			try (plot (data_sub$DO_deficit, data_sub$DO_diff, ylim = c ( min_y, max_y), xlim = c ( min_x, max_x), xlab = 'Oxygen deficit (milligrams per litre)', ylab = 'Change in oxygen \n concentration \n (milligrams per litre)', cex.axis = cex_axis, cex.lab = cex_lab))
			fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		
			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])


			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			
			abline (fit)


			#legend("topleft", inset=c( 0,-0.4), legend =  paste ("Night", i), pch=c(NA), bty = 'n', cex = cex_lab )

			mtext (side = 3, line = 1, at = 0.05, paste ("Night", i), cex = cex_lab )
			
			#text ( 2,1.5, paste ( 'Date: ',x_ndx, '  ', i, sep = ''))
			text ( 2,1.5, paste ( 'Date: ',x_ndx, sep = ''))
			
			}

	par (xpd = FALSE)
	par ( mfrow = c ( 1,1))

	print (m_coeffs)
	print (round (m_coeffs, 3))
	
	}


#PlotRegressionER_Ks_02DataSet ( indata )

























































PlotModelOutputNightTime <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'Modeloutput_cf_obs_sim_night_2pane_col.png' )
	{
	library (parker)
	data (parker)
	
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_nights = unique ( indata$daynight1)

	PngOn(n_height)
		{
		par (mfrow  = c (1,2))
		par ( mar = c ( 7,5,4,3))

		for ( i in 1:length(x_nights))
			{
			data_sub <- indata[indata$daynight1 == x_nights[i],]

			max_y <- max ( data_sub$Cnow, data_sub$DO )
			min_y <- min ( data_sub$Cnow, data_sub$DO )

			plot ( data_sub$date, data_sub$Cnow, ylim = c ( min_y, max_y), col = 'gray65', pch = 2, type = 'n', lty = 2, ylab = 'DO (mg per litre)', xlab = 'Time', cex.axis = cex_lab, cex.lab = cex_lab, cex.main = cex_main)
			points ( indata$date, indata$C_sat, type = 'p', col = 'grey90', cex = 0.4)
			points ( data_sub$date, data_sub$DO, type = 'p')
			points ( data_sub$date, data_sub$Cnow, type = 'p',  col = 'gray65', pch = 2, cex = 0.4 )
			
			text ( data_sub$date[120], max_y - 0.2, paste ( 'Night', i), cex = cex_lab)
			
			legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey65'), lwd = c (2,2), lty = c (NA,NA) )
			#legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey65'), lwd = c (2,2) )
			
			}
		}
	PngOff()
	par (mfrow  = c (1,1))

	}

#PlotModelOutputNightTime ( Ebble_CE1_2013_08_08_ModelNight )







































































PlotModelOutputNightTimeMol <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'Modeloutput_cf_obs_sim_night_2pane_col.png' )
	{
	library (parker)
	data (parker)
	
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_nights = unique ( indata$daynight1)

	PngOn(n_height)
		{
		par (mfrow  = c (1,2))
		par ( mar = c ( 7,5,4,3))

		for ( i in 1:length(x_nights))
			{
			data_sub <- indata[indata$daynight1 == x_nights[i],]

			max_y <- max ( data_sub$Cnow, data_sub$DO )
			min_y <- min ( data_sub$Cnow, data_sub$DO )

			plot ( data_sub$date, data_sub$Cnow, ylim = c ( min_y, max_y), col = 'gray65', pch = 2, type = 'n', lty = 2, ylab = 'DO (micromoles per litre)', xlab = 'Time', cex.axis = cex_lab, cex.lab = cex_lab, cex.main = cex_main)
			points ( indata$date, indata$C_sat, type = 'p', col = 'grey90', cex = 0.4)
			points ( data_sub$date, data_sub$DO, type = 'p')
			points ( data_sub$date, data_sub$Cnow, type = 'p',  col = 'gray65', pch = 2, cex = 0.4 )
			
			text ( data_sub$date[120], max_y - 0.2, paste ( 'Night', i), cex = cex_lab)
			
			legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey65'), lwd = c (2,2), lty = c (NA,NA) )
			#legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey65'), lwd = c (2,2) )
			
			}
		}
	PngOff()
	par (mfrow  = c (1,1))

	}

#PlotModelOutputNightTime ( Ebble_CE1_2013_08_08_ModelNight )


















































































































































PlotModelOutputDay <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'Modeloutput_cf_obs_sim_DayNight.png' )
	{
	library (parker)
	data (parker)
	
	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	max_y <- ceiling (max ( indata$Cnow, indata$DO,  na.rm = T )) + 1
	min_y <- floor (min ( indata$Cnow, indata$DO,  na.rm = T ))

	pngfilename = outfile
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))
		
		plot ( indata$date, indata$Cnow, ylim = c ( min_y, max_y), col = 'gray65', pch = 2, type = 'n', lty = 2, xlab = 'Time', ylab = 'DO (mg per litre)', cex.axis = cex_lab, cex.lab = cex_lab, cex.main = cex_main)
		points ( indata$date, indata$C_sat, type = 'p', col = 'grey90', cex = 0.4)
		points ( indata$date, indata$DO, type = 'p')
		points ( indata$date, indata$Cnow, type = 'p',  col = 'gray65', pch = 2, cex = 0.4 )
		
		
		legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey65'), lwd = c (2,2), lty = c (NA,NA) )
		}
	PngOff()
	par (mfrow  = c (1,1))

	}

#PlotModelOutputDay ( indata = Ebble_CE1_2013_08_08_ModelDayNight )
































































PlotModelOutputDayMol <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'Modeloutput_cf_obs_sim_DayNight.png' )
	{
	library (parker)
	data (parker)
	
	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	max_y <- ceiling (max ( indata$Cnow, indata$DO,  na.rm = T )) + 1
	min_y <- floor (min ( indata$Cnow, indata$DO,  na.rm = T ))

	pngfilename = outfile
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))
		
		plot ( indata$date, indata$Cnow, ylim = c ( min_y, max_y), col = 'gray65', pch = 2, type = 'n', lty = 2, xlab = 'Time', ylab = 'DO (micromoles per litre)', cex.axis = cex_lab, cex.lab = cex_lab, cex.main = cex_main)
		points ( indata$date, indata$C_sat, type = 'p', col = 'grey90', cex = 0.4)
		points ( indata$date, indata$DO, type = 'p')
		points ( indata$date, indata$Cnow, type = 'p',  col = 'gray65', pch = 2, cex = 0.4 )
		
		
		legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey65'), lwd = c (2,2), lty = c (NA,NA) )
		}
	PngOff()
	par (mfrow  = c (1,1))

	}

#PlotModelOutputDay ( indata = Ebble_CE1_2013_08_08_ModelDayNight )

























































































































































PlotPartitions01 <- function ( indata, min_y, max_y, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned.png')
	{
	library (parker)
	data (parker)

	#max_y <- ceiling (max ( indata$Cnow, indata$DO,  na.rm = T )) + 1
	#min_y <- floor (min ( indata$Cnow, indata$DO,  na.rm = T ))

	pngfilename = outfile

	pngfilename = 'Lorenzo_Ebble_2013_partition_01.png'
	PngOn()
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata$date, indata$Phot, ylim = c ( min_y, max_y), pch = 1, type = 'b', lty = 2, xlab = x_labeldate, ylab = 'DO (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata$date, indata$Gas, type = 'b', col = 'gray50' , pch = 3)
		points ( indata$date, indata$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)

		#text ( as.POSIXct ( '2013-04-27 08:30:00'), 0.05, 'Photosynthesis ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), 0.012, 'Reaeration ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), -0.01, 'Respiration ')
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		#legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (NA,NA) )
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata = Ebble_CE1_2013_08_08_ModelDayNight )



































































PlotPartitions <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned.png' )
	{
	library (parker)
	data (parker)

	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	max_y <- ceiling (max ( indata$Cnow, indata$DO,  na.rm = T )) + 1
	min_y <- floor (min ( indata$Cnow, indata$DO,  na.rm = T ))

	pngfilename = outfile

	pngfilename = 'Lorenzo_Ebble_2013_partition_01.png'
	PngOn()
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata$date, indata$Phot, ylim = c ( -0.06,0.06), pch = 1, type = 'b', lty = 2, xlab = x_labeldate, ylab = 'DO (mg per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata$date, indata$Gas, type = 'b', col = 'gray50' , pch = 3)
		points ( indata$date, indata$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)
		#text ( as.POSIXct ( '2013-04-27 08:30:00'), 0.05, 'Photosynthesis ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), 0.012, 'Reaeration ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), -0.01, 'Respiration ')
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		#legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (NA,NA) )
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata = Ebble_CE1_2013_08_08_ModelDayNight )

















































































PlotPartitionsMol01 <- function ( indata, min_y, max_y, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned.png')
	{
	library (parker)
	data (parker)

	#max_y <- ceiling (max ( indata$Cnow, indata$DO,  na.rm = T )) + 1
	#min_y <- floor (min ( indata$Cnow, indata$DO,  na.rm = T ))

	pngfilename = outfile

	pngfilename = 'Lorenzo_Ebble_2013_partition_01.png'
	PngOn()
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata$date, indata$Phot, ylim = c ( min_y, max_y), pch = 1, type = 'b', lty = 2, xlab = x_labeldate, ylab = 'DO (micromoles per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata$date, indata$Gas, type = 'b', col = 'gray50' , pch = 3)
		points ( indata$date, indata$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)

		#text ( as.POSIXct ( '2013-04-27 08:30:00'), 0.05, 'Photosynthesis ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), 0.012, 'Reaeration ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), -0.01, 'Respiration ')
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		#legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (NA,NA) )
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata = Ebble_CE1_2013_08_08_ModelDayNight )


































































PlotPartitionsMol <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned.png' )
	{
	library (parker)
	data (parker)

	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1


	max_y <- ceiling (max ( indata$Cnow, indata$DO,  na.rm = T )) + 1
	min_y <- floor (min ( indata$Cnow, indata$DO,  na.rm = T ))

	pngfilename = outfile

	pngfilename = 'Lorenzo_Ebble_2013_partition_01.png'
	PngOn()
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata$date, indata$Phot, ylim = c ( min_y, max_y), pch = 1, type = 'b', lty = 2, xlab = x_labeldate, ylab = 'DO (micromoles per litre)', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata$date, indata$Gas, type = 'b', col = 'gray50' , pch = 3)
		points ( indata$date, indata$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)

		#text ( as.POSIXct ( '2013-04-27 08:30:00'), 0.05, 'Photosynthesis ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), 0.012, 'Reaeration ')
		#text ( as.POSIXct ( '2013-04-27 02:30:00'), -0.01, 'Respiration ')
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		
		#legend ( 'topright', c ( 'Observed','Simulated'), pch = c ( 1,2 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (NA,NA) )
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata = Ebble_CE1_2013_08_08_ModelDayNight )






























































































PlotPartitionsHour <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned_hour.png' )
	{
	
	#indata <- Sem_AS2_2013_05_01_ModelDayNight
	library (parker)
	data (parker)
	data (meta_data_rovelli)
	
	
	#these give the same
	#format ( as.POSIXct ('2013-01-01 13:00:00'), '%Y-%m-%d %H')
	#format ( as.POSIXct ('2013-01-01 13:59:00'), '%Y-%m-%d %H')
	#therefore add on 30 mins to time stamp, so that 13:59 becomes 14:29 and therefore 2 p.m (rounded)
	print (indata$date)
	indata$date <- indata$date + 30*60
	print (indata$date)
	
	indata$date_hour <- format ( indata$date, '%Y-%m-%d %H')
	
	gas_hour <- with (indata, aggregate ( Gas, by = list (date_hour), FUN = sum ))
	gas_hour <- aggregate ( indata[c ('Phot', 'Gas', 'Resp')], by = list (indata$date_hour), FUN = sum )
	
	
	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1

	indata_agg <- gas_hour
	indata_agg$date <- as.POSIXct ( paste ( indata_agg$Group.1, ':00:00', sep = ''))
	str (indata_agg)

	max_y <- ceiling (max ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))
	min_y <- floor (min ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))

	
	pngfilename = outfile
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata_agg$date, indata_agg$Phot, ylim = c ( min_y, max_y), pch = 1, type = 'b', lty = 3, xlab = x_labeldate, ylab = 'DO  flux (mg per litre per hour)', main = '', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata_agg$date, indata_agg$Gas, type = 'b', col = 'gray50' , pch = 4, lty = 3)
		#points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)

		#legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux'), pch = c ( 1,4 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (3,3), bty = 'n' )
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata_agg = Ebble_CE1_2013_08_08_ModelDayNight )













































































PlotPartitionsHour5 <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned_hour.png' )
	{
	
	#indata <- Sem_AS2_2013_05_01_ModelDayNight
	library (parker)
	data (parker)
	data (meta_data_rovelli)
	
	
	#these give the same
	#format ( as.POSIXct ('2013-01-01 13:00:00'), '%Y-%m-%d %H')
	#format ( as.POSIXct ('2013-01-01 13:59:00'), '%Y-%m-%d %H')
	#therefore add on 30 mins to time stamp, so that 13:59 becomes 14:29 and therefore 2 p.m (rounded)
	print (indata$date)
	indata$date <- indata$date + 30*60
	print (indata$date)
	
	indata$date_hour <- format ( indata$date, '%Y-%m-%d %H')
	
	gas_hour <- with (indata, aggregate ( Gas, by = list (date_hour), FUN = sum ))
	gas_hour <- aggregate ( indata[c ('Phot', 'Gas', 'Resp')], by = list (indata$date_hour), FUN = sum )
	
	
	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1

	indata_agg <- gas_hour
	indata_agg$date <- as.POSIXct ( paste ( indata_agg$Group.1, ':00:00', sep = ''))
	str (indata_agg)

	max_y <- ceiling (max ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))
	min_y <- floor (min ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))

	
	pngfilename = outfile
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata_agg$date, indata_agg$Phot, ylim = c (-5, 5), pch = 1, type = 'b', lty = 3, xlab = x_labeldate, ylab = 'DO  flux (mg per litre per hour)', main = '', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata_agg$date, indata_agg$Gas, type = 'b', col = 'gray50' , pch = 4, lty = 3)
		#points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)

		#legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux'), pch = c ( 1,4 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (3,3), bty = 'n' )
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata_agg = Ebble_CE1_2013_08_08_ModelDayNight )





















































































































PlotPartitionsHourMol <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned_hour.png' )
	{
	
	#indata <- Sem_AS2_2013_05_01_ModelDayNight
	library (parker)
	data (parker)
	data (meta_data_rovelli)
	
	
	#these give the same
	#format ( as.POSIXct ('2013-01-01 13:00:00'), '%Y-%m-%d %H')
	#format ( as.POSIXct ('2013-01-01 13:59:00'), '%Y-%m-%d %H')
	#therefore add on 30 mins to time stamp, so that 13:59 becomes 14:29 and therefore 2 p.m (rounded)
	print (indata$date)
	indata$date <- indata$date + 30*60
	print (indata$date)
	
	indata$date_hour <- format ( indata$date, '%Y-%m-%d %H')
	
	gas_hour <- with (indata, aggregate ( Gas, by = list (date_hour), FUN = sum ))
	gas_hour <- aggregate ( indata[c ('Phot', 'Gas', 'Resp')], by = list (indata$date_hour), FUN = sum )
	
	
	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1

	indata_agg <- gas_hour
	indata_agg$date <- as.POSIXct ( paste ( indata_agg$Group.1, ':00:00', sep = ''))
	str (indata_agg)

	max_y <- ceiling (max ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))
	min_y <- floor (min ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))

	
	pngfilename = outfile
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata_agg$date, indata_agg$Phot, ylim = c ( min_y, max_y), pch = 1, type = 'b', lty = 3, xlab = x_labeldate, ylab = 'DO  flux (micromoles per litre per hour)', main = '', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata_agg$date, indata_agg$Gas, type = 'b', col = 'gray50' , pch = 4, lty = 3)
		#points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)

		#legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux'), pch = c ( 1,4 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (3,3), bty = 'n' )
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata_agg = Ebble_CE1_2013_08_08_ModelDayNight )























































PlotPartitionsHourMol100 <- function ( indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, outfile = 'O2_partitioned_hour.png' )
	{
	
	#indata <- Sem_AS2_2013_05_01_ModelDayNight
	library (parker)
	data (parker)
	data (meta_data_rovelli)
	
	
	#these give the same
	#format ( as.POSIXct ('2013-01-01 13:00:00'), '%Y-%m-%d %H')
	#format ( as.POSIXct ('2013-01-01 13:59:00'), '%Y-%m-%d %H')
	#therefore add on 30 mins to time stamp, so that 13:59 becomes 14:29 and therefore 2 p.m (rounded)
	print (indata$date)
	indata$date <- indata$date + 30*60
	print (indata$date)
	
	indata$date_hour <- format ( indata$date, '%Y-%m-%d %H')
	
	gas_hour <- with (indata, aggregate ( Gas, by = list (date_hour), FUN = sum ))
	gas_hour <- aggregate ( indata[c ('Phot', 'Gas', 'Resp')], by = list (indata$date_hour), FUN = sum )
	
	
	#indata = Ebble_CE1_2013_08_08_ModelDayNight
	print ( head (indata))

	l_png = l_png
	l_png = l_tif1

	indata_agg <- gas_hour
	indata_agg$date <- as.POSIXct ( paste ( indata_agg$Group.1, ':00:00', sep = ''))
	str (indata_agg)

	max_y <- ceiling (max ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))
	min_y <- floor (min ( gas_hour[c ('Phot', 'Gas')], na.rm = T ))

	
	pngfilename = outfile
	PngOn (n_height = n_height)
		{
		par (mfrow  = c (1,1))
		par ( mar = c ( 7,7,4,3))

		plot ( indata_agg$date, indata_agg$Phot, ylim = c ( -100, 100 ), pch = 1, type = 'b', lty = 3, xlab = x_labeldate, ylab = 'DO  flux (micromoles per litre per hour)', main = '', cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		
		points ( indata_agg$date, indata_agg$Gas, type = 'b', col = 'gray50' , pch = 4, lty = 3)
		#points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		points ( indata_agg$date, indata_agg$Resp, type = 'b', col = 'gray50', pch = 2)

		abline (0,0, col = 'grey75', lty = 2)

		#legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux'), pch = c ( 1,4 ), cex = 1.6, col = c ( 'black','grey50'), lwd = c (2,2), lty = c (3,3), bty = 'n' )
		
		legend ( 'topleft', c ( 'Oxygen production flux','Aeration flux', 'Respiration / groundwater flux'), pch = c ( 1,4, 2 ), cex = 1.6, col = c ( 'black','grey50', 'grey50'), lwd = c (2,2,2), lty = c (3,3,3), bty = 'n' )
		
		
		}
	PngOff()
	par (mfrow  = c (1,1))
	}

#PlotPartitions ( indata_agg = Ebble_CE1_2013_08_08_ModelDayNight )

