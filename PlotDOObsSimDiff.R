

PlotDOObsSimDiff <- function (indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode, outfile = 'a1.png', start_date = as.POSIXct ('2013-01-01'), end_date = as.POSIXct ('2017-01-01'), max_y, min_y)
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

	indata <- indata[indata$date > start_date & indata$date < end_date,]

	lon <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lon']
	lat <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lat']

	solar_noon <- SolarNoon (indata$date, lon, lat)
	
	indata$obs_sim <- indata$DO - indata$Cnow
	
	if (missing(max_y))
		{
		max_y <- max ( indata$obs_sim, indata$obs_sim, na.rm = T )
		min_y <- min ( indata$obs_sim, indata$obs_sim, na.rm = T )

		max_y <- ceiling (max_y)
		min_y <- floor (min_y)
		}



	x_label = paste ( 'Date ( ', min ( as.Date(indata$date)), max ( as.Date(indata$date)), ')' )
	y_label = 'DO (mg per litre)'

	#max_y_moles <- max ( indata$obs_simDO_mol, na.rm = T )
	#min_y_moles <- min ( indata$obs_sim$DO_mol, na.rm = T )

	#max_y_moles <- ceiling (max_y_moles)
	#min_y_moles <- floor (min_y_moles)

	pngfilename <- outfile
	
	Png ( pick1 = x, pick2 = y, outfile =  'DOObsSimDiff', n_height = n_height)
		par ( mar = c ( 7,7,7,4))
		
		plot ( indata$date, indata$obs_sim, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_label, ylab = y_label, cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		abline (0,0)

		for ( i in 1:length(solar_noon))
			{
			abline (v = solar_noon[i], lty = 2, col = 'gray50', lwd = 2)
			text ( solar_noon[i], -1, "Solar noon", srt=90, pos = 4, cex = cex_lab * .75) 
			}
	

		points ( indata$date, indata$obs_sim, type = 'p')
		with ( indata, points ( date[daynight == 'night'], obs_sim[daynight == 'night'], type = 'p', col = 'grey70'))
				
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA), bty = 'n')

	Pnh( off = (x | y))
	}

#PlotDOObsSimDiff ( indata = get (data (DO_obs_sim_Rovelli_Ebble2013)), SiteCode = 'CE1')
#PlotDOObsSimDiff ( indata = get (data (DO_obs_sim_Rovelli_Ebble2013)), SiteCode = 'AS1', l_png = T )

















































































PlotDOObsSimDiffMol <- function (indata, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode, outfile = 'a1.png', start_date = as.POSIXct ('2013-01-01'), end_date = as.POSIXct ('2017-01-01'), max_y, min_y)
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

	indata <- indata[indata$date > start_date & indata$date < end_date,]

	lon <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lon']
	lat <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lat']

	solar_noon <- SolarNoon (indata$date, lon, lat)
	
	indata$obs_sim <- indata$DO - indata$Cnow
	
	if (missing(max_y))
		{
		max_y <- max ( indata$obs_sim, indata$obs_sim, na.rm = T )
		min_y <- min ( indata$obs_sim, indata$obs_sim, na.rm = T )

		max_y <- ceiling (max_y)
		min_y <- floor (min_y)
		}



	x_label = paste ( 'Date ( ', min ( as.Date(indata$date)), max ( as.Date(indata$date)), ')' )
	y_label = 'DO (micromoles per litre)'

	#max_y_moles <- max ( indata$obs_simDO_mol, na.rm = T )
	#min_y_moles <- min ( indata$obs_sim$DO_mol, na.rm = T )

	#max_y_moles <- ceiling (max_y_moles)
	#min_y_moles <- floor (min_y_moles)

	pngfilename <- outfile
	
	Png ( pick1 = x, pick2 = y, outfile =  'DOObsSimDiff', n_height = n_height)
		par ( mar = c ( 7,7,7,4))
		
		plot ( indata$date, indata$obs_sim, ylim = c ( min_y, max_y), col = 'gray30', pch = 3, type = 'n', lty = 2, xlab = x_label, ylab = y_label, cex.axis = cex_axis, cex.lab = cex_lab, cex.main = 1.6)
		abline (0,0)

		for ( i in 1:length(solar_noon))
			{
			abline (v = solar_noon[i], lty = 2, col = 'gray50', lwd = 2)
			text ( solar_noon[i], -1, "Solar noon", srt=90, pos = 4, cex = cex_lab * .75) 
			}
	

		points ( indata$date, indata$obs_sim, type = 'p')
		with ( indata, points ( date[daynight == 'night'], obs_sim[daynight == 'night'], type = 'p', col = 'grey70'))
				
		legend ( 'topleft', c ( 'Daytime','Nighttime'), col = c ( 'black','grey70' ), pch =  c(1,1), cex = 1.6, lwd = c ( 2,2), lty = c (NA,NA), bty = 'n')

	Pnh( off = (x | y))
	}

#PlotDOObsSimDiff ( indata = get (data (DO_obs_sim_Rovelli_Ebble2013)), SiteCode = 'CE1')
#PlotDOObsSimDiff ( indata = get (data (DO_obs_sim_Rovelli_Ebble2013)), SiteCode = 'AS1', l_png = T )
