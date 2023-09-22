
AnalysisGeneric <- function ( row_number )
	{

	#library(rovelli); row_number = 3
	#row_number = 3

	data(meta_data_rovelli)
	if ( missing(row_number)) { print (meta_data_rovelli) ; return () }

	c_sub <- meta_data_rovelli[row_number,]
	start_date <- c_sub$start_date
	end_date <- c_sub$end_date

	c_windfile <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, 'wind', sep = '_')
	c_basedata <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, sep = '_')
	c_basedata_ER <- paste ( c_basedata, '_ER_Ks', sep = '' )
	c_basedata_Pmax <- paste ( c_basedata, '_Pmax_Ik', sep = '' )


	basedata <- get( do.call ( data, list(c_basedata)))
	
	
	c_indata03 <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, sep = '_')
	c_indata04 <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, sep = '_')

	try (PlotWindspeed (c_rda = c_windfile, c_indata1 = c_windfile))


	ER_Ks_timeseries <- MakeERKsTimeSeries (c_basedata, c_basedata_ER, which_return = 2)

	#PlotPmaxByHourSegment ( c_indata1 = c_basedata, c_indata2 = 'ER_Ks_timeseries', c_library = 'rovelli', n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode, outfile = 'PmaxByHour.png' )



	PlotDayNight ( c_basedata )
	PlotDeficit_DODiff ( c_basedata )



	#calculate coefficients ER and Ks and save as new data sets
	#the resulting datasets must be saved in the library 'rovelli'
	#then the library re-installed including the new datasets
	FitRegressionParameters ( c_indata = c_basedata )
	PlotRegressionER_Ks ( c_indata = c_basedata )
	PlotRegressionER_Ks_01 ( c_indata = c_basedata )
	PlotRegressionER_Ks_02 ( c_indata = c_basedata )
	#PlotRegressionER_Ks_03 ( c_indata = c_basedata )
	#PlotRegressionER_Ks_03 ( c_indata = c_basedata, c_library = 'mdot.data', l_png = TRUE, outfile = c_basedata )
	#PlotRegressionER_Ks_Res ( c_indata = c_basedata )

	#make ER Ks time series for a particular data set
	#ER_Ks_timeseries <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)



	do.call (data, list (c_basedata_ER))
	ER_Ks <- get ( paste (c_basedata_ER, '_for' , sep = ''))
	print ( ER_Ks$ER )
	print ( round (ER_Ks$ER * 12, 3))
	print ( ER_Ks$Ks )
	print ( round (ER_Ks$Ks * 12, 3))
	
	
	data_May <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)
	#data_April <- MakeERKsTimeSeries ('Ebble_CE1_2013_04_25', 'Ebble_CE1_2013_04_25_ER_Ks')
	plot(data_May$PARraw) 
	compute_DOPhot <- CalcDOPhot (data_May)
	PlotDOPhot (compute_DOPhot)
	



	data_May_sub <- data_May[data_May$PARraw > 10,]
	x_fit <- FitProductionCurve ( data_May_sub, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data_May_sub)
	summary(data03)
	

	
	n_dates <- 	unique ( as.Date ( data_May$date))
	parameter_df <- as.data.frame (matrix (nrow = length (n_dates), ncol = 3))
	parameter_df[,2:3] <- x_fit
	parameter_df[,1] <- n_dates

	names (parameter_df) <- c ('date1','Pmax','I_k')


	setwd ( dirdmp )
	assign ( c_basedata_Pmax, parameter_df )
	save ( list  =  c(c_basedata_Pmax), file =  paste ( c_basedata_Pmax, '.rda', sep = ''))


	
	data02 <- CutDataDataSetByTime (indata = data_May, timeinterval = 120, n_date = start_date,  lon = -1.92392, lat = 51.02816)
	x_fit <- FitProductionCurve ( data02, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data02)
	with ( data03, plot (  PARraw, DO_Phot))


	data02 <- CutDataDataSetByTime (indata = data_May, timeinterval = 120, n_date = start_date + 1,  lon = -1.92392, lat = 51.02816)
	x_fit <- FitProductionCurve ( data02, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data02)
	with ( data03, plot (  PARraw, DO_Phot))























































	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	

	
	
		
	
	#################################
	########Simulation night time
	#library (rovelli)
	#check fit of ER and Ks
	#check fit of ER and Ks
	x1 <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)
	model_night01 <- ModelDONightTime(indata = x1, c_night = 'night0002')
	model_night02 <- ModelDONightTime(indata = x1, c_night = 'night0003')
	model_night02 <- ModelDONightTime01(indata = x1, c_night = 'night0003')
	output_ModelNight <- rbind ( model_night01, model_night02 )
	PlotModelOutputNightTime ( output_ModelNight )
	#################################
	
	
	








	
	
	
	#################################
	########Simulation day time
	########Simulation day time
	########Simulation day time
	########Simulation day time
	########Simulation day time
	########Simulation day time
	#################################
	#model entire timeseries
	#library (rovelli)
	x1 <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)
	head (x1,3)
	plot (x1$date,x1$DO)
	do.call ( data, list (c_basedata_Pmax))
	output_ModelDayNight <- ModelDODayNight (indata = x1, inPhot = get (c_basedata_Pmax))
	output_ModelDayNight <- ModelDODayNight01 (indata = x1, inPhot = get (c_basedata_Pmax))
	output_ModelDayNight$daynight <- x1$daynight
	output_ModelDayNight$daynight1 <- x1$daynight1
	
	
	data_sub <- output_ModelDayNight[x1$daynight1 != 'day0004',]
	banana <- approxfun ( data_sub$date, data_sub$Cnow )
	
	data_sub <- output_ModelDayNight[x1$daynight1 != 'day0004',]
	PlotModelOutputDay ( indata = data_sub )
	
	write.csv (data_sub, file = paste (dirdmp, 'output.csv', sep = ''), row.names = FALSE)
	
	Cnow_additional <- banana ( basedata$date[basedata$qualityFilter == 1] )
	points ( basedata$date[basedata$qualityFilter == 1], Cnow_additional, col = 'grey65', pch = 2, cex = 0.4 )
	
	#data_additional <- data.frame ( basedata$date[basedata$qualityFilter == 1], Cnow_additional)
	#write.csv (data_additional, file = paste (dirdmp, 'output1.csv', sep = ''))
	
	#################################

	
	data_hour <- output_ModelDayNight
	
	data_hour$date_hour <- format ( data_hour$date  + 30*60, '%Y-%m-%d %H')
	data_sub_hour <- aggregate ( data_hour[c ('Cnow', 'Phot', 'Resp', 'Gas', 'DO')], by = list (data_hour$date_hour), FUN = sum )
	
	data_sub_hour[c ('Cnow','DO')] <- aggregate ( data_hour[c ('Cnow', 'DO')], by = list (data_hour$date_hour), FUN = mean, na.rm = TRUE )[c ('Cnow','DO')]
	
	data_sub_hour$date <- as.POSIXct ( paste (data_sub_hour$Group.1, ':30:00', sep = ''))
	data_sub_hour <- data_sub_hour[ c ('date', 'Cnow', 'Phot',  'Resp', 'Gas', 'DO')]
	names (data_sub_hour) <- c ('date', 'Cnow', 'Phot',  'Resp', 'Gas', 'DO')
	print (names (data_sub_hour))


	write.csv (data_sub_hour, file = paste (dirdmp, 'output_analysis_hour.csv', sep = ''), row.names = FALSE)
	
	data_sub_hour_mol <- data_sub_hour
	data_sub_hour_mol[ c('Cnow', 'Phot',  'Resp', 'Gas', 'DO')] <- MgToMol (data_sub_hour_mol[ c('Cnow', 'Phot',  'Resp', 'Gas', 'DO')]) * 10^6
	

	write.csv (data_sub_hour_mol, file = paste (dirdmp, 'output_analysis_hour_micmol.csv', sep = ''), row.names = FALSE)


	output_list <- try ( list ( output_ModelNight, output_ModelDayNight, compute_DOPhot, data_sub_hour, data_sub_hour_mol ))
	names (output_list) <- try ( c ( paste (c_basedata, '_ModelNight', sep = ''), paste (c_basedata, '_ModelDayNight', sep = ''), 'compute_DOPhot', paste (c_basedata, '_ModelNight_hour', sep = ''), paste (c_basedata, '_ModelNight_hour_mol', sep = '')  ))
	
	
	output_name <- paste (c_basedata, '_ModelOutput', sep = '')
	assign ( output_name, output_list )
	save (list = output_name, file = paste (dirdmp, output_name, '.rda', sep = ''))

	
	
	try ( return ( output_list))
	}


#output_analysis <- AnalysisMaySem ()
#library(rovelli); output_analysis <- AnalysisGeneric (1)
#output_analysis <- AnalysisGeneric (1)
#output_analysis <- AnalysisGeneric (2)
#output_analysis <- AnalysisGeneric (3)
#output_analysis_hour <- output_analysis[[4]]; write.csv (output_analysis_hour, file = paste (dirdmp, 'output_analysis_hour.csv', sep = ''), row.names = FALSE);
#a1 <- output_analysis_hour
#with (a1, plot ( date, Cnow, type = 'b')); with (a1, points ( date, DO, type = 'b', col = 'red'))
#X11(); with (a2, plot ( date, Cnow, type = 'b')); with (a2, points ( date, DO, type = 'b', col = 'red'))
#X11(); with (a3, plot ( date, Cnow, type = 'b')); with (a3, points ( date, DO, type = 'b', col = 'red'))
#output_analysis_hour <- output_analysis[[4]]
#with (output_analysis_hour, plot ( date, Cnow, type = 'b'))
#with (output_analysis_hour, plot ( date, Phot, type = 'b', ylim = c ( -1.6,1.6)))
#with (output_analysis_hour, points ( date, Gas, type = 'b', ylim = c ( -2,2), col = 'blue'))
#with (output_analysis_hour, points ( date, Resp, type = 'b', ylim = c ( -2,2), col = 'red'))






































































AnalysisGenericDataSet <- function ( c_basedata )
	{

	#library(rovelli); row_number = 3
	#row_number = 3

	data(meta_data_rovelli)
	if ( missing(row_number)) { print (meta_data_rovelli) ; return () }

	c_sub <- meta_data_rovelli[row_number,]
	start_date <- c_sub$start_date
	end_date <- c_sub$end_date

	c_windfile <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, 'wind', sep = '_')
	c_basedata <- c_basedata
	c_basedata_ER <- paste ( c_basedata, '_ER_Ks', sep = '' )
	c_basedata_Pmax <- paste ( c_basedata, '_Pmax_Ik', sep = '' )


	basedata <- get( c_basedata )
	
	
	c_indata03 <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, sep = '_')
	c_indata04 <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, sep = '_')

	try (PlotWindspeed (c_rda = c_windfile, c_indata1 = c_windfile))


	ER_Ks_timeseries <- MakeERKsTimeSeries (c_basedata, c_basedata_ER, which_return = 2)

	#PlotPmaxByHourSegment ( c_indata1 = c_basedata, c_indata2 = 'ER_Ks_timeseries', c_library = 'rovelli', n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode, outfile = 'PmaxByHour.png' )



	PlotDayNight ( c_basedata )
	PlotDeficit_DODiff ( c_basedata )



	#calculate coefficients ER and Ks and save as new data sets
	#the resulting datasets must be saved in the library 'rovelli'
	#then the library re-installed including the new datasets
	FitRegressionParameters ( c_indata = c_basedata )
	PlotRegressionER_Ks ( c_indata = c_basedata )
	PlotRegressionER_Ks_01 ( c_indata = c_basedata )
	PlotRegressionER_Ks_02 ( c_indata = c_basedata )
	#PlotRegressionER_Ks_03 ( c_indata = c_basedata )
	#PlotRegressionER_Ks_03 ( c_indata = c_basedata, c_library = 'mdot.data', l_png = TRUE, outfile = c_basedata )
	#PlotRegressionER_Ks_Res ( c_indata = c_basedata )

	#make ER Ks time series for a particular data set
	#ER_Ks_timeseries <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)



	do.call (data, list (c_basedata_ER))
	ER_Ks <- get ( paste (c_basedata_ER, '_for' , sep = ''))
	print ( ER_Ks$ER )
	print ( round (ER_Ks$ER * 12, 3))
	print ( ER_Ks$Ks )
	print ( round (ER_Ks$Ks * 12, 3))
	
	
	data_May <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)
	#data_April <- MakeERKsTimeSeries ('Ebble_CE1_2013_04_25', 'Ebble_CE1_2013_04_25_ER_Ks')
	plot(data_May$PARraw) 
	compute_DOPhot <- CalcDOPhot (data_May)
	PlotDOPhot (compute_DOPhot)
	



	data_May_sub <- data_May[data_May$PARraw > 10,]
	x_fit <- FitProductionCurve ( data_May_sub, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data_May_sub)
	summary(data03)
	

	
	n_dates <- 	unique ( as.Date ( data_May$date))
	parameter_df <- as.data.frame (matrix (nrow = length (n_dates), ncol = 3))
	parameter_df[,2:3] <- x_fit
	parameter_df[,1] <- n_dates

	names (parameter_df) <- c ('date1','Pmax','I_k')


	setwd ( dirdmp )
	assign ( c_basedata_Pmax, parameter_df )
	save ( list  =  c(c_basedata_Pmax), file =  paste ( c_basedata_Pmax, '.rda', sep = ''))


	
	data02 <- CutDataDataSetByTime (indata = data_May, timeinterval = 120, n_date = start_date,  lon = -1.92392, lat = 51.02816)
	x_fit <- FitProductionCurve ( data02, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data02)
	with ( data03, plot (  PARraw, DO_Phot))


	data02 <- CutDataDataSetByTime (indata = data_May, timeinterval = 120, n_date = start_date + 1,  lon = -1.92392, lat = 51.02816)
	x_fit <- FitProductionCurve ( data02, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data02)
	with ( data03, plot (  PARraw, DO_Phot))























































	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	########Simulations
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	#################################
	

	
	
		
	
	#################################
	########Simulation night time
	#library (rovelli)
	#check fit of ER and Ks
	#check fit of ER and Ks
	x1 <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)
	model_night01 <- ModelDONightTime(indata = x1, c_night = 'night0002')
	model_night02 <- ModelDONightTime(indata = x1, c_night = 'night0003')
	model_night02 <- ModelDONightTime01(indata = x1, c_night = 'night0003')
	output_ModelNight <- rbind ( model_night01, model_night02 )
	PlotModelOutputNightTime ( output_ModelNight )
	#################################
	
	
	








	
	
	
	#################################
	########Simulation day time
	########Simulation day time
	########Simulation day time
	########Simulation day time
	########Simulation day time
	########Simulation day time
	#################################
	#model entire timeseries
	#library (rovelli)
	x1 <- MakeERKsTimeSeries (c_basedata, c_basedata_ER)
	head (x1,3)
	plot (x1$date,x1$DO)
	do.call ( data, list (c_basedata_Pmax))
	output_ModelDayNight <- ModelDODayNight (indata = x1, inPhot = get (c_basedata_Pmax))
	output_ModelDayNight <- ModelDODayNight01 (indata = x1, inPhot = get (c_basedata_Pmax))
	output_ModelDayNight$daynight <- x1$daynight
	output_ModelDayNight$daynight1 <- x1$daynight1
	
	
	data_sub <- output_ModelDayNight[x1$daynight1 != 'day0004',]
	banana <- approxfun ( data_sub$date, data_sub$Cnow )
	
	data_sub <- output_ModelDayNight[x1$daynight1 != 'day0004',]
	PlotModelOutputDay ( indata = data_sub )
	
	write.csv (data_sub, file = paste (dirdmp, 'output.csv', sep = ''), row.names = FALSE)
	
	Cnow_additional <- banana ( basedata$date[basedata$qualityFilter == 1] )
	points ( basedata$date[basedata$qualityFilter == 1], Cnow_additional, col = 'grey65', pch = 2, cex = 0.4 )
	
	#data_additional <- data.frame ( basedata$date[basedata$qualityFilter == 1], Cnow_additional)
	#write.csv (data_additional, file = paste (dirdmp, 'output1.csv', sep = ''))
	
	#################################

	
	data_hour <- output_ModelDayNight
	
	data_hour$date_hour <- format ( data_hour$date  + 30*60, '%Y-%m-%d %H')
	data_sub_hour <- aggregate ( data_hour[c ('Cnow', 'Phot', 'Resp', 'Gas', 'DO')], by = list (data_hour$date_hour), FUN = sum )
	
	data_sub_hour[c ('Cnow','DO')] <- aggregate ( data_hour[c ('Cnow', 'DO')], by = list (data_hour$date_hour), FUN = mean, na.rm = TRUE )[c ('Cnow','DO')]
	
	data_sub_hour$date <- as.POSIXct ( paste (data_sub_hour$Group.1, ':30:00', sep = ''))
	data_sub_hour <- data_sub_hour[ c ('date', 'Cnow', 'Phot',  'Resp', 'Gas', 'DO')]
	names (data_sub_hour) <- c ('date', 'Cnow', 'Phot',  'Resp', 'Gas', 'DO')
	print (names (data_sub_hour))


	write.csv (data_sub_hour, file = paste (dirdmp, 'output_analysis_hour.csv', sep = ''), row.names = FALSE)
	
	data_sub_hour_mol <- data_sub_hour
	data_sub_hour_mol[ c('Cnow', 'Phot',  'Resp', 'Gas', 'DO')] <- MgToMol (data_sub_hour_mol[ c('Cnow', 'Phot',  'Resp', 'Gas', 'DO')]) * 10^6
	

	write.csv (data_sub_hour_mol, file = paste (dirdmp, 'output_analysis_hour_micmol.csv', sep = ''), row.names = FALSE)


	output_list <- try ( list ( output_ModelNight, output_ModelDayNight, compute_DOPhot, data_sub_hour, data_sub_hour_mol ))
	names (output_list) <- try ( c ( paste (c_basedata, '_ModelNight', sep = ''), paste (c_basedata, '_ModelDayNight', sep = ''), 'compute_DOPhot', paste (c_basedata, '_ModelNight_hour', sep = ''), paste (c_basedata, '_ModelNight_hour_mol', sep = '')  ))
	
	
	output_name <- paste (c_basedata, '_ModelOutput', sep = '')
	assign ( output_name, output_list )
	save (list = output_name, file = paste (dirdmp, output_name, '.rda', sep = ''))

	
	
	try ( return ( output_list))
	}


#output_analysis <- AnalysisMaySem ()
#library(rovelli); output_analysis <- AnalysisGeneric (1)
#output_analysis <- AnalysisGeneric (1)
#output_analysis <- AnalysisGeneric (2)
#output_analysis <- AnalysisGeneric (3)
#output_analysis_hour <- output_analysis[[4]]; write.csv (output_analysis_hour, file = paste (dirdmp, 'output_analysis_hour.csv', sep = ''), row.names = FALSE);
#a1 <- output_analysis_hour
#with (a1, plot ( date, Cnow, type = 'b')); with (a1, points ( date, DO, type = 'b', col = 'red'))
#X11(); with (a2, plot ( date, Cnow, type = 'b')); with (a2, points ( date, DO, type = 'b', col = 'red'))
#X11(); with (a3, plot ( date, Cnow, type = 'b')); with (a3, points ( date, DO, type = 'b', col = 'red'))
#output_analysis_hour <- output_analysis[[4]]
#with (output_analysis_hour, plot ( date, Cnow, type = 'b'))
#with (output_analysis_hour, plot ( date, Phot, type = 'b', ylim = c ( -1.6,1.6)))
#with (output_analysis_hour, points ( date, Gas, type = 'b', ylim = c ( -2,2), col = 'blue'))
#with (output_analysis_hour, points ( date, Resp, type = 'b', ylim = c ( -2,2), col = 'red'))
