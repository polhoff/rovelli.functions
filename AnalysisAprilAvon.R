
AnalysisAprilAvon <- function ( SiteCode = 'GA2')
	{
	try (PlotWindspeed (c_rda = "Avon_GA2_2013_04_28_wind", c_indata1 = "Avon_GA2_2013_04_28_wind"))


	ER_Ks_timeseries <- MakeERKsTimeSeries ('Avon_GA2_2013_04_28', 'Avon_GA2_2013_04_28_ER_Ks', which_return = 2)

	#PlotPmaxByHourSegment ( c_indata1 = 'Avon_GA2_2013_04_28', c_indata2 = 'ER_Ks_timeseries', c_library = 'rovelli', n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode, outfile = 'PmaxByHour.png' )



	PlotDayNight ( 'Avon_GA2_2013_04_28' )
	PlotDeficit_DODiff ( 'Avon_GA2_2013_04_28' )



	#calculate coefficients ER and Ks and save as new data sets
	#the resulting datasets must be saved in the library 'rovelli'
	#then the library re-installed including the new datasets
	FitRegressionParameters ( c_indata = 'Avon_GA2_2013_04_28' )
	PlotRegressionER_Ks ( c_indata = 'Avon_GA2_2013_04_28' )
	PlotRegressionER_Ks_01 ( c_indata = 'Avon_GA2_2013_04_28' )
	PlotRegressionER_Ks_02 ( c_indata = 'Avon_GA2_2013_04_28' )
	PlotRegressionER_Ks_03 ( c_indata = 'Avon_GA2_2013_04_28' )
	PlotRegressionER_Ks_Res ( c_indata = 'Avon_GA2_2013_04_28' )

	#make ER Ks time series for a particular data set
	#ER_Ks_timeseries <- MakeERKsTimeSeries ('Avon_GA2_2013_04_28', 'Avon_GA2_2013_04_28_ER_Ks')



	data (Avon_GA2_2013_04_28_ER_Ks)
	print ( Avon_GA2_2013_04_28_ER_Ks_for$ER )
	print ( round (Avon_GA2_2013_04_28_ER_Ks_for$ER * 12, 3))
	print ( Avon_GA2_2013_04_28_ER_Ks_for$Ks )
	print ( round (Avon_GA2_2013_04_28_ER_Ks_for$Ks * 12, 3))
	
	
	data_May <- MakeERKsTimeSeries ('Avon_GA2_2013_04_28', 'Avon_GA2_2013_04_28_ER_Ks')
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
	assign ( 'Avon_GA2_2013_04_28_Pmax_Ik', parameter_df )
	save ( Avon_GA2_2013_04_28_Pmax_Ik, file = 'Avon_GA2_2013_04_28_Pmax_Ik.rda')


	
	data02 <- CutDataDataSetByTime (indata = data_May, timeinterval = 120, n_date = as.Date ('2013-05-01'),  lon = -1.92392, lat = 51.02816)
	x_fit <- FitProductionCurve ( data02, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data02)
	with ( data03, plot (  PARraw, DO_Phot))


	data02 <- CutDataDataSetByTime (indata = data_May, timeinterval = 120, n_date = as.Date ('2013-05-02'),  lon = -1.92392, lat = 51.02816)
	x_fit <- FitProductionCurve ( data02, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	data03 <- CalcDOPhot (data02)
	with ( data03, plot (  PARraw, DO_Phot))



	

	
	
		
	
	#################################
	#library (rovelli)
	#check fit of ER and Ks
	#check fit of ER and Ks
	x1 <- MakeERKsTimeSeries ('Avon_GA2_2013_04_28', 'Avon_GA2_2013_04_28_ER_Ks')
	Avon_model_night01 <- ModelDONightTime(indata = x1, c_night = 'night0002')
	Avon_model_night02 <- ModelDONightTime(indata = x1, c_night = 'night0003')
	Avon_model_night02 <- ModelDONightTime01(indata = x1, c_night = 'night0003')
	Avon_GA2_2013_04_28_ModelNight <- rbind ( Avon_model_night01, Avon_model_night02 )
	PlotModelOutputNightTime ( Avon_GA2_2013_04_28_ModelNight )
	#################################
	
	
	
	
	
	

	#################################
	#model entire timeseries
	#library (rovelli)
	x1 <- MakeERKsTimeSeries ('Avon_GA2_2013_04_28', 'Avon_GA2_2013_04_28_ER_Ks')
	head (x1,3)
	plot (x1$date,x1$DO)
	data (Avon_GA2_2013_04_28_Pmax_Ik)
	Avon_GA2_2013_04_28_ModelDayNight <- ModelDODayNight (indata = x1, inPhot = Avon_GA2_2013_04_28_Pmax_Ik)
	Avon_GA2_2013_04_28_ModelDayNight <- ModelDODayNight01 (indata = x1, inPhot = Avon_GA2_2013_04_28_Pmax_Ik)
	Avon_GA2_2013_04_28_ModelDayNight$daynight <- x1$daynight
	Avon_GA2_2013_04_28_ModelDayNight$daynight1 <- x1$daynight1
	
	
	data_sub <- Avon_GA2_2013_04_28_ModelDayNight[x1$daynight1 != 'day0004',]
	banana <- approxfun ( data_sub$date, data_sub$Cnow )
	
	data_sub <- Avon_GA2_2013_04_28_ModelDayNight[x1$daynight1 != 'day0004',]
	PlotModelOutputDay ( indata = data_sub )
	
	Cnow_additional <- banana ( Avon_GA2_2013_04_28$date[Avon_GA2_2013_04_28$qualityFilter == 1] )
	points ( Avon_GA2_2013_04_28$date[Avon_GA2_2013_04_28$qualityFilter == 1], Cnow_additional, col = 'grey65', pch = 2, cex = 0.4 )
	
	#################################

	
	
	
	#using 
	#data(Avon_GA2_Pmax_Ik)
	#print (Avon_GA2_Pmax_Ik)
	#Avon_GA2_2013_04_28_ModelDayNight <- ModelDODayNight (indata = x1, inPhot = Avon_GA2_Pmax_Ik)
	#PlotModelOutputDay ( indata = Avon_GA2_2013_04_28_ModelDayNight )
		
	
	
	#################################
	#using filtered data
	#library (rovelli)
	#check fit of ER and Ks
	#check fit of ER and Ks
	x1 <- MakeERKsTimeSeries ('Avon_GA2_2013_04_28', 'Avon_GA2_2013_04_28_ER_Ks', which_return = 1)
	Avon_model_night01 <- ModelDONightTime(indata = x1, c_night = 'night0002')
	Avon_model_night02 <- ModelDONightTime(indata = x1, c_night = 'night0003')
	Avon_GA2_2013_04_28_ModelNight <- rbind ( Avon_model_night01, Avon_model_night02 )
	PlotModelOutputNightTime ( Avon_GA2_2013_04_28_ModelNight )
	#################################
	
	output_list <- try ( list ( Avon_GA2_2013_04_28_ModelNight, Avon_GA2_2013_04_28_ModelDayNight, compute_DOPhot ))
	names (output_list) <- try ( c ('Avon_GA2_2013_04_28_ModelNight', 'Avon_GA2_2013_04_28_ModelDayNight', 'compute_DOPhot' ))
	
	try ( return ( output_list))
	}

#output_analysis <- AnalysisAprilAvon ()
