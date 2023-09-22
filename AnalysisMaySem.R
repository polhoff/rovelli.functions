
AnalysisMaySem <- function ( SiteCode = 'AS2')
	{
	PlotWindspeed (c_rda = "Sem_AS2_2013_05_01_wind", c_indata1 = "Sem_AS2_2013_05_01_wind")


	ER_Ks_timeseries <- MakeERKsTimeSeries ('Sem_AS2_2013_05_01', 'Sem_AS2_2013_05_01_ER_Ks', which_return = 2)

	#PlotPmaxByHourSegment ( c_indata1 = 'Sem_AS2_2013_05_01', c_indata2 = 'ER_Ks_timeseries', c_library = 'rovelli', n_date = as.Date ('2013-04-26'), timeinterval = 120, lon = -1.92392, lat = 51.02816, l_png = F, l_tif1 = F, cex_axis = 1.6, cex_lab = 1.6, cex_main = 1.6, n_height = 600, SiteCode, outfile = 'PmaxByHour.png' )



	PlotDayNight ( 'Sem_AS2_2013_05_01' )
	PlotDeficit_DODiff ( 'Sem_AS2_2013_05_01' )



	#calculate coefficients ER and Ks and save as new data sets
	#the resulting datasets must be saved in the library 'rovelli'
	#then the library re-installed including the new datasets
	FitRegressionParameters ( c_indata = 'Sem_AS2_2013_05_01' )
	PlotRegressionER_Ks ( c_indata = 'Sem_AS2_2013_05_01' )
	PlotRegressionER_Ks_01 ( c_indata = 'Sem_AS2_2013_05_01' )
	PlotRegressionER_Ks_02 ( c_indata = 'Sem_AS2_2013_05_01' )
	PlotRegressionER_Ks_03 ( c_indata = 'Sem_AS2_2013_05_01' )
	PlotRegressionER_Ks_Res ( c_indata = 'Sem_AS2_2013_05_01' )

	#make ER Ks time series for a particular data set
	#ER_Ks_timeseries <- MakeERKsTimeSeries ('Sem_AS2_2013_05_01', 'Sem_AS2_2013_05_01_ER_Ks')



	data (Sem_AS2_2013_05_01_ER_Ks)
	print ( Sem_AS2_2013_05_01_ER_Ks_for$ER )
	print ( round (Sem_AS2_2013_05_01_ER_Ks_for$ER * 12, 3))
	print ( Sem_AS2_2013_05_01_ER_Ks_for$Ks )
	print ( round (Sem_AS2_2013_05_01_ER_Ks_for$Ks * 12, 3))
	
	
	data_May <- MakeERKsTimeSeries ('Sem_AS2_2013_05_01', 'Sem_AS2_2013_05_01_ER_Ks')
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
	assign ( 'Sem_AS2_2013_05_01_Pmax_Ik', parameter_df )
	save ( Sem_AS2_2013_05_01_Pmax_Ik, file = 'Sem_AS2_2013_05_01_Pmax_Ik.rda')


	
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
	x1 <- MakeERKsTimeSeries ('Sem_AS2_2013_05_01', 'Sem_AS2_2013_05_01_ER_Ks')
	Sem_model_night01 <- ModelDONightTime(indata = x1, c_night = 'night0002')
	Sem_model_night02 <- ModelDONightTime(indata = x1, c_night = 'night0003')
	Sem_model_night02 <- ModelDONightTime01(indata = x1, c_night = 'night0003')
	Sem_AS2_2013_05_01_ModelNight <- rbind ( Sem_model_night01, Sem_model_night02 )
	PlotModelOutputNightTime ( Sem_AS2_2013_05_01_ModelNight )
	#################################
	
	
	
	
	
	

	#################################
	#model entire timeseries
	#library (rovelli)
	x1 <- MakeERKsTimeSeries ('Sem_AS2_2013_05_01', 'Sem_AS2_2013_05_01_ER_Ks')
	head (x1,3)
	plot (x1$date,x1$DO)
	data (Sem_AS2_2013_05_01_Pmax_Ik)
	Sem_AS2_2013_05_01_ModelDayNight <- ModelDODayNight (indata = x1, inPhot = Sem_AS2_2013_05_01_Pmax_Ik)
	Sem_AS2_2013_05_01_ModelDayNight <- ModelDODayNight01 (indata = x1, inPhot = Sem_AS2_2013_05_01_Pmax_Ik)
	Sem_AS2_2013_05_01_ModelDayNight$daynight <- x1$daynight
	Sem_AS2_2013_05_01_ModelDayNight$daynight1 <- x1$daynight1
	
	
	data_sub <- Sem_AS2_2013_05_01_ModelDayNight[x1$daynight1 != 'day0004',]
	banana <- approxfun ( data_sub$date, data_sub$Cnow )
	
	data_sub <- Sem_AS2_2013_05_01_ModelDayNight[x1$daynight1 != 'day0004',]
	PlotModelOutputDay ( indata = data_sub )
	
	Cnow_additional <- banana ( Sem_AS2_2013_05_01$date[Sem_AS2_2013_05_01$qualityFilter == 1] )
	points ( Sem_AS2_2013_05_01$date[Sem_AS2_2013_05_01$qualityFilter == 1], Cnow_additional, col = 'grey65', pch = 2, cex = 0.4 )
	
	#################################

	
	
	
	#using 
	#data(Sem_AS2_Pmax_Ik)
	#print (Sem_AS2_Pmax_Ik)
	#Sem_AS2_2013_05_01_ModelDayNight <- ModelDODayNight (indata = x1, inPhot = Sem_AS2_Pmax_Ik)
	#PlotModelOutputDay ( indata = Sem_AS2_2013_05_01_ModelDayNight )
		
	
	
	#################################
	#using filtered data
	#library (rovelli)
	#check fit of ER and Ks
	#check fit of ER and Ks
	x1 <- MakeERKsTimeSeries ('Sem_AS2_2013_05_01', 'Sem_AS2_2013_05_01_ER_Ks', which_return = 1)
	Sem_model_night01 <- ModelDONightTime(indata = x1, c_night = 'night0002')
	Sem_model_night02 <- ModelDONightTime(indata = x1, c_night = 'night0003')
	Sem_AS2_2013_05_01_ModelNight <- rbind ( Sem_model_night01, Sem_model_night02 )
	PlotModelOutputNightTime ( Sem_AS2_2013_05_01_ModelNight )
	#################################
	
	output_list <- try ( list ( Sem_AS2_2013_05_01_ModelNight, Sem_AS2_2013_05_01_ModelDayNight, compute_DOPhot ))
	names (output_list) <- try ( c ('Sem_AS2_2013_05_01_ModelNight', 'Sem_AS2_2013_05_01_ModelDayNight', 'compute_DOPhot' ))
	
	try ( return ( output_list))
	}

#output_analysis <- AnalysisMaySem ()
