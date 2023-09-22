
PlotAprilEbble <- function ( indata = output_analysis, c_indata = 'Ebble_CE1_2013_04_25', SiteCode = 'CE1' )
	{
	c_names <- names (indata)
	
	for ( i in 1:length (c_names))
		{
		assign ( c_names[i], indata[[i]] )
		}
	try (PlotWindspeed (c_rda = 'Ebble_CE1_2013_04_25_wind', c_indata1 = 'Ebble_CE1_2013_04_25_wind'))

	NewW(); 	PlotDayNight ( c_indata )
	NewW(); 	PlotDeficit_DODiff ( c_indata )

	NewW(); 	PlotRegressionER_Ks ( c_indata = c_indata )
	NewW(); 	PlotRegressionER_Ks_01 ( c_indata = c_indata )
	NewW(); 	PlotRegressionER_Ks_02 ( c_indata = c_indata )
	NewW(); 	PlotRegressionER_Ks_03 ( c_indata = c_indata )
	#NewW(); 	PlotRegressionER_Ks_Res ( c_indata = c_indata )

	NewW(); 	PlotDOPhot ( compute_DOPhot )

	NewW(); 	PlotModelOutputNightTime ( Ebble_CE1_2013_04_25_ModelNight )
	NewW();
	
	PlotModelOutputDay ( indata = Ebble_CE1_2013_04_25_ModelDayNight )
	
	PlotDOObsSimDiff ( indata = Ebble_CE1_2013_04_25_ModelDayNight, SiteCode = 'CE1', start_date = as.POSIXct('2013-04-24'), end_date = as.POSIXct('2013-04-28'))
		
	NewW(); PlotPartitions (indata = Ebble_CE1_2013_04_25_ModelDayNight)
	NewW(); PlotPartitionsHour (indata = Ebble_CE1_2013_04_25_ModelDayNight)
	}

#PlotAprilEbble()
#ClsDev()

