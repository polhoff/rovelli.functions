
PlotMaySem <- function ( indata = output_analysis, c_indata = 'Sem_AS2_2013_05_01', SiteCode = 'AS2' )
	{
	c_names <- names (indata)
	
	for ( i in 1:length (c_names))
		{
		assign ( c_names[i], indata[[i]] )
		}
	PlotWindspeed (c_rda = 'Sem_AS2_2013_05_01_wind', c_indata1 = 'Sem_AS2_2013_05_01_wind')

	NewW(); 	PlotDayNight ( c_indata )
	NewW(); 	PlotDeficit_DODiff ( c_indata )

	NewW(); 	PlotRegressionER_Ks ( c_indata = c_indata )
	NewW(); 	PlotRegressionER_Ks_01 ( c_indata = c_indata )
	NewW(); 	PlotRegressionER_Ks_02 ( c_indata = c_indata )
	NewW(); 	PlotRegressionER_Ks_03 ( c_indata = c_indata )
	NewW(); 	PlotRegressionER_Ks_Res ( c_indata = c_indata )

	NewW(); 	PlotDOPhot ( compute_DOPhot )

	NewW(); 	PlotModelOutputNightTime ( Sem_AS2_2013_05_01_ModelNight )
	NewW();
	
	PlotModelOutputDay ( indata = Sem_AS2_2013_05_01_ModelDayNight )
	
	PlotDOObsSimDiff ( indata = Sem_AS2_2013_05_01_ModelDayNight, SiteCode = 'AS2', start_date = as.POSIXct('2013-04-30'), end_date = as.POSIXct('2013-05-03 05:30:00'))
	
	PlotDOObsSimDiff ( indata = Sem_AS2_2013_05_01_ModelDayNight, SiteCode = 'AS2', start_date = as.POSIXct('2013-04-30'), end_date = as.POSIXct('2013-05-03 05:30:00'))
	
	NewW(); PlotPartitions (indata = Sem_AS2_2013_05_01_ModelDayNight)
	NewW(); PlotPartitionsHour (indata = Sem_AS2_2013_05_01_ModelDayNight)
	}

#library (parker); library (rovelli)
#output_analysis <- AnalysisMaySem ()	
#PlotMaySem()
#ClsDev()
