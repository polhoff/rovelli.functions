
UnlistOutputAnalysis <- function ( indata = output_analysis )
	{
	c_names <- names (indata)
	
	for ( i in 1:length (c_names))
		{
		assign ( c_names[i], indata[[i]], env = .GlobalEnv )
		}
	
	}


#library (rovelli)
#output_analysis <- AnalysisMaySem ()	
#ls(); UnlistOutputAnalysis(); ls()
#PlotMaySem()
