
CreateWindspeedApprox <- function ()
	{
	library(rovelli)
	data (Ebble_Eddy)
	data (Ebble_windspeed)
	
	banana = with ( Ebble_windspeed_2013_04, approxfun ( date, windspeed_mpersec ))
	r = banana ( Ebble_Eddy$date )


	windspeed_approx <- data.frame ( Ebble_Eddy$date, r, Ebble_Eddy$date1 )
	names (windspeed_approx) <- names (Ebble_windspeed_2013_04)

	return (windspeed_approx)
	}
