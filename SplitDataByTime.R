


#no facility for normalizing
CalcPhaseDiff_v1.0 <- function (indata, SiteCode, n_start = -120, n_stop = 120, n_int = 10 )
	{
	dim_sim <- length (indata$Cnow)
	n_lag = seq ( n_start, n_stop, length = n_int )

	diff_matrix = matrix ( nrow = dim_sim, ncol = length (n_lag))

	obs <- c (rep (NA, length (indata$DO)), indata$DO, rep (NA, length (indata$DO)))
	sim <- c (rep (NA, length (indata$Cnow)), indata$Cnow, rep (NA, length (indata$Cnow)))

	start_count <- dim_sim + 1

	for ( i in 1:length (n_lag) )
		{
		DO_diff <- obs[start_count:(start_count + dim_sim - 1)] - sim[(start_count + n_lag[i]):(start_count + n_lag[i] + dim_sim - 1)]
		diff_matrix[,i] <- DO_diff
		}
		
	return (diff_matrix)
	}



















































CalcPhaseDiff <- function (indata, SiteCode, n_start = -120, n_stop = 120, n_int = 10, l_norm = TRUE )
	{
	dim_sim <- length (indata$Cnow)
	n_lag = seq ( n_start, n_stop, length = n_int )

	diff_matrix = matrix ( nrow = dim_sim, ncol = length (n_lag))

	obs <- c (rep (NA, length (indata$DO)), indata$DO, rep (NA, length (indata$DO)))
	sim <- c (rep (NA, length (indata$Cnow)), indata$Cnow, rep (NA, length (indata$Cnow)))

	#remove bias
	if (l_norm)
		{
		mean_obs <- mean (obs, na.rm = TRUE)
		mean_sim <- mean (sim, na.rm = TRUE)
		
		obs <- obs - mean_obs
		sim <- sim - mean_sim
		}

	start_count <- dim_sim + 1

	for ( i in 1:length (n_lag) )
		{
		DO_diff <- obs[start_count:(start_count + dim_sim - 1)] - sim[(start_count + n_lag[i]):(start_count + n_lag[i] + dim_sim - 1)]
		diff_matrix[,i] <- DO_diff
		}
		
	return (diff_matrix)
	}





































































SplitDataBySolarNoon <- function (indata, n_date, SiteCode )
	{
	
	stopifnot ( class (n_date) == 'Date' )
	
	indata <- indata [indata$date1 == n_date,]
	
	library (sun)
	library (avon)
	data (SiteLocations)

	lon <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lon']
	lat <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lat']

	solar_noon <- SolarNoon (n_date, lon, lat)
	sunrise <- Sunrise (n_date, lon, lat)
	sunset <- Sunset (n_date, lon, lat)
	
	indata <- indata [indata$date > sunrise,]
	indata <- indata [indata$date < sunset,]
	
	BeforeSolarNoon <- indata [indata$date < solar_noon,]
	AfterSolarNoon <- indata [indata$date > solar_noon,]
	
	x = list ( 'BeforeSolarNoon' = BeforeSolarNoon,  'AfterSolarNoon'  =  AfterSolarNoon)
	#names (x) = c ( 'BeforeSolarNoon', 'AfterSolarNoon' )
	}

