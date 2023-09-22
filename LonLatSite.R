
LonLatSite <- function ( SiteCode )
	{

	library (avon)
	data (SiteLocations)

	lon <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lon']
	lat <- SiteLocations[SiteLocations$SiteCode == SiteCode, 'lat']

	return ( c ('lon' = lon,'lat' = lat))
	}

LonLatSite ('AS1')
LonLatSite ('AS2')
LonLatSite ('CE1')
LonLatSite ('CW2')
LonLatSite ('GA2')
LonLatSite ('GN1')

