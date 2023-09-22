

LoadData <- function (row_number)
	{
	data (meta_data_rovelli)
	
	if ( missing(row_number)) { print (meta_data_rovelli) ; return () }
	
	c_sub <- meta_data_rovelli[row_number,]
	
	c_indata01 <- paste ( c_sub$river, c_sub$site, c_sub$start_date_ch, sep = '_')
	
	x1 <- do.call (data, list (c_indata01))
	x1 <- get(x1)
	
	print ('##############################')
	print ('##############################')
	print ('##############################')
	print (paste ('You have loaded ', c_indata01))
	print ('##############################')
	print ('##############################')
	print ('##############################')
	
	return (x1)
	
	}

#LoadData()
#indata <- LoadData (1)
















































































ReadRovelliDataOrig <- function ( infile, outname, indir = paste ( dirtop, "avon/orig/", sep = "" ))
	{

	library (gnumeric)
	library (st)
	data(st)
	
	setwd (indir)
	
	a1 <- read.gnumeric.sheet (file = infile, head = TRUE, sheet =  'data', stringsAsFactors = FALSE)
	x1 <- names (a1)
	x1[x1 == 'Csat_mol'] <- try ('CSat_mol')
	names (a1) <- x1

	assign (outname, a1)
	#names(get(outname)) <- x1

	setwd (dirdmp)
	save ( list = outname, file = paste ( outname, '.rda', sep = ''))
	
	x = get(outname)
	}

#x1 <- ReadRovelliDataOrig ( infile = 'C1CE1ec2.gnumeric', outname = 'Ebble_CE1_2013_04_25_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1CE1wind.gnumeric', outname = 'Ebble_CE1_2013_04_25_wind_orig' )


#x1 <- ReadRovelliDataOrig ( infile = 'C2CE1ec2_summer.gnumeric', outname = 'Ebble_CE1_2013_08_08_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C2CE1wind_summer.gnumeric', outname = 'Ebble_CE1_2013_08_08_wind_orig' )

#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )

c_file <- 'C1CW2'
c_in <- paste('C1CW2')
#x1 <- ReadRovelliDataOrig ( infile = 'C1CW2.gnumeric', indir = dirdmp, outname = 'C1CW2_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadRovelliDataOrig ( infile = 'C1GA2.gnumeric', outname = 'Avon_GA2_2013_04_28_orig' )





















































ReadRovelliMeta <- function ()
	{
	indir <- paste ( dirtop, 'Rpackage/rovelli/', sep = '' )
	
	meta_data_rovelli = read.csv( paste ( indir, 'meta_data_rovelli.csv', sep = ''), stringsAsFactors = F)
	meta_data_rovelli$start_date <- as.Date (meta_data_rovelli$start_date)
	meta_data_rovelli$end_date <- as.Date (meta_data_rovelli$end_date)
	
	setwd (dirdmp)
	save (meta_data_rovelli, file = 'meta_data_rovelli.rda')
	}

#ReadRovelliMeta ()
