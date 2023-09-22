

BalancedAeration <- function(indata, analysisperiod = c(50,70), d_date, Ks = 0.01, l_halfday = TRUE, l_lateday = FALSE)
	{
	data_sub <- indata[indata$date1 == d_date,]

	if(l_halfday & !l_lateday)
		{
		TimeAfterSunrise <- c(NA,with(data_sub,difftime(date, Sunrise, units = 'mins')))
		#less than 6 hours after sunrise
		data_sub <- data_sub[TimeAfterSunrise < 360,]
		}

	if(l_lateday)
		{
		TimeAfterSunrise <- c(NA,with(data_sub,difftime(date, Sunrise, units = 'mins')))
		#less than 6 hours after sunrise
		data_sub <- data_sub[TimeAfterSunrise > 360,]
		}

	#this analysis assumes a timestep of 1 minute
	#using 1 minute timestep the default aeration rate above is 0.01 per minute
	start_date <- data_sub$date[!is.na(data_sub$date)][1]

	outputs <- matrix(nrow = length(analysisperiod), ncol = 3)
	outputs <- data.frame(outputs)
	names(outputs) <- c('DO_change','TimeInterval','SummedPAR')

	for (i in 1:2)
		{
		endpoint <- analysisperiod[i]
		#endpoint <- 70
		base_vector <- 1:endpoint

		#matrix for indices of all minute points in the day in blocks of 60 or thirty etc...
		m_indices <- matrix(nrow = endpoint, ncol = 1440)
		#m_indices <- matrix(nrow = endpoint, ncol = 1440)

		#initialize
		out <- base_vector
		for (j in 2:1440)
			{
			out <- out + 1
			m_indices[,j] <- out
			}


		m_CSat <- m_indices
		m_CSat[] <- NA
		
		
		for (j in 1:dim(m_CSat)[2])
			{
			m_CSat[,j] <- data_sub$DO_deficit[m_indices[,j]]
			}

		aeration <- Ks * m_CSat
		aeration <- colSums(aeration)
		aeration <- (aeration ^ 2) ^ 0.5
		aer_ndx <- which.min(aeration)

		data_sub01 <- data_sub[m_indices[,aer_ndx],]

		outputs$DO_change[i] <- tail(data_sub01$DO,1) - head(data_sub01$DO,1)
		
		time_diff <- difftime(tail(data_sub01$date,1),head(data_sub01$date,1), units = 'mins')
		outputs$TimeInterval[i] <- time_diff
		outputs$SummedPAR[i] <- with(data_sub01, mean(PARraw, na.rm = TRUE)) * as.numeric(time_diff)


		#assign(paste('DO_change_', i, sep = ''), tail(data_sub01$DO,1) - head(data_sub01$DO,1))
		#assign(paste('time_diff_', i, sep = ''), difftime(tail(data_sub01$date,1),head(data_sub01$date,1), units = 'mins'))
		#assign(paste('PAR_sum_', i, sep = ''), with(data_sub01, mean(PARraw, na.rm = TRUE)) * as.numeric(time_diff01))
		}

	#attach(outputs)
	part1 <- with(outputs, TimeInterval[1] * DO_change[2] - TimeInterval[2] * DO_change[1])
	part2 <- with(outputs, TimeInterval[1] * SummedPAR[2] - TimeInterval[2] * SummedPAR[1])
	
	k <- part1 / part2
	ER1 <- with(outputs, (DO_change[1] - k*SummedPAR[1]) / TimeInterval[1])
	ER2 <- with(outputs, (DO_change[2] - k*SummedPAR[2]) / TimeInterval[2])
	
	ER <- round(mean(ER1,ER2),4)

	outputs	<- list(outputs)
	outputs$parameters = c('k' = k, 'ER' = ER)
	
	return(outputs)
	}

#library(rovelli)
#indata <- get(data(Ebble_CE1_2013_04_25)); d_date <- as.Date('2013-04-26')
#BalancedAeration (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01)
#BalancedAeration (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01,l_lateday = TRUE)
