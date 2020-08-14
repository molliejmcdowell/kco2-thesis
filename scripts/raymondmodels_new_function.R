# Function for calculating k600 (m/d) from seven models (Raymond et al 2012, Table 2)
# Based on velocity (m/s), slope (unitless), depth (m), discharge (m3/s), and Froude number (V/(gD)^0.5)
# Table 2 also provides standard deviations(+/- 1 SD) for equation parameters; r2; slope (+/- SE); and y-intercept (+/- SE for regressions of the equation output vs actual outputs - see Fig 3)

raymondmodels.new <- function(dat) {
	mkrf.co2 <- dat
	mkrf.co2$timestamp <- as.POSIXct(mkrf.co2$timestamp, format = "%Y-%m-%d %H:%M:%S")
	mkrf.co2$record <- NULL
	mkrf.co2$ctd2_depth <- mkrf.co2$ctd2_depth - 240
	mkrf.co2$q.salt <- q.salt(mkrf.co2$ctd2_depth)
	mkrf.co2$v.salt <- v.salt(mkrf.co2$ctd2_depth)
	mkrf.co2$t.salt <- t.salt(mkrf.co2$ctd2_depth)
	
	V <- mean(mkrf.co2$v.salt)
	S <- 0.236
	D <- mean(mean(c(mkrf.co2$ctd1_depth, mkrf.co2$ctd2_depth)))
	Q <- mean(mkrf.co2$q.salt)
	
	D <- D * 0.001
	Q <- Q * 0.001
	
	Fr <- V/((9.81 * D) ^ (0.5))
	
	k1 <- (V * S) ^ 0.89 * D ^ 0.54 * 5037
	
	k2 <- 5937 * (1 - 2.54 * Fr ^ 2) * (V * S) ^ 0.89 * D ^ 0.58
	
	k3 <- 1162 * S ^ 0.77 * V ^ 0.85 
	
	k4 <- (V * S) ^ 0.76 * 951.5
	
	k5 <- V * S * 2841 + 2.02
	
	k6 <- 929 * (V * S) ^ 0.75 * Q ^ 0.011
	
	k7 <- 4725 * (V * S) ^ 0.86 * Q ^ -0.14 * D ^ 0.66
	
	models <- c(k1, k2, k3, k4, k5, k6, k7)
	
	model <- mean(models)
	#model <- k7
	modelsd <- sd(models)
	
	dt <- as.POSIXct(mkrf.co2[1,1], format = "%Y-%m-%d %H:%M:%S")
	meantemp <- mean(c(mkrf.co2$gs3_1_temp, mkrf.co2$gs3_2_temp))
	meanq <- mean(mkrf.co2$q.salt)
	meanv <- mean(mkrf.co2$v.salt)
	
	raymondnew.df <- cbind.data.frame(dt, model, modelsd, meantemp, meanq, meanv)
	#raymondnew.df <- cbind.data.frame(dt, model, meantemp, meanq, meanv)
	return(raymondnew.df)
}
