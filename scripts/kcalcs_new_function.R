kcalcs.new <- function(dat){
	source("scripts/kco2_function.R")
	source("scripts/k600_function.R")
	source("scripts/co2calibration_function.R")
	
	mkrf.co2 <- dat
	#mkrf.co2 <- read.csv(file.choose())
	mkrf.co2$timestamp <- as.POSIXct(mkrf.co2$timestamp, format = "%Y-%m-%d %H:%M:%S")
	mkrf.co2$record <- NULL
	mkrf.co2$ctd2_depth <- mkrf.co2$ctd2_depth - 240
	mkrf.co2$q.salt <- q.salt(mkrf.co2$ctd2_depth)
	mkrf.co2$v.salt <- v.salt(mkrf.co2$ctd2_depth)
	mkrf.co2$t.salt <- t.salt(mkrf.co2$ctd2_depth)
	
	co2.cal <- co2calibrate(mkrf.co2$co2_1,mkrf.co2$co2_2,mkrf.co2$gs3_1_temp,
		mkrf.co2$gs3_2_temp,mkrf.co2$ctd1_depth,mkrf.co2$ctd2_depth)
	co2.cal <- as.data.frame(co2.cal)
	
	mkrf.co2$co2_1 <- co2.cal$co2_1
	mkrf.co2$co2_2 <- co2.cal$co2_2
	
	co2_1.sort <- mkrf.co2[order(mkrf.co2$co2_1), ]
	min.co2_1 <- co2_1.sort[1:10, ]
	min.co2_1 <- mean(min.co2_1$co2_1)
	
	co2_2.sort <- mkrf.co2[order(mkrf.co2$co2_2), ]
	min.co2_2 <- co2_2.sort[1:10, ]
	min.co2_2 <- mean(min.co2_2$co2_2)
	
	co2_1.sort <- mkrf.co2[order(mkrf.co2$co2_1, decreasing = TRUE), ]
	max.co2_1 <- co2_1.sort[1:100, ]
	max.co2_1 <- mean(max.co2_1$co2_1)
	
	co2_2.sort <- mkrf.co2[order(mkrf.co2$co2_2, decreasing = TRUE), ]
	max.co2_2 <- co2_2.sort[1:100, ]
	max.co2_2 <- mean(max.co2_2$co2_2)
	
	co2_1.rel <- max.co2_1 - min.co2_1
	co2_2.rel <- max.co2_2 - min.co2_2
	
	co2_1.rel.hi <- co2_1.rel + co2_1.rel*0.02
	co2_1.rel.lo <- co2_1.rel - co2_1.rel*0.02
	co2_2.rel.hi <- co2_2.rel + co2_2.rel*0.02
	co2_2.rel.lo <- co2_2.rel - co2_2.rel*0.02
	
	CO2.1 <- co2_1.rel
	CO2.2 <- co2_2.rel
	Tw.1 <- mean(mkrf.co2$gs3_1_temp)
	Tw.2 <- mean(mkrf.co2$gs3_2_temp)
	t <- mean(mkrf.co2$t.salt)
	d <- mean(c(mkrf.co2$ctd1_depth, mkrf.co2$ctd2_depth))
	Q.L.s <- mean(mkrf.co2$q.salt)
	SA <- 4.26*31
	
	k.m.d <- kco2(CO2.1, CO2.2, Tw.1, Tw.2, t, d, Q.L.s, SA)
	
	k.hi <- kco2(co2_1.rel.hi, co2_2.rel.lo, Tw.1, Tw.2, t, d, Q.L.s, SA)
	k.lo <- kco2(co2_1.rel.lo, co2_2.rel.hi, Tw.1, Tw.2, t, d, Q.L.s, SA)
	
	Tw.mean <- (Tw.1 + Tw.2)/2
	
	k600.m.d <- k600(k.m.d, Tw.mean)
	k600.hi <- k600(k.hi, Tw.mean)
	k600.lo <- k600(k.lo, Tw.mean)
	
	dt <- as.POSIXct(mkrf.co2[1,1], format = "%Y-%m-%d %H:%M:%S")
	meantemp <- mean(c(mkrf.co2$gs3_1_temp, mkrf.co2$gs3_2_temp))
	meanq <- mean(mkrf.co2$q.salt)
	meanv <- mean(mkrf.co2$v.salt)
	
	k.df <- cbind.data.frame(dt, k.m.d, k600.m.d, meantemp, meanq, meanv, 
		k.hi, k.lo, k600.hi, k600.lo, CO2.1, CO2.2, t, d)
	return(k.df)
}