efflux <- function(dat){
	source("scripts/kpred_function.R")
	#dat <- kcompare
	mkrf.30 <- dat
	co2aq <- dat$co2_1
	Tw <- dat$gs3_1_temp
	co2air <- 427
	
	k <- dat$k
	
	Tw.K <- Tw + 273.15
	T_K <- 273.15 + 25 # temp in K at STP
	Kh.CO2.stp <- 0.035
	T_depend.CO2 <- 2400 #temperature dependency constant for CO2
	Kh.CO2 <- Kh.CO2.stp*exp(T_depend.CO2*(1/Tw.K - 1/T_K))
	flux <- k*(co2aq - co2air)*Kh.CO2*12*1000*1e-6
}