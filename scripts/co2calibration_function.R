co2calibrate <- function(CO2.1, CO2.2, Tw.1, Tw.2, d.1, d.2) {
	Ct.1 <- 0.003*CO2.1*(25-Tw.1)
	Ct.2 <- 0.003*CO2.2*(25-Tw.2)
	
	Pw.1 <- 9.81/(d.1/10)
	Pw.2 <- 9.81/(d.2/10)
	
	Cp.1 <- 0.015*CO2.1*Pw.1
	Cp.2 <- 0.015*CO2.2*Pw.2
	
	co2_1 <- CO2.1 - Ct.1 - Cp.1
	co2_2 <- CO2.2 - Ct.2 - Cp.2
	
	result <- cbind(co2_1,co2_2)
	return(result)
}