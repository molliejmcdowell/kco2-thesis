# Henry's Law -- Most clear formulas for computing are a combination of 
# -- the Henry's compilation by Rolf Sander (see henry.pdf)
# -- the pages from NIST (http://webbook.nist.gov/cgi/cbook.cgi?ID=C124389&Mask=10 for CO2)
# -- the pages from NIST (http://webbook.nist.gov/cgi/cbook.cgi?ID=C7782447&Mask=10#Solubility for O2)
## Now using from NIST -- this differs only by 2% from the above, and is much cleaner
## Note that these are from Lide and Frederikse (1995), CRC Handbook of Chemistry and Physics, 76th Edition, D. R. Lide and H. P. R. Frederikse, ed(s)., CRC Press, Inc., Boca Raton, FL, 1995.

# for formula: need following as inputs:
# Tw upstream (Tw.1)
# Tw downstream (Tw.2)
# CO2.1 upstream in ppm (CO2.1) during injection
# CO2.2 downstream in ppm (CO2.2) during injection
# CO2.1.bg  = upstream CO2 prior to injection
# CO2.2.bg  = downstream CO2 prior to injection
# t = travel time in s between centroids of salt slug injection
# d = average depth in mm

#*** Additional parameters needed
# SA = surface area of reach in m2
# Q.L.s = discharge in L per sec

kco2 <- function(CO2.1, CO2.2, Tw.1, Tw.2, t, d, Q.L.s, SA) {
	Tw.1.K <- Tw.1 + 273.15
	Tw.2.K <- Tw.2 + 273.15
	T_K <- 273.15 + 25 # temp in K at STP
	Kh.CO2.stp <- 0.035
	T_depend.CO2 <- 2400 #temperature dependency constant for CO2
	Kh.CO2.1 <- Kh.CO2.stp*exp(T_depend.CO2*(1/Tw.1.K - 1/T_K))
	CO2.1.aq.mgC.L <- Kh.CO2.1 * CO2.1 * 1e-6 * 12 * 1e3 # 1e-6 to go from ppm to atm; 12 to go from mol CO2 to g of C; 1e3 to put in mgC
	
	Kh.CO2.2 <- Kh.CO2.stp*exp(T_depend.CO2*(1/Tw.2.K - 1/T_K))
	CO2.2.aq.mgC.L <- Kh.CO2.2 * CO2.2 * 1e-6 * 12 * 1e3 # 1e-6 to go from ppm to atm; 12 to go from mol CO2 to g of C; 1e3 to put in mgC
	
	CO2.mean <- (CO2.1+CO2.2)/2
	Tw.mean <- (Tw.1 + Tw.2)/2
	Tw.mean.K <- Tw.mean + 273.15
	Kh.CO2.mean <- Kh.CO2.stp*exp(T_depend.CO2*(1/Tw.mean.K - 1/T_K))
	
	### Determine excess CO2(aq)
	CO2.air.atm <- 427 * 1e-6 # in atm  #global average CO2 in 2016
	CO2.aq.atm <- CO2.mean * 1e-6 # aqueous CO2 in atm
	delCO2.atm <- (CO2.aq.atm - CO2.air.atm)
	delCO2.aq.mol.L <- Kh.CO2.mean * delCO2.atm #excess CO2(aq) in mol CO2/L 
	delCO2.aq.mgC.L <- delCO2.aq.mol.L * 12 * 1e3 #excess CO2(aq) in mg CO2-C/L 
	
	### Compute evasion flux of CO2 in mgC/L
	### NOTE - not yet considering input from groundwater CO2 #### [this would be determined from CO2.1.bg - CO2.2.bg]
	fCO2.mgC.L <- (CO2.1.aq.mgC.L - CO2.2.aq.mgC.L) / t
	
	# flux of CO2 across water surface of reach, in mgC per m2 per sec
	fCO2.mg.m2.s <- (CO2.1.aq.mgC.L - CO2.2.aq.mgC.L) * Q.L.s / SA
	
	### compute k 
	k.m.s <- fCO2.mg.m2.s / delCO2.aq.mgC.L / 1000 # divide by 1000 to go from L to m3
	
	k.m.d <- k.m.s * 60 * 60 * 24
	k.m.d
}