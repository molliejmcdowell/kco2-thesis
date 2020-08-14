# function for calculating discharge from using dry injection method (Hudson and Fraser, 2005)
# ec1 and ec2 are objects containing EC values from upstream and downstream sensors (or columns from dataframe)
# salt_g is the mass of salt injected in grams
# the denominator is in units milligram-seconds per liter, which is equal to g s/m3
# Q is in L/s

saltinject <- function (ec2, salt_g)
{
	delta_ts <- 5 # logging interval in seconds (5s)
	calk <- 0.486 # calibration constant from Richardson et al unpublished
	
	bg2 <- ec2[1] # the first EC value (background)
	ec2_rel <- ec2 - bg2 # difference between all values and background value
	q <- salt_g / (calk * delta_ts * sum(ec2_rel))

	q <- q * 1000
	q
}