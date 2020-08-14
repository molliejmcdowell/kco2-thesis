k600 <- function(k.m.d, Tw.mean) {
	# First, calculate schmidt number for gas of interest
	# a, b, c, d are constants
	# also need water temp
	
	a <- 1742
	b <- - 91.24
	c <- 2.208
	d <- - 0.0219
	t <- Tw.mean
	sc <- a + b*t + c*t^2 + d*t^3
	
	# Next, calculate k600 from schmidt number and k
	k600 <- (600/sc)^(-0.5)*k.m.d
	k600
}