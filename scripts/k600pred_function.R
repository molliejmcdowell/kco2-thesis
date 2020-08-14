k600pred <- function(dat){
	mkrf.30 <- dat
	q <- mkrf.30$q.salt
	k600 <- exp(2.8111 + 0.0092 * q + 0.48/2)
	vark600 <- exp(2*0.48 + 2*(2.8111 + 0.0092 * q)) - exp(0.48 + 2*(2.8111 + 0.0092 * q))
	k600pred <- cbind.data.frame(k600,vark600)
	return(k600pred)
}