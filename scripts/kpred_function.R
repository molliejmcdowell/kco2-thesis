kpred <- function(dat){
	mkrf.30 <- dat
	q <- mkrf.30$q.salt
	#q <- ifelse(!is.na(q), q, mean(na.omit(q)))
	k <- exp(2.6276 + 0.0084 * q + 0.4/2)
	vark <- exp(0.8 + 2*(2.6276 + 0.0084 * q)) - exp(0.4 + 2*(2.6276 + 0.0084 * q))
	kpred <- cbind.data.frame(k,vark)
	return(kpred)
}