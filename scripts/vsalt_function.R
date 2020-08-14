v.salt <- function(d){
	v.salt <- 0.0009 * d - 0.0357
	#v.salt <- v.salt / 1000
	return(v.salt)
}