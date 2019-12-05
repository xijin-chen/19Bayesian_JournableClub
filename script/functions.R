# Returns the beta parameters a,b from given mean m and sd s
beta.parameters <- function(m,s){
	sum.ab <- m*(1-m)/s^2-1
	a <- m*(sum.ab)
	b <- sum.ab-a
	return(c(a,b))
}

# Returns mean, median and mode of a Beta[a,b] distribution
beta.mean.median.mode <- function(a,b){ 
	mean <- a/(a+b)
	median <- qbeta(0.5, a, b)
	mode <- (a-1)/(a+b-2)
	return(c(mean, median, mode))
}