#---------------------------------------------------------------------------------------------------------------
# Code for example 3.14 in Spiegelhalter 2006
# Date: 2019/05/27
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES

#-------------------------------------------------


## True distribution
## -----------------
set.seed(2019)
par(mfrow = c(1, 4))
x <-  0:10
p <- dbinom(x, 10, 0.5)


## Simulations
## -----------
# Sample with 100 throws
sample.100 <- replicate(100, sample(c(1, 0), 10, prob = c(0.5, 0.5),
																		replace = TRUE))  # head = 1
no.head.100 <- apply(sample.100, 2, sum)

# Sample with 1000 throws
sample.1000 <- replicate(1000, sample(c(1, 0), 10, prob = c(0.5, 0.5),
												 replace = TRUE))  # head = 1
no.head.1000 <- apply(sample.1000, 2, sum)

# Sample with 10000 throws
sample.10000 <- replicate(10000, sample(c(1, 0), 10, prob = c(0.5, 0.5),
													replace = TRUE))  # head = 1
no.head.10000 <- apply(sample.10000, 2, sum)


## Plot
## ----
# True distribution
par(mfrow = c(1, 4))
plot(x, p,
	type = "h",
	main = "True distribution",
	xlab = "number of heads",
	ylab = "probability",
	frame.plot = FALSE)

hist(no.head.100,
	probability = T,
	main = "100 throws",
	xlab = "number of heads",
	ylab = "probability")

hist(no.head.1000,
	probability = T,
	main = "1000 throws",
	xlab = "number of heads",
	ylab = "probability")

hist(no.head.10000,
	probability = T,
	main = "10000 throws",
	xlab = "number of heads",
	ylab = "probability")
