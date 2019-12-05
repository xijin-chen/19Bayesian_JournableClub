#---------------------------------------------------------------------------------------------------------------
# Code for example 5.3 in Spiegelhalter 2006
# Date: 2019/05/24
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)

#It is a continuation of the example 5.1
# We compare clinical prior (obtained in the example 5.1) with a sceptical prior

#First, we plot  clinical "prior" from example 5.1
plot(exp(seq(-1, 0.4, 0.01)),dnorm(seq(-1, 0.4, 0.01), mean=m1, sd=sd1), 
     col="red", lwd=2, type="l", log = "x", ylim = c(0,2.3), 
     ylab = " ", yaxt = "n", xlab = "HR")
abline(v = 0.73,lwd = 1, col = "red")
abline(v = 1,lwd = 1, col = "blue")
text(0.75, 1.75, expression(theta), cex = 2)
text(0.76, 1.65, expression(a), cex = 1)

#now we add sceptical prior (the idea is to put a small probabilty on the 
#values of parameter equal or bigger than value under alternative hypothesis)
theta_alter<-log(log(0.25)/log(0.15))
# in this example Theta_alternative = log(log(0.25)/log(0.15)) = log(0.73) = -0.31
# 1.65 means prior will show 5% chance of being less that value of parameter 
#under alternative hypothesis and can be calculated using qnorm(1-0.05)
points(exp(seq(-1,2, by = 0.01)), 
       dnorm(seq(-1,2, by = 0.01), 0, 
             sqrt(var_scept_prior(theta_alter, qnorm(1-0.05)))),
       col="blue", lwd = 2, type = "l", log = "x")
legend("topright", legend = c("clinical prior", "sceptical prior"), 
       col = c ("red", "blue"), lwd = 2, bty = "n")
