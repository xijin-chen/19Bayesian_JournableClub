#---------------------------------------------------------------------------------------------------------------
# Code for example 5.5 in Spiegelhalter 2006
# Date: 2019/05/24
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)

# Continuation of the example 3.6 from the book
# mu = -0.26
# no = 236.7
# m = 30.5
# sigma = 2
# observed OR = 0.48 , log OR = -0.74

# First, we need to estimate parameters of the normal predictive distribution:
mu_pred_2<-predictiveNormal(-0.26,
                            2/sqrt(236.7), 2/sqrt(30.5))[1]
sd_pred_2<-predictiveNormal(-0.26,
                            2/sqrt(236.7), 2/sqrt(30.5))[2]
x<-seq(-1.5,0.8, by = 0.001)

plot(exp(x), dnorm(x, mu_pred_2, sd_pred_2),
     cex = 0.1,ylab = "", col = "blue",
     type = "l", lwd = 2, log = "x",
     xlab = "Predicted odds ratio of 30-days
     mortality on home therapy to control",
     main = "Predictive distribution")
polygon(c( exp(x)[exp(x)<=0.48], 0.48 ),
        c(dnorm(x, mu_pred_2, sd_pred_2)[exp(x)<=0.48],0.01 ), col="grey")
abline(v = 0.48, lwd =2)
text(0.35, 0.5, "observed OR = 0.48" )
text(0.4, 0.1, "Pbox/2" )

# Box measure
bm<-2*min(pnorm((-0.74 - mu_pred_2)/sd_pred_2),
          1-pnorm((-0.74 + mu_pred_2)/sd_pred_2))
# or using bayesianBiostatUZH package:
Boxstat (-0.74, mu_pred_2, sd_pred_2 )
