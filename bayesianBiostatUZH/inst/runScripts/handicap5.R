alpha = 0.05 #size
beta = 0.1 #90% power
gamma = 0.05 #probability that a treatment effect is as larg as althernative hypothesis
n0_n<-( qnorm(1-gamma)/(qnorm(1-alpha/2)+qnorm(1-beta)) )^2
n0_n
