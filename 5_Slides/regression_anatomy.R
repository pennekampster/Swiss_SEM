# regression anatomy 
# take correlation input and get reg coefficients
# suggestions always welcome
# Fred Oswald - foswald@rice.edu - https://workforce.rice.edu
# 8/20/20

library(MASS)
library(lavaan)
rmtx <- matrix (c(1, .5, .4, .4,
                  .5, 1, .2, .5,
                  .4, .2, 1, .3,
                  .4, .5, .3, 1),nrow=4)
rownames(rmtx)<-colnames(rmtx)<- c("X1","X2","X3","Y")

# generate some data to fit the correlation matrix exactly*
x <- as.data.frame(mvrnorm(n=100,rep(0,4),rmtx,empirical=TRUE))

# do the path analysis three ways:

#1 path analysis using lavaan (and the correlation matrix)
pmodel <- 'Y ~ X1 + X2 + X3
           X1 ~~ X2
           X2 ~~ X3
           X1 ~~ X3'
pmobj1 <- sem(pmodel, sample.cov=rmtx, sample.nobs=100)
summary(pmobj1,standardized=TRUE)

#2 path analysis using lavaan (and the data)
pmobj2 <- sem(pmodel, data=x, sample.nobs=100)
summary(pmobj2,standardized=TRUE)

#3 ...regression using lm (and the data)
pmobj3 <- summary(lm(Y ~ X1 + X2 + X3, data=x))
round(pmobj3$coefficients[,1],3)
