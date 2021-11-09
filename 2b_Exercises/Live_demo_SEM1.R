# Live coding part I

library(lavaan)
library(ggplot2)
library(AICcmodavg)

# simulate a dataset with known causal structure (make drawing)
set.seed(2397348)
N <- 50
dat <- data.frame(x1 = runif(N))
dat$x2 = 0.9 * dat$x1 + runif(N)
dat$x3 = -0.5 * dat$x2 + runif(N)
dat$y = 1.7 * dat$x1 + 0.8 * dat$x2 + runif(N) 

ggplot(data=dat, aes(x=x1,y=y)) + geom_point() + stat_smooth(method="lm")
ggplot(data=dat, aes(x=x2,y=y)) + geom_point() + stat_smooth(method="lm")
ggplot(data=dat, aes(x=x3,y=y)) + geom_point() + stat_smooth(method="lm")

# fit multiple regression

# with lm
summary(lm(y~x1+x2+x3, data=dat))

# in lavaan
model <- ' 
y ~ x1 + x2 + x3
'
fit <- sem(model, data=dat)
summary(fit, rsq=T)


# Fit first SEM
model2 <- ' 
y ~ x1 + x3
x2 ~ x1
x3 ~ x2
'
fit2 <- sem(model2, data=dat)
summary(fit2)

# which important paths have been omitted?
modindices(fit2)
subset(modindices(fit2), modindices(fit2)$mi > 3.84)

# let's include additional path
model3 <- ' 
y ~ x1 + x2 + x3
x2 ~ x1 
x3 ~ x2
'

fit3 <- sem(model3, data=dat)
summary(fit3)

summary(fit3, fit.measures = T)

# model pruning
model_true <- ' 
y ~ x1 + x2 
x2 ~ x1
x3 ~ x2
'

fit_true <- sem(model_true, data=dat)
summary(fit_true, fit.measures = T)

# any more paths to include?
subset(modindices(fit_true))

# compare nested fits
anova(fit2, fit_true)
aictab(list(fit2, fit_true), c("fit 2", "fit true"))


model_true <- ' 
y ~ x1 + x2 
x2 ~ x1
x3 ~ x2

# setting covariance to zero
y ~~ 0*x3 
'

fit3 <- sem(model_true, data=dat)
summary(fit3, fit.measures = T)

# compare nested fits
anova(fit3, fit_true)
aictab(list(fit3, fit_true), c("fit 3", "fit true"))


# Parameter labelling and derived quantities
model1a <- ' 
y ~ b1 * x1 +  b2 * x2 
x2 ~ b3 * x1
x3 ~ b4 * x2


dir.x1.y := b1
indir.x1.y := b3 * b2
tot.x1.y := b3 * b2 + b1 
'

fit1a <- sem(model1a, data=dat)
summary(fit1a, fit.measures = F, standardized=T)


# Fixing parameters

model1b <- ' 
y ~ b1 * x1 +  b2 * x2 
x2 ~ b3 * x1
x3 ~ b4 * x2

b3 == 0

dir.x1.y := b1
indir.x1.y := b3 * b2
tot.x1.y := b3 * b2 + b1 
'

fit1b <- sem(model1b, data=dat)
summary(fit1b, fit.measures = F, standardized=T)

# diagnostics
resid(fit1, type="cor")


# interpretation

# table of parameter estimates (confidence intervals)
parameterEstimates(fit1)
subset(parameterEstimates(fit1, standardized=T), op == "~")

# get standardized estimates, SE and CIs
standardizedSolution(fit1, type = "std.all")

# standardized parameters and R square values (std.lv vs std.all)
summary(fit1, standardized=T, rsq=T)
