# Live coding part I

library(lavaan)
library(ggplot2)
library(AICcmodavg)
library(patchwork)
library(ggdag)

# simulate a dataset with known causal structure (make drawing)
set.seed(2397348)
N <- 50
dat <- data.frame(x1 = rnorm(N))
dat$x2 = 0.9 * dat$x1 + rnorm(N)
dat$x3 = -0.5 * dat$x2 + rnorm(N)
dat$y = 1.7 * dat$x1 + 0.8 * dat$x2 + rnorm(N) 

dagify(y ~ x2 + x1,
       x2 ~ x1,
       x3 ~ x2) %>% 
  ggdag() 

p1 <- ggplot(data=dat, aes(x=x1,y=y)) + geom_point() + stat_smooth(method="lm")
p2 <- ggplot(data=dat, aes(x=x2,y=y)) + geom_point() + stat_smooth(method="lm")
p3 <- ggplot(data=dat, aes(x=x3,y=y)) + geom_point() + stat_smooth(method="lm")

p1 + p2 +p3

# fit multiple regression

dagify(y ~ x1,
       y ~ x2,
       y ~ x3) %>% 
  ggdag() 


# with lm
summary(lm(y~x1+x2+x3, data=dat))

# same in lavaan
model <- '
y ~ x1 + x2 + x3'

fit <- sem(model, meanstructure=T, data=dat)
summary(fit)


# Fit first SEM
model2 <- '
y ~ x1 + x3
x2 ~ x1
x3 ~ x2
'

dagify(y ~ x1 + x3,
       x2 ~ x1,
       x3 ~ x2) %>% 
  ggdag() 

fit2 <- sem(model2, data=dat)
summary(fit2)

# which important paths have been omitted?
modindices(fit2)
subset(modindices(fit2), modindices(fit2)$mi > 3.84)

# let's include additional path (x2 on y)
model3 <- ' 
y ~ x1 + x2 + x3
x2 ~ x1 
x3 ~ x2
'

dagify(y ~ x1 + x2 + x3,
       x2 ~ x1,
       x3 ~ x2) %>% 
  ggdag() 


fit3 <- sem(model3, data=dat)
summary(fit3)

summary(fit3, fit.measures = T)

# model pruning
model_true <- ' 
y ~ x1 + x2 
x2 ~ x1
x3 ~ x2
'

dagify(y ~ x1 + x2,
       x2 ~ x1,
       x3 ~ x2) %>% 
  ggdag()

fit_true <- sem(model_true, data=dat)
summary(fit_true)

# any more paths to include?
subset(modindices(fit_true))

# compare nested fits
anova(fit2, fit_true)
aictab(list(fit2, fit_true), c("fit 2", "fit true"))

# look at the underlying representations
lavInspect(fit2, what = "observed")
lavInspect(fit2, what = "implied")
lavInspect(fit2, what = "resid")


model4 <- ' 
y ~ x1 + x2 
x2 ~ x1
x3 ~ x2

# setting covariance to zero
y ~~ 0*x3 
'

fit4 <- sem(model4, data=dat)
summary(fit4)

# compare nested fits
anova(fit3, fit4)
aictab(list(fit3, fit4), c("fit 3", "fit 4"))


model2_wrong <- ' 
y ~ x1 + x3 
x2 ~ x1
x3 ~ x2
y ~~ x3 
'

fit2_wrong <- sem(model2_wrong, data=dat)
summary(fit2_wrong, fit.measures = T)

# compare nested fits
anova(fit3, fit2_wrong)
aictab(list(fit3, fit2_wrong), c("fit 3", "fit 2 wrong"))






# Any questions?


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


fit1a <- sem(model1a, data=dat, se = "bootstrap", bootstrap = 100)
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
resid(fit1b, type="cor")


# interpretation

# table of parameter estimates (confidence intervals)
parameterEstimates(fit1b)
subset(parameterEstimates(fit1b, standardized=T), op == "~")

# get standardized estimates, SE and CIs
standardizedSolution(fit1b, type = "std.all")

# standardized parameters and R square values (std.lv vs std.all)
summary(fit1a, standardized=T, rsq=T)
