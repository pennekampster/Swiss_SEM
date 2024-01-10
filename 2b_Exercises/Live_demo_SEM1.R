# Live coding part I

library(lavaan)
library(ggplot2)
library(AICcmodavg)
library(patchwork)
library(ggdag)

# simulate a dataset with known causal structure (make drawing)
set.seed(2397348)
N <- 100
dat <- data.frame(x1 = rnorm(N))
dat$x2 = 0.2 * dat$x1 + rnorm(N)
dat$x3 = 0.3 * dat$x2 + rnorm(N)
dat$y = .7 * dat$x2 + 0.8 * dat$x3 + rnorm(N) 

dagify(y ~ x2 + x3,
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


# Summarize the results
# no direct effect of X1, but we know there is an effect mediated by X2 and X3 on Y
# Let's fit a SEM instead


# Fit first SEM
model2 <- '
y ~ x1 + x3
x2 ~ x1
x3 ~ x2
'

dagify(y ~ x1 + x3,
       x2 ~ x1,
       x3 ~ x1) %>% 
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
       x3 ~ x1) %>% 
  ggdag() 


fit3 <- sem(model3, data=dat)
summary(fit3)

# what do the other fit metrics say
summary(fit3, fit.measures = T)

# any more paths to include?
subset(modindices(fit_true))




# after a fitting model is found, let's do some pruning
model_true <- ' 
y ~ x2 + x3 
x2 ~ x1
x3 ~ x2
'

dagify(y ~ x2 + x3,
       x2 ~ x1,
       x3 ~ x2) %>% 
  ggdag()

fit_true <- sem(model_true, data=dat)
summary(fit_true)


# compare fits
aictab(list(fit2, fit_true), c("fit 2", "fit true"))
aictab(list(fit3, fit_true), c("fit 3", "fit true"))

# look at the underlying representations
lavInspect(fit2, what = "observed")
lavInspect(fit2, what = "implied")
lavInspect(fit2, what = "resid")


model2_wrong <- ' 
y ~ x2 + x3 
x2 ~ x1
x3 ~ x2
y ~~ x1 
'

fit2_wrong <- sem(model2_wrong, data=dat)
summary(fit2_wrong)

# compare fits
aictab(list(fit_true, fit2_wrong), c("fit true", "fit wrong"))






# Any questions?


# Parameter labelling and derived quantities
model1a <- ' 
y ~ b1 * x2 +  b2 * x3 
x2 ~ b3 * x1
x3 ~ b4 * x2


dir.x2.y := b1
indir.x2.y := b4 * b2
tot.x2.y := b4 * b2 + b1 
'

fit1a <- sem(model1a, data=dat)
summary(fit1a, fit.measures = F, standardized=T)


fit1a <- sem(model1a, data=dat, se = "bootstrap", bootstrap = 100)
summary(fit1a, fit.measures = F, standardized=T)


# Fixing parameters

model1b <- ' 
y ~ b1 * x2 +  b2 * x3 
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
