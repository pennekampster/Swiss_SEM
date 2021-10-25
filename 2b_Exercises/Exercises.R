# First exercise after fitting first SEM:

library(lavaan)
library(visreg)
library(ggplot2)
library(AICcmodavg)

set.seed(2397348)
N <- 500
dat <- data.frame(x1 = runif(N))
dat$x2 = 0.9 * dat$x1 + runif(N)
dat$x3 = 0.5 * dat$x2 + runif(N)
dat$y = 1.7 * dat$x1 +0.8 * dat$x2 + 0.9 * (dat$x1 * dat$x2) + runif(N)
dat$x1x2 = dat$x1 * dat$x2

ggplot(data=dat, aes(x=x1,y=y)) + geom_point() + stat_smooth(method="lm")
ggplot(data=dat, aes(x=x2,y=y)) + geom_point() + stat_smooth(method="lm")
ggplot(data=dat, aes(x=x3,y=y)) + geom_point() + stat_smooth(method="lm")

summary(lm(y~x1+x2+x3, data=dat))
visreg(lm(y~x1+x2+x3, data=dat), partial =T, gg=TRUE)

model <- ' 
y ~ x1 + x2 + x3
'

fit <- sem(model, data=dat)
summary(fit, fit.measures = TRUE, standardized=T)
modindices(fit)

model1 <- ' 
y ~ x1 + x2 
x2 ~ x1
x3 ~ x2
'

fit1 <- sem(model1, data=dat)
summary(fit1, fit.measures = TRUE, standardized=T)
modindices(fit1)

aictab(list(fit, fit1))


lm(y ~ x1 +  x2 + x1x2, data=dat)

model2a <- ' 
y ~ comp.int 
x2 ~ x1
x3 ~ x2

comp.int <~ 1.6750 * x1 +  0.7345 * x2 + 1.0067 * x1x2

'

fit2a <- sem(model2a, data=dat)
summary(fit2a, fit.measures = TRUE, standardized=T)
modindices(fit2a)

model2b <- ' 
y ~ comp.int 
x2 ~ x1
x3 ~ x2

comp.int <~ 1 * x1 +  x2 + x1x2

'

fit2b <- sem(model2b, data=dat)
summary(fit2b, fit.measures = TRUE, standardized=T)
modindices(fit2b)


aictab(list(fit2a, fit2b))




# Let's test whether the effect of disturbance is mediated via its
# effect on richness and eveness, rather than directly on biomass
# add paths from disk to rich and even, remove the path to mass.above
# compare model fit to simple model
# what do you conclude?

modindices(fit.simple.up, minimum.value = 0.01)

simple.exc <-
"mass.above ~ nadd + rich + even + precip.mm 
rich ~ nadd + precip.mm + disk
even ~ nadd + precip.mm +  disk
rich ~~ even"

fit.simple.exc <- sem(simple.exc, data = seabloom, estimator = "MLM")
summary(fit.simple.exc, fit.measures = TRUE, rsq = TRUE)
modindices(fit.simple.exc, minimum.value = 0.01)

AIC(fit.simple.up, fit.simple.exc)



# Exercise to construct composite variables

# Composite variables are convenient to model nonlinear relationships

ggplot(data=seabloom, aes(precip.gs, mass.above)) + geom_point() + 
  stat_smooth(method="lm", formula = "y ~ poly(x, 2)", se=F) + 
  stat_smooth(method="lm", formula = "y ~ x", colour="red")


comp2 <- "
landuse2 <~  0.06544565 * nadd + 0.4466863 * disk

rich ~ landuse2 + precip.mm
even ~ landuse2 + precip.mm

mass.above ~ landuse2 + rich + even + precip.mm

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom)
summary(fit.comp2, standardize = T)



comp2 <- "
landuse2 <~   nadd + 1 * disk

rich ~ landuse2 + precip.mm
even ~ landuse2 + precip.mm

mass.above ~ landuse2 + rich + even + precip.mm

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom, )
summary(fit.comp2, standardize=T)



