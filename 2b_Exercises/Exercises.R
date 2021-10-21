# First exercise after fitting first SEM:

set.seed(2397348)
dat <- data.frame(x1 = runif(50), x3 = runif(50))
dat$x2 = 0.9 * dat$x1 + runif(50)
dat$x3 = 0.5 * dat$x2 + runif(50)
dat$y = 0.8 * dat$x2 + 1.7 * dat$x1 + 0.9 * (dat$x1 * dat$x2) + runif(50)
dat$x1x2 = dat$x1 * dat$x2

library(lavaan)

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


model2 <- ' 
y ~ comp.int 
x3 ~ x2

comp.int <~ 1 * x1 +  x2 + x1x2

'

fit2 <- sem(model2, data=dat)
summary(fit2, fit.measures = TRUE, standardized=T)
modindices(fit2)


AIC(fit1, fit2)




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



