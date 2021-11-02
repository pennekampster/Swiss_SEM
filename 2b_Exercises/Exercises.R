# Live coding part I

library(lavaan)
library(visreg)
library(ggplot2)
library(AICcmodavg)

set.seed(2397348)
N <- 50
dat <- data.frame(x1 = runif(N))
dat$x2 = 0.9 * dat$x1 + runif(N)
dat$x3 = -0.5 * dat$x2 + runif(N)
dat$y = 1.7 * dat$x1 + 0.8 * dat$x2 + runif(N) # + 0.9 * (dat$x1 * dat$x2) + runif(N)
#dat$x1x2 = dat$x1 * dat$x2

ggplot(data=dat, aes(x=x1,y=y)) + geom_point() + stat_smooth(method="lm")
ggplot(data=dat, aes(x=x2,y=y)) + geom_point() + stat_smooth(method="lm")
ggplot(data=dat, aes(x=x3,y=y)) + geom_point() + stat_smooth(method="lm")

summary(lm(y~x1+x2+x3, data=dat))
visreg(lm(y~x1+x2+x3, data=dat), partial =T, gg=TRUE)

# fit multiple regressions
model <- ' 
y ~ x1 + x2 + x3
'

fit <- sem(model, data=dat)
summary(fit)


# Fit SEMs
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

# model pruning?
model_true <- ' 
y ~ x1 + x2 
x2 ~ x1
x3 ~ x2

# setting covariance to zero
y ~~ 0*x3 
'

fit1 <- sem(model_true, data=dat)
summary(fit1, fit.measures = T)

# any more paths to include?
subset(modindices(fit1), mi > 3.84)

# compare nested fits
anova(fit1, fit3)
aictab(list(fit1, fit3))


model_true <- ' 
y ~ x1 + x2 
x2 ~ x1
x3 ~ x2

# setting covariance to zero
y ~~ 0*x3 
'

fit1 <- sem(model_true, data=dat)
summary(fit1, fit.measures = T)

# compare nested fits
anova(fit1, fit3)
aictab(list(fit1, fit3))


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








# interaction

dat$y = 1.7 * dat$x1 + 0.8 * dat$x2 + 0.9 * (dat$x1 * dat$x2) + runif(N)
dat$x1x2 = dat$x1 * dat$x2

lm(y ~ x1 +  x2 + x1x2, data=dat)

model2a <- ' 
y ~ comp.int 
x2 ~ x1
x3 ~ x2

comp.int <~ 1.5745 * x1 +  0.7477 * x2 + 0.9668 * x1x2
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

model2c <- ' 
y ~ b1 * x1 +  b2 * x2 + b3 * x1x2 
x2 ~ b4 * x1
x3 ~ b5 * x2


ind.x1.y := b4 * b2
ind.x1.x3 := b5 * b2

tot.x1.y := b4 * b2 + b1 
'

fit2c <- sem(model2c, data=dat)
summary(fit2c, fit.measures = TRUE, standardized=T)
modindices(fit2c)


aictab(list(fit2a, fit2b, fit2c))










# Exercise: derived quantities
#
# Add variables that calculate the total, direct and indirect effect of each variable






# First exercise after fitting first SEM:

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


# Exercise to construct latent variables
library(faux)

# Single indicator variable

set.seed(6553454)
dat <- rnorm_multi(n = 50, 
                   mu = c(10, 10, 10),
                   sd = c(1, 1, 1),
                   r = c(0.9), 
                   varnames = c("M1", "M2", "y"),
                   empirical = F)

pairs(dat)


N <- 50
dat <- data.frame(M1 = runif(N))
dat$M2 = dat$M1 + runif(N)
dat$Xi = 0.9 * dat$M1 + 0.9 * dat$M2
dat$y <- dat$Xi + runif(N)


cfa <- "y ~ M1"
fit <- cfa(cfa, data=dat)
summary(fit, standardized=T, rsq=T)


latent <- '
xi =~ lambda*M1 + lambda*M2 # exogenous latent

eta =~ y # endogenous latent

eta ~ xi # path model

'
fit <- cfa(latent, data=dat)
summary(fit, standardized=T, rsq=T)




# Multiple indicator variables
set.seed(72643276)
dat <- rnorm_multi(n = 100, 
                   mu = c(10, 20, 20),
                   sd = c(4, 5, 5),
                   r = c(0.4, 0.1, 0.3), 
                   varnames = c("length", "width", "mass"),
                   empirical = F)

pairs(dat)

cfa <- "body_size =~ length + width + mass"
fit <- cfa(cfa, data=dat)
summary(fit, standardized=T, rsq=T)


# Two factor latent variable
dat$length_std <- (mean(dat$length)-dat$length) / sd(dat$length)
dat$width_std <- (mean(dat$width)-dat$width) / sd(dat$width)

cfa <- "body_size =~ lambda*length_std + lambda*width_std"
fit <- cfa(cfa, data=dat)
summary(fit, standardized=T, rsq=T)


# Exercise 2 indicator latent variables
cor.test(seabloom$precip.gs, seabloom$precip.mm)
plot(seabloom$precip.gs, seabloom$precip.mm)

seabloom$precip.mm_std <- (mean(seabloom$precip.mm)-seabloom$precip.mm) / sd(seabloom$precip.mm)
seabloom$precip.gs_std <- (mean(seabloom$precip.gs)-seabloom$precip.gs) / sd(seabloom$precip.gs)



lv <-"
# Latent variable definition
climate =~ lambda*precip.mm_std + lambda*precip.gs_std
"

fit.lv <- cfa(lv, data = seabloom, estimator = "MLM")
summary(fit.lv, standardized=T)
modindices(fit.lv, minimum.value = 3.84)

lv <-"
# Latent variable definition
climate =~ lambda*precip.mm_std + lambda*precip.gs_std

mass.above ~ nadd + disk + climate
rich ~ nadd + climate + disk
even ~ nadd + climate + disk"

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv, standardized=T)
modindices(fit.lv, minimum.value = 3.84)





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



