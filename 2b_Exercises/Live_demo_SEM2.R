# Live demo latent and composite variables
library(lavaan)
library(AICcmodavg)
library(faux)

## Latent variables

# Accounting for measurement error with 2 indicator variables
set.seed(1)
n = 100
x = rnorm(n)

eta1 = rnorm(n) # measurement error1
noisy1 = x + eta1

eta2 = rnorm(n) # measurement error2
noisy2 = x + eta2

u = rnorm(n)
beta0=0; beta1 = .25
y = beta0+beta1*x + u

dat <- data.frame(y, eta1, eta2, noisy1, noisy2)

# attenuation bias
lm <- '
y ~ noisy1 # exogenous latent
'
fit <- sem(lm, data=dat)
summary(fit, standardized=T)

# use latent variable to model underlying cause
latent <- '
xi =~ lambda*noisy1 + lambda*noisy2 # exogenous latent
y ~ xi # path model
'
fit <- sem(latent, data=dat)
summary(fit, standardized=T, rsq=T)


# fully latent model
latent <- '
xi =~ lambda*noisy1 + lambda*noisy2 # exogenous latent

eta =~ y # endogenous latent

eta ~ xi # path model
'

fit <- sem(latent, data=dat)
summary(fit, standardized=T, rsq=T)


# example CFA with more than two indicator variables
set.seed(72643276)
dat <- rnorm_multi(n = 100, 
                   mu = c(20, 20, 20),
                   sd = c(5, 5, 5),
                   r = c(0.4, 0.5, 0.7), 
                   varnames = c("length", "width", "mass"),
                   empirical = F)

pairs(dat)

cfa <- "body_size =~ mass + width + length"
fit <- cfa(cfa, data=dat)
summary(fit, standardized=T, rsq=T)





# Interactions

# Multigroup fitting: interaction

# let's generate some data (make drawing)
N <- 50
set.seed(2397348)
dat <- data.frame(x1 = rnorm(N))
dat$group = rep(c("1","2"), each = N/2)
dat$y <- ifelse(dat$group == "1", 0.2 * dat$x1, 0.9 * dat$x1) + rnorm(N, 0, .1)

# ANCOVA
summary(lm(y~x1 * group, data=dat))

model3a <- ' 
y ~ x1
'

fit3a <- sem(model3a, group = "group", data=dat)
summary(fit3a)


model3b <- ' 
y ~ c("b1", "b1") * x1
'

fit3b <- sem(model3b, group = "group", data=dat)
summary(fit3b, fit.measures=T)
fit3b
aictab(list(fit3a, fit3b))






# Composite variables: interaction

# let's generate some data (make drawing)
set.seed(2397348)
N <- 50
dat <- data.frame(x1 = rnorm(N))
dat$x2 = rnorm(N)
dat$x1x2 = dat$x1 * dat$x2
dat$y = 1.7 * dat$x1 + 0.8 * dat$x2 + -0.8 * dat$x1x2 + rnorm(N)

pairs(dat)

summary(lm(y ~ x1 * x2, data=dat))

# fit composite in lavaan (automatically)

model2a <- ' 
comp.int <~ 1 * x1 + x2 + x1x2 
y ~ comp.int 
'

fit2a <- sem(model2a, data=dat)
summary(fit2a, standardized=T)
modindices(fit2a)

# fit composite in lavaan (manually)

# get coefficients
summary(lm(y ~ x1 * x2, data=dat))
coef(lm(y ~ x1 * x2, data=dat))


model2b <- ' 
comp.int <~ 1.72075 * x1 +   0.43481 * x2 + -0.67734  * x1x2

y ~ comp.int
'

fit2b <- sem(model2b, data=dat)
summary(fit2b, standardized=T)

# compare output to show that factor loadings are the same when standardized

# fully manual model with interaction
model2c <- ' 
y ~ b1 * x1 +  b2 * x2 + b3 * x1x2 
'

fit2c <- sem(model2c, data=dat)
summary(fit2c, standardized=T)
modindices(fit2c)




