# Live demo latent and composite variables
library(faux)
library(lavaan)
library(AICcmodavg)

# Accounting for measurement error with 2 indicator variables

# let's generate some data, where the two measurements are correlated (make a drawing)
set.seed(6553454)
dat <- rnorm_multi(n = 50, 
                   mu = c(10, 10),
                   sd = c(1, 1),
                   r = c(0.9), 
                   varnames = c("M1", "M2"),
                   empirical = F)
dat$Xi = 0.3 * dat$M1 + 0.3 * dat$M2
dat$y <- dat$Xi + runif(50)

pairs(dat)
cor.test(dat$M1, dat$M2)

# direct effect of M1 on y (check R2)
cfa <- "y ~ M1"
fit <- cfa(cfa, data=dat)
summary(fit, standardized=T, rsq=T)

# effect of M1 and M2 on y (check R2)
latent <- '
xi =~ lambda*M1 + lambda*M2 # exogenous latent

eta =~ y # endogenous latent

eta ~ xi # path model
'

fit <- cfa(latent, data=dat)
summary(fit, standardized=T, rsq=T)


# example CFA with more than two indicator variables
set.seed(72643276)
dat <- rnorm_multi(n = 100, 
                   mu = c(20, 20, 20),
                   sd = c(5, 5, 5),
                   r = c(0.4, 0.5, 0.3), 
                   varnames = c("length", "width", "mass"),
                   empirical = F)

pairs(dat)

cfa <- "body_size =~ mass + width + length"
fit <- cfa(cfa, data=dat)
summary(fit, standardized=T, rsq=T)



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
summary(fit2c, fit.measures = TRUE, standardized=T)
modindices(fit2c)


# Multigroup fitting: interaction

# let's generate some data (make drawing)
set.seed(2397348)
N <- 100
dat <- data.frame(x1 = rnorm(N))
dat$group = rep(c("1","2"), each = N/2)
dat$y <- ifelse(dat$group == "1", 0.5 * dat$x1, -0.7 * dat$x1) + rnorm(N)

# no group
summary(lm(y~x1+group, data=dat))

model3a <- ' 
y ~ x1
'

fit3a <- sem(model3a, group = "group", data=dat)
summary(fit3a, fit.measures=T)





