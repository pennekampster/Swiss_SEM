# Live demo latent and composite variables
library(faux)

# Accounting for measurement error with 2 indicator variables

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


# More than two indicator variables
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
set.seed(2397348)
N <- 50
dat <- data.frame(x1 = runif(N))
dat$x2 = 0.9 * dat$x1 + runif(N)
dat$x3 = -0.5 * dat$x2 + runif(N)
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