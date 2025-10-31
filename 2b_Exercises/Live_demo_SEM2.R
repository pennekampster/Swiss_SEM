# Live demo latent and composite variables
library(lavaan)
library(AICcmodavg)
library(faux)

## Latent variables

# Accounting for measurement error using latent variables
set.seed(1)
n <- 100

# True relationship: y = 0.9*x + error
x_true <- rnorm(n)
y <- 0.9 * x_true + rnorm(n)

# But we measure x with error
measure1 <- x_true + rnorm(n)
measure2 <- x_true + rnorm(n)

dat <- data.frame(y, measure1, measure2)

# WRONG: Direct regression (biased toward 0)
summary(lm(y ~ measure1, data = dat))

# CORRECT: Latent variable (recovers true effect)
latent <- '
  x_true_latent =~ a*measure1 + a*measure2
  y ~ x_true_latent
'
summary(sem(latent, data = dat), standardized = T, rsq = T)

# Generate 2 correlated indicators of "body size"
set.seed(72643276)
dat <- rnorm_multi(
  n = 100, 
  mu = c(40, 80),              # length, mass
  sd = c(4, 5),              # CV = 0.25 for both
  r = 0.5,                     # correlation between them
  varnames = c("length", "mass"),
  empirical = FALSE
)

# Are indicators correlated?
pairs(dat)

# CFA with 2 indicators
cfa_model <- "body_size =~ length + mass"
fit <- cfa(cfa_model, data=dat)

# Note: Model has 0 degrees of freedom (just identified)
# This means we can't test model fit

# Force equal loadings (adds 1 df) to estimate parameters
# Assumption: The latent body size variable has an equal effect on both measurements. 
# Requires that both are equally reliable, i.e., are measured with the same precision."

cfa_equal <- "body_size =~ a*length + a*mass"

fit_equal <- cfa(cfa_equal, data=dat)
summary(fit_equal, standardized=T)



# CFA with more than two indicator variables
set.seed(72643276)
dat <- rnorm_multi(
  n = 100, 
  mu = c(40, 15, 80, 200),           # Similar scales
  sd = c(10, 3.75, 20, 50),          # CV = 0.25 for all
  r = c(0.5, 0.6, 0.5, 0.6, 0.5, 0.5),  # 6 correlations for 4 vars
  varnames = c("length", "width", "mass", "height"),
  empirical = FALSE
)

# Are indicators correlated?
pairs(dat)

cfa <- "body_size =~ mass + width + length + height"
fit <- cfa(cfa, data=dat)

# check that standardised lambdas are of similar magnitude (similar loadings) 
# suggesting common cause
summary(fit, standardized=T, rsq=T)

# Composite variables

# Composite variables: model nonlinear relationships

# let's generate some data (make drawing)
set.seed(2397348)
N <- 150
dat <- data.frame(x = rnorm(N))
dat$x2 = dat$x^2
dat$y = 2 * dat$x -1.5 * dat$x2 + rnorm(N)

# visualize the data
plot(dat$x, dat$y)

# fit quadratic in lm
summary(lm(y ~ x + I(x^2), data=dat))

# fit composite in lavaan automatically using "<~" operator
model1a_nonlinear <- '
comp.nonlinear <~ 1* x + x2
y ~ comp.nonlinear
'
fit1a_nonlinear <- sem(model1a_nonlinear, data=dat)
summary(fit1a_nonlinear, standardized=T)





# Composite variables: model continuous interactions

# let's generate some data (make drawing)
set.seed(2397348)
N <- 150
dat <- data.frame(x1 = rnorm(N))
dat$x2 = rnorm(N)
dat$x1x2 = dat$x1 * dat$x2
dat$y = 1.7 * dat$x1 + 0.8 * dat$x2 -0.8 * dat$x1x2 + rnorm(N)

summary(lm(y ~ x1 + x2 + x1:x2, data=dat))

# fit composite in lavaan automatically using "<~" operator

model2a_auto <- ' 
comp.int <~  1* x1 + x2 + x1x2 
y ~ comp.int 
'

fit2a_auto <- sem(model2a_auto, data=dat)
summary(fit2a, standardized=T)

# 1) fit composite in lavaan manually (understand how composite is built internally)

# get coefficients
summary(lm(y ~ x1 * x2, data=dat))
coef(lm(y ~ x1 * x2, data=dat))


model2b_manual <- ' 
comp.int <~ 1.6860798 * x1 +   0.7543838 * x2 + -0.8553332  * x1x2
y ~ comp.int
'

fit2b_manual <- sem(model2b_manual, data=dat)
summary(fit2b_manual, standardized=T)


# 2) fit composite in lavaan manually (in case > 1 composite variables needed, which often leads to convergence issues)
dat$comp.int.manual <- 1.6860798 * dat$x1 +   0.7543838 * dat$x2 + -0.8553332  * dat$x1x2
  
model2c_manual <- ' 
y ~ comp.int.manual
'

fit2c_manual <- sem(model2c_manual, data=dat)
summary(fit2c_manual, standardized=T)


# Multigroup fitting: interaction

# let's generate some data (make drawing)
N <- 100
set.seed(2397348)
dat <- data.frame(x1 = rnorm(N), x2 = rnorm(N))
dat$group = rep(c("0","1"), each = N/2)
dat$y <- ifelse(dat$group == "1", 0.2 * dat$x1, 0.9 * dat$x1) + 0.5 * dat$x2 + rnorm(N)
dat$group_num <- as.numeric(dat$group)

model3a <- ' 
y ~ x1 + x2
'

# naive model without groups
fit3a <- sem(model3a, data=dat)
summary(fit3a)

# multigroup model with free parameters
fit3b <- sem(model3a, group = "group", data=dat)
summary(fit3b)

# multigroup model with specified parameters
model3c <- ' 
y ~ c("b1", "b2") * x1 + c("b3", "b3") * x2
'

fit3c <- sem(model3c, group = "group", data=dat)
summary(fit3c)

# compare models
aictab(list(fit3a, fit3b, fit3c), modnames=c("no groups", "free params", "spec. params"))

# Any questions?




