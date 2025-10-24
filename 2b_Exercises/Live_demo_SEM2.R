# Live demo latent and composite variables
library(lavaan)
library(AICcmodavg)
library(faux)

## Latent variables

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
summary(fit, standardized=T, rsq=T)

# Note: Model has -1 degrees of freedom (under-identified)
# This means we can't test model fit

# Force equal loadings (adds 1 df) to estimate parameters
# Assumption: The latent body size variable has an equal effect on both measurements. 
# Requires that both are equally reliable, i.e., are measured with the same precision."

cfa_equal <- "body_size =~ a*length + a*mass"

fit_equal <- cfa(cfa_equal, data=dat)
summary(fit_equal, standardized=T, rsq=T, fit.measures=T)

# CFA with more than two indicator variables
set.seed(72643276)
dat <- rnorm_multi(
  n = 100, 
  mu = c(40, 15, 80, 200),           # Similar scales
  sd = c(10, 3.75, 20, 50),          # CV = 0.25 for all
  r = c(0.5, 0.6, 0.5, 0.6, 0.5, 0.5),  # 6 correlations for 4 vars
  varnames = c("length", "width", "mass", "height"),
  empirical = TRUE
)

# Are indicators correlated?
pairs(dat)

cfa <- "body_size =~ mass + width + length + height"
fit <- cfa(cfa, data=dat)

# check that standardised lambdas are of similar magnitude (similar loadings) 
# suggesting common cause
summary(fit, standardized=T, rsq=T)




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
  x_true =~ a*measure1 + a*measure2
  y ~ x_true
'
summary(sem(latent, data = dat), standardized = T, rsq = T)

# Interactions

# Composite variables: interaction

# let's generate some data (make drawing)
set.seed(2397348)
N <- 50
dat <- data.frame(x1 = rnorm(N))
dat$x2 = rnorm(N)
dat$x1x2 = dat$x1 * dat$x2
dat$y = 1.7 * dat$x1 + 0.8 * dat$x2 + -0.8 * dat$x1x2 + rnorm(N)

pairs(dat)

summary(lm(y ~ x1 + x2 + x1:x2, data=dat))

# fit composite in lavaan (automatically)

model2a <- ' 
comp.int <~  1* x1 + x2 + x1x2 
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

dat$comp.int <- 1.72075 * dat$x1 +   0.43481 * dat$x2 + -0.67734  * dat$x1x2
  
  
model2b <- ' 
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



# Multigroup fitting: interaction

# let's generate some data (make drawing)
N <- 50
set.seed(2397348)
dat <- data.frame(x1 = rnorm(N))
dat$group = rep(c("0","1"), each = N/2)
dat$y <- ifelse(dat$group == "1", 0.2 * dat$x1, 0.9 * dat$x1) + rnorm(N, 0, .1)
dat$group_num <- as.numeric(dat$group)
# ANCOVA
summary(lm(y~x1 * group, data=dat))

model3a <- ' 
y ~ x1 
'

fit3a <- sem(model3a, group = "group", data=dat)
summary(fit3a)

dat$int_term <- dat$x1 * dat$group_num


model3a <- ' 
y ~ x1 + group_num + int_term
'

fit3a <- sem(model3a, data=dat)
summary(fit3a)


model3b <- ' 
y ~ c("b1", "b2") * x1
b1 == 0
'

fit3b <- sem(model3b, group = "group", data=dat)
summary(fit3b, fit.measures=T)
fit3b
aictab(list(fit3a, fit3b))










