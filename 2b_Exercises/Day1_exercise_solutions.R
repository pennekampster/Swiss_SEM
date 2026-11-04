library(lavaan)
library(visreg)
library(tidyverse)
library(AICcmodavg)
library(here)
library(corrplot)      # For correlation plots
library(MVN)
library(broom)

# load the dataset
seabloom <- read.table(here("2_Modeling/Data_preparation/seabloom-2020-ele-dryad-data/cdr-e001-e002-output-data.csv"),
                       sep = ",", header = TRUE)

#turn nutrient treatments into factor
seabloom$ntrt <- as.factor(seabloom$ntrt)

# average across years
seabloom <- seabloom %>% group_by(exp, field, plot, disk, yr.plowed, ntrt, nadd, other.add) %>% summarise(across(mass.above:ens.pie, mean))

# run this for exercises of Day 2
# seabloom$mass.above <- seabloom$mass.above / 100

# Day 1:
# Exercise 1:

# 1. COLLINEARITY TESTS
vars_to_check <- c("nadd", "disk", "rich", "even", "ens.pie", "mass.above")
cor_matrix <- cor(seabloom[, vars_to_check], use = "complete.obs")
# --- Method 2: Correlation Plot (Visual) ---
corrplot(cor_matrix, 
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         title = "Correlation Matrix",
         mar = c(0,0,1,0))

# Rule of thumb: correlations > 0.9 indicate potential collinearity issues


# 2. MULTIVARIATE NORMALITY TESTS

lm.dir <- lm(mass.above ~ nadd  + disk + rich + even, data = seabloom)
summary(lm.dir)
par(mfrow=c(2,2))
plot(lm.dir)
par(mfrow=c(1,1))

lm.rich <- lm(rich ~ nadd + disk, data = seabloom)
summary(lm.rich)
par(mfrow=c(2,2))
plot(lm.rich)
par(mfrow=c(1,1))

lm.even <- lm(even ~ nadd + disk, data = seabloom)
summary(lm.even)
par(mfrow=c(2,2))
plot(lm.even)
par(mfrow=c(1,1))

# extract residuals from regressions
lm.dir.resid <- augment(lm.dir)$.resid
lm.even.resid <- augment(lm.even)$.resid
lm.rich.resid <- augment(lm.rich)$.resid
resid.df <- data.frame(lm.dir.resid, lm.even.resid, lm.rich.resid)

# test multivariate normality
mvn_result <- mvn(data = resid.df)
summary(mvn_result, select = "mvn")
plot(mvn_result, diagnostic = "multivariate", type = "qq")

# What to do when multivariate normality assumption is violated?
# Do estimation with a Satorra-Bentler correction, 
# to be included in the sem() or cfa() function: estimator="MLM"



# Exercise 2:
simple <-
"mass.above ~ nadd + rich + even + disk
rich ~ nadd
even ~ nadd"

# use robust estimator due to non-normality
fit.simple <- sem(simple, data = seabloom, estimator = "MLM")

#Rescale variables
seabloom$mass.above <- seabloom$mass.above / 100

# estimate again
fit.simple <- sem(simple, data = seabloom, estimator = "MLM")
summary(fit.simple)

# modification indices
modindices(fit.simple, minimum.value = 3.84)

# add error correlation between evenness and richness
simple.up <-
"mass.above ~ nadd + rich + even + disk
rich ~ nadd
even ~ nadd
rich ~~ even"

fit.simple.up <- sem(simple.up, data = seabloom, estimator = "MLM")
summary(fit.simple.up, fit.measures = TRUE)

# modification indices
modindices(fit.simple.up)



# Exercise 3: standardized coefficients and derived quantities

# get standardized coefficients
standardizedsolution(fit.simple.up, type = "std.all")

# Calculate the total, direct and indirect effect of each variable

derived <-
"mass.above ~ b1 * nadd + b2 * rich + b3 * even + disk
rich ~ b4 * nadd 
even ~ b5 * nadd 

rich ~~ even

dir.nut.effect   := b1
indir.nut.effect := b2 * b4 + b3 * b5
tot.nut.effect :=  b1 + b2 * b4 + b3 * b5
"

fit.derived <- sem(derived, data = seabloom, estimator = "MLM")
summary(fit.derived, standardized = TRUE)

# Exercise 4: Saturated model

satur <-
"mass.above ~ nadd + rich + even +  disk
rich ~ nadd + disk
even ~ nadd + disk

rich ~~ even"

fit.satur <- sem(satur, data = seabloom, estimator = "MLM")
summary(fit.satur)

aictab(list(fit.simple.up, fit.satur),
       c("simple", "saturated"))

# model pruning (remove non-significant paths)
prune <-
"mass.above ~ nadd + rich + even +  disk
rich ~ nadd + disk
even ~ nadd   

rich ~~ even"

fit.prune <- sem(prune, data = seabloom, estimator = "MLM")
summary(fit.prune, rsq = TRUE, fit.measures = TRUE)

aictab(list(fit.satur, fit.prune),
       c("saturated", "pruned"))

# Take away: a saturated model is not always useless, since one can use model comparisons to assess whether all paths are necessary
# Generally, one should assess model fit first, then prune non-significant paths to arrive at a more parsimonious model, rather than fitting a saturated model and then simplifying it.


# Exercise 5: Mediation 

# Let's test whether the effect of disturbance is mediated via its
# effect on richness and evenness, rather than directly on biomass
# What do you conclude?

no.mediation <-
"mass.above ~ nadd + rich + even + disk
rich ~ nadd
even ~ nadd
rich ~~ even"

fit.no.mediation <- sem(no.mediation, data = seabloom, estimator = "MLM")
summary(fit.no.mediation, rsq = TRUE)

partial.mediation <-
"mass.above ~ nadd + rich + even + disk
rich ~ nadd + disk
even ~ nadd + disk
rich ~~ even"

fit.partial.mediation <- sem(partial.mediation, data = seabloom, estimator = "MLM")
summary(fit.partial.mediation, rsq = TRUE)

full.mediation <-
"mass.above ~ nadd + rich + even 
rich ~ nadd + disk
even ~ nadd + disk
rich ~~ even"

fit.full.mediation <- sem(full.mediation, data = seabloom, estimator = "MLM")
summary(fit.full.mediation, rsq = TRUE)

aictab(list(fit.no.mediation, fit.partial.mediation, fit.full.mediation),
       c("no mediation", "partial mediation", "full mediation"))









