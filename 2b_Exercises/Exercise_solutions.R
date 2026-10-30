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

# Rule of thumb: correlations > 0.9 indicate potential collinearity issues, correlations between 0.7-0.9 warrant attention


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
# Do estimation with a Satorra-Bentler correction, to be included in the sem() or cfa() function: estimator="MLM"



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
summary(fit.simple, fit.measures = TRUE)

# modification indices
modindices(fit.simple, minimum.value = 3.84)

# add error correlation between evenness and richness
fit.simple.up <- update(fit.simple, add = "rich ~~ even")
summary(fit.simple.up, fit.measures = TRUE)

# modification indices
modindices(fit.simple.up, minimum.value = 0.01)



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
rich ~ nadd
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


# Day 2:

# Set up:

simple <-
"mass.above ~ nadd + disk + rich + even 
rich ~ nadd 
even ~ nadd

rich ~~ even"

fit.simple <- sem(simple, data = seabloom, estimator = "MLM")
summary(fit.simple)


# Exercise 1:

# investigate correlations among diversity metrics
cor.test(seabloom$even, seabloom$rich)

diversity <- 'div =~ rich + even'
fit.diversity <- cfa(diversity, data = seabloom, estimator = "MLM")
# CFA model fails with Heywood case (negative variance estimate)

# Construct simpler 2 indicator latent variable using two steps:
# Standardising puts richness (counts) and evenness (0-1) on a common scale. 
seabloom$rich_std <- (seabloom$rich-mean(seabloom$rich)) / sd(seabloom$rich)
seabloom$even_std <- (seabloom$even-mean(seabloom$even)) / sd(seabloom$even)

diversity <- 'div =~ rich_std + even_std'
fit.diversity <- cfa(diversity, data = seabloom, estimator = "MLM")

# Richness and evenness are strongly NEGATIVELY correlated
# (r = -0.71). A reflective latent with EQUAL (same-sign) loadings implies its
# indicators are POSITIVELY correlated (cov = lambda^2 * var(div)). To reproduce a
# negative observed correlation the model would need a negative latent variance -
# an impossible, improper (Heywood) solution, so it fails to give a usable fit.

# Reversing evenness makes cor(richness, even.rev) = +0.71, so equal positive
# loadings are consistent with the data and the latent variance is valid.
# (Note: high div then means high richness AND low evenness - a dominance axis.)
seabloom$even.rev_std <- 1- seabloom$even_std

# test measurement model
diversity <- 'div =~ lambda*even.rev_std + lambda*rich_std'  
fit.diversity <- cfa(diversity, data = seabloom, estimator = "MLM")
summary(fit.diversity, standardized=T)


# Exercise 2:

# fit full SEM with latent diversity variable 
lv <- '
# Latent variable definition
diversity =~ lambda*even.rev_std + lambda*rich_std

mass.above ~ nadd + disk + diversity
diversity ~ nadd
'

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv)


# Exercise 3: composite variable 'landuse' (joint cause: nutrients + disturbance)

# A composite is a weighted sum of its indicators (here disk and nadd), capturing
# their collective "land use" influence. Here land use drives biomass, richness
# and evenness (see the Ex3 DAG). The composite's error variance is fixed to 0
# (it is fully determined by its indicators).

# (a) Construct the composite directly in lavaan with the "<~" operator.
# IMPORTANT: fit it on the SAME data and estimator as fit.simple (plain seabloom,
# MLM). Rescaling the variables for only one of the two models would make their
# log-likelihoods - and therefore the AIC comparison below - meaningless.
comp.auto <-
'comp.landuse <~ 1 * disk + nadd
mass.above ~ comp.landuse + rich + even
rich ~ comp.landuse
even ~ comp.landuse
rich ~~ even'

fit.comp.auto <- sem(comp.auto, data = seabloom, estimator = "MLM")
summary(fit.comp.auto, standardized = TRUE, rsq = TRUE)

# (b) Compare the starting model with the composite model (both on plain seabloom,
# both MLM, same observed variables -> a valid comparison).
aictab(list(fit.simple, fit.comp.auto),
       c("separate disk and nadd", "composite landuse"))
# The starting model is far better supported (much lower AICc): forcing nutrients
# and disturbance onto a single 1-D "land use" axis fits the data clearly worse
# than letting the two drivers act separately.
# Caveat: because disk is binary and sits inside the composite, the composite's
# fit (unlike the starting model's) is not invariant to rescaling its indicators,
# so always compare the two models on identical data, as done here.





# Exercise 4 (interactions):

# manual calculation of product of two predictors
seabloom$diskxnadd <- seabloom$disk * seabloom$nadd

# We cannot build the interaction composite with "<~" here, because nadd would be
# BOTH an indicator of the composite AND a direct cause of richness and evenness.
# lavaan's composite (LISREL) representation does not allow an indicator to also
# have that second role, and the model errors out. So we build the composite by
# hand: take the weights from the interaction regression of biomass on disk, nadd
# and their product, form the composite as a data column, and enter it as an
# observed predictor.
w.int <- coef(lm(mass.above ~ disk + nadd + diskxnadd, data = seabloom))
w.int
seabloom$comp.int.manual <- w.int["disk"] * seabloom$disk +
                     w.int["nadd"] * seabloom$nadd +
                     w.int["diskxnadd"] * seabloom$diskxnadd

int.full <-
"mass.above ~ rich + even + comp.int.manual
rich ~ nadd
even ~ nadd

rich ~~ even"

fit.int.full <- sem(int.full, data = seabloom, estimator = "MLM")
summary(fit.int.full, standardized = TRUE, rsq = TRUE)


# Does interaction term increase variance explained?
lavInspect(fit.int.full, "R2")
lavInspect(fit.simple, "R2")

# Yes, inclusion of interaction term increases R2 for mass.above from 0.57 to 0.60
# what about goodness-of-fit?
fitmeasures(fit.int.full, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
fitmeasures(fit.simple, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea"))


# Exercise 5:

int.mg <-
"mass.above ~ rich + even + nadd
rich ~ nadd 
even ~ nadd 

rich ~~ even"

fit.int.mg <- sem(int.mg, group = "disk", data = seabloom)
summary(fit.int.mg, standardized = TRUE, rsq = TRUE)

# visualize how coefficients differ among groups
fit_tab <- (summary(fit.int.mg, standardized = TRUE, rsq = TRUE))$pe
fit_tab$term <- paste0(fit_tab$lhs, " ", fit_tab$op, " ", fit_tab$rhs)
fit_tab$group_chr <- ifelse(fit_tab$group == 1, "undisturbed", 
                            ifelse(fit_tab$group == 2, "disturbed", NA))

ggplot(data=subset(fit_tab, op == "~"), aes(y=est, x=group_chr, colour=group_chr)) + 
  geom_point(size=1) + geom_errorbar(aes(ymin = est-se*1.96, ymax = est+se*1.96)) + 
  facet_wrap(~term, scales="free") + 
  scale_colour_manual(values = c("blue",  "red")) + 
  geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed") + 
  guides(colour="none") +
  theme_bw() 

# Test the hypothesis that ALL paths are the same across disturbed and undisturbed
# plots: refit the same model but constrain every regression coefficient to be equal
# across groups (group.equal = "regressions").
fit.int.mg.constrain <- sem(int.mg, group = "disk",
                            group.equal = "regressions", data = seabloom)
summary(fit.int.mg.constrain, standardized = TRUE, rsq = TRUE)

fit_tab <- (summary(fit.int.mg.constrain, standardized = TRUE, rsq = TRUE))$pe
fit_tab$term <- paste0(fit_tab$lhs, " ", fit_tab$op, " ", fit_tab$rhs)
fit_tab$group_chr <- ifelse(fit_tab$group == 1, "undisturbed", 
                            ifelse(fit_tab$group == 2, "disturbed", NA))

ggplot(data=subset(fit_tab, op == "~"), aes(y=est, x=group_chr, colour=group_chr)) + 
  geom_point(size=1) + geom_errorbar(aes(ymin = est-se*1.96, ymax = est+se*1.96)) + 
  facet_wrap(~term, scales="free") + 
  scale_colour_manual(values = c("blue",  "red")) + 
  geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed") + 
  guides(colour="none") +
  theme_bw() 


library(AICcmodavg)
aictab(list(fit.int.mg, fit.int.mg.constrain),
       c("free (per-group paths)", "all paths equal"))
# Although the "all paths equal" model is more parsimonious (fewer parameters), it
# has a much higher AICc, so it is not supported: the path strengths differ between
# disturbed and undisturbed plots (i.e. disturbance interacts with the other drivers).










