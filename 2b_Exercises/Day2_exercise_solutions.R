# Day 2:

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

# rescale
seabloom$mass.above <- seabloom$mass.above / 100


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
summary(fit.diversity)

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


# Exercise 3: composite variable 'land use' (joint cause: nutrients + disturbance)

# A composite is a weighted sum of its indicators (here disk and nadd), capturing
# their collective "land use" influence. Here land use drives biomass, richness
# and evenness (see the Ex3 DAG). The composite's error variance is fixed to 0
# (it is fully determined by its indicators).

# (a) Construct the composite directly in lavaan with the "<~" operator.

comp.auto <-
'comp.landuse <~ disk + nadd
mass.above ~ comp.landuse + rich_std + even_std
rich_std ~ comp.landuse
even_std ~ comp.landuse
rich_std ~~ even_std'

fit.comp.auto <- sem(comp.auto, data = seabloom, estimator = "MLM")
summary(fit.comp.auto, standardized = TRUE, rsq = TRUE)

# (b) Compare the starting model with the composite model (both on plain seabloom,
# both MLM, same observed variables -> a valid comparison).
aictab(list(fit.simple, fit.comp.auto),
       c("separate disk and nadd", "composite landuse"))
# The starting model is far better supported (much lower AICc): forcing nutrients
# and disturbance onto a single 1-D "land use" axis fits the data clearly worse
# than letting the two drivers act separately.

# Exercise 4:

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



# Exercise 5 (model interaction with composite):

# manual calculation of product of two predictors
seabloom$diskxnadd <- seabloom$disk * seabloom$nadd

# We build the composite by
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

