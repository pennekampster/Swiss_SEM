library(lavaan)
library(visreg)
library(ggplot2)
library(AICcmodavg)


# Day 1:
# Exercise 1:

lm.dir <- lm(mass.above ~ nadd + precip.mm + rich + even, data = seabloom)
summary(lm.dir)

lm.rich <- lm(rich ~ nadd + precip.mm, data = seabloom)
summary(lm.rich)

lm.even <- lm(even ~ nadd + precip.mm, data = seabloom)
summary(lm.even)

# Exercise 2:

simple <-
  "mass.above ~ nadd + rich + even + precip.mm + disk
rich ~ nadd + precip.mm
even ~ nadd + precip.mm"

fit.simple <- sem(simple, data = seabloom, estimator = "MLM")

#Rescale variables
seabloom$mass.above <- seabloom$mass.above / 100
seabloom$precip.mm <- seabloom$precip.mm / 100

fit.simple <- sem(simple, data = seabloom, estimator = "MLM")
summary(fit.simple, fit.measures = TRUE)

# modification indices
modindices(fit.simple, minimum.value = 3.84)

fit.simple.up <- update(fit.simple, add = "rich ~~ even")
summary(fit.simple.up, fit.measures = TRUE, rsq = TRUE)

# modification indices
modindices(fit.simple.up, minimum.value = 0.01)


# Exercise 3: derived quantities
#

# standardized coefficients

standardizedsolution(fit.simple.up, type = "std.all")

# Add variables that calculate the total, direct and indirect effect of each variable

derived <-
"mass.above ~ b1 * nadd + b2 * rich + b3 * even + disk
rich ~ b4 * nadd + disk
even ~ b5 * nadd

dir.nut.effect   := b1
indir.nut.effect := b2 * b4 + b3 * b5
tot.nut.effect :=  b1 + b2 * b4 + b3 * b5
"

fit.derived <- sem(derived, data = seabloom, estimator = "MLM")
summary(fit.derived, rsq = TRUE)

# Exercise 4: Saturated model

satur <-
"mass.above ~ nadd + rich + even + precip.mm + disk
rich ~ nadd + precip.mm + disk
even ~ nadd + precip.mm + disk

rich ~~ even"

fit.satur <- sem(satur, data = seabloom, estimator = "MLM")
summary(fit.satur, rsq = TRUE)

# model pruning
prune <-
  "mass.above ~ nadd + rich + even + precip.mm + disk
rich ~ nadd + precip.mm + disk
even ~ nadd + precip.mm 

rich ~~ even"

fit.prune <- sem(prune, data = seabloom, estimator = "MLM")
summary(fit.prune, rsq = TRUE, fit.measures = TRUE)


aictab(list(fit.satur, fit.prune),
       c("saturated", "pruned"))


# Exercise 5: Mediation 

# Let's test whether the effect of disturbance is mediated via its
# effect on richness and evenness, rather than directly on biomass
# Add paths from disk to rich and even, remove the path to mass.above
# Compare model fit to simple model
# What do you conclude?

partial.mediation <-
"mass.above ~ nadd + rich + even + precip.mm + disk
rich ~ nadd + precip.mm + disk
even ~ nadd + precip.mm +  disk
rich ~~ even"

fit.partial.mediation <- sem(partial.mediation, data = seabloom, estimator = "MLM")
summary(fit.partial.mediation, rsq = TRUE)

full.mediation <-
"mass.above ~ nadd + rich + even + precip.mm 
rich ~ nadd + precip.mm + disk
even ~ nadd + precip.mm +  disk
rich ~~ even"

fit.full.mediation <- sem(full.mediation, data = seabloom, estimator = "MLM")
summary(fit.full.mediation, rsq = TRUE)

AIC(fit.partial.mediation, fit.full.mediation)


# Day 2:

# Exercise 1:

simple <-
"mass.above ~ nadd + disk + rich + even + precip.mm
rich ~ nadd + precip.mm
even ~ nadd + precip.mm

rich ~~ even"

fit.simple <- sem(simple, data = seabloom, estimator = "MLM")
summary(fit.simple)


# exercise 2:

cor.test(seabloom$even, seabloom$rich)
cor.test(seabloom$ens.pie, seabloom$rich)
cor.test(seabloom$even, seabloom$ens.pie)

plot(seabloom$ens.pie, seabloom$even)

seabloom$even.rev <- 1 - seabloom$even
diversity <- 'div =~ rich + even.rev + ens.pie'
fit.diversity <- cfa(diversity, data = seabloom, estimator = "MLM")


seabloom$rich_std <- (mean(seabloom$rich)-seabloom$rich) / sd(seabloom$rich)
seabloom$even.rev_std <- (mean(seabloom$even.rev)-seabloom$even.rev) / sd(seabloom$even.rev)

diversity <- 'div =~ lambda*rich_std + lambda*even.rev_std'

fit.diversity <- cfa(diversity, data = seabloom, estimator = "MLM")
summary(fit.diversity, standardized=T)

lv <- '
# Latent variable definition
diversity =~ lambda*even.rev_std + lambda*rich_std

mass.above ~ nadd + disk + diversity + precip.mm
diversity ~ nadd + precip.mm
'

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv)





