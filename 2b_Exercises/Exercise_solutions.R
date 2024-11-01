library(lavaan)
library(visreg)
library(tidyverse)
library(AICcmodavg)
library(here)

seabloom <- read.table(here("2_Modeling/Data_preparation/seabloom-2020-ele-dryad-data/cdr-e001-e002-output-data.csv"),
                       sep = ",", header = TRUE)
seabloom <- seabloom %>% group_by(exp, field, plot, disk, yr.plowed, ntrt, nadd, other.add) %>% 
  summarise(across(mass.above:ens.pie, mean))

# seabloom$mass.above <- seabloom$mass.above / 100

# Day 1:
# Exercise 1:

lm.dir <- lm(mass.above ~ nadd  + rich + even, data = seabloom)
summary(lm.dir)
par(mfrow=c(2,2))
plot(lm.dir)
par(mfrow=c(1,1))

lm.rich <- lm(rich ~ nadd, data = seabloom)
summary(lm.rich)
par(mfrow=c(2,2))
plot(lm.rich)
par(mfrow=c(1,1))

lm.even <- lm(even ~ nadd, data = seabloom)
summary(lm.even)
par(mfrow=c(2,2))
plot(lm.even)
par(mfrow=c(1,1))

library(broom)

lm.dir.resid <- augment(lm.dir)$.resid
lm.even.resid <- augment(lm.even)$.resid
lm.rich.resid <- augment(lm.rich)$.resid

resid.df <- data.frame(lm.dir.resid, lm.even.resid, lm.rich.resid)
library("MVN")
mvn(data = resid.df, mvnTest = "hz", univariatePlot = "qqplot")



mvn(data = seabloom[, c("rich", "even", "mass.above")], mvnTest = "hz", univariatePlot = "qqplot")

histWithDensity <- function(variable, name){
  hist(variable, prob = TRUE, main = "", xlab = name)
  x <- seq(min(variable), max(variable), length = 400)
  y <- dnorm(x, mean = mean(variable), sd = sd(variable))
  lines(x, y, col = "red", lwd = 2)
}
par(mfrow = c(1, 3))
histWithDensity(seabloom$mass.above, "mass.above")
histWithDensity(seabloom$rich, "rich")
histWithDensity(seabloom$even, "even")

log.mass.above <- sqrt(seabloom$mass.above)
log.even <- sqrt(seabloom$even)
log.rich <- sqrt(seabloom$rich)

mvn(data = data.frame(log.mass.above, log.even, log.rich), mvnTest = "hz", univariatePlot = "qqplot")

# Exercise 2:

simple <-
"mass.above ~ nadd + rich + even + disk
rich ~ nadd
even ~ nadd"

fit.simple <- sem(simple, data = seabloom, estimator = "MLM")

#Rescale variables
seabloom$mass.above <- seabloom$mass.above / 100

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
rich ~ b4 * nadd 
even ~ b5 * nadd 

rich ~~ even

dir.nut.effect   := b1
indir.nut.effect := b2 * b4 + b3 * b5
tot.nut.effect :=  b1 + b2 * b4 + b3 * b5
"

fit.derived <- sem(derived, data = seabloom, estimator = "MLM")
summary(fit.derived, rsq = TRUE, standardized=T)

# Exercise 4: Saturated model

satur <-
"mass.above ~ nadd + rich + even +  disk
rich ~ nadd + disk
even ~ nadd + disk

rich ~~ even"

fit.satur <- sem(satur, data = seabloom, estimator = "MLM")
summary(fit.satur, rsq = TRUE)

# model pruning
prune <-
"mass.above ~ nadd + rich + even +  disk
rich ~ nadd
even ~ nadd   

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

AIC(fit.no.mediation, fit.partial.mediation, fit.full.mediation)





# Day 2:

# Exercise 1:

simple <-
"mass.above ~ nadd + disk + rich + even 
rich ~ nadd 
even ~ nadd

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


scale(seabloom$rich, center=T)

seabloom$rich_std <- (mean(seabloom$rich)-seabloom$rich) / sd(seabloom$rich)
seabloom$even.rev_std <- (mean(seabloom$even.rev)-seabloom$even.rev) / sd(seabloom$even.rev)

diversity <- 'div =~ lambda*rich + lambda*even.rev
rich ~~ 1*rich
even.rev ~~ 1*even.rev
'

fit.diversity <- cfa(diversity, data = seabloom, estimator = "MLM")
summary(fit.diversity, standardized=T)

# Exercise 3:

lv <- '
# Latent variable definition
diversity =~ lambda*even.rev_std + lambda*rich_std

mass.above ~ nadd + disk + diversity
diversity ~ nadd + disk
'

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv)

# issue with the direction of diversity on AGB. Can you explain?

# compare to ens.pie

lv <- '
mass.above ~ nadd + disk + ens.pie
ens.pie ~ nadd + disk
'

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv)





# Exercise 4 (composite):
  
comp <- "
comp.landuse <~ 1 * disk + nadd

rich ~ comp.landuse
even ~ comp.landuse

mass.above ~ comp.landuse + rich + even

rich ~~ even
"

fit.comp <- sem(comp, data = seabloom)
summary(fit.comp, standardized=T, rsq=T)


# manually construct composite (if lavaan struggles)
comp.man <- 'mass.above ~ disk + nadd'
fit.comp.man <- sem(comp.man, data = seabloom)
summary(fit.comp.man)

seabloom$landuse <- lavInspect(fit.comp.man, what = "est")$beta[1, 2] * seabloom$disk +
  lavInspect(fit.comp.man, what = "est")$beta[1, 3] * seabloom$nadd


# fit composite as part of SEM model
comp <- "
rich ~ landuse
even ~ landuse

mass.above ~ landuse + rich + even
"

fit.comp <- sem(comp, data = seabloom)
summary(fit.comp, standardized=T, rsq=T)


# Exercise 5 (interactions):

# manual calculation of product of two predictors
seabloom$diskxnadd <- seabloom$disk * seabloom$nadd

int <- "mass.above ~ disk + nadd + diskxnadd"
fit.int <- sem(int, data = seabloom)
summary(fit.int, standardized=T)

compint <-
"comp.int <~ 1 * disk + nadd + diskxnadd
mass.above ~ comp.int"

fit.compint <- sem(compint, data = seabloom)
summary(fit.compint, standardized = TRUE)

# Full model:

int.full <-
"comp.int <~ 1 * disk + nadd + diskxnadd

mass.above ~ rich + even 
rich ~ comp.int 
even ~ comp.int

rich ~~ even"

fit.int.full <- sem(int.full, data = seabloom)
summary(fit.int.full, standardized = TRUE, rsq = TRUE)


# Does interaction term increase variance explained?
lavInspect(fit.int.full, "R2")
lavInspect(fit.simple, "R2")


# Exercise 6:

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

# constrain certain pathways to be the same
int.mg.constrain <-
  "mass.above ~  c('b1', 'b1') * rich + c('b2', 'b2') * even 
rich ~ c('b4a', 'b4b') * nadd 
even ~ c('b6a', 'b6b') *  nadd

rich ~~ even"

fit.int.mg.constrain <- sem(int.mg.constrain, group = "disk", data = seabloom)
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
aictab(list(fit.int.mg, fit.int.mg.constrain))











