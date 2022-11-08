library(lavaan)
library(visreg)
library(ggplot2)
library(AICcmodavg)
library(here)

seabloom <- read.table(here("2_Modeling/Data_preparation/seabloom-2020-ele-dryad-data/cdr-e001-e002-output-data.csv"),
                       sep = ",", header = TRUE)

# seabloom$mass.above <- seabloom$mass.above / 100
# seabloom$precip.mm <- seabloom$precip.mm / 100
# seabloom$precip.gs <- seabloom$precip.gs / 100

# Day 1:
# Exercise 1:

lm.dir <- lm(mass.above ~ nadd + precip.mm + rich + even, data = seabloom)
summary(lm.dir)
par(mfrow=c(2,2))
plot(lm.dir)
par(mfrow=c(1,1))

lm.rich <- lm(rich ~ nadd + precip.mm, data = seabloom)
summary(lm.rich)
par(mfrow=c(2,2))
plot(lm.rich)
par(mfrow=c(1,1))

lm.even <- lm(even ~ nadd + precip.mm, data = seabloom)
summary(lm.even)
par(mfrow=c(2,2))
plot(lm.even)
par(mfrow=c(1,1))


# Exercise 2:

library("MVN")

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
"mass.above ~ b1 * nadd + b2 * rich + b3 * even + disk +  precip.mm
rich ~ b4 * nadd + precip.mm
even ~ b5 * nadd + precip.mm

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

# Exercise 3:

lv <- '
# Latent variable definition
diversity =~ lambda*even.rev_std + lambda*rich_std

mass.above ~ nadd + disk + diversity + precip.mm
diversity ~ nadd + precip.mm
'

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv)

# issue with the direction of diversity on AGB. Can you explain?


lv <- '
mass.above ~ nadd + disk + ens.pie + precip.mm
ens.pie ~ nadd + precip.mm
'

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv)

# Exercise 4 (composite):
  
comp <- "
comp.landuse <~ 1 * disk + nadd

rich ~ precip.mm + comp.landuse
even ~ precip.mm + comp.landuse
mass.above ~ comp.landuse + rich + even + precip.mm

rich ~~ even"

fit.comp <- sem(comp, data = seabloom)
summary(fit.comp, standardized=T)

# manual composite (if lavaan struggles)
comp.man <- 'mass.above ~ disk + nadd'
fit.comp.man <- sem(comp.man, data = seabloom)
summary(fit.comp.man)

seabloom$landuse <- lavInspect(fit.comp.man, what = "est")$beta[1, 2] * seabloom$disk +
  lavInspect(fit.comp.man, what = "est")$beta[1, 3] * seabloom$nadd

comp.man2 <- "mass.above ~ landuse"
fit.comp.man2 <- sem(comp.man2, data = seabloom)

comp <- "
rich ~ precip.mm + landuse
even ~ precip.mm + landuse

mass.above ~ landuse + rich + even + precip.mm

rich ~~ even"

fit.comp <- sem(comp, data = seabloom)
summary(fit.comp, standardized=T)

# Exercise 5 (interactions):

# manually
seabloom$diskxnadd <- seabloom$disk * seabloom$nadd

int <- "mass.above ~ disk + nadd + diskxnadd"
fit.int <- sem(int, data = seabloom)
summary(fit.int)

compint <-
  "comp.int <~ 1 * disk + nadd + diskxnadd
mass.above ~ comp.int"

fit.compint <- sem(compint, data = seabloom)
summary(fit.compint, standardized = TRUE, rsq = TRUE)

# Full model:

int.full <-
  "comp.int <~ 1 * disk + nadd + diskxnadd

mass.above ~ comp.int + rich + even + precip.mm
rich ~ nadd + precip.mm
even ~ nadd + precip.mm

rich ~~ even"

fit.int.full <- sem(int.full, data = seabloom)
summary(fit.int.full, fit.measures = TRUE, standardized = TRUE, rsq = TRUE)


# Exercise 6:

int.mg <-
  "mass.above ~ rich + even + precip.mm
rich ~ nadd + precip.mm
even ~ nadd + precip.mm

rich ~~ even"

fit.int.mg <- sem(int.mg, group = "disk", data = seabloom)
summary(fit.int.mg, standardized = TRUE, rsq = TRUE)

fit_tab <- (summary(fit.int.mg, standardized = TRUE, rsq = TRUE))$pe
fit_tab$term <- paste0(fit_tab$lhs, " ", fit_tab$op, " ", fit_tab$rhs)
fit_tab$group_chr <- ifelse(fit_tab$group == 1, "undist", 
                            ifelse(fit_tab$group == 2, "dist", NA))

ggplot(data=subset(fit_tab, op == "~"), aes(y=est, x=group_chr, colour=group_chr)) + 
  geom_point(size=1) + geom_errorbar(aes(ymin = est-se*1.96, ymax = est+se*1.96)) + 
  facet_wrap(~term, scales="free") + 
  scale_colour_manual(values = c("blue",  "red")) + 
  geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed") + 
  guides(colour="none") +
  theme_bw() 

int.mg.constrain <-
  "mass.above ~  c('b1', 'b1') * rich + c('b2', 'b2') * even + c('b3', 'b3') * precip.mm
rich ~ c('b4a', 'b4b') * nadd + c('b5', 'b5') * precip.mm
even ~ c('b6a', 'b6b') *  nadd + c('b7', 'b7') * precip.mm

rich ~~ even"

fit.int.mg.constrain <- sem(int.mg.constrain, group = "disk", data = seabloom)
summary(fit.int.mg.constrain, standardized = TRUE, rsq = TRUE)

fit_tab <- (summary(fit.int.mg.constrain, standardized = TRUE, rsq = TRUE))$pe
fit_tab$term <- paste0(fit_tab$lhs, " ", fit_tab$op, " ", fit_tab$rhs)
fit_tab$group_chr <- ifelse(fit_tab$group == 1, "undist", 
                            ifelse(fit_tab$group == 2, "dist", NA))

ggplot(data=subset(fit_tab, op == "~"), aes(y=est, x=group_chr, colour=group_chr)) + 
  geom_point(size=1) + geom_errorbar(aes(ymin = est-se*1.96, ymax = est+se*1.96)) + 
  facet_wrap(~term, scales="free") + 
  scale_colour_manual(values = c("blue",  "red")) + 
  geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed") + 
  guides(colour="none") +
  theme_bw() 


library(AICcmodavg)
aictab(list(fit.int.mg, fit.int.mg.constrain))

# Exercise 7:

library("lavaan.survey")

design <- svydesign(ids = ~ plot, strata = ~ field, nest = TRUE, 
                    data = seabloom)
summary(design)

fit.simple.nest <- lavaan.survey(lavaan.fit = fit.simple,
                                 survey.design = design)
summary(fit.simple.nest, rsq = TRUE)













