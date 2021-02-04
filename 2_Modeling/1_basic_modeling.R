## -------------------------------------------------------------------------------
rm(list = ls())
seabloom <- read.table("/home/r/Swiss_SEM/2_Modeling/Data_preparation/CedarCreek_Seabloom/seabloom-2020-ele-dryad-data/cdr-e001-e002-output-data.csv",
                       sep = ",", header = TRUE)


## -------------------------------------------------------------------------------
unique(seabloom$year)

# seabloom <- seabloom[seabloom$year == 2004, ]
dim(seabloom)


## -------------------------------------------------------------------------------
# lm.dir <- lm(mass.above ~ nadd + precip.mm + rich + even,
#              data = seabloom)
# summary(lm.dir)
#
#
# ## -------------------------------------------------------------------------------
# summary(seabloom$precip.mm)
#
#
# ## -------------------------------------------------------------------------------
# lm.dir <- lm(mass.above ~ nadd + rich + even, data = seabloom)
# summary(lm.dir)
#
#
# ## -------------------------------------------------------------------------------
# lm.rich <- lm(rich ~ nadd, data = seabloom)
# summary(lm.rich)
#
# lm.even <- lm(even ~ nadd, data = seabloom)
# summary(lm.even)


## -------------------------------------------------------------------------------
library("lavaan")


seabloom[, c(4, 7, 9, 11, 13:15)] <- apply(seabloom[, c(4, 7, 9, 11, 13:15)],
                                        2, scale)
# seabloom[, c(4, 7, 11, 13:15)] <- apply(seabloom[, c(4, 7, 11, 13:15)],
#                                         2, scale)


## -------------------------------------------------------------------------------
simple <-
"mass.above ~ nadd + rich + even + disk + year
rich ~ nadd + disk + year
even ~ nadd + disk + year

# rich ~~ even
"


fit.simple <- sem(simple, data = seabloom)
summary(fit.simple, rsq = TRUE)


## -------------------------------------------------------------------------------
modindices(fit.simple, minimum.value = 3)


## -------------------------------------------------------------------------------
fit.simple.up <- update(fit.simple, add = "rich ~~ even")
# fit.simple.up <- update(fit.simple, add = "rich ~ mass.above")

summary(fit.simple.up, rsq = TRUE, fit.measures = TRUE)
# standardizedsolution(fit.simple.up)
# inspect(fit.simple.up, "r2")

modindices(fit.simple.up, minimum.value = 3)
fit.simple.up2 <- update(fit.simple.up, add = "rich ~ mass.above")
summary(fit.simple.up2, rsq = TRUE, fit.measures = TRUE)


## -------------------------------------------------------------------------------
modindices(fit.simple.up, minimum.value = 3)


## -------------------------------------------------------------------------------
library(lavaanPlot)

lavaanPlot(model = fit.simple.up, node_options = list(shape = "box",
                                                    fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE, stand = FALSE,
           sig = 0.05)


## -------------------------------------------------------------------------------
library(piecewiseSEM)
library(nlme)

Psem1List <- list(
  lm(mass.above ~ rich + even + nadd + disk, seabloom),
  lm(rich ~ nadd, seabloom),
  lm(even ~ nadd, seabloom),
  even %~~% rich
)


Psem1 <- as.psem(Psem1List)
summary(Psem1, .progressBar = F)


## -------------------------------------------------------------------------------
Psem1RandomList <- list(
  lme(mass.above ~ rich + even + nadd + disk, random = ~ 1|field, seabloom),
  lme(rich ~ nadd, random = ~ 1|field, seabloom),
  lme(even ~ nadd, random = ~ 1|field, seabloom),
  even %~~% rich
)


Psem1Random <- as.psem(Psem1RandomList)
summary(Psem1Random, .progressBar = F)


## -------------------------------------------------------------------------------
# modfit<-sem(model, data=dat, estimator = "mlm")
# survey.design <- svydesign(ids=~1, strata = ~ spatial_block, prob =~1, data=dat)
# fit_with_blocks <- lavaan.survey(modfit, survey.design)
# summary(fit_with_blocks, rsquare=T, standardized = T, fit.measures = T)


# library("survey")
library("lavaan.survey")

modfit <- sem(model = fit.simple.up, data = seabloom, estimator = "mlm")
survey.design <- svydesign(ids = ~ 1, strata = ~ field, prob = ~ 1,
                           data = seabloom)
# fit_with_blocks <- lavaan.survey(modfit, survey.design)
# summary(fit_with_blocks, rsquare = TRUE, standardized = TRUE,
#         fit.measures = TRUE)


## -------------------------------------------------------------------------------
sem2 <-
"mass.above ~ nadd + rich + even + disk
rich ~ nadd + disk
even ~ nadd + disk

rich ~~ even"

fit.sem2 <- sem(sem2, data = seabloom)
summary(fit.sem2, rsq = TRUE)


## -------------------------------------------------------------------------------
modindices(fit.sem2, minimum.value = 3)


## -------------------------------------------------------------------------------
Psem2RandomList <- list(
  lme(mass.above ~ rich + even + nadd + disk, random = ~ 1|field, seabloom),
  lme(rich ~ nadd + disk, random = ~ 1|field, seabloom),
  lme(even ~ nadd + disk, random = ~ 1|field, seabloom),
  even %~~% rich
)

Psem2Random <- as.psem(Psem2RandomList)
summary(Psem2Random, .progressBar = FALSE)


## -------------------------------------------------------------------------------
sem.prune <-
"mass.above ~ nadd + rich
rich ~ nadd
even ~ nadd

rich ~~ even"

fit.sem.prune <- sem(sem.prune, data = seabloom)
summary(fit.sem.prune)

