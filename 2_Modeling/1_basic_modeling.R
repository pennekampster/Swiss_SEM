## ----setup, include=FALSE----------------------------------------------------------


## ----------------------------------------------------------------------------------
rm(list = ls())
seabloom <- read.table("~/Swiss_SEM/2_Modeling/Data_preparation/seabloom-2020-ele-dryad-data/cdr-e001-e002-output-data.csv",
                       sep = ",", header = TRUE)


## ----------------------------------------------------------------------------------
dim(seabloom)
str(seabloom)


## ----------------------------------------------------------------------------------
panel.points <- function(x, y) {
  points(x, y, cex = 0.1)
  }

pairs(seabloom[, -c(1:3, 9)], lower.panel = NULL, upper.panel = panel.points)


## ----------------------------------------------------------------------------------
seabloom <- seabloom[seabloom$year == 2000, ]
dim(seabloom)
str(seabloom)


## ----------------------------------------------------------------------------------
lm.dir <- lm(mass.above ~ nadd + precip.mm + rich + even,
             data = seabloom)
summary(lm.dir)


## ----------------------------------------------------------------------------------
summary(seabloom$precip.mm)


## ----------------------------------------------------------------------------------
lm.dir <- lm(mass.above ~ nadd + rich + even, data = seabloom)
summary(lm.dir)


## ----------------------------------------------------------------------------------
lm.rich <- lm(rich ~ nadd, data = seabloom)
summary(lm.rich)

lm.even <- lm(even ~ nadd, data = seabloom)
summary(lm.even)


## ----------------------------------------------------------------------------------
library("lavaan")


## ----------------------------------------------------------------------------------
cor(seabloom[, c(4, 7, 11, 13:15)])


## ----------------------------------------------------------------------------------
simple <-
"mass.above ~ nadd + disk + rich + even
rich ~ nadd
even ~ nadd"

fit.simple <- sem(simple, data = seabloom)


## ----------------------------------------------------------------------------------
varTable(fit.simple)


## ----------------------------------------------------------------------------------
boxplot(seabloom[, c(4, 7, 13:15)])


## ----------------------------------------------------------------------------------
seabloom[, c(4, 7, 13:15)] <- apply(seabloom[, c(4, 7, 13:15)],
                                        2, scale)
boxplot(seabloom[, c(4, 7, 13:15)])


## ----------------------------------------------------------------------------------
fit.simple <- sem(simple, data = seabloom)
summary(fit.simple, rsq = TRUE)


## ----------------------------------------------------------------------------------
modindices(fit.simple, minimum.value = 3)


## ----------------------------------------------------------------------------------
fit.simple.up <- update(fit.simple, add = "rich ~~ even")
# fit.simple.up <- update(fit.simple, add = "rich ~ mass.above")

summary(fit.simple.up, rsq = TRUE, fit.measures = TRUE)
# standardizedsolution(fit.simple.up)
# inspect(fit.simple.up, "r2")

modindices(fit.simple.up, minimum.value = 3)


## ----------------------------------------------------------------------------------
modindices(fit.simple.up, minimum.value = 3)


## ----------------------------------------------------------------------------------
library("lavaanPlot")

lavaanPlot(model = fit.simple.up,
           node_options = list(shape = "box", color = "gray",
                               fontname = "Helvetica"),
           edge_options = list(color = "black"),
           coefs = TRUE, covs = TRUE, stars = c("covs", "regress"))


## ----------------------------------------------------------------------------------
library("piecewiseSEM")
library("nlme")

PsimpleList <- list(lm(mass.above ~ rich + even + nadd + disk, seabloom),
                    lm(rich ~ nadd, seabloom),
                    lm(even ~ nadd, seabloom),
                    even %~~% rich)

Psimple <- as.psem(PsimpleList)
summary(Psimple, .progressBar = FALSE)


## ----------------------------------------------------------------------------------
PsimpleRandomList <- list(lme(mass.above ~ rich + even + nadd + disk,
                              random = ~ 1|field, seabloom),
                          lme(rich ~ nadd, random = ~ 1|field, seabloom),
                          lme(even ~ nadd, random = ~ 1|field, seabloom),
                          even %~~% rich)

PsimpleRandom <- as.psem(PsimpleRandomList)
summary(PsimpleRandom, .progressBar = FALSE)


## ----------------------------------------------------------------------------------
library("lavaan.survey")

# modfit<-sem(model, data=dat, estimator = "mlm")
# survey.design <- svydesign(ids=~1, strata = ~ spatial_block, prob =~1, data=dat)
# fit_with_blocks <- lavaan.survey(modfit, survey.design)
# summary(fit_with_blocks, rsquare=T, standardized = T, fit.measures = T)


# modfit <- sem(model = fit.simple.up, data = seabloom, estimator = "mlr")
# survey.design <- svydesign(ids = ~ 1, strata = ~ field, probs = ~ 1,
#                            data = seabloom)

# fit_with_blocks <- lavaan.survey(modfit, survey.design)
# summary(fit_with_blocks, rsquare = TRUE, standardized = TRUE,
#         fit.measures = TRUE)


design <- svydesign(ids = ~ field, nest = TRUE, data = seabloom)
fit.simple.up.nest <- lavaan.survey(lavaan.fit = fit.simple.up,
                                     survey.design = design)
summary(fit.simple.up.nest, rsq = TRUE)


## ----------------------------------------------------------------------------------
sem2 <-
"mass.above ~ nadd + rich + even + disk
rich ~ nadd + disk
even ~ nadd + disk

rich ~~ even"

fit.sem2 <- sem(sem2, data = seabloom)
summary(fit.sem2, rsq = TRUE)


## ----------------------------------------------------------------------------------
modindices(fit.sem2, minimum.value = 3)


## ----------------------------------------------------------------------------------
PsaturList <- list(
  lme(mass.above ~ rich + even + nadd + disk, random = ~ 1|field, seabloom),
  lme(rich ~ nadd + disk, random = ~ 1|field, seabloom),
  lme(even ~ nadd + disk, random = ~ 1|field, seabloom),
  even %~~% rich
)

Psatur <- as.psem(PsaturList)
summary(Psatur, .progressBar = FALSE)


## ----------------------------------------------------------------------------------
sem.prune <-
"mass.above ~ nadd + rich
rich ~ nadd
even ~ nadd

rich ~~ even"

fit.sem.prune <- sem(sem.prune, data = seabloom)
summary(fit.sem.prune)

