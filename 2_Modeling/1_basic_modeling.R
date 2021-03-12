## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------
rm(list = ls())
seabloom <- read.table("~/Swiss_SEM/2_Modeling/Data_preparation/seabloom-2020-ele-dryad-data/cdr-e001-e002-output-data.csv",
                       sep = ",", header = TRUE)


## ----------------------------------------------------------------------------------
dim(seabloom)
str(seabloom)

table(seabloom$nadd)
table(seabloom$disk)


## ----------------------------------------------------------------------------------
panel.points <- function(x, y) {
  points(x, y, cex = 0.1)
  abline(lm(y ~ x), lty = 2, col = "red")
  }

pairs(seabloom[, -c(1:3, 5:6, 8:10, 12, 16)],
      lower.panel = NULL, upper.panel = panel.points)


## ----------------------------------------------------------------------------------
seabloom <- seabloom[seabloom$year == 2000, ]
dim(seabloom)


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
round(cor(seabloom[, c(4, 7, 13:15)]), digits = 2)


## ----------------------------------------------------------------------------------
simple <-
"mass.above ~ nadd + disk + rich + even
rich ~ nadd
even ~ nadd"

fit.simple <- sem(simple, data = seabloom)


## ----------------------------------------------------------------------------------
varTable(fit.simple)


## ----------------------------------------------------------------------------------
seabloom[, c(4, 7, 13:15)] <- apply(seabloom[, c(4, 7, 13:15)],  2, scale)
boxplot(seabloom[, c(4, 7, 13:15)])


## ----------------------------------------------------------------------------------
fit.simple <- sem(simple, data = seabloom)
summary(fit.simple, fit.measures = TRUE, rsq = TRUE)


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
modindices(fit.simple.up, minimum.value = 0)


## ----------------------------------------------------------------------------------
library("lavaanPlot")

lavaanPlot(model = fit.simple.up,
           node_options = list(shape = "box", color = "gray",
                               fontname = "Helvetica"),
           edge_options = list(color = "black"),
           coefs = TRUE, covs = TRUE, stars = c("covs", "regress"))


## ----------------------------------------------------------------------------------
library("lavaan.survey")

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
sem.prune <-
"mass.above ~ nadd + rich
rich ~ nadd
even ~ nadd

rich ~~ even"

fit.sem.prune <- sem(sem.prune, data = seabloom)
summary(fit.sem.prune)

