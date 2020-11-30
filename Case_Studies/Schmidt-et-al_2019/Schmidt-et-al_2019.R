## Project: Structural Equation Modelling Workshop - From Theory to Practice
## Script purpose: Reproduction of a case study (Schmidt et. al., 2019)
## Date: 10/11/2020
## Author: Fabienne Wiederkehr

# Full citation of study: Schmidt, T.S., Van Metre, P.C., Carlisle, D.M. (2019). Linking the Agricultural Landscape of the Midwest to Stream Health with Structural Equation Modeling. Environ. Sci. Technol. 53. 452-462. https://doi.org/10.1021/acs.est.8b04381
# Data set available at: https://www.sciencebase.gov/catalog/item/5b463148e4b060350a15a836

## Preparation ####
# Set working directory, load packages and data
setwd("~/Git_Projects/Swiss_SEM/Case_Studies/Schmidt-et-al_2019")
require(psych)
require(data.table)
require(lavaan)
require(semPlot)
require(AICcmodavg)
dat <- read.csv("Schmidt-et-al_2019.csv", sep = ";")

# Data transformations
dat2 <- dat
dat2$BasinArea <- log10(dat$BasinArea) #Basin Area
dat2$Bank <- log10(dat$Bank + 1) #Channel Erosion
dat2$PctCanopy <- asin(sqrt(dat$PctCanopy/100)) #Percent Canopy
dat2$RBS <- dat$RBS^(1/4) #Relative Bed Stability
dat2$Bifenthrin <- log10(dat$Bifenthrin + 1) #Bifenthrin
dat2$imidacloprid <- log10(dat$imidacloprid + 1) #Imidacloprid
dat2$pyrethroid_deg_sum <- log10(dat$pyrethroid_deg_sum + 1) #Pyrethroid degradates
dat2$median.of.sumconcn_triazH <- log10(dat$median.of.sumconcn_triazH + 1) #Triazine Herbizides
dat2$NH3_MED_42DAY <- log10(dat$NH3_MED_42DAY+1) #Ammonia
dat2$TN_MED_42DAY <- log10(dat$TN_MED_42DAY) #Total Nitrogen
dat2$TP_MED_42DAY <- log10(dat$TP_MED_42DAY) #Total Phosphorus
dat2$Tmax42.C.OBSYN <- log10(dat$Tmax42.C.OBSYN)
dat2$BC_2.RelAbun <- (dat$BC_2.RelAbun*100)^(1/4)
dat2$BC_3.RelAbun <- (dat$BC_3.RelAbun*100)^(1/2)
dat2$BC_4.RelAbun <- (dat$BC_4.RelAbun*100)^(1/2)

# Data centring and standardisation
dat3 <- data.table(scale(dat2[,-1]))
dat3$TSTAID <- dat2$TSTAID

## Stressor Model ####
# Step 1: Specify model
mod.stressor <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  imidacloprid ~ UrbanTotal2011 + SandContent + AgricultureTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'

# Step 2: Estimate model
fit.mod.stressor <- sem(mod.stressor, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")

# Step 3: Extract results
summary(fit.mod.stressor, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
standardizedSolution(fit.mod.stressor)

# Request modification indices
mi.stressor <- modindices(fit.mod.stressor); print(mi.stressor[mi.stressor$mi > 4.0,])

# Bootstrap
fit.mod.stressor.bs <- sem(fit.mod.stressor, data = dat3, test = "bollen.stine", se = "boot", bootstrap = 1000, fixed.x = FALSE)
standardizedSolution(fit.mod.stressor.bs)

# Derive covariance matrix
lavInspect(mod.stressor.bs, what="sampstat")

## Structural Equation Model ####
mod.stressor.2<-'
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011 + UrbanTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  imidacloprid ~ UrbanTotal2011 + SandContent + AgricultureTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'

fit.mod.stressor.2<-sem(mod.stressor.2,data=dat3, missing="listwise", fixed.x=FALSE,estimator="MLM")

mod.stressor.3<-'
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  imidacloprid ~ UrbanTotal2011 + SandContent + AgricultureTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy + UrbanTotal2011
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
' # note that we renamed dat3$median.of.sumconcn_triazH to dat3$triazH for simplicity

fit.mod.stressor.3 <- sem(mod.stressor.3, data = dat3, fixed.x = FALSE, estimator = "MLM")
aictab(list(fit.mod.stressor,fit.mod.stressor.2,fit.mod.stressor.3),c("Model 1","Model 2","Model 3"))


# Plot
semPaths(fit.mod.stressor,whatLabels="std",layout="tree",residuals = FALSE, sizeMan=9, sizeMan2=4)
semPaths(fit.mod.stressor,whatLabels="std",layout="circle",residuals = FALSE, sizeMan=9, sizeMan2=4)

labels<-c("Urban Land Use","Agriculture Land Use","Channel Erosion","Riparian Forest","Percent Canopy","Relative Bed Stability","Ammonia","Bifenthrin","Pyrethroid degradates","Imidacloprid","Total Nitrogen","Total Phosphorus","Triazine Herbizides","Max Temperature","Sand Content","Basin Area")
layout<-matrix(c(1.5,4, 4.5,4, 1,3, 2.5,3, 7,3, 2,2, 6,2, 1,1, 2,1, 3,1, 4,1, 5,1, 6,1, 7,1, 3,4, 6,4), nrow=16, ncol=2, byrow=TRUE)
semPaths(fit.mod.stressor, whatLabels="std",layout=layout, nodeLabels=labels, residuals=FALSE)
# Box size
semPaths(fit.mod.stressor,whatLabels="std",layout=layout,nodeLabels=labels,residuals = FALSE, sizeMan=9, sizeMan2=4)
# Same text size
semPaths(fit.mod.stressor,whatLabels="std",layout=layout,nodeLabels=labels,residuals = FALSE, sizeMan=9, sizeMan2=4, label.scale=FALSE)
# Smaller text
semPaths(fit.mod.stressor,whatLabels="std",layout=layout,nodeLabels=labels,residuals = FALSE, sizeMan=9, sizeMan2=4, label.scale=FALSE, label.cex=0.8)
# Smaller edges
semPaths(fit.mod.stressor,whatLabels="std",layout=layout,nodeLabels=labels,residuals = FALSE, sizeMan=9, sizeMan2=4, label.scale=FALSE, label.cex=0.8, edge.label.cex = 0.4)

## Algal Model ####
# Step 1: Specify model
mod.algae <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  imidacloprid ~ UrbanTotal2011 + SandContent + AgricultureTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'

# Step 2: Estimate model
fit.mod.algae <- sem(mod.algae, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")

# Step 3: Extract results
standardizedSolution(fit.mod.algae)

# Trim Algae ~ SandContent
mod.algae.2 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  imidacloprid ~ UrbanTotal2011 + SandContent + AgricultureTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum
          + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.2 <- sem(mod.algae.2, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.2)

# Trim Algae ~ r50_ForestTotal2011
mod.algae.3 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  imidacloprid ~ UrbanTotal2011 + SandContent + AgricultureTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + BasinArea + Bank + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + imidacloprid
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.3 <- sem(mod.algae.3, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.3)

# Trim Algae ~ imidacloprid
mod.algae.4 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + BasinArea + Bank + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.4 <- sem(mod.algae.4, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.4)

# Trim Algae ~ BasinArea
mod.algae.5 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + Bank + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + TN_MED_42DAY + TP_MED_42DAY
          + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.5 <- sem(mod.algae.5, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.5)

# Trim Algae ~ pyrethroid_deg_sum
mod.algae.6 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + Bank + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
          + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.6 <- sem(mod.algae.6, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.6)

# Trim Algae ~ RBS
mod.algae.7 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + Bank + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
          + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.7 <- sem(mod.algae.7, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.7)

# Trim NH3_MED_42DAY ~ r50_ForestTotal2011
mod.algae.8 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + Bank + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
          + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.8 <- sem(mod.algae.8, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.8)

# Trim Algae ~ UrbanTotal2011
mod.algae.9 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ AgricultureTotal2011 + Bank + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.9 <- sem(mod.algae.9, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.9)

# Trim Algae ~ AgricultureTotal2011
mod.algae.10 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.10 <- sem(mod.algae.10, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.10)

# Trim NH3_MED_42DAY ~ BasinArea
mod.algae.11 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.11 <- sem(mod.algae.11, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.11)

# Trim Algae ~ NH3_MED_42DAY
mod.algae.12 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank + PctCanopy + Bifenthrin + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.12 <- sem(mod.algae.12, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.12)

# Trim Algae ~ Bifenthrin
mod.algae.13 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank + PctCanopy + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.13 <- sem(mod.algae.13, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.13)

# Request modification indices
mi.algae.13 <- modindices(fit.mod.algae.13); print(mi.algae.13[mi.algae.13$mi > 4.0,])

# Add TP_MED_42DAY ~~ median.of.sumconcn_triazH
mod.algae.14 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank + PctCanopy + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
  TP_MED_42DAY ~~ median.of.sumconcn_triazH
'
fit.mod.algae.14 <- sem(mod.algae.14, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.14)

# Compare stressor models
aictab(list(fit.mod.algae.13, fit.mod.algae.14), c("Model 13", "Model 14"))

# Bootstrap
set.seed(301)
fit.mod.algae.14.bs<-sem(fit.mod.algae.14, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.algae.14.bs)

# Trim Algae ~ Bank
mod.algae.15 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ PctCanopy + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
  TP_MED_42DAY ~~ median.of.sumconcn_triazH
'
fit.mod.algae.15 <- sem(mod.algae.15, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.15)

# Trim Algae ~ TN_MED_42DAY
mod.algae.16 <- '
  # Latent Construct
  Algae =~ BC_2.RelAbun + BC_3.RelAbun + BC_4.RelAbun

  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ PctCanopy + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TP_MED_42DAY ~~ median.of.sumconcn_triazH
'
fit.mod.algae.16 <- sem(mod.algae.16, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.algae.16)

# Compare stressor models
fitMeasures(fit.mod.algae.15, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))
fitMeasures(fit.mod.algae.16, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))

# Bootstrap
set.seed(301)
fit.mod.algae.16.bs<-sem(fit.mod.algae.16, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.algae.16.bs)

# Derive covariance matrix
lavInspect(mod.stressor.bs, what="sampstat")