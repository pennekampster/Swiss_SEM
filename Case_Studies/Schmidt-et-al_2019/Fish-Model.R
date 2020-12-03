## Fish Model ####
mod.fish <- '
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
  MMI_NRSA ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish <- sem(mod.fish, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish)

# Trim MMI_NRSA ~ PctCanopy
mod.fish.2 <- '
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
  MMI_NRSA ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.2 <- sem(mod.fish.2, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.2)

# Trim MMI_NRSA ~ BasinArea
mod.fish.3 <- '
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
  MMI_NRSA ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + Bank + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.3 <- sem(mod.fish.3, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.3)

# Trim MMI_NRSA ~ Bank
mod.fish.4 <- '
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
  MMI_NRSA ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.4 <- sem(mod.fish.4, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.4)

# Trim MMI_NRSA ~ UrbanTotal2011
mod.fish.5 <- '
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
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.5 <- sem(mod.fish.5, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.5)

# Trim NH3_MED_42DAY ~ r50_ForestTotal2011
mod.fish.6 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  imidacloprid ~ UrbanTotal2011 + SandContent + AgricultureTotal2011
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.6 <- sem(mod.fish.6, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.6)

# Trim MMI_NRSA ~ imidacloprid
mod.fish.7 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  NH3_MED_42DAY ~ BasinArea
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.7 <- sem(mod.fish.7, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.7)

# Trim NH3_MED_42DAY ~ BasinArea
mod.fish.8 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  PctCanopy ~ BasinArea + r50_ForestTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.8 <- sem(mod.fish.8, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.8)

# Trim MMI_NRSA ~ pyrethroid_deg_sum
mod.fish.9 <- '
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
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + NH3_MED_42DAY + Bifenthrin
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.9 <- sem(mod.fish.9, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.9)

# Trim MMI_NRSA ~ NH3_MED_42DAY
mod.fish.10 <- '
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
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + Bifenthrin
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.10 <- sem(mod.fish.10, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.10)

# Trim MMI_NRSA ~ Tmax42.C.OBSYN
mod.fish.11 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + Bifenthrin
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.11 <- sem(mod.fish.11, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.11)

# Trim TP_MED_42DAY ~ RBS
mod.fish.12 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + Bifenthrin
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.12 <- sem(mod.fish.12, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.12)

# Trim TP_MED_42DAY ~ UrbanTotal2011
mod.fish.13 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + Bifenthrin
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.13 <- sem(mod.fish.13, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.13)

# Trim TP_MED_42DAY ~ AgricultureTotal2011
mod.fish.14 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ SandContent + r50_ForestTotal2011
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS + Bifenthrin
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.14 <- sem(mod.fish.14, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.14)

# Request modification indices
mi.fish.14 <- modindices(fit.mod.fish.14); print(mi.fish.14[mi.fish.14$mi > 4.0,])

# Bootstrap
set.seed(301)
fit.mod.fish.14.bs<-sem(fit.mod.fish.14, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.fish.14.bs)

# Trim MMI_NRSA ~ AgricultureTotal2011
mod.fish.15 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ SandContent + r50_ForestTotal2011
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + r50_ForestTotal2011 + RBS + Bifenthrin
          + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.fish.15 <- sem(mod.fish.15, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.15)

# Trim MMI_NRSA ~ TN_MED_42DAY
mod.fish.16 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ SandContent + r50_ForestTotal2011
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + r50_ForestTotal2011 + RBS + Bifenthrin
          + TP_MED_42DAY + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.fish.16 <- sem(mod.fish.16, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.16)

# Trim MMI_NRSA ~ TP_MED_42DAY
mod.fish.17 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + r50_ForestTotal2011 + RBS + Bifenthrin
          + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.fish.17 <- sem(mod.fish.17, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.17)

# Request modification indices
mi.fish.17 <- modindices(fit.mod.fish.17); print(mi.fish.17[mi.fish.17$mi > 4.0,])

# Bootstrap
set.seed(500) #result depends on seed, with seed 301 for example, we would chose this model
fit.mod.fish.17.bs<-sem(fit.mod.fish.17, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.fish.17.bs)

# Trim MMI_NRSA ~ Bifenthrin
mod.fish.18 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  MMI_NRSA ~ SandContent + r50_ForestTotal2011 + RBS
          + median.of.sumconcn_triazH
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.fish.18 <- sem(mod.fish.18, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.18)

# Trim MMI_NRSA ~ median.of.sumconcn_triazH
mod.fish.19 <- '
  # Regressions
  UrbanTotal2011 ~ SandContent
  AgricultureTotal2011 ~ SandContent + BasinArea
  Bank ~ UrbanTotal2011 + SandContent
  r50_ForestTotal2011 ~ UrbanTotal2011 + AgricultureTotal2011
  RBS ~ UrbanTotal2011 + Bank + r50_ForestTotal2011
  MMI_NRSA ~ SandContent + r50_ForestTotal2011 + RBS
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.fish.19 <- sem(mod.fish.19, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.fish.19)

# Request modification indices
mi.fish.19 <- modindices(fit.mod.fish.19); print(mi.fish.19[mi.fish.19$mi > 4.0,])

# Bootstrap
set.seed(301)
fit.mod.fish.19.bs<-sem(fit.mod.fish.19, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.fish.19.bs)
