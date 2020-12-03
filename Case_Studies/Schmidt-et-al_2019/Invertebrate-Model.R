## Invertebrate Model ####
mod.invert <- '
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
  MMI_BENT ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.invert <- sem(mod.invert, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert)

# Trim MMI_BENT ~ UrbanTotal2011
mod.invert.2 <- '
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
  MMI_BENT ~ SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.invert.2 <- sem(mod.invert.2, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.2)

# Trim MMI_BENT ~ TP_MED_42DAY
mod.invert.3 <- '
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
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_BENT ~ SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
'
fit.mod.invert.3 <- sem(mod.invert.3, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.3)

# Trim MMI_BENT ~ median.of.sumconcn_triazH
mod.invert.4 <- '
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
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_BENT ~ SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
'
fit.mod.invert.4 <- sem(mod.invert.4, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.4)

# Trim NH3_MED_42DAY ~ r50_ForestTotal2011
mod.invert.5 <- '
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
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_BENT ~ SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
'
fit.mod.invert.5 <- sem(mod.invert.5, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.5)

# Trim MMI_BENT ~ imidacloprid
mod.invert.6 <- '
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
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_BENT ~ SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.invert.6 <- sem(mod.invert.6, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.6)



aictab(list(fit.mod.invert.6, fit.mod.invert.7, fit.mod.invert.8, fit.mod.invert.9), c("Model 6", "Model 7", "Model 8", "Model 9"))
