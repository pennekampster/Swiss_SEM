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

# Compare models
aictab(list(fit.mod.invert, fit.mod.invert.2), c("Model 1", "Model 2"))

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

# Compare models
fitMeasures(fit.mod.invert.2, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))
fitMeasures(fit.mod.invert.3, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))

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

# Compare models
fitMeasures(fit.mod.invert.3, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))
fitMeasures(fit.mod.invert.4, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))

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

# Compare models
aictab(list(fit.mod.invert.4, fit.mod.invert.5), c("Model 4", "Model 5"))

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

# Compare models
fitMeasures(fit.mod.invert.5, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))
fitMeasures(fit.mod.invert.6, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))

# Trim NH3_MED_42DAY ~ BasinArea
mod.invert.7 <- '
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
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_BENT ~ SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.invert.7 <- sem(mod.invert.7, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.7)

# Trim MMI_BENT ~ AgricultureTotal2011
mod.invert.8 <- '
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
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_BENT ~ SandContent + BasinArea + Bank + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.invert.8 <- sem(mod.invert.8, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.8)

# Trim MMI_BENT ~ Bank
mod.invert.9 <- '
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
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  MMI_BENT ~ SandContent + BasinArea + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.invert.9 <- sem(mod.invert.9, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.9)

# Trim MMI_BENT ~ Tmax42.C.OBSYN
mod.invert.10 <- '
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
  MMI_BENT ~ SandContent + BasinArea + r50_ForestTotal2011 + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.invert.10 <- sem(mod.invert.10, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.10)

# Request modification indices
mi.invert.10 <- modindices(fit.mod.invert.10); print(mi.invert.10[mi.invert.10$mi > 4.0,])

# Bootstrap
set.seed(301)
fit.mod.invert.10.bs<-sem(fit.mod.invert.10, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.invert.10.bs)

# Trim MMI_BENT ~ NH3_MED_42DAY
mod.invert.11 <- '
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
  MMI_BENT ~ SandContent + BasinArea + r50_ForestTotal2011 + PctCanopy + RBS + Bifenthrin
          + pyrethroid_deg_sum + TN_MED_42DAY
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
'
fit.mod.invert.11 <- sem(mod.invert.11, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
standardizedSolution(fit.mod.invert.11)

# Request modification indices
mi.invert.11 <- modindices(fit.mod.invert.11); print(mi.invert.11[mi.invert.11$mi > 4.0,])

# Bootstrap
set.seed(301)
fit.mod.invert.11.bs<-sem(fit.mod.invert.11, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.invert.11.bs)
