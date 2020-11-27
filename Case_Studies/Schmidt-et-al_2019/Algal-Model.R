## DO AGAIN, BUT USE standardizedSolution() FOR SUMMARY OUTPUT

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
  Algae ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + BasinArea + Bank + r50_ForestTotal2011
          + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'

# Step 2: Estimate model
fit.mod.algae <- sem(mod.algae, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")

# Step 3: Extract results
summary(fit.mod.algae, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)


# Trim Algae ~ r50_ForestTotal2011
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
  Algae ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + BasinArea + Bank
          + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.2 <- sem(mod.algae.2, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim Algae ~ SandContent
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
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + BasinArea + Bank
          + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + imidacloprid + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  pyrethroid_deg_sum ~~ imidacloprid
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.3 <- sem(mod.algae.3, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.3, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

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
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + BasinArea + Bank
          + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.4 <- sem(mod.algae.4, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.4, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

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
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + Bank
          + PctCanopy + RBS + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
  
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.5 <- sem(mod.algae.5, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.5, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim Algae ~ RBS
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
  pyrethroid_deg_sum ~ UrbanTotal2011 + AgricultureTotal2011 + Bifenthrin
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + Bank
          + PctCanopy + NH3_MED_42DAY + Bifenthrin + pyrethroid_deg_sum + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.6 <- sem(mod.algae.6, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.6, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim Algae ~ pyrethroid_deg_sum
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
  Algae ~ UrbanTotal2011 + AgricultureTotal2011 + Bank
          + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.7 <- sem(mod.algae.7, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.7, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim Algae ~ UrbanTotal2011
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
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ AgricultureTotal2011 + Bank
          + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.8 <- sem(mod.algae.8, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.8, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim Algae ~ AgricultureTotal2011
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
  NH3_MED_42DAY ~ BasinArea + r50_ForestTotal2011
  Bifenthrin ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank
          + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.9 <- sem(mod.algae.9, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.9, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim NH3_MED_42DAY ~ r50_ForestTotal2011
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
  Algae ~ Bank
          + PctCanopy + NH3_MED_42DAY + Bifenthrin + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.10 <- sem(mod.algae.10, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.10, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim Algae ~ Bifenthrin
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
  NH3_MED_42DAY ~ BasinArea
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank
          + PctCanopy + NH3_MED_42DAY + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.11 <- sem(mod.algae.11, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.11, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Trim NH3_MED_42DAY ~ BasinArea
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
  TN_MED_42DAY ~ UrbanTotal2011 + AgricultureTotal2011 + RBS
  TP_MED_42DAY ~ UrbanTotal2011 + SandContent + AgricultureTotal2011 + r50_ForestTotal2011 + RBS
  median.of.sumconcn_triazH ~ SandContent + AgricultureTotal2011
  Tmax42.C.OBSYN ~ SandContent + BasinArea + PctCanopy
  Algae ~ Bank
          + PctCanopy + NH3_MED_42DAY + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
'
fit.mod.algae.12 <- sem(mod.algae.12, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.12, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Modification index
m.i <- modindices(fit.mod.algae.12); print(m.i[m.i$mi > 4.0,])

# Add TP_MED_42DAY ~~ median.of.sumconcn_triazH
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
  Algae ~ Bank
          + PctCanopy + NH3_MED_42DAY + TN_MED_42DAY
          + TP_MED_42DAY + median.of.sumconcn_triazH + Tmax42.C.OBSYN
          
  # Covariances
  UrbanTotal2011 ~~ AgricultureTotal2011
  TN_MED_42DAY ~~ TP_MED_42DAY
  TP_MED_42DAY ~~ median.of.sumconcn_triazH
'
fit.mod.algae.13 <- sem(mod.algae.13, data = dat3, missing = "listwise", fixed.x = FALSE, estimator = "MLM")
summary(fit.mod.algae.13, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Bootstrap
set.seed(301)
fit.mod.algae.13.bs<-sem(fit.mod.algae.13, data=dat3, test="bollen.stine", se="boot", bootstrap=1000, fixed.x=FALSE)
standardizedSolution(fit.mod.algae.13.bs)

# Compare stressor models
aictab(list(fit.mod.algae, fit.mod.algae.2, fit.mod.algae.3, fit.mod.algae.4, fit.mod.algae.5, fit.mod.algae.6, fit.mod.algae.7, fit.mod.algae.8, fit.mod.algae.9, fit.mod.algae.10, fit.mod.algae.11, fit.mod.algae.12, fit.mod.algae.13, fit.mod.algae.14), c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Model 11", "Model 12", "Model 13", "Model 14"))
fitMeasures(fit.mod.algae.3, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))
fitMeasures(fit.mod.algae.4, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "ifi"))
