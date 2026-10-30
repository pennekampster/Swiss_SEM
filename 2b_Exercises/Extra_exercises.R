# Exercise 2 indicator latent variables
cor.test(seabloom$precip.gs, seabloom$precip.mm)
plot(seabloom$precip.gs, seabloom$precip.mm)

seabloom$precip.mm_std <- (mean(seabloom$precip.mm)-seabloom$precip.mm) / sd(seabloom$precip.mm)
seabloom$precip.gs_std <- (mean(seabloom$precip.gs)-seabloom$precip.gs) / sd(seabloom$precip.gs)

# CFA
lv <-"
# Latent variable definition
climate =~ lambda*precip.mm_std + lambda*precip.gs_std
"

fit.lv <- cfa(lv, data = seabloom, estimator = "MLM")
summary(fit.lv, standardized=T)
modindices(fit.lv, minimum.value = 3.84)

# Full model
lv <-"
# Latent variable definition
climate =~ lambda*precip.mm_std + lambda*precip.gs_std

mass.above ~ nadd + disk + climate
rich ~ nadd + climate + disk
even ~ nadd + climate + disk"

fit.lv <- sem(lv, data = seabloom, estimator = "MLM")
summary(fit.lv, standardized=T)
modindices(fit.lv, minimum.value = 3.84)


# Exercise 3: construct composite variables

comp2 <- "
landuse2 <~   nadd + 1 * disk

rich ~ landuse2 + precip.mm
even ~ landuse2 + precip.mm

mass.above ~ landuse2 + rich + even + precip.mm

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom, )
summary(fit.comp2, standardize=T)


comp2 <- "
landuse2 <~  0.06544565 * nadd + 0.4466863 * disk

rich ~ landuse2 + precip.mm
even ~ landuse2 + precip.mm

mass.above ~ landuse2 + rich + even + precip.mm

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom)
summary(fit.comp2, standardize = T)







# Composite variables are convenient to model nonlinear relationships

ggplot(data=seabloom, aes(precip.gs, mass.above)) + geom_point() + 
  stat_smooth(method="lm", formula = "y ~ poly(x, 2)", se=F) + 
  stat_smooth(method="lm", formula = "y ~ x", colour="red")

# Exercise to construct composite variables

seabloom$precip.gs.std <- (mean(seabloom$precip.gs)-seabloom$precip.gs) / sd(seabloom$precip.gs)
seabloom$precip.gs.sq.std <- seabloom$precip.gs.std ^ 2

# Fit nonlinear composite in one step

comp2 <- "

precip_tot <~  precip.gs.sq.std + precip.gs.std 

rich ~ nadd + precip_tot
even ~ nadd + precip_tot

mass.above ~ rich + even + precip_tot + nadd

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom)
summary(fit.comp2, standardize = T)


# Fit nonlinear composite in two steps

comp2 <- "mass.above ~  precip.gs.sq.std + precip.gs.std"

fit.comp2 <- sem(comp2, data = seabloom)
summary(fit.comp2, standardize = T)


comp2 <- "

precip_tot <~  -0.216 * precip.gs.sq.std + -0.277 * precip.gs.std 

rich ~ nadd + precip_tot
even ~ nadd + precip_tot

mass.above ~ rich + even + precip_tot + nadd

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom)
summary(fit.comp2, standardize = T, fit.measures=T)



# compare with model that includes the squared term
comp2 <- "

rich ~ nadd + precip.gs.std + precip.gs.sq.std
even ~ nadd + precip.gs.std

mass.above ~ rich + even + precip.gs.std + precip.gs.sq.std + nadd

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom)
summary(fit.comp2, standardize = T, fit.measures=T)



# compare with model without the squared term
comp2 <- "

rich ~ nadd + precip.gs.std
even ~ nadd + precip.gs.std

mass.above ~ rich + even + precip.gs.std + nadd

rich ~~ even"

fit.comp2 <- sem(comp2, data = seabloom)
summary(fit.comp2, standardize = T, fit.measures=T)



# ============================================================
# Exercise: two hand-coded composites
#   (1) a nonlinear (saturating) nutrient effect
#   (2) a diversity composite from richness + evenness
# ============================================================

# ---- Composite 1: nonlinear effect of nutrients ------------------------
# Centre nadd before squaring (reduces collinearity between the linear and
# quadratic terms). Check first that the curvature is real:
seabloom$nadd.c  <- seabloom$nadd - mean(seabloom$nadd)
seabloom$nadd.c2 <- seabloom$nadd.c^2
anova(lm(mass.above ~ nadd.c,            data = seabloom),
      lm(mass.above ~ nadd.c + nadd.c2,  data = seabloom))
# -> adding nadd^2 is highly significant (F = 25, p = 1e-6): biomass saturates
#    with nutrient addition rather than rising linearly.

# Build the composite by hand: weights = coefficients of that regression.
w.nut <- coef(lm(mass.above ~ nadd.c + nadd.c2, data = seabloom))
seabloom$nutrient <- w.nut["nadd.c"] * seabloom$nadd.c +
  w.nut["nadd.c2"] * seabloom$nadd.c2   # one "nutrient" axis

# ---- Composite 2: diversity (richness + evenness) ----------------------
# Standardise both indicators first so they share a common scale (richness is
# a count, evenness is 0-1). Note they are strongly NEGATIVELY correlated:
seabloom$rich.z <- as.numeric(scale(seabloom$rich))
seabloom$even.z <- as.numeric(scale(seabloom$even))
cor(seabloom$rich.z, seabloom$even.z)                      # ~ -0.71

w.div <- coef(lm(mass.above ~ rich.z + even.z, data = seabloom))
seabloom$diversity <- w.div["rich.z"] * seabloom$rich.z +
  w.div["even.z"] * seabloom$even.z    # one "diversity" axis

# ---- SEM with the two composites ---------------------------------------
two.comp <- "mass.above ~ nutrient + diversity"
fit.two.comp <- sem(two.comp, data = seabloom, estimator = "MLM")
summary(fit.two.comp, standardized = TRUE, rsq = TRUE)

# ---- Extension: nutrients also shape diversity (mediation) -------------
two.comp.med <- "mass.above ~ nutrient + diversity
                 diversity  ~ nadd.c"
fit.med <- sem(two.comp.med, data = seabloom, estimator = "MLM")
summary(fit.med, standardized = TRUE, rsq = TRUE)
