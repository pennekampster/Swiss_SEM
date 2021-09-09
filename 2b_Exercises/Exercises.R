# First exercise after fitting first SEM:

# Let's test whether the effect of disturbance is mediated via its
# effect on richness and eveness, rather than directly on biomass
# add paths from disk to rich and even, remove the path to mass.above
# compare model fit to simple model
# what do you conclude?

modindices(fit.simple.up, minimum.value = 0.01)

simple.exc <-
"mass.above ~ nadd + rich + even + precip.mm 
rich ~ nadd + precip.mm + disk
even ~ nadd + precip.mm +  disk
rich ~~ even"

fit.simple.exc <- sem(simple.exc, data = seabloom, estimator = "MLM")
summary(fit.simple.exc, fit.measures = TRUE, rsq = TRUE)
modindices(fit.simple.exc, minimum.value = 0.01)

AIC(fit.simple.up, fit.simple.exc)



