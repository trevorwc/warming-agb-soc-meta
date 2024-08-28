# This script conducts the central meta-analysis,
# as well as the heterogeneity analysis

library(openxlsx)
library(dplyr)
library(meta)
library(metafor)
library(ggplot2)

##### Analysis #####
set.seed(1)

# Read in the dataset
paired_AGBSOC <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/metaAGBSOCECO_8-28-24.xlsx') 

# Create a custom color palette 
agbsoc_colors <- c("AGB" = "#117733",
"SOC" = "#582707")

# this is where the actual analysis occurs
meta_analysisAGB <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_AGBSOC,
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)

meta_analysisSOC <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_AGBSOC,
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)


meta_analysisECO <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_AGBSOC,
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)

# Now specifically look at the sites with high vs. low N 
meta_analysisAGB_Nlim <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_AGBSOC %>% subset(N == "Low"),
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)

meta_analysisAGB_Nhigh <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_AGBSOC %>% subset(N == "High"),
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)


pairedFull <- paired_AGBSOC[!apply(paired_AGBSOC[,c("DT", "Ecosystem", "yi.AGB", "yi.SOC", "SOC.C", "MAT", "MAP", "Elevation", "avg_SCN", "Warming.Method", "Aridity", "phh2o_0_15", "clay_0_15")], 1, anyNA),]

corrplot(cor(pairedFull[,c("MAT", "MAP",  "avg_SCN", "DT", "Aridity", "clay_0_15", "phh2o_0_15", "Elevation")]))

mf <- MetaForest(
  yi.AGB ~ Ecosystem + DT + clay_0_15 + MAP + MAT + N, 
  v = "vi.AGB",
  data = pairedFull, 
  whichweights = "random",
  num.trees = 10000
)

plot(mf)

pre.mf <- preselect(mf, replications = 100, algorithm = "bootstrap")

preselect_vars(pre.mf, cutoff = 0.95)
# Output: "DT"  "MAP" "MAT"

VarImpPlot(mf)

mf1 <- MetaForest(
  yi.AGB ~  DT + MAP + MAT, #DT + MAP +clay_0_15,
  v = "vi.AGB",
  data = paired_AGBSOC %>% subset(MAT > -999 & MAP > -999), 
  whichweights = "random",
  num.trees = 10000
)
# Save the metaforest object here for future use in upscaling results globally
#saveRDS(mf1, file = "/Users/trevor/Desktop/Research/Warming Ecosystem C/Code/metaforest1.rds")

# Evaluate performance 
p <- data.frame(predict(mf1, paired_AGBSOC))
paired_AGBSOC$prediction <- p$prediction
# RSQ
rsq <- cor(paired_AGBSOC$yi.AGB, p)^2
rsq <- summary(lm(prediction ~ yi.AGB, data = paired_AGBSOC))$r.squared
# the metaforest gets 69% on the training  
print(mf1)
# 40.36% on "test"
# MAE: 
mean(abs(paired_AGBSOC$yi.AGB- p$prediction))

##### Figures #####

