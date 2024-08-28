# This script conducts the central meta-analysis,
# as well as the heterogeneity analysis

library(openxlsx)
library(dplyr)
library(meta)
library(metafor)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(plotbiomes)
library(corrplot)
library(metaforest)

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

# Figure 1 
mean_predint <- data.frame(
  Pool = c("AGB", "SOC"),
  Mean = c(0.0865, -0.0042),
  Mean.L = c(0.0366,-0.0306 ),
  Mean.U = c(0.1365, 0.0222),
  P.L = c(0.0865+1.96*0.2567,-0.0042+1.96*0.1057 ),
  P.U = c(0.0865-1.96*0.2567,-0.0042-1.96*0.1057 )
)

effect_size_eco_df <- data.frame(
  "Ecosystem" = c("Shrub/Heath", "Tundra", "Grassland", "Farmland", "Forest", "Shrub/Heath", "Tundra", "Grassland", "Farmland", "Forest"),
  "Mean" = c(0.0784,0.3462, 0.0480, 0.0669, 0.2871, -0.0641 , 0.0307, -0.0060, -0.0038, 0.1220),
  "LowerCI" = c(-0.1044, 0.1986, -0.0152,  0.0186, 0.0709, -0.2876, -0.0651, -0.0388, -0.0085, -0.0929),
  "UpperCI" = c(0.2612, 0.4938, 0.1112, 0.1153, 0.5032, 0.1595,0.1264,  0.0268,0.0009, 0.3370),
  "Pool" = c("AGB", "AGB","AGB","AGB","AGB","SOC","SOC","SOC","SOC","SOC")
)

effect_size_temp_df <- data.frame(
  "Temperature" = c("< 1.5", "1.5 - 2.5", "2.5 - 4.5", "> 4.5", "< 1.5", "1.5 - 2.5", "2.5 - 4.5", "> 4.5"),
  "Mean" = c(-0.0048,0.0715, 0.2751, 0.5958, -0.0164, -0.0017, -0.0014, 0.0247),
  "LowerCI" = c(-0.0822,  0.0143, 0.1484, 0.3733,-0.0528,-0.0455, -0.0072,  -0.2194),
  "UpperCI" = c(0.0726, 0.1287, 0.4018, 0.8183, 0.0199, 0.0421, 0.0043, 0.2688),
  "Pool" = c("AGB", "AGB","AGB","AGB","SOC","SOC","SOC","SOC")
)

paired_AGBSOC_long <- paired_AGBSOC %>%
  gather(key = "Type", value = "EffectSize", yi.AGB, yi.SOC) %>%
  mutate(Pool = ifelse(Type == "yi.AGB", "AGB", "SOC"))

means_rema <- ggplot(mean_predint, aes(x = Mean, y = as.factor(Pool), color = Pool )) +
  geom_vline(xintercept = 0, color = "grey") +
  xlim(-1, 1) +
  geom_pointrange(aes( x = Mean, xmin = Mean.L, xmax = Mean.U), size = 0.5) +
  geom_errorbar(aes(xmin = Mean.L, xmax = Mean.U), width = 0.1)+
  geom_jitter(data = paired_AGBSOC_long, aes(x = EffectSize, y = as.factor(Pool)), size = 0.5, alpha = 0.25) +
  #geom_errorbar(aes(xmin = P.L, xmax = P.U), width = 0, position = position_dodge(width = -0.5)) +
  scale_color_manual(values = agbsoc_colors) +
  theme_light() +
  theme(text = element_text(size = 12)) +
  labs(x = "Effect Size", y = "") +
  theme(legend.position = "none") +
  stat_function(fun = dnorm, args = list(mean = 0.0865, sd = 0.2567),
                color = "#117733", geom = "line", linewidth = 0.5, alpha = 0.5, n = 10000) +
  stat_function(fun = dnorm, args = list(mean = -0.0042, sd = 0.1057), 
                color = "#582707", geom = "line", linewidth = 0.5, alpha = 0.5, n = 10000) 

plteco <- ggplot(effect_size_eco_df, aes(y = Mean, x = Ecosystem, color = Pool )) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = `LowerCI`, ymax =  `UpperCI`), width = 0.2, position = position_dodge(width = -0.5))+
  geom_point(position = position_dodge(width = -0.5), size = 2.5) +
  scale_color_manual(values = agbsoc_colors) +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  ylab("Effect Size") +
  xlab("")

plttmp<- ggplot(effect_size_temp_df, aes(y = Mean, x = Temperature, color = Pool )) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = `LowerCI`, ymax =  `UpperCI`), width = 0.2, position = position_dodge(width = -0.5))+
  geom_point(position = position_dodge(width = -0.5), size = 2.5) +
  scale_color_manual(values = agbsoc_colors) +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  xlab("Warming") +
  ylab("Effect Size")

grid.arrange(means_rema, 
             arrangeGrob(plteco,plttmp, ncol = 1), ncol = 2)

# Figure 2

yi.AGB_navail_density <- ggplot(paired_AGBSOC %>% subset(avg_SCN > -999), aes(x = yi.AGB, color = N )) +
  geom_density()+
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  ylab("Density") +
  xlab("yi.AGB") +
  labs(color = "N") +
  theme_light()

N_df <- data.frame(
  "N Availability" = c("Low", "High", "Low", "High"),
  "Mean" =c(0.1897, 0.0537, -0.0464, -0.0003 ),
  "LowerCI" =c( 0.0761,-0.0076, -0.1126 , -0.0262),
  "UpperCI" =c(0.3033, 0.1151, 0.0197, 0.0255),
  "Pool" = c("AGB", "AGB", "SOC", "SOC")
)

pltN <- ggplot(N_df, aes(y = Mean, x = `N.Availability`, color = Pool )) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = `LowerCI`, ymax =  `UpperCI`), width = 0.2, position = position_dodge(width = -0.5))+
  geom_point(position = position_dodge(width = -0.5), size = 2.5) +
  scale_color_manual(values = agbsoc_colors) +
  theme_light() +
  theme(text = element_text(size = 12)) +
  xlab("N Availability") +
  ylab("Effect Size")

dt_n <- ggplot(paired_AGBSOC %>% subset(avg_SCN > -999), aes(x = DT.cat, y = yi.AGB, color =N )) +
  geom_boxplot() +
  theme_light()

grid.arrange(pltN, grid.arrange(yi.AGB_navail_density,dt_n,  ncol =1), ncol = 2)

# Figure 3

plt_agb_soc_eco <- ggplot(paired_AGBSOC, aes(x = yi.AGB, y = yi.SOC, size = vi.ECO)) +
  geom_vline(aes(xintercept = 0), color = "black") +
  geom_hline(aes(yintercept =0), color = "black")+
  geom_point(shape = 21, aes(fill = yi.ECO), color = "black", stroke = 1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "yi.ECO", na.value = "grey50") +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  theme_light()

SOC_yi.AGB_df <- data.frame(
  "AGB Response" = c("AGB Increase", "AGB Decrease"),
  "Mean" =c(0.0194, -0.0531),
  "LowerCI" =c( -0.0151,-0.0881),
  "UpperCI" =c( 0.0539, -0.0181),
  "Pool" = c("SOC", "SOC")
)

pltSOCyi <- ggplot(SOC_yi.AGB_df, aes(y = Mean, x = `AGB.Response`, color = Pool )) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = `LowerCI`, ymax =  `UpperCI`), width = 0.2, position = position_dodge(width = -0.5))+
  geom_point(position = position_dodge(width = -0.5), size = 2.5) +
  scale_color_manual(values = agbsoc_colors) +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  ylab("Effect Size") +
  xlab("") +
  theme_light() +
  theme(legend.position = "bottom")

grid.arrange(plt_agb_soc_eco,pltSOCyi,  ncol = 2, widths = c(2,1))

# Figure 4

list_partial <- PartialDependence(mf1, bw = FALSE, output = "list")
list_partial1 <- PartialDependence(mf1, bw = FALSE, moderator = "DT", output = "list")
list_partial2 <- PartialDependence(mf1, bw = FALSE, moderator = "MAP", output = "list")
list_partial3 <- PartialDependence(mf1, bw = FALSE, moderator = "MAT", output = "list")

rounded_rsq <- round(rsq, 3)

grid.arrange(
  ggplot(paired_AGBSOC, aes( x=  yi.AGB, y = prediction)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    xlim(-0.75,1) +
    ylim(-0.75,1) +
    xlab("AGB Log Response Ratio") +
    ylab("Predicted ~ MAT + MAP + DT") +
    annotate("text", x = -0.5, y = 0.8, label = paste("R^2 ==", rounded_rsq), parse = TRUE, color = "blue", size = 5) +
    theme_light(),grid.arrange(
      grid.arrange(list_partial[1][[1]], 
                   list_partial[2][[1]],
                   list_partial[3][[1]], ncol = 3),
      grid.arrange(list_partial1[2][[1]],
                   list_partial2[2][[1]], 
                   list_partial2[1][[1]], ncol = 3), ncol = 1), ncol = 2)

# Figure S1
whittaker_base_plot() +
  geom_point(data = paired_AGBSOC, aes(x = MAT, y = MAP*0.1)) +
  theme_minimal()

# Figure S2
VarImpPlot(mf)

# Figure S3

duragb <- ggplot(paired_AGBSOC, aes(x = Duration, y = yi.AGB)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  labs (y = "yi.AGB") +
  theme(text = element_text(size = 15))

dursoc <- ggplot(paired_AGBSOC, aes(x = Duration, y = yi.SOC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs (y = "yi.SOC") +
  theme(text = element_text(size = 15))

grid.arrange(duragb, dursoc, ncol = 2)

summary(lm(RR.AGB ~ Duration + MAT + MAP +  Magnitude, raw%>% subset(Treatment.Group == "W")))

summary(lm(RR.SOC ~ Duration + MAT + MAP +  Magnitude, raw%>% subset(Treatment.Group == "W")))



