#==== Analysis- paired datset ==================================================
 # Interested in also quantifying the effects in the directly paired measurements
 # Mostly want to re-run analysis, remake figures
 # The input data is a pre-matched dataset that has been ammended to 
 # include ancillary variables
 # 
 # Note path: users/

#==== Package Imports ==========================================================

library(openxlsx)
library(meta)
library(dplyr)
library(ggplot2)
library(cowplot)
library(MASS)
library(tidyr)


#===== Functions ===============================================================
categorize_temperature <- function(df) {
  df %>% mutate(
    maxWarming = pmax(`Air.DT`, `Soil.DT`, na.rm = TRUE),
    warmingCategory = case_when(
      maxWarming < 1.5 ~ "< 1.5",
      (maxWarming >= 1.5) & (maxWarming < 2.5) ~ "1.5 - 2.5",
      (maxWarming >= 2.5) & (maxWarming < 4.5) ~ "2.5 - 4.5",
      maxWarming >= 4.5 ~ "> 4.5"
    )
  )
}

standardize_ecosystem_type <- function(df) {
  col = "Ecosystem"
  df %>% mutate(
    Ecosystem = case_when(
      .data[[col]] %in% c("Farmland") ~ "Agriculture",
      .data[[col]] %in% c("Forest") ~ "Forest",
      .data[[col]] %in% c("grassland", "Grassland") ~ "Grassland",
      .data[[col]] %in% c("shrubland","Shrubland/Heathland") ~ "Shrubland",
      .data[[col]] %in% c("tundra", "Tundra") ~ "Tundra"
    )
  ) 
}

#==== Dataset Imports ==========================================================

figureFolder <- '/Users/trevor/Desktop/Research/Warming Ecosystem C/Figures/25november_pairedData'
dataFolder <- '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported'

paired_AGBSOC <- read.xlsx(paste0(dataFolder, '/metaAGBSOCECO_8-28-24.xlsx')) %>%
  standardize_ecosystem_type() %>%
  categorize_temperature()

#.... Basic dataset infomation .................................................

nrow(unique(paired_AGBSOC %>% dplyr::select(c(Latitude, Longitude))))

#.... Conduct separate meta-analyses ...........................................
meta_analysisAGB <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_AGBSOC,
  subgroup = warmingCategory,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)

 # Random effects model (HK) 0.0824 [0.0315; 0.1333] 3.20  0.0017

meta_analysisSOC <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_AGBSOC,
  subgroup = warmingCategory,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0059 [-0.0327; 0.0209] -0.43  0.6644


meta_analysisECO <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_AGBSOC,
  subgroup = warmingCategory,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.0120 [-0.0132; 0.0371] 0.94  0.3483

meta_analysisAGB.byeco <- metagen(
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

# Random effects model (HK) 0.0824 [0.0315; 0.1333] 3.20  0.0017

meta_analysisSOC.byeco <- metagen(
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
# Random effects model (HK) -0.0059 [-0.0327; 0.0209] -0.43  0.6644


meta_analysisECO.byeco <- metagen(
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
# Random effects model (HK) 0.0120 [-0.0132; 0.0371] 0.94  0.3483

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
 # Random effects model (HK) 0.1808 [0.0476; 0.3140] 2.86  0.0108

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
 # Random effects model (HK) 0.0537 [-0.0076; 0.1151] 1.74  0.0854

meta_analysisSOC_Nlim <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_AGBSOC %>% subset(N == "Low"),
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
# Random effects model (HK) -0.0629 [-0.1330; 0.0072] -1.89  0.0755

meta_analysisSOC_Nhigh <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_AGBSOC %>% subset(N == "High"),
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
# Random effects model (HK) -0.0003 [-0.0262; 0.0255] -0.03  0.9791

meta_analysisECO_Nlim <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_AGBSOC %>% subset(N == "Low"),
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
# Random effects model (HK) -0.0353 [-0.1142; 0.0435] -0.95  0.3573

meta_analysisECO_Nhigh <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_AGBSOC %>% subset(N == "High"),
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
# Random effects model (HK) 0.0159 [-0.0111; 0.0428] 1.17  0.2449


meta_analysisSOC.AGBinc <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_AGBSOC %>% subset(yi.AGB > 0),
  comb.fixed = FALSE, 
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)

meta_analysisSOC.AGBdec <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_AGBSOC %>% subset(yi.AGB <= 0),
  comb.fixed = FALSE, 
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)

metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_AGBSOC %>% subset((yi.SOC < 0) & (yi.AGB > 0)),
  subgroup = Ecosystem,
  studlab = Study,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)

#---- Visualizations -----------------------------------------------------------

#.... Define a consistent color palette.........................................

agbsoc_colors <- c("AGB" = "#117733",
                   "SOC" = "#582707",
                   "ECO" = "darkgray")


#.... Figure 1: Visualize overall results.......................................
overall_results = data.frame(
  "yi" = c(meta_analysisAGB$TE.random, meta_analysisSOC$TE.random, meta_analysisECO$TE.random),
  "yi.lower" = c(meta_analysisAGB$lower.random, meta_analysisSOC$lower.random, meta_analysisECO$lower.random),
  "yi.upper" = c(meta_analysisAGB$upper.random, meta_analysisSOC$upper.random, meta_analysisECO$upper.random),
  "Pool" = c("AGB", "SOC", "ECO")
) %>% mutate(Pool = factor(Pool, levels = c("AGB", "SOC", "ECO"))
)


overall <- ggplot(overall_results %>% subset(Pool != "ECO"), aes(x = Pool, y = yi, color = Pool)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5) +
  scale_color_manual(values = agbsoc_colors) +
  geom_errorbar(aes(ymin = yi.lower, ymax = yi.upper), width = 0.2, linewidth = 1) +
  labs(
    x = "Carbon Pool",
    y = "Estimate (lnRR)"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 30)
  )

ggsave(
  filename = paste0(figureFolder,"/overall.png"),  
  plot = overall,                             
  width = 8, height = 6, units = "in" , bg='#ffffff'  
)

#.... Figure 2: Visualize results separated by N................................

n_df = data.frame(table(mutate(paired_AGBSOC, N = case_when(
  avg_SCN < 15 ~ "High Nitrogen",
  avg_SCN >= 15 ~ "Low Nitrogen"
))$N)) %>% rename(N = Var1)

ncomp_results = data.frame(
  "yi" = c(meta_analysisAGB_Nlim$TE.random, meta_analysisAGB_Nhigh$TE.random, meta_analysisSOC_Nlim$TE.random, meta_analysisSOC_Nhigh$TE.random, meta_analysisECO_Nlim$TE.random, meta_analysisECO_Nhigh$TE.random),
  "yi.lower" = c(meta_analysisAGB_Nlim$lower.random, meta_analysisAGB_Nhigh$lower.random, meta_analysisSOC_Nlim$lower.random, meta_analysisSOC_Nhigh$lower.random, meta_analysisECO_Nlim$lower.random, meta_analysisECO_Nhigh$lower.random),
  "yi.upper" = c(meta_analysisAGB_Nlim$upper.random, meta_analysisAGB_Nhigh$upper.random, meta_analysisSOC_Nlim$upper.random, meta_analysisSOC_Nhigh$upper.random, meta_analysisECO_Nlim$upper.random, meta_analysisECO_Nhigh$upper.random),
  "Pool" = c("AGB", "AGB", "SOC", "SOC", "ECO", "ECO"),
  "N" = c("Low Nitrogen", "High Nitrogen", "Low Nitrogen", "High Nitrogen", "Low Nitrogen", "High Nitrogen")
) %>% mutate(Pool = factor(Pool, levels = c("AGB", "SOC", "ECO")),
             N = factor(N, levels = c("Low Nitrogen", "High Nitrogen"))
) %>% merge(n_df, on = "N")

n_plot <- ggplot(ncomp_results %>% subset(Pool != "ECO"), aes(x = Pool, y = yi, color = Pool)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5) +
  scale_color_manual(values = agbsoc_colors) +
  geom_errorbar(aes(ymin = yi.lower, ymax = yi.upper), width = 0.2, linewidth = 1) +
  labs(
    x = "Carbon Pool",
    y = "Treatment Effect (lnRR)"
  ) +
  theme_light() +
  geom_text(
    data = ncomp_results,
    aes(x = Inf, y = Inf, label = paste0("n = ", Freq)), 
    hjust = 1.5, vjust = 2, 
    size = 10,
    inherit.aes = FALSE,
    fontface = "italic" 
  ) +
  facet_wrap(~ N)  +
  theme(
    text = element_text(size = 30),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 30, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    #axis.title.y = element_text(face = "bold") 
  )

ggsave(
  filename = paste0(figureFolder,"/n_plot.png"),  
  plot = n_plot,                             
  width = 8, height = 6, units = "in" , bg='#ffffff'  
)

#.... Visualize data by warming magnitude ......................................
warm_levels = meta_analysisAGB$subgroup.levels

warmcomp_results = data.frame(
  "yi" = c(meta_analysisAGB$TE.random.w, meta_analysisSOC$TE.random.w, meta_analysisECO$TE.random.w),
  "yi.lower" = c(meta_analysisAGB$lower.random.w, meta_analysisSOC$lower.random.w, meta_analysisECO$lower.random.w ),
  "yi.upper" = c(meta_analysisAGB$upper.random.w, meta_analysisSOC$upper.random.w, meta_analysisECO$upper.random.w),
  "Pool" = c(rep("AGB", 4), rep("SOC", 4), rep("ECO", 4)),
  "Warming" = c(rep(warm_levels, 3))
) %>% mutate(
  Warming = factor(Warming, levels = c('< 1.5',  '1.5 - 2.5',  '2.5 - 4.5','> 4.5'))
)

warm_n <- data.frame(table(paired_AGBSOC$warmingCategory)) %>%
  rename(Warming = Var1)

warm <- ggplot(warmcomp_results %>% subset(Pool != "ECO"), aes(x = Warming, y = yi, color = Pool)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = agbsoc_colors) +
  ylim(-0.5,1) +
  geom_errorbar(aes(ymin = yi.lower, ymax = yi.upper), width = 0.2, linewidth = 1, position = position_dodge(width = 0.5)) +
  labs(
    x = "Magnitude of Warming (\u00b0C)",
    y = "Treatment Effect (lnRR)",
    color = "Carbon Pool"
  ) +
  theme_light() +
  geom_text(
    data = warm_n,
    aes(x = Warming, y = Inf, label = paste0("n = ", Freq)),  vjust = 1.5, 
    size = 10,
    inherit.aes = FALSE,
    fontface = "italic" 
  ) +
  theme(
    text = element_text(size = 30),
    #legend.position = "none",
    #legend.position = "bottom",  
    legend.position = c(0.05, 0.01), 
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = "black"), 
    legend.key = element_blank(), 
    legend.text = element_text(size = 20),
    strip.background = element_blank(),
    strip.text = element_text(size = 30, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold") 
  )

#.... Visualize results by ecosystem type ......................................

eco_order = meta_analysisAGB.byeco$subgroup.levels

eco_levels = c("Grassland", "Agriculture", "Shrubland", "Forest", "Tundra")

ecocomp_results = data.frame(
  "yi" = c(meta_analysisAGB.byeco$TE.random.w, meta_analysisSOC.byeco$TE.random.w, meta_analysisECO.byeco$TE.random.w),
  "yi.lower" = c(meta_analysisAGB.byeco$lower.random.w, meta_analysisSOC.byeco$lower.random.w, meta_analysisECO.byeco$lower.random.w ),
  "yi.upper" = c(meta_analysisAGB.byeco$upper.random.w, meta_analysisSOC.byeco$upper.random.w, meta_analysisECO.byeco$upper.random.w),
  "Pool" = c(rep("AGB", 5), rep("SOC", 5), rep("ECO", 5)),
  "Ecosystem" = c(rep(eco_order, 3))
) %>% mutate(
  Ecosystem = factor(Ecosystem, levels = eco_levels)
)

eco_n <- data.frame(table(paired_AGBSOC$Ecosystem)) %>%
  rename(Ecosystem = Var1)

eco <- ggplot(ecocomp_results %>% subset(Pool != "ECO"), aes(x = Ecosystem, y = yi, color = Pool)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = agbsoc_colors) +
  ylim(-1,1) +
  geom_errorbar(aes(ymin = yi.lower, ymax = yi.upper), width = 0.2, linewidth = 1, position = position_dodge(width = 0.5)) +
  labs(
    x = "Ecosystem Type",
    y = "Treatment Effect (lnRR)"
  ) +
  theme_light() +
  geom_text(
    data = eco_n,
    aes(x = Ecosystem, y = Inf, label = paste0("n = ", Freq)),  vjust = 1.5, 
    size = 10,
    inherit.aes = FALSE,
    fontface = "italic" 
  ) + scale_x_discrete(
    labels = c("Grassland" = "Grass.",
               "Agriculture"= "Ag.",
               "Shrubland" = "Shrub.",
               "Forest" = "Forest",
               "Tundra" = "Tundra")  
  )+
  theme(
    text = element_text(size = 30),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 30, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold") 
  )

#.... Figure 3: Visualize by warming/ ecosystem type  ..........................

warm_eco <- plot_grid(warm, eco,labels = c("a", "b"), label_size = 30)


ggsave(
  filename = paste0(figureFolder,"/warm_eco.png"),  
  plot = warm_eco,                             
  width = 15, height = 12, units = "in" , bg='#ffffff'  
)

#.... Figure 4: Visualize regression results ...................................

results <- paired_AGBSOC %>%
  mutate(group = ifelse(yi.AGB > 0, "yi.AGB > 0", "yi.AGB < 0")) %>%
  group_by(group) %>%
  summarize(
    fit = list(rlm(yi.SOC ~ yi.AGB, data = pick(everything()))),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    p_value = {
      model_summary <- summary(fit)
      2 * (1 - pt(abs(model_summary$coefficients[2, "t value"]), 
                  df = model_summary$df[2]))
    }
  ) %>%
  dplyr::select(group, p_value)

paired_AGBSOC <- paired_AGBSOC %>%
  mutate(group = ifelse(yi.AGB > 0, "yi.AGB > 0", "yi.AGB < 0"))

soc_agb_incdec = data.frame(
  "yi" = c(meta_analysisSOC.AGBinc$TE.random, meta_analysisSOC.AGBdec$TE.random),
  "yi.lower" = c(meta_analysisSOC.AGBinc$lower.random, meta_analysisSOC.AGBdec$lower.random),
  "yi.upper" = c(meta_analysisSOC.AGBinc$upper.random, meta_analysisSOC.AGBdec$upper.random),
  "group" = c("yi.AGB > 0", "yi.AGB < 0"),
  "nobs" = c(nrow(paired_AGBSOC %>% subset(group == "yi.AGB > 0")), nrow(paired_AGBSOC %>% subset(group == "yi.AGB < 0")))
)

scatter_trend_agbsoc <- ggplot(paired_AGBSOC, aes(x = yi.AGB, y = yi.SOC, color = group)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)+
  geom_point() +
  geom_smooth(method = "rlm", method.args = list(maxit = 50)) +
  labs(
    x = "AGB Effect Size",
    y = "SOC Effect Size",
    color = ""
  ) +
  geom_text(
    data = results,
    aes(x = c(-0.25, 0.5), y = -0.5, label = paste0("p = ", signif(p_value, 3))),
    inherit.aes = FALSE,
    vjust = 1.5,
    size = 7.5,
    fontface = "italic" 
  ) +
  theme_light() +
  theme(
    text = element_text(size = 30),
    legend.position = "top",
    legend.direction = "horizontal", 
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold") 
  )

point_error_agbsoc <- ggplot(soc_agb_incdec, aes(x = group, y = yi, color = group)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_text(
    data = soc_agb_incdec,
    aes(x = group, y = 0.075, label = paste0("n = ", nobs)),  vjust = 1.5, 
    size = 10,
    inherit.aes = FALSE,
    fontface = "italic" 
  ) +
  geom_errorbar(
    data = soc_agb_incdec,
    aes(x = group, ymin = yi.lower, ymax = yi.upper, color = group),  
    width = 0.2,
    linewidth = 1,
    inherit.aes = TRUE
  ) +
  labs(
    x = "AGB Response",
    y = "Overall SOC Effect Size"
  ) +
  scale_x_discrete(
    labels = c("yi.AGB < 0" = "lnRR < 0", "yi.AGB > 0"= "lnRR > 0")  
  ) +
  theme_light() +
  theme(
    text = element_text(size = 30),
    legend.position = "none",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold") 
  )

trend_error <- plot_grid(scatter_trend_agbsoc, point_error_agbsoc, labels = c("a", "b"), ncol = 2, rel_widths = c(3,1.75), label_size = 30)

# This data suggests that AGB responses are important for stabilizing/
# increasing SOC concentrations when AGB increases, but if AGB decreases, 
# this response is relatively unimportant and soil C decreases by a "fixed" amt

ggsave(
  filename = paste0(figureFolder,"/trend_error.png"),  
  plot = trend_error,                             
  width = 13, height = 8, units = "in" , bg='#ffffff'  
)


#.... Principle components analysis ............................................ 
vars_of_interest = c('bd_0_15', 'Aridity', 'clay_0_15', 'phh2o_0_15',
                     'maxWarming', 'yi.SOC', 'yi.AGB', 'yi.ECO', 
                     'MAT', 'MAP', 'Elevation', 'avg_SCN')

paired_full <- paired_AGBSOC %>% dplyr::select(all_of(vars_of_interest)) %>% tidyr::drop_na()

paired.pca <- paired_full %>% prcomp(center = TRUE, scale. = TRUE)

summary(paired.pca)

biplot(paired.pca)
