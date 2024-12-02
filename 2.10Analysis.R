#==== Analysis =================================================================
 # After pre-processing and generating site level average in terms of g/m2
 # It is time to perform the analysis

#==== Package Imports ==========================================================
library(meta)
library(metafor)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(MASS)
library(lmodel2)
library(gridExtra)
library(cowplot)
library(ggfortify)

#==== Functions ================================================================

calc_effect_sizes <- function(df){
  df2 <- escalc(
    measure = "ROM",
    m1i = M.T.ECO, sd1i = sqrt(V.T.ECO), n1i = T.n,
    m2i = M.C.ECO, sd2i = sqrt(V.C.ECO), n2i = C.n,
    data = df
  ) %>% rename(
    yi.ECO = yi,
    vi.ECO = vi
  )
  
  df3 <- escalc(
    measure = "ROM",
    m1i = M.T.AGB, sd1i = sqrt(V.T.AGB), n1i = T.n,
    m2i = M.C.AGB, sd2i = sqrt(V.C.AGB), n2i = C.n,
    data = df2
  ) %>% rename(
    yi.AGB = yi,
    vi.AGB = vi
  )
  
  df4 <- escalc(
    measure = "ROM",
    m1i = M.T.SOC, sd1i = sqrt(V.T.SOC), n1i = T.n,
    m2i = M.C.SOC, sd2i = sqrt(V.C.SOC), n2i = C.n,
    data = df3
  ) %>% rename(
    yi.SOC = yi,
    vi.SOC = vi
  )
  return(df4)
}

categorize_temperature <- function(df) {
  df %>% mutate(
    maxWarming = pmax(`Air.DT.AGB`, `Soil.DT.AGB`, `Air.DT.SOC`, `Soil.DT.SOC`, na.rm = TRUE),
    warmingCategory = case_when(
      maxWarming < 1.5 ~ "< 1.5",
      (maxWarming >= 1.5) & (maxWarming < 2.5) ~ "1.5 - 2.5",
      (maxWarming >= 2.5) & (maxWarming < 4.5) ~ "2.5 - 4.5",
      maxWarming >= 4.5 ~ "> 4.5"
    )
  )
}

#==== Dataset Imports ==========================================================
dataFolder <- '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported'
figureFolder <- '/Users/trevor/Desktop/Research/Warming Ecosystem C/Figures/22november'

paired_full <- read.xlsx(paste0(dataFolder,'/pairedfull_11-22.xlsx'))
paired_last <- read.xlsx(paste0(dataFolder, '/pairedlast_11-22.xlsx'))


paired_full_yi <- paired_full %>% 
  calc_effect_sizes() %>% 
  categorize_temperature() %>%
  subset((vi.AGB/yi.AGB < 1000) & !is.na(warmingCategory)) 

paired_last_yi <- paired_last %>% 
  calc_effect_sizes() %>% 
  categorize_temperature() %>%
  subset((vi.AGB/yi.AGB < 1000) & !is.na(warmingCategory)) 

#----- Last observation dataset ------------------------------------------------

#.... AGB ......................................................................

meta.agb.last <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_last_yi,
  comb.fixed = FALSE, 
  comb.random = TRUE,
  subgroup = warmingCategory,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.0511 [-0.0182; 0.1204] 1.47  0.1462

meta.agb.last.byeco <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_last_yi,
  comb.fixed = FALSE, 
  comb.random = TRUE,
  subgroup = Ecosystem,
  hakn = TRUE,
  method.tau = "REML"
)

meta.agb.last.byN <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_last_yi %>% subset(avg_SCN > 0),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  subgroup = (avg_SCN >= 15),
  hakn = TRUE,
  method.tau = "REML"
)

meta.agb.last.lowN <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_last_yi %>% subset(avg_SCN >= 15),
  comb.fixed = FALSE, 
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
# Random effects model (HK) 0.2777 [0.1403; 0.4152] 4.57  0.0013

print(100*(exp(0.2777)-1))
print(100*(exp(0.1403)-1))
print(100*(exp(0.4152)-1))

meta.agb.last.highN <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_full_yi %>% subset(avg_SCN < 15),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.0215 [-0.0426; 0.0857] 0.67  0.5052

#.... SOC ......................................................................

meta.soc.last <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_last_yi,
  comb.fixed = FALSE, 
  comb.random = TRUE,
  subgroup = warmingCategory,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0045 [-0.0303; 0.0214] -0.35  0.7298

meta.soc.last.byeco <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_last_yi,
  comb.fixed = FALSE, 
  comb.random = TRUE,
  subgroup = Ecosystem,
  hakn = TRUE,
  method.tau = "REML"
)

meta.soc.last.byN <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_last_yi %>% subset(avg_SCN > 0),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  subgroup = (avg_SCN >= 15), # N low?
  hakn = TRUE,
  method.tau = "REML"
)

meta.SOC.last.lowN <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_last_yi %>% subset(avg_SCN >= 15),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0651 [-0.1547; 0.0246] -1.64  0.1352

meta.SOC.last.highN <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_last_yi %>% subset(avg_SCN < 15),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0126 [-0.0439; 0.0188] -0.81  0.4241

#.... ECO ......................................................................

meta.eco.last <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_last_yi ,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  subgroup = warmingCategory,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.0058 [-0.0200; 0.0316] 0.45  0.6559 

meta.eco.last.byeco <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_last_yi ,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  subgroup = Ecosystem,
  hakn = TRUE,
  method.tau = "REML"
)

meta.eco.last.byN <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_last_yi %>% subset(avg_SCN > 0),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  subgroup = (avg_SCN >= 15),
  hakn = TRUE,
  method.tau = "REML"
)
 
meta.eco.last.lowN <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_last_yi %>% subset(avg_SCN >= 15) ,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0113 [-0.1314; 0.1088] -0.21  0.8367

meta.eco.last.highN <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_last_yi %>% subset(avg_SCN >= 15) ,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0113 [-0.1314; 0.1088]


#----- Full observation dataset ------------------------------------------------

meta.agb.full <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_full_yi,
  comb.fixed = FALSE, 
  comb.random = TRUE,
  subgroup = warmingCategory,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.0451 [-0.0097; 0.0998] 1.64  0.1053

meta.agb.lowN <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_full_yi %>% subset(avg_SCN >= 15),
  comb.fixed = FALSE, 
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.2689 [0.1304; 0.4073] 4.33  0.0015

meta.agb.full.highN <- metagen(
  TE = yi.AGB, 
  seTE = sqrt(vi.AGB),
  data = paired_full_yi %>% subset(avg_SCN < 15),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.0215 [-0.0426; 0.0857] 0.67  0.5052

#....SOC .......................................................................

meta.soc.full <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_full_yi,
  comb.fixed = FALSE, 
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0048 [-0.0077; -0.0019] -3.27  0.0015

meta.SOC.lowN <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_full_yi %>% subset(avg_SCN >= 15),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0650 [-0.1487; 0.0188] -1.73  0.1146

meta.SOC.highN <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_full_yi %>% subset(avg_SCN < 15),
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0103 [-0.0379; 0.0173] -0.74  0.4590

#.... ECO ......................................................................

meta.eco.full <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_full_yi ,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) 0.0054 [-0.0172; 0.0281] 0.48  0.6353

meta.eco.full.lowN <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_full_yi %>% subset(avg_SCN >= 15) ,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0113 [-0.1233; 0.1007] -0.22  0.8270

meta.eco.full.highN <- metagen(
  TE = yi.ECO, 
  seTE = sqrt(vi.ECO),
  data = paired_full_yi %>% subset(avg_SCN < 15) ,
  comb.fixed = FALSE, # random-effects model
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0021 [-0.0289; 0.0247] -0.16  0.8770

#---- Subgroup Analysis --------------------------------------------------------

meta.SOC.last.AGBinc <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_last_yi %>% subset(yi.AGB > 0),
  comb.fixed = FALSE, 
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0017 [-0.0085; 0.0050] -0.52  0.6038

meta.SOC.last.AGBdec <- metagen(
  TE = yi.SOC, 
  seTE = sqrt(vi.SOC),
  data = paired_last_yi %>% subset(yi.AGB < 0),
  comb.fixed = FALSE, 
  comb.random = TRUE,
  hakn = TRUE,
  method.tau = "REML"
)
 # Random effects model (HK) -0.0494 [-0.0698; -0.0291] -4.98 < 0.0001

#---- Visualizations -----------------------------------------------------------

#.... Define a consistent color palette.........................................

agbsoc_colors <- c("AGB" = "#117733",
                   "SOC" = "#582707",
                   "ECO" = "darkgray")


#.... Figure 1: Visualize overall results.......................................
overall_results = data.frame(
  "yi" = c(meta.agb.last$TE.random, meta.soc.last$TE.random, meta.eco.last$TE.random),
  "yi.lower" = c(meta.agb.last$lower.random, meta.soc.last$lower.random, meta.eco.last$lower.random),
  "yi.upper" = c(meta.agb.last$upper.random, meta.soc.last$upper.random, meta.eco.last$upper.random),
  "Pool" = c("AGB", "SOC", "ECO")
) %>% mutate(Pool = factor(Pool, levels = c("AGB", "SOC", "ECO"))
)


overall <- ggplot(overall_results, aes(x = Pool, y = yi, color = Pool)) +
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

n_df = data.frame(table(mutate(paired_last_yi, N = case_when(
  avg_SCN < 15 ~ "High Nitrogen",
  avg_SCN >= 15 ~ "Low Nitrogen"
))$N)) %>% rename(N = Var1)

ncomp_results = data.frame(
  "yi" = c(meta.agb.last.lowN$TE.random, meta.agb.last.highN$TE.random, meta.SOC.last.lowN$TE.random, meta.SOC.last.highN$TE.random, meta.eco.last.lowN$TE.random, meta.eco.last.highN$TE.random),
  "yi.lower" = c(meta.agb.last.lowN$lower.random, meta.agb.last.highN$lower.random, meta.SOC.last.lowN$lower.random, meta.SOC.last.highN$lower.random, meta.eco.last.lowN$lower.random, meta.eco.last.highN$lower.random),
  "yi.upper" = c(meta.agb.last.lowN$upper.random, meta.agb.last.highN$upper.random, meta.SOC.last.lowN$upper.random, meta.SOC.last.highN$upper.random, meta.eco.last.lowN$upper.random, meta.eco.last.highN$upper.random),
  "Pool" = c("AGB", "AGB", "SOC", "SOC", "ECO", "ECO"),
  "N" = c("Low Nitrogen", "High Nitrogen", "Low Nitrogen", "High Nitrogen", "Low Nitrogen", "High Nitrogen")
) %>% mutate(Pool = factor(Pool, levels = c("AGB", "SOC", "ECO")),
             N = factor(N, levels = c("Low Nitrogen", "High Nitrogen"))
) %>% merge(n_df, on = "N")

n_plot <- ggplot(ncomp_results, aes(x = Pool, y = yi, color = Pool)) +
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
warm_levels = c('< 1.5', '1.5 - 2.5', '2.5 - 4.5', '> 4.5')

warmcomp_results = data.frame(
  "yi" = c(meta.agb.last$TE.random.w, meta.soc.last$TE.random.w, meta.eco.last$TE.random.w),
  "yi.lower" = c(meta.agb.last$lower.random.w, meta.soc.last$lower.random.w, meta.eco.last$lower.random.w ),
  "yi.upper" = c(meta.agb.last$upper.random.w, meta.soc.last$upper.random.w, meta.eco.last$upper.random.w),
  "Pool" = c(rep("AGB", 4), rep("SOC", 4), rep("ECO", 4)),
  "Warming" = c(rep(warm_levels, 3))
) %>% mutate(
  Warming = factor(Warming, levels = warm_levels)
)
 
warm_n <- data.frame(table(paired_last_yi$warmingCategory)) %>%
  rename(Warming = Var1)

warm <- ggplot(warmcomp_results, aes(x = Warming, y = yi, color = Pool)) +
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

eco_levels = c("Grassland", "Agriculture", "Shrubland", "Forest", "Tundra")

ecocomp_results = data.frame(
  "yi" = c(meta.agb.last.byeco$TE.random.w, meta.soc.last.byeco$TE.random.w, meta.eco.last.byeco$TE.random.w),
  "yi.lower" = c(meta.agb.last.byeco$lower.random.w, meta.soc.last.byeco$lower.random.w, meta.eco.last.byeco$lower.random.w ),
  "yi.upper" = c(meta.agb.last.byeco$upper.random.w, meta.soc.last.byeco$upper.random.w, meta.eco.last.byeco$upper.random.w),
  "Pool" = c(rep("AGB", 5), rep("SOC", 5), rep("ECO", 5)),
  "Ecosystem" = c(rep(eco_levels, 3))
) %>% mutate(
  Ecosystem = factor(Ecosystem, levels = eco_levels)
)

eco_n <- data.frame(table(paired_last_yi$Ecosystem)) %>%
  rename(Ecosystem = Var1)

eco <- ggplot(ecocomp_results, aes(x = Ecosystem, y = yi, color = Pool)) +
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

results <- paired_last_yi %>%
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

paired_last_yi <- paired_last_yi %>%
  mutate(group = ifelse(yi.AGB > 0, "yi.AGB > 0", "yi.AGB < 0"))

soc_agb_incdec = data.frame(
  "yi" = c(meta.SOC.last.AGBinc$TE.random, meta.SOC.last.AGBdec$TE.random),
  "yi.lower" = c(meta.SOC.last.AGBinc$lower.random, meta.SOC.last.AGBdec$lower.random),
  "yi.upper" = c(meta.SOC.last.AGBinc$upper.random, meta.SOC.last.AGBdec$upper.random),
  "group" = c("yi.AGB > 0", "yi.AGB < 0"),
  "nobs" = c(nrow(paired_last_yi %>% subset(group == "yi.AGB > 0")), nrow(paired_last_yi %>% subset(group == "yi.AGB < 0")))
)

scatter_trend_agbsoc <- ggplot(paired_last_yi, aes(x = yi.AGB, y = yi.SOC, color = group)) +
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
    aes(x = group, y = 0.02, label = paste0("n = ", nobs)),  vjust = 1.5, 
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



