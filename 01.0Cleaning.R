# This script pairs some additional ancillary variables
# also calculates the ecosystem-level sink

library(openxlsx)
library(dplyr)
library(metafor)

##### Functions #####
convertUnits <- function(df){
  df %>% mutate(
    soc_conversion_factor = case_when(
      Units.SOC == "g m2" ~ 1,
      Units.SOC == "g m-2" ~ 1,
      Units.SOC == "g kg-1" ~ 10*as.numeric(Depth_cm),
      Units.SOC == "g kg" ~ 10*as.numeric(Depth_cm),
      Units.SOC == "g_kg" ~ 10*as.numeric(Depth_cm),
      Units.SOC == "kg m-2" ~ 1000,
      TRUE ~ -1
    ),
    agb_conversion_factor = case_when(
      Units.AGB == "g m-2" ~ 1,
      Units.AGB == "g_m2" ~ 1,
      Units.AGB == "g 0.25m-2" ~ 4,
      Units.AGB == "kg ha-1" ~ 0.1,
      Units.AGB == "g" ~ 1/Plot.area.m2,
      TRUE ~ -1
    )
  )
}

##### Main #####

paired_AGBSOC <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/metaAGBSOC_6-7-24.xlsx') %>%
  mutate(
    N = case_when(
      avg_SCN >= 15 ~ "Low",
      avg_SCN < 15 ~ "High",
      TRUE ~ NA
    )
  )

paired_AGBSOC$Nhigh_bin <- ifelse(paired_AGBSOC$N == "Low", 0, ifelse(paired_AGBSOC$N == "High", 1, NA))
paired_AGBSOC$Nlow_bin <- ifelse(paired_AGBSOC$N == "Low", 1, ifelse(paired_AGBSOC$N == "High", 0, NA))
raw <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/SoilC - Ji Chen.csv') %>% rename(SOC.C = C.SOC, SOC.T = T.SOC,AGB.C = C.AGB, Heat.A.S = Others, Longitude = Logitude) %>% mutate(
  Treatment.Group = case_when(
    Others.1 == "N addition" | Others.1 == "low N addition" | Others.1 == "high N addition" | Others.1 == "N+P" | 
      Others.2 == "N addition" ~ "FW", 
    Others.1 == "CO2 elevated" | Others.2 == "CO2 elevated" ~ "CW", Others.1 == "Increased precipitation" ~ "PW",
    Others.1 == "Clipped" ~ "GW", Others.1 == "Grazing" | Others.1 == "Winter grazing" ~ "GW", 
    TRUE ~ "W"
  )
)

paired_AGBSOC$pct_AGB <- 100*(exp(paired_AGBSOC$yi.AGB)-1)
paired_AGBSOC$pct_SOC <- 100*(exp(paired_AGBSOC$yi.SOC)-1)

raw$SOC.T <- as.numeric(raw$SOC.T)
raw$SOC.C <- as.numeric(raw$SOC.C)

paired_AGBSOC$SOC.T <- as.numeric(paired_AGBSOC$SOC.T)
paired_AGBSOC$SOC.C <- as.numeric(paired_AGBSOC$SOC.C)

paired_AGBSOC <- paired_AGBSOC %>% left_join(raw %>% select(Duration, SOC.C, SOC.T,AGB.C, Depth, Depth2, Season, Heat.A.S, RR.LPC, RR.RPC), by = c('SOC.C', 'SOC.T', 'AGB.C'))


length(unique(interaction(paired_AGBSOC$Latitude, paired_AGBSOC$Longitude)))
nrow(paired_AGBSOC)
aridity <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/aridityPts_jun27.csv') %>% rename("Aridity" = "b1") %>% select(Latitude, Longitude, Aridity) %>% unique()

bd <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/bulkDensityPts_jun27.csv')

clay <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/clay_g_kg_d10_jun27.csv')

pH <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/pH_d10_jun27.csv')

# units are kg/m3
bd$bd_0_15 <- 1000*((5/15)*bd$bdod_0.5cm_mean + (10/15)*bd$bdod_5.15cm_mean)/100

clay$clay_0_15 <- ((5/15)*clay$clay_0.5cm_mean + (10/15)*clay$clay_5.15cm_mean)/10

pH$phh2o_0_15 <- ((5/15)*pH$phh2o_0.5cm_mean + (10/15)*pH$phh2o_5.15cm_mean)/10

paired_AGBSOC <- paired_AGBSOC %>% left_join(bd  %>% select(Latitude, Longitude, bd_0_15)%>% unique(), by = c('Latitude', 'Longitude'))

paired_AGBSOC <- paired_AGBSOC %>% left_join(aridity, by = c('Latitude', 'Longitude'))

paired_AGBSOC <- paired_AGBSOC %>% left_join(clay  %>% select(Latitude, Longitude, clay_0_15)%>% unique(), by = c('Latitude', 'Longitude'))

paired_AGBSOC <- paired_AGBSOC %>% left_join(pH  %>% select(Latitude, Longitude, phh2o_0_15)%>% unique(), by = c('Latitude', 'Longitude'))

paired_AGBSOC <- convertUnits(paired_AGBSOC)

paired_AGBSOC$SOC.C.gm2 <- paired_AGBSOC$SOC.C * paired_AGBSOC$soc_conversion_factor
paired_AGBSOC$SD.C.SOC.gm2 <- paired_AGBSOC$SD.C.SOC * paired_AGBSOC$soc_conversion_factor
paired_AGBSOC$SD.T.SOC.gm2 <- paired_AGBSOC$SD.T.SOC * paired_AGBSOC$soc_conversion_factor
paired_AGBSOC$SOC.T.gm2 <- as.numeric(paired_AGBSOC$SOC.T) * paired_AGBSOC$soc_conversion_factor

paired_AGBSOC$AGB.C.gm2 <- paired_AGBSOC$AGB.C * paired_AGBSOC$agb_conversion_factor
paired_AGBSOC$SD.C.AGB.gm2 <- paired_AGBSOC$SD.C.AGB * paired_AGBSOC$agb_conversion_factor
paired_AGBSOC$SD.T.AGB.gm2 <- paired_AGBSOC$SD.T.AGB * paired_AGBSOC$agb_conversion_factor
paired_AGBSOC$AGB.T.gm2 <- as.numeric(paired_AGBSOC$AGB.T) * paired_AGBSOC$agb_conversion_factor

pairedAGB_SOC_eco <- subset(paired_AGBSOC, agb_conversion_factor > 0 & soc_conversion_factor > 0)

pairedAGB_SOC_eco$ECO.C_gm2 <- pairedAGB_SOC_eco$SOC.C.gm2 + pairedAGB_SOC_eco$AGB.C.gm2
pairedAGB_SOC_eco$ECO.T_gm2 <- pairedAGB_SOC_eco$SOC.T.gm2 + pairedAGB_SOC_eco$AGB.T.gm2

pairedAGB_SOC_eco$SD.ECO.C_gm2 <- sqrt((pairedAGB_SOC_eco$SD.C.SOC.gm2)^2 + (pairedAGB_SOC_eco$SD.C.AGB.gm2)^2)
pairedAGB_SOC_eco$SD.ECO.T_gm2 <- sqrt((pairedAGB_SOC_eco$SD.T.SOC.gm2)^2 + (pairedAGB_SOC_eco$SD.T.AGB.gm2)^2)

pairedAGB_SOC_eco$D_AGB <- pairedAGB_SOC_eco$AGB.T.gm2 - pairedAGB_SOC_eco$AGB.C.gm2
pairedAGB_SOC_eco$D_SOC <- pairedAGB_SOC_eco$SOC.T.gm2 - pairedAGB_SOC_eco$SOC.C.gm2

paired_AGBSOC$N.gm2 <- paired_AGBSOC$SOC.C.gm2/paired_AGBSOC$avg_SCN


pairedAGB_SOC_eco <- escalc(
  measure = "ROM",
  m1i = as.numeric(ECO.T_gm2), sd1i = as.numeric(SD.ECO.T_gm2), n1i = as.numeric(T.n.AGB),
  m2i = as.numeric(ECO.C_gm2), sd2i = as.numeric(SD.ECO.C_gm2), n2i = as.numeric(C.n.AGB),
  data = pairedAGB_SOC_eco
) %>% rename(
  yi.ECO = yi,
  vi.ECO = vi
)

write.xlsx(pairedAGB_SOC_eco, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/metaAGBSOCECO_8-28-24.xlsx')
