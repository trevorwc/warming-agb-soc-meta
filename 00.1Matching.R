# Pair and export observations
library(openxlsx)

pairedChen <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedChenDatsaset2-2.xlsx')
socData <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/soilDataset2-2.xlsx')
agbData <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/biomassDataset2-2.xlsx')

chenRaw <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/SoilC - Ji Chen.csv') %>% select(Latitude, Logitude, Method, C.SOC, SD.SOC, T.SOC, SD.SOC.1, C.AGB, T.AGB) %>% rename(Warming.Method = Method, Longitude = Logitude, SOC.C = C.SOC, SD.C.SOC = SD.SOC, SOC.T = T.SOC, SD.T.SOC = SD.SOC.1, AGB.C = C.AGB, AGB.T = T.AGB) %>%
  mutate(Warming.Method = case_when(
    Warming.Method == 'OTC' ~ 'O',
    Warming.Method == 'Cables' ~ 'C',
    Warming.Method == 'IH' ~ 'I',
    Warming.Method == 'Curtain' ~ 'Cu',
    TRUE ~ 'N.A.'
  ))

soc_bai <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/bai_Warming-SOC-meta.xlsx') %>% rename(yi.BGB = yi,vi.BGB = vi, Latitude = latitude, Longitude = longitude, DT = SoilT1) %>%
  mutate(
    Latitude = round(Latitude, 2),
    Longitude = round(Longitude, 2)
  ) %>% select(
    Latitude, Longitude, SCN, SCN_source
  )  %>% group_by(Latitude, Longitude) %>%
  summarize(avg_SCN = mean(SCN),
            SCN_source= list(unique(SCN_source))) %>%
  ungroup()

pairedChen <- pairedChen %>% left_join(chenRaw, by = c('Latitude', 'Longitude', 'SOC.C','SD.C.SOC', 'SOC.T', 'SD.T.SOC', 'AGB.C', 'AGB.T')) %>% unique()

pairedChen <- pairedChen %>% mutate(
  Latitude = round(Latitude, 2),
  Longitude = round(Longitude, 2)
) %>% left_join(soc_bai, by = c("Latitude", "Longitude"))

# Try to match unpaired points 
socData <- socData %>%  rename(C.n.SOC = C.n, T.n.SOC = T.n, SD.C.SOC = SD.C, SD.T.SOC = SD.T, Units.SOC = Units)
agbData <- agbData %>%  rename(C.n.AGB = C.n, T.n.AGB = T.n, SD.C.AGB = SD.C, SD.T.AGB = SD.T, Units.AGB = Units)

# fix type errors 
socData$Elevation <- as.double(socData$Elevation)
agbData$Elevation <- as.double(agbData$Elevation)

socData$Sampled.Year <- as.double(socData$Sampled.Year)
agbData$Sampled.Year <- as.double(agbData$Sampled.Year)

pairedAGBSOC <- socData %>% left_join(agbData, by = c('Sampled.Year', 'Treatment', 'Latitude', 'Longitude', 'Air.DT', 'Soil.DT'), suffix = c("","."))

# no SCN data 
pairedAGBSOC$avg_SCN <- NA

pairedAGBSOC$SCN_source <- NA

colsInt <- intersect(names(pairedAGBSOC), names(pairedChen))

paired <- rbind(pairedChen %>% select(all_of(colsInt), `Warming.Method`), pairedAGBSOC %>% select(all_of(colsInt), Warming.Method)) %>% subset(AGB.C > -999 & SOC.C > -999)
# added 8 sites

# fix for consistency 
paired <- paired %>%
  mutate(Ecosystem = ifelse(Ecosystem == "forest", "Forest", Ecosystem)) %>%
  mutate(Ecosystem = ifelse(Ecosystem == "temperate_forest", "Forest", Ecosystem))

# * Get warming only 
pairedC <- subset(paired, Treatment == 'W')
pairedCW <- subset(paired, Treatment == 'CW')

# type error 
pairedC$SOC.C <- as.numeric(pairedC$SOC.C)
pairedC$AGB.C <- as.numeric(pairedC$AGB.C)

lnRRdata <- escalc(
  measure = "ROM",
  m1i = as.numeric(SOC.T), sd1i = as.numeric(SD.T.SOC), n1i = as.numeric(T.n.SOC),
  m2i = as.numeric(SOC.C), sd2i = as.numeric(SD.C.SOC), n2i = as.numeric(C.n.SOC),
  data = pairedC
) %>% rename(
  yi.SOC = yi,
  vi.SOC = vi
)


lnRRdata <- escalc(
  measure = "ROM",
  m1i = as.numeric(AGB.T), sd1i = as.numeric(SD.T.AGB), n1i = as.numeric(T.n.AGB),
  m2i = as.numeric(AGB.C), sd2i = as.numeric(SD.C.AGB), n2i = as.numeric(C.n.AGB),
  data = lnRRdata
) %>% rename(
  yi.AGB = yi,
  vi.AGB = vi
)

lnRRdata_full <- lnRRdata %>% subset(vi.AGB > -999 & vi.SOC > -999)

lnRRdata_full$Soil.DT[is.na(lnRRdata_full$Soil.DT)] <- -999

lnRRdata_full$DT <- pmax(lnRRdata_full$Air.DT, lnRRdata_full$Soil.DT )

lnRRdata_full$DT.cat <- cut(lnRRdata_full$DT,  breaks = c(-Inf, 1.5, 2.5, 4.5, Inf), labels = c("< 1.5", "1.5-2.5", "2.5-4.5", ">4.5"), right = FALSE)

lnRRdata_full[is.na(lnRRdata_full)] <- ""

#write.xlsx(lnRRdata_full, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/metaAGBSOC_6-7-24.xlsx')




