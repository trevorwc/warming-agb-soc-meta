#==== Data Cleaning ============================================================
 #    Identify sites with potential AGB/SOC measurements
 #    Datasets:
 #        Proprietary dataset (collected by Ji Chen, CAS)
 #        Zhou et al., 2020. Sci. Total Env. 
 #         + second version
 #        MESI; Van Sundert et al., 2023. Global Change Biology
 #    Pair lat/lon measurements from previous data,
 #        Bai et al., 
 #    determine whether present in proprietary

#==== Imports ==================================================================
library(dplyr)
library(openxlsx)
library(tidyr)
library(writexl)
library(readr)

#==== Define Functions =========================================================
get_mode <- function(x) {
  x <- x[!is.na(x)]  
  if (length(x) == 0) return(NA)  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_write_xlsx <- function(data, path) {
  if (!file.exists(path)) {
    write_xlsx(data, path)
    message("File written to: ", path)
  } else {
    message("File already exists: ", path)
  }
}

#==== Identify additional sites ================================================
# Import datasets
dataFolder <- '/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/data_sources_2024/'

chenSoilC <- read.xlsx(paste0(dataFolder, 'SoilC - Ji Chen.xlsx'))
 # Proprietary dataset

# Round lat lon values to 2 decimals
zhouBiomass <- read_csv(paste0(dataFolder, 'Dataset_of_global_warming_on_plant_biomass.csv'), show_col_types = FALSE) %>%
  mutate(Latitude = round(Latitude, 2),
         Longitude = round(Longitude, 2))
 # Zhou et al., 2022, Science of the Total Environment

MESI <- read.csv(paste0(dataFolder, 'mesi_main.csv'))
 # Van Sundert et al., 2023, GCB

baiSOC <- read.xlsx(paste0(dataFolder, 'Warming-SOC-meta.xlsx'))
 # Bai et al., 2023, GCB

# Round lat lon values
sBiomass <-  read.xlsx(paste0(dataFolder,'Data S1-S4.xlsx'), check.names = TRUE) %>% 
  mutate(Latitude = round(Latitude, 2),
         Longitude = round(Longitude, 2))
 # Zhou et al.,  (Internal)

# Determine "pairs" in proprietary (having both AGB and SOC meas)
chenPairs <- chenSoilC %>% subset(C.SOC > 0 & C.AGB > 0)
print(nrow(chenPairs))
 #    151 pairs

# Create AGB subset
zhouAGB <- zhouBiomass %>% subset(Biomass == "AGB")
print(nrow(zhouAGB))
 #    172 obs

# Get measurements of AGB, AGB C, or coarse AGB under warming
mesiAGB <- MESI %>% 
  subset(grepl("w", treatment)) %>% 
  subset(response == "agb" |
          response == "agb_c" |
           response == "agb_coarse")
print(nrow(mesiAGB))
 #    1279 obs

# Get "proprietary" measurements of AGB where SOC is missing
chenAGB <- chenSoilC %>% subset(C.AGB > 0 & is.na(C.SOC))
print(nrow(chenAGB))
 #    0 obs

# Get additional AGB measurements (remove overlap sites)
sAGB <- sBiomass %>% subset(`Aboveground..Biomass.control` > 0) %>% anti_join(zhouAGB, by = c('Latitude', 'Longitude'))
print(nrow(sAGB))
 # 11 obs

# Create SOC subset
# Get entries with SOC but no AGB
chenSOC <- chenSoilC %>% subset(C.SOC > 0 & is.na(C.AGB))
print(nrow(chenSOC))
 # 786 obs

mesiSOC <- MESI %>% 
  subset(grepl("w", treatment)) %>% 
  subset(response == "soc")
print(nrow(mesiSOC))
 # 225 obs

# Find sites that may have been unpaired in proprietary
agbSites <- data.frame(
  "Latitude" = c(round(zhouAGB$Latitude, 2), 
                 round(mesiAGB$Latitude, 2), 
                 round(chenAGB$Latitude, 2),
                 round(sAGB$Latitude, 2)),
  "Longitude" = c(round(zhouAGB$Longitude, 2), 
                  round(mesiAGB$Longitude, 2), 
                  round(chenAGB$Longitude, 2),
                  round(sAGB$Longitude, 2))
) %>% unique()

socSites <- data.frame(
  "Latitude" = c(round(chenSOC$Latitude, 2),
                 round(baiSOC$Latitude, 2),
                 round(mesiSOC$Latitude, 2)),
  "Longitude" = c(round(chenSOC$Longitude, 2),
                 round(baiSOC$Longitude, 2),
                 round(mesiSOC$Longitude, 2))
) %>% unique()

pairedSites <- chenPairs %>% dplyr::select(c('Latitude', 'Longitude')) %>% 
  round(2) %>% 
  unique()
nrow(pairedSites)
 # 51 unique sites

potentialPairs <- agbSites %>% inner_join(socSites,
                                          by = c('Latitude', 'Longitude'))
print(nrow(potentialPairs))
 # 55 potential sites in 'unpaired'

# Remove sites in the paired datset
# overlooked sites have AGB and SOC (potent. asynch. meas.) but not in paired dataset
overlooked <- potentialPairs %>% anti_join(pairedSites,
                                           by = c('Latitude', 'Longitude'))
print(nrow(overlooked))
 # 40 potentially overlooked sites

#---- Determine which datasets may contain overlooked sites---------------------

print(nrow(zhouAGB %>% right_join(overlooked, by = c("Latitude", "Longitude"))))

print(nrow(mesiAGB %>% right_join(overlooked, by = c("Latitude", "Longitude"))))

print(nrow(chenAGB %>% right_join(overlooked, by = c("Latitude", "Longitude"))))

print(nrow(sAGB %>% right_join(overlooked, by = c("Latitude", "Longitude"))))

print(nrow(chenSOC %>% right_join(overlooked, by = c("Latitude", "Longitude"))))

print(nrow(baiSOC %>% right_join(overlooked, by = c("Latitude", "Longitude"))))

print(nrow(mesiSOC %>% right_join(overlooked, by = c("Latitude", "Longitude"))))

#---- Standardize information presentation -------------------------------------

# checked completeness
zhouAGB <- zhouAGB %>% 
  mutate(
    Air.DT = case_when(
      `Note (temperature type)` == "air temperature" ~ Tdelta,
      TRUE ~ NA
    ),
    Soil.DT = case_when(
      `Note (temperature type)` == "soil temperature" ~ Tdelta,
      TRUE ~ NA  
    ),
    Warming.Method = case_when(
      `Warming method` == "heated OTC/greenhouse" ~ "O",
      `Warming method` == "Infrared heater" ~ "I",
      `Warming method` == "Cable" ~ "C",
      `Warming method` == "Passive" ~ "P",
      TRUE ~ NA
    ),
      Treatment.Group = case_when(
        Treatment == "warming+N" | Nitrogen == "y"  
        | Treatment == "+nutrients" | Treatment == "+ nutrients"
        | Treatment == "warming+NP" | Treatment == "grazing excluded+N"
        | Treatment == "warming+nutrients" ~ "FW",
        Treatment == "drought" | Treatment == "Drought" 
        | Treatment == "warming+drought" ~ "DW",
        Treatment == "warming+h2o" | Treatment == "warming+water" 
        | Treatment == "wet" ~ "IW",
        Treatment == "+clipping" | Treatment == "+grazing" 
        | Treatment == "low grazing" | Treatment == "high grazing"
        | Treatment == "infrequently cut" | Treatment == "frequently cut" 
        | Treatment == "warming+grazing"~ "GW",
        Treatment == "warming+grazing+N" | Treatment == "low grazing+N" ~ "GFW",
        Treatment == "warming+water+N" ~"IFW",
        Treatment == "warming+litter" ~ "LW",
        TRUE ~ "W"
      )
  )


mesiSOC <- mesiSOC %>%
  mutate(
    Treatment.Group = case_when(
      treatment == "cdw" ~ "CDW", 
      treatment == "cfw" ~ "CFW",
      treatment == "cw" ~ "CW",
      treatment == "dw" ~ "DW",
      treatment == "fw" ~ "FW",
      treatment == "iw" ~ "IW",
      treatment == "w" ~ "W"
    ),
    Air.DT = w_t2,
    Soil.DT = w_t3,
    Warming.Method = case_when(
      w_t1 == "_0001" ~ "I",
      w_t1 == "_0110" ~ "O", 
      TRUE ~ NA
    ),
    Sampled.Year = start_year + as.numeric(sampling_year) - 1
)

mesiAGB <- mesiAGB %>% 
  mutate(
    Treatment.Group = case_when(
      treatment == "cdw" ~ "CDW", 
      treatment == "cdfw" ~ "CDFW",
      treatment == "cfw" ~ "CFW",
      treatment == "ciw" ~ "CIW",
      treatment == "cw" ~ "CW",
      treatment == "dw" ~ "DW",
      treatment == "fw" ~ "FW",
      treatment == "w" ~ "W",
      treatment == "ifw" ~ "IFW",
      treatment == "IW" ~ "IW",
      treatment == "SW" ~ "SW",
      TRUE ~ "W"
    ),
    Air.DT = w_t2,
    Soil.DT = w_t3,
    Warming.Method = case_when(
      w_t1 == "_0001" ~ "I",
      w_t1 == "_0110" ~ "O", 
      TRUE ~ NA
    ),
    Sampled.Year = start_year + as.numeric(sampling_year) - 1
  )

# checked
chenSOC <- chenSOC %>% 
  mutate(
    Treatment.Group = case_when(
      Others.1 == "N addition" | Others.1 == "low N addition" 
      | Others.1 == "Low N addition" | Others.1 == "N-addition"
      | Others.1 == "Nitrogen"
      | Others.1 == "high N addition" | Others.1 == "N+P" 
      | Others.1 == "High N addition"
      | Others.2 == "N addition" | Others.1 == "Fertilized" ~ "FW", 
      Others.1 == "CO2 elevated" | Others.2 == "CO2 elevated" | Others.1 == "CO2" ~ "CW", 
      Others.1 == "Increased precipitation" | Others.1 == 'Irrigation' 
      | Others.1 == "Precipitation" | Others.1 == "increased precipitation"
      | Others.1 == "Precipitation increased" | Others.1 == "Watering" ~ "IW",
      Others.1 == "Clipped" | Others.1 == "Grazing" | Others.1 == "Winter grazing"
      | Others.1 == "High Grazing" | Others.1 == "High grazing" | Others.1 == "Low grazing" ~ "GW",
      Others.1 == "CO2 elevated+Drought" ~ "CDW",
      Others.1 == "Drought" | Others.1 == "Precipitation reduced" 
      | Others.1 == "reduced precipitation" | Others.1 == "Reduced precipitation" ~ "DW",
      Others.1 == "N addition+ Increased precipitation" | Others.1 == "Irrigation+fertilization" 
      | Others.1 == "water addition+N addition(F10)" | Others.1 == "water addition+N addition(F50)" ~ "IFW",
      Others.1 == "N addition+ Reduced precipitation" ~ "DFW",
      Others.1 == "Irrigation+fertilization" | Others.1 == "N addition+Increased precipitation" ~ "IFW",
      Others.1 == "Burned" | Others.1 == "Burned+Dry" | Others.1 == "light burned" ~ "BW",
      TRUE ~ "W"
    ),
    Air.DT = case_when(
      Others == "Air" ~ Magnitude, 
      TRUE ~ NA_real_
    ),
    Soil.DT = case_when(
      Others == "Soil" ~ Magnitude,
      TRUE ~ NA_real_
    ),
    Warming.Method = case_when(
      Method == "OTC" ~ "O",
      Method == "IH" ~ "I",
      Method == "Cables" ~ "C",
      Others.1 == "OTC-1" |Others.1 == "OTC-2" | Others.1 == "OTC1" | Others.1 == "OTC2" ~ "O",
      TRUE ~ NA_character_
    )
  )


baiSOC <- baiSOC %>% 
  mutate(
    Warming.Method = case_when(
      Method == "Heating cables" ~ "C",
      Method == "IR heaters" ~ "I",
      Method == "greenhouse&OTC" ~ "O",
      TRUE ~ NA_character_
    )
)

# checked
sAGB <- sAGB %>% 
  mutate(
    Treatment.Group = case_when(
      Treatment == "+N" | Nitrogen == "y"  
      | Treatment == "+nutrients" | Treatment == "+ nutrients" |
        Treatment == "+NP" ~ "FW",
      Treatment == "+drought" | Treatment == "drought" 
      | Treatment == "Drought" ~ "DW",
      Treatment == "+H2O" | Treatment == "+water" 
      | Treatment == "wet" ~ "IW",
      Treatment == "+clipping" | Treatment == "+grazing"  | Treatment == "+cliping"
      | Treatment == "low grazing" | Treatment == "high grazing"
      | Treatment == "infrequently cut" | Treatment == "frequently cut"~ "GW",
      Treatment == "+grazing+N" | Treatment == "high grazing+N" | Treatment == "low grazing+N" ~ "GFW",
      TRUE ~ "W"
    ),
    Air.DT = case_when(
      `Note..temperature.type.` == "air temperature" ~ Tdelta,
      `Note..temperature.type..1` == "air temperature" ~ Tdelta.1,
      TRUE ~ NA_real_
    ),
    Soil.DT = case_when(
      `Note..temperature.type.` == "soil temperature" ~ Tdelta,
      `Note..temperature.type..1` == "soil temperature" ~ Tdelta.1,
      TRUE ~ NA_real_  
    )
  )

#---- Combine ------------------------------------------------------------------
# Soil: chenSOC, baiSOC, mesiSOC
soilDataset <- data.frame(
    "Study" = c(chenSOC$Publication, baiSOC$study, mesiSOC$citation),
    "Experiment" = c(chenSOC$Title, baiSOC$unique.site, mesiSOC$exp),
    "Latitude" = c(round(chenSOC$Latitude,2), round(baiSOC$Latitude,2), round(mesiSOC$Latitude,2)),
    "Longitude" = c(round(chenSOC$Longitude,2), round(baiSOC$Longitude,2), round(mesiSOC$Longitude,2)),
    "Treatment" = c(chenSOC$Treatment.Group, rep("W", nrow(baiSOC)), mesiSOC$Treatment.Group),
    "Sampled.Year" = c(rep(NA, nrow(chenSOC)), rep(NA, nrow(baiSOC)), mesiSOC$Sampled.Year),
    "Response" = c(rep("SOC", nrow(chenSOC)), rep("SOC", nrow(baiSOC)), mesiSOC$response),
    "Duration" = c(chenSOC$Duration, baiSOC$Duration, mesiSOC$sampling_year),
    "SCN" = c(rep(NA, nrow(chenSOC)), baiSOC$SCN, rep(NA, nrow(mesiSOC))),
    "C.n" = c(chenSOC$C.Num, baiSOC$C.Num, mesiSOC$rep_c),
    "T.n" = c(chenSOC$T.Num, baiSOC$T.Num, mesiSOC$rep_t),
    "AGB.C" = c(chenSOC$C.SOC, rep(NA, nrow(baiSOC)), mesiSOC$x_c),
    "SD.C" = c(chenSOC$SD.SOC, rep(NA, nrow(baiSOC)), mesiSOC$sd_c),
    "AGB.T" = c(chenSOC$T.SOC, rep(NA, nrow(baiSOC)), mesiSOC$x_t),
    "SD.T" = c(chenSOC$SD.SOC.1, rep(NA, nrow(baiSOC)), mesiSOC$sd_t),
    "Units" = c(chenSOC$Unit.SOC, rep(NA, nrow(baiSOC)), mesiSOC$x_units),
    "Depth" = c(chenSOC$Depth, rep(NA, nrow(baiSOC)), mesiSOC$sampling_depth),
    "Depth.Category" = c(chenSOC$Depth2, rep(NA, nrow(baiSOC)), mesiSOC$sampling_depth),
    "Air.DT" = c(chenSOC$Air.DT, rep(NA, nrow(baiSOC)), mesiSOC$Air.DT),
    "Soil.DT" = c(chenSOC$Soil.DT, baiSOC$SoilT1, mesiSOC$Soil.DT),
    "Warming.Method" = c(chenSOC$Warming.Method, baiSOC$Warming.Method, mesiSOC$Warming.Method),
    "MAT" = c(chenSOC$MAT, baiSOC$MAT, mesiSOC$MAT),
    "MAP" = c(chenSOC$MAP, baiSOC$MAP, mesiSOC$MAP),
    "Elevation" = c(chenSOC$Elevation, rep(NA, nrow(baiSOC)), mesiSOC$Elevation),
    "Ecosystem" = c(chenSOC$Vegetation, baiSOC$Ecosystem, mesiSOC$ecosystem_type),
    "Vegetation.Type" = c(chenSOC$Vegetation, baiSOC$Ecosystem, mesiSOC$vegetation_type)
)


soilCleaned <- soilDataset %>%
  # create a variable for the maximium of soil/ air warming
  mutate(
    maxWarming = round(pmax(Air.DT, Soil.DT), 1)
  ) %>%
  group_by(Latitude, 
           Longitude, 
           SOC.C, 
           SOC.T, 
           Treatment, 
           maxWarming) %>%
  mutate(non_na_count = rowSums(!is.na(across()))) %>%
  arrange(desc(non_na_count)) %>%
  summarise(
    across(
      .cols = everything(),
      .fns = ~ {
        first_non_na <- first(.[!is.na(.)])
        if (!is.na(first_non_na)) first_non_na else get_mode(.)
      }
    ),
    .groups = 'drop'
  ) %>%
  select(-non_na_count, -maxWarming) %>%
  subset(Treatment == "W")

' # template:
data.frame(
  "Study" = 
  "Experiment" =
  "Latitude" = 
  "Longitude" = 
  "Treatment" = 
  "Sampled.Year" = 
  "Response" = 
  "Duration" = 
  "SCN" = 
  "C.n" = 
  "T.n" = 
  "SOC.C" = 
  "SD.C" = 
  "SOC.T" = 
  "SD.T" = 
  "Units" = 
  "Depth" = 
  "Depth.Category" = 
  "Air.DT" = 
  "Soil.DT" =
  "Warming.Method" = 
  "MAT" = 
  "MAP" = 
  "Elevation" = 
  "Ecosystem" = 
  "Vegetation.Type" = 
  "Dominant.Vegetation" = 
)
'
biomassDataset <- data.frame(
  "Study" = c(zhouAGB$Study, mesiAGB$citation, sAGB$Study),
    "Experiment" = c(zhouAGB$so, mesiAGB$study, sAGB$Source),
    "Latitude" = c(zhouAGB$Latitude, mesiAGB$Latitude, sAGB$Latitude),
    "Longitude" = c(zhouAGB$Longitude, mesiAGB$Longitude, sAGB$Longitude),
    "Treatment" = c(zhouAGB$Treatment.Group, mesiAGB$Treatment.Group, sAGB$Treatment.Group),
    "Sampled.Year" = c(rep(NA,nrow(zhouAGB)), mesiAGB$Sampled.Year, rep(NA, nrow(sAGB))),
    "Response" = c(zhouAGB$Biomass, mesiAGB$response, rep("AGB", nrow(sAGB))), 
    "Duration" = c(zhouAGB$Duration, mesiAGB$sampling_year, sAGB$Duration),
    "SCN" = c(zhouAGB$SCN, rep(NA, nrow(mesiAGB)), sAGB$SCN),
    "C.n" = c(zhouAGB$n, mesiAGB$rep_c, sAGB$n.1), 
    "T.n" = c(zhouAGB$n, mesiAGB$rep_t, sAGB$n.1),
    "SOC.C" = c(zhouAGB$Control, mesiAGB$x_c, sAGB$Aboveground..Biomass.control),
    "SD.C" = c(zhouAGB$Csd, mesiAGB$sd_c, sAGB$Csd.1),
    "SOC.T" = c(zhouAGB$Warmed, mesiAGB$x_t, sAGB$Aboveground.Biomass.warmed),
    "SD.T" = c(zhouAGB$Tsd, mesiAGB$sd_t, sAGB$Tsd.1),
    "Units" = c(rep(NA, nrow(zhouAGB)), mesiAGB$x_units, rep(NA, nrow(sAGB))),
    "Air.DT" = c(zhouAGB$Air.DT, mesiAGB$Air.DT, sAGB$Air.DT),
    "Soil.DT" = c(zhouAGB$Soil.DT, mesiAGB$Soil.DT, sAGB$Soil.DT),
    "Warming.Method" = c(zhouAGB$Warming.Method, mesiAGB$Warming.Method, sAGB$Warming.method),
    "MAT" = c(zhouAGB$MAT, mesiAGB$MAT, sAGB$MAT),
    "MAP" = c(zhouAGB$MAP, mesiAGB$MAP, sAGB$MAP),
    "Elevation" = c(rep(NA, nrow(zhouAGB)), mesiAGB$Elevation, rep(NA, nrow(sAGB))),
    "Ecosystem" = c(zhouAGB$Ecosystems, mesiAGB$ecosystem_type, sAGB$Ecosystems),
    "Vegetation.Type" = c(zhouAGB$`Vegetation type`, mesiAGB$vegetation_type, sAGB$Vegetation.type),
    "Dominant.Vegetation" = c(zhouAGB$`Mycorrhizal Type`, mesiAGB$dominant_species, sAGB$Mycorrhizal.Type)
)

biomassCleaned <- biomassDataset %>%
  # create a variable for the maximium of soil/ air warming
  mutate(
    maxWarming = round(pmax(Air.DT, Soil.DT), 1)
  ) %>%
  group_by(Latitude, 
           Longitude, 
           SOC.C, 
           SOC.T, 
           Treatment, 
           maxWarming) %>%
  mutate(non_na_count = rowSums(!is.na(across()))) %>%
  arrange(desc(non_na_count)) %>%
  summarise(
    across(
      .cols = everything(),
      .fns = ~ {
        first_non_na <- first(.[!is.na(.)])
        if (!is.na(first_non_na)) first_non_na else get_mode(.)
      }
    ),
    .groups = 'drop'
  ) %>%
  select(-non_na_count, -maxWarming) %>%
  subset(Treatment == "W")

biomassCleaned[] <- lapply(biomassCleaned, function(x) iconv(as.character(x), from = "", to = "UTF-8", sub = ""))


safe_write_xlsx(soilCleaned, "/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/soilDataset11-7.xlsx")
safe_write_xlsx(biomassCleaned, "/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/biomassDataset11-7.xlsx")
safe_write_xlsx(chenPairs, "/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedChenDatsaset11-7.xlsx")

# Now that the data has been exported, need to fill in missing data, and then perform a unit conversion where necessary
