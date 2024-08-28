library(dplyr)
library(openxlsx)
library(reshape2)
library(dplyr)
library(tidyr)
library(writexl)

##### Functions #####
"%nin%" <- Negate("%in%")

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
##### Main ####

# Read in the datasets

chenSoilC <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/SoilC - Ji Chen.csv')
zhouBiomass <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/zhou_etal2022edited.csv')[,0:52]
MESI <- read.csv('/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/mesi_main.csv')

# Now just want to clean up the datasets a little bit
# First, we can subset the MESI to warming treatments, and relevant responses

MESIw <- subset(MESI, grepl("w", treatment))
MESIBiomass <- subset(MESIw, 
                      #response == "fine_root_biomass" |
                      #response == "total_biomass" |
                      # response == "bgb_coarse" |
                      # response == "bgb" |
                      response == "agb" |
                        # response == "leaf_biomass" |
                        #response == "stem_biomass" |
                        response == "agb_c" |
                        response == "agb_coarse" #|
                      #response == "bgb_c" |
                      #response == "total_biomass_c" |
                      #response == "wood_biomass" |
                      #response == "wood_c"
)

MESISoil <- subset(MESIw, response == "soc" | response == "soil_total_c" )

# Make a new treatment group variable
MESISoil <- MESISoil %>% mutate(
  Treatment.Group = case_when(
    treatment == "cdw" ~ "CDW", 
    treatment == "cfw" ~ "CFW",
    treatment == "cw" ~ "CW",
    treatment == "dw" ~ "DW",
    treatment == "fw" ~ "FW",
    treatment == "w" ~ "W"
  )) 

chenSoilC <- chenSoilC %>% mutate(
  Treatment.Group = case_when(
    Others.1 == "N addition" | Others.1 == "low N addition" | Others.1 == "high N addition" | Others.1 == "N+P" | 
      Others.2 == "N addition" ~ "FW", 
    Others.1 == "CO2 elevated" | Others.2 == "CO2 elevated" ~ "CW", Others.1 == "Increased precipitation" ~ "PW",
    Others.1 == "Clipped" ~ "GW", Others.1 == "Grazing" | Others.1 == "Winter grazing" ~ "GW", 
    TRUE ~ "W"
  )
)

MESIBiomass <- MESIBiomass %>% mutate(
  Treatment.Group = case_when(
    treatment == "cdfw" ~ "CDFW",
    treatment == "cdw" ~ "CDW", 
    treatment == "cifw" ~ "CIFW",
    treatment == "cfw" ~ "CFW",
    treatment == "cw" ~ "CW",
    treatment == "dw" ~ "DW",
    treatment == "fw" ~ "FW",
    treatment == "ifw" ~ "IFW",
    treatment == "iw" ~ "IW",
    treatment == "sw" ~ "SW",
    treatment == "w" ~ "W"
  )) 

zhouBiomass <- zhouBiomass %>% mutate(
  Treatment.Group = case_when(
    Treatment == "+N" ~ "FW",
    Treatment == "+grazing" ~ "GW",
    Treatment == "+grazing+N" ~ "GFW", 
    TRUE ~ "W"
  )
)

# Now, want to create two new variables, soil and air delta T

MESISoil$Air.DT <- MESISoil$w_t2
MESISoil$Soil.DT <- MESISoil$w_t3

MESIBiomass$Air.DT <- MESIBiomass$w_t2
MESIBiomass$Soil.DT <- MESIBiomass$w_t3

chenSoilC <- chenSoilC %>% mutate(
  Air.DT = case_when(
    Others == "Air" ~ Magnitude, 
    TRUE ~ -999
  ),
  Soil.DT = case_when(
    Others == "Soil" ~ Magnitude,
    TRUE ~ -999
  )
)


zhouBiomass <- zhouBiomass %>% mutate(
  Air.DT = case_when(
    Note..temperature.type. == "air temperature" ~ Tdelta,
    Note..temperature.type..1 == "air temperature" ~ Tdelta.1,
    TRUE ~ -999
  ),
  Soil.DT = case_when(
    Note..temperature.type. == "soil temperature" ~ Tdelta,
    Note..temperature.type..1 == "soil temperature" ~ Tdelta.1,
    TRUE ~ -999  
  )
)

# And then fix the warming methodology variable

MESISoil <- MESISoil %>% mutate(
  Warming.Method = case_when(
    w_t1 == "_0001" ~ "I",
    w_t1 == "_0110" ~ "O", 
    TRUE ~ "N.A."
  )
)

MESIBiomass <- MESIBiomass %>% mutate(
  Warming.Method = case_when(
    w_t1 == "_0110" ~ "O",
    w_t1 == "_0001" ~ "I",
    TRUE ~ "N.A."
  )
)

chenSoilC <- chenSoilC %>% mutate(
  Warming.Method = case_when(
    Method == "OTC" ~ "O",
    Method == "IH" ~ "I",
    Method == "Cables" ~ "C",
    Others.1 == "OTC-1" |Others.1 == "OTC-2" | Others.1 == "OTC1" | Others.1 == "OTC2" ~ "O",
    TRUE ~ "N.A."
  )
)

zhouBiomass <- zhouBiomass %>% mutate(
  Warming.Method = case_when(
    Warming.method == "heated OTC/greenhouse" ~ "O",
    Warming.method == "Infrared heater" ~ "I",
    Warming.method == "Cable" ~ "C",
    TRUE ~ "N.A."
  )
)

# Make a year sampled variable

MESISoil$Sampled.Year <- MESISoil$start_year + as.numeric(MESISoil$sampling_year) - 1

MESIBiomass$Sampled.Year <- MESIBiomass$start_year + as.numeric(MESIBiomass$sampling_year) - 1

chenSoilC$Sampled.Year <- "NA"

zhouBiomass$Sampled.Year <- zhouBiomass$Duration

###################

chenSoilC$LatLonUnique <- paste0(chenSoilC$Latitude, chenSoilC$Logitude, chenSoilC$Treatment.Group, chenSoilC$Warming.Method, chenSoilC$Air.DT, chenSoilC$Soil.DT)
zhouBiomass$LatLonUnique <- paste0(zhouBiomass$Lat, zhouBiomass$Long, zhouBiomass$Treatment.Group, zhouBiomass$Warming.Method, zhouBiomass$Air.DT, zhouBiomass$Soil.DT)
MESISoil$LatLonUnique <-paste0(MESISoil$lat, MESISoil$lon, MESISoil$Treatment.Group, MESISoil$Warming.Method, MESISoil$Air.DT, MESISoil$Soil.DT)
MESIBiomass$LatLonUnique <-paste0(MESIBiomass$lat, MESIBiomass$lon, MESIBiomass$Treatment.Group, MESIBiomass$Warming.Method, MESIBiomass$Air.DT, MESIBiomass$Soil.DT)

chenSoilCCompleteData <- subset(chenSoilC, C.AGB > -999 & T.AGB > -999 & C.SOC > -999 & T.SOC > -999)

chenSoilCIncompleteData <- subset(chenSoilC, LatLonUnique %nin% chenSoilCCompleteData$LatLonUnique)

soilSites <- c(chenSoilCIncompleteData$LatLonUnique, MESISoil$LatLonUnique) %>% unique()
biomassSites <- c(MESIBiomass$LatLonUnique, zhouBiomass$LatLonUnique) %>% unique()
chenSites <- unique(chenSoilCCompleteData$LatLonUnique)

commonLatLon <- intersect(soilSites, biomassSites)

nonOverlapSites <- commonLatLon[commonLatLon %nin% chenSoilCCompleteData$LatLonUnique]

MESIbUnique <- subset(MESIBiomass, LatLonUnique %in% nonOverlapSites)
MESIsUnique <- subset(MESISoil, LatLonUnique %in% nonOverlapSites)
# Filter out non-overlap sites 
zhouUnique <- subset(zhouBiomass, LatLonUnique %in% nonOverlapSites & LatLonUnique %nin% MESIbUnique$LatLonUnique)
chenICunique <- subset(chenSoilCIncompleteData,LatLonUnique %in% nonOverlapSites & LatLonUnique %nin% MESIsUnique$LatLonUnique)
length(unique(c(MESIbUnique$LatLonUnique, MESIsUnique$LatLonUnique, zhouUnique$LatLonUnique, chenICunique$LatLonUnique)) %nin% chenSoilCCompleteData$LatLonUnique)

##############

# Soil: MESI, Chen
soilDataset <- data.frame(
  "Study" = c(MESIsUnique$citation, chenICunique$Publication),
  "Experiment" = c(MESIsUnique$exp, chenICunique$Title),
  "Sampled.Year" = c(MESIsUnique$Sampled.Year, chenICunique$Sampled.Year),
  "Treatment" = c(MESIsUnique$Treatment.Group, chenICunique$Treatment.Group), 
  "Response" = c(MESIsUnique$response, rep("SOC", nrow(chenICunique))),
  "C.n" = c(MESIsUnique$rep_c, chenICunique$C.Num),
  "T.n" = c(MESIsUnique$rep_t, chenICunique$T.Num),
  "SOC.C" = c(MESIsUnique$x_c, chenICunique$C.SOC),
  "SD.C" = c(MESIsUnique$sd_c, chenICunique$SD.SOC),
  "SOC.T" = c(MESIsUnique$x_t, chenICunique$T.SOC),
  "SD.T" = c(MESIsUnique$sd_t, chenICunique$SD.SOC.1),
  "Units" = c(MESIsUnique$x_units, chenICunique$Unit.SOC),
  "Depth" = c(MESIsUnique$sampling_depth, chenICunique$Depth),
  "Depth.Category" = c(rep("N.A.", nrow(MESIsUnique)), chenICunique$Depth2),
  "Air.DT" = c(MESIsUnique$Air.DT, chenICunique$Air.DT),
  "Soil.DT" = c(MESIsUnique$Soil.DT, chenICunique$Soil.DT),
  "Warming.Method" = c(MESIsUnique$Warming.Method, chenICunique$Warming.Method), 
  "Latitude" = c(MESIsUnique$lat, chenICunique$Latitude),
  "Longitude" = c(MESIsUnique$lon, chenICunique$Logitude),
  "LatLonUnique" = c(MESIsUnique$LatLonUnique, chenICunique$LatLonUnique),
  "MAT" = c(MESIsUnique$mat, chenICunique$MAT),
  "MAP" = c(MESIsUnique$map, chenICunique$MAP),
  "Elevation" = c(MESIsUnique$elevation, chenICunique$Elevation),
  "Ecosystem" = c(MESIsUnique$ecosystem_type, chenICunique$Vegetation),
  "Vegetation.Type" = c(MESIsUnique$vegetation_type, chenICunique$Vegetation),
  "Dominant.Vegetation" = c(MESIsUnique$dominant_species, rep("N.A.", nrow(chenICunique)))
)

biomassDataset <- data.frame(
  "Study" = c(MESIbUnique$citation, zhouUnique$Source),
  "Experiment" = c(MESIbUnique$exp, zhouUnique$Experiment.name),
  "Sampled.Year" = c(MESIbUnique$Sampled.Year, zhouUnique$Sampled.Year),
  "Treatment" = c(MESIbUnique$Treatment.Group, zhouUnique$Treatment.Group),
  "Response" = c(MESIbUnique$response, rep("AGB", nrow(zhouUnique))),
  # It appears that nT and nC are the same (assuming for Zhou)
  "C.n" = c(MESIbUnique$rep_c, zhouUnique$n.1),
  "T.n" = c(MESIbUnique$rep_t, zhouUnique$n.1),
  "AGB.C" = c(MESIbUnique$x_c, zhouUnique$Aboveground..Biomass.control),
  "SD.C" = c(MESIbUnique$sd_c, zhouUnique$Csd.1),
  "AGB.T" = c(MESIbUnique$x_t, zhouUnique$Aboveground.Biomass.warmed),
  "SD.T" = c(MESIbUnique$sd_t, zhouUnique$Tsd.1),
  "Units" = c(MESIbUnique$x_units, rep("N.A.", nrow(zhouUnique))),
  "Air.DT" = c(MESIbUnique$Air.DT, zhouUnique$Air.DT),
  "Soil.DT" = c(MESIbUnique$Soil.DT, zhouUnique$Soil.DT),
  "Warming.Method" = c(MESIbUnique$Warming.Method, zhouUnique$Warming.Method), 
  "Latitude" = c(MESIbUnique$lat, zhouUnique$Lat),
  "Longitude" = c(MESIbUnique$lon, zhouUnique$Long),
  "LatLonUnique" = c(MESIbUnique$LatLonUnique, zhouUnique$LatLonUnique),
  "MAT" = c(MESIbUnique$mat, zhouUnique$MAT),
  "MAP" = c(MESIbUnique$map, zhouUnique$MAP),
  "Elevation" = c(MESIbUnique$elevation, rep("N.A.", nrow(zhouUnique))),
  "Ecosystem" = c(MESIbUnique$ecosystem_type, zhouUnique$Ecosystems),
  "Vegetation.Type" = c(MESIbUnique$vegetation_type, zhouUnique$Vegetation.type),
  # Might also want to fix this variable
  "Dominant.Vegetation" = c(MESIbUnique$dominant_species, zhouUnique$Species.1)
)

# Now want to rename the paired datapoints from Chen and export
chenPairedDataset <- data.frame(
  "Study" = chenSoilCCompleteData$Publication,
  "Experiment" = rep(chenSoilCCompleteData$Title),
  "Treatment" = chenSoilCCompleteData$Treatment.Group,
  "Warming.Method" = chenSoilCCompleteData$Warming.Method,
  "Response" = rep("SOC", nrow(chenSoilCCompleteData)),
  "C.n.SOC" = chenSoilCCompleteData$C.Num,
  "T.n.SOC" = chenSoilCCompleteData$T.Num,
  "SOC.C" = chenSoilCCompleteData$C.SOC,
  "SD.C.SOC" = chenSoilCCompleteData$SD.SOC,
  "SOC.T" = chenSoilCCompleteData$T.SOC,
  "SD.T.SOC" = chenSoilCCompleteData$SD.SOC.1,
  "Units.SOC" = chenSoilCCompleteData$Unit.SOC,
  "Depth.SOC" = chenSoilCCompleteData$Depth,
  "Notes.SOC" = chenSoilCCompleteData$Source.SOC, 
  "C.n.AGB" =chenSoilCCompleteData$C.Num.1,
  "T.n.AGB" = chenSoilCCompleteData$T.Num.1,
  "AGB.C" =chenSoilCCompleteData$C.AGB,
  "AGB.T" = chenSoilCCompleteData$T.AGB,
  "SD.C.AGB" = chenSoilCCompleteData$SD.AGB,
  "SD.T.AGB" = chenSoilCCompleteData$SD.AGB.1,
  "Units.AGB" = chenSoilCCompleteData$Unit.AGB,
  "Notes.AGB" = chenSoilCCompleteData$Source.AGB,
  "Air.DT" = chenSoilCCompleteData$Air.DT,
  "Soil.DT" = chenSoilCCompleteData$Soil.DT,
  "Latitude" = chenSoilCCompleteData$Latitude,
  "Longitude" = chenSoilCCompleteData$Logitude,
  "LatLonUnique" = chenSoilCCompleteData$LatLonUnique,
  "MAT" = chenSoilCCompleteData$MAT,
  "MAP" = chenSoilCCompleteData$MAP,
  "Elevation" = chenSoilCCompleteData$Elevation,
  "Ecosystem" = chenSoilCCompleteData$Vegetation,
  "Vegetation.Type" = chenSoilCCompleteData$Vegetation,
  "Dominant.Vegetation" = rep("N.A.", nrow(chenSoilCCompleteData))
)



# WILL OVERWRITE! must remove overwrite = FALSE
#write_xlsx(soilDataset, "/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/soilDataset6-5.xlsx", overwrite = FALSE)
#write_xlsx(biomassDataset, "/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/biomassDataset6-5.xlsx", overwrite = FALSE)
#write_xlsx(chenPairedDataset, "/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedChenDatsaset6-5.xlsx", overwrite = FALSE)

# Now that the data has been exported, need to fill in missing data, and then perform a unit conversion where necessary
