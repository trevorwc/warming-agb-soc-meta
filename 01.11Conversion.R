#==== Data conversion ==========================================================
 #    After filling in any necessary but missing bulk density data
 #    want to create site-level averages and pair

#==== Package Imports ==========================================================
library(openxlsx)
library(dplyr)
library(stringr)
library(purrr)
library(meta)

#==== Functions ================================================================
`%nin%` <- Negate(`%in%`)

mode <- function(col) {
  frq_table <- table(col)
  mode <- names(frq_table)[which.max(frq_table)]
  return(mode)
}


safe_write_xlsx <- function(data, path) {
  if (!file.exists(path)) {
    write_xlsx(data, path)
    message("File written to: ", path)
  } else {
    message("File already exists: ", path)
  }
}

convert_to_g_m2 <- function(df) {
  #.... Given a dataframe of C pool measurements,
  #.... convert observations to g/m2
  if ("SOC.C" %in% colnames(df) & "AGB.C" %nin% colnames(df)){
    df %>% mutate(
      # If the df contains SOC measurements,
      # find the depth interval 
      Depth2 = str_remove(Depth, "cm"),  
      Depth_split = str_split(Depth2, "-"),  
      Depth_lower_cm = map_dbl(Depth_split, ~ as.numeric(.x[1])), 
      Depth_upper_cm = map_dbl(Depth_split, ~ as.numeric(.x[2])),
      Depth_cm = as.numeric(Depth_upper_cm) - as.numeric(Depth_lower_cm),
      Conversion_factor = case_when(
        Units == 'g kg-1' ~ 10*Bulk.Density*Depth_cm,
        Units == 'g m-2' ~ 1,
        Units == 'g_100g' ~ 100*Bulk.Density*Depth_cm
      ),
      SOC.C.gm2 = SOC.C * Conversion_factor,
      SOC.T.gm2 = SOC.T * Conversion_factor,
      SD.C.gm2 = SD.C * Conversion_factor,
      SD.T.gm2 = SD.T * Conversion_factor
    ) 
  } else if ("AGB.C" %in% colnames(df) & "SOC.C" %nin% colnames(df)){
    # If the df contains only AGB, 
    # then conversion is simpler
    # and biomass is assumed to be 50% C
    df %>% mutate(
      Conversion_factor = case_when(
        Units == "g_c_m2" ~ 1, 
        Units == "g_m2" ~ 0.5, 
        Units == "kg_m2" ~ 500
      ),
      AGB.C.gm2 = AGB.C * Conversion_factor,
      AGB.T.gm2 = AGB.T * Conversion_factor,
      SD.C.gm2 = SD.C * Conversion_factor,
      SD.T.gm2 = SD.T * Conversion_factor
    )
  } else if ("C.AGB" %in% colnames(df) & "C.SOC" %in% colnames(df)){ 
    # If both AGB and SOC are present in the df
    # (representing the paired dataset)
    # then want to do both conversions
    df %>% mutate(
      AGB.Conversion_factor = case_when(
        Unit.AGB == "g m-2" ~ 1,
        Unit.AGB == "kg ha-1" ~ 0.1,
        Unit.AGB == "g 0.25m-2" ~ 0.0625,
        TRUE ~ NA
      ),
      Depth2 = str_remove(Depth, "cm"),  
      Depth_split = str_split(Depth2, "-"),  
      Depth_lower_cm = map_dbl(Depth_split, ~ as.numeric(.x[1])), 
      Depth_upper_cm = map_dbl(Depth_split, ~ as.numeric(.x[2])),
      Depth_cm = as.numeric(Depth_upper_cm) - as.numeric(Depth_lower_cm),
      SOC.Conversion_factor = case_when(
        Unit.SOC == 'g kg-1' ~ 10*BD*Depth_cm,
        Unit.SOC == 'g m-2' ~ 1,
        Unit.SOC == 'kg m-2' ~ 1000
      ),
      C.AGB.gm2 = C.AGB * AGB.Conversion_factor,
      T.AGB.gm2 = T.AGB * AGB.Conversion_factor,
      C.SD.AGB.gm2 = SD.AGB * AGB.Conversion_factor,
      T.SD.AGB.gm2 = SD.AGB.1 * AGB.Conversion_factor,
      C.SOC.gm2 = C.SOC * SOC.Conversion_factor,
      T.SOC.gm2 = T.SOC * SOC.Conversion_factor,
      C.SD.SOC.gm2 = SD.SOC * SOC.Conversion_factor,
      T.SD.SOC.gm2 = SD.SOC.1 * SOC.Conversion_factor
    )
  }
}

site_level_avg <- function(df) {
  #.... Perform a fixed effects meta-analysis
  #.... at the site level
  groupVars = c("Latitude", "Longitude", "Sampled.Year")
  # Group by latitude, longitude, and the sampled year
  if ('AGB.C' %in%  colnames(df) | 'AGB.C.gm2' %in% colnames(df) | 'C.AGB.gm2' %in% colnames(df)){
    # dynamically adjust X and SD variables
    if ('C.SD.AGB.gm2' %in% colnames(df)){
      cVar = "C.AGB.gm2"
      tVar = "T.AGB.gm2"
      cSD = 'C.SD.AGB.gm2'
      tSD = 'T.SD.AGB.gm2'
      nVarC = 'C.Num.1'
      nVarT = 'T.Num.1'
    } else { 
      cVar = "AGB.C.gm2"
      tVar = "AGB.T.gm2"
      cSD = "SD.C.gm2"
      tSD = "SD.T.gm2"
      nVarC = 'C.n'
      nVarT = 'T.n'
    }
    df %>% 
      mutate(
        maxWarming = round(pmax(Air.DT, Soil.DT, na.rm = TRUE), 1)
      ) %>%
      group_by(across(all_of(c(groupVars, "Warming.Treatment.Group")))) %>%
      summarize(
        M.C = sum((1/(.data[[cSD]]^2))*.data[[cVar]])/ sum(1/(.data[[cSD]]^2)),
        V.C = sum(1/(1/(.data[[cSD]]^2))),
        SE.C = sqrt(V.C),
        M.T = sum((1/(.data[[tSD]]^2))*.data[[tVar]])/ sum(1/(.data[[tSD]]^2)),
        V.T = sum(1/(1/(.data[[tSD]]^2))),
        SE.T = sqrt(V.T),
        C.n = sum(.data[[nVarC]]),
        T.n = sum(.data[[nVarT]]),
        MAT = mean(MAT),
        MAP = mean(MAP),
        Elevation = mean(Elevation),
        Ecosystem = mode(Ecosystem),
        Air.DT = mean(Air.DT),
        Soil.DT = mean(Soil.DT),
        .groups = "drop"
      )
  } else {
    if ('C.SD.SOC.gm2' %in% colnames(df)){
      cVar = "C.SOC.gm2"
      tVar = "T.SOC.gm2"
      cSD = 'C.SD.SOC.gm2'
      tSD = 'T.SD.SOC.gm2'
      nVarC = 'C.Num'
      nVarT = 'T.Num'
    } else {
      cVar = "SOC.C.gm2"
      tVar = "SOC.T.gm2"
      cSD = 'SD.C.gm2'
      tSD = 'SD.T.gm2'
      nVarC = 'C.n'
      nVarT = 'T.n'
    }
    # When analyzing SOC, we additionally want to 
    # group by depth
    groupVars = c(groupVars, "Depth")
    df %>% 
      group_by(across(all_of(c(groupVars, "Warming.Treatment.Group")))) %>%
      summarize(
        M.C = sum((1/(.data[[cSD]]^2))*.data[[cVar]])/ sum(1/(.data[[cSD]]^2)),
        V.C = sum(1/(1/(.data[[cSD]]^2))),
        SE.C = sqrt(V.C),
        M.T = sum((1/(.data[[tSD]]^2))*.data[[tVar]])/ sum(1/(.data[[tSD]]^2)),
        V.T = sum(1/(1/(.data[[tSD]]^2))),
        SE.T = sqrt(V.T),
        C.n = sum(.data[[nVarC]]),
        T.n = sum(.data[[nVarT]]),
        avg_SCN = mean(avg_SCN),
        MAT = mean(MAT),
        MAP = mean(MAP),
        Elevation = mean(Elevation),
        Air.DT = mean(Air.DT),
        Soil.DT = mean(Soil.DT),
        Ecosystem = mode(Ecosystem),
        .groups = "drop"
      )
  }
}

sum_depth <- function(df) {
  #.... Now, given a df of SOC measurements averaged to depth,
  #.... want to sum adjacent "horizons"
  #.... for an understanding of the total soil stock
  df %>% mutate(
    Depth2 = str_remove(Depth, "cm"),  
    Depth_split = str_split(Depth2, "-"),  
    Depth_lower_cm = map_dbl(Depth_split, ~ as.numeric(.x[1])), 
    Depth_upper_cm = map_dbl(Depth_split, ~ as.numeric(.x[2])),
  ) %>% 
    group_by(across(all_of(c("Latitude", "Longitude","Sampled.Year", "Warming.Treatment.Group")))) %>%
    summarize(
      M.C = sum(M.C),
      V.C = sum(V.C),
      SE.C = sqrt(V.C),
      M.T = sum(M.T),
      V.T = sum(V.T),
      SE.T = sqrt(V.T),
      Depth = paste0(min(Depth_lower_cm), "-", max(Depth_upper_cm), "cm"),
      avg_SCN = mean(avg_SCN),
      MAT = mean(MAT),
      MAP = mean(MAP),
      Elevation = mean(Elevation),
      Air.DT = mean(Air.DT),
      Soil.DT = mean(Soil.DT),
      Ecosystem = mode(Ecosystem),
      .groups = "drop" 
    )
}

get_last_obs <- function(df) {
  #.... Given a df of averaged (and for SOC, summed) obs,
  #.... return a df which contains the last observation
  groupVars = c('Latitude', 'Longitude', 'Warming.Treatment.Group')
  df %>%
    group_by(across(all_of(groupVars))) %>%
    slice_max(Sampled.Year)
}

calc_eco <- function(df) {
  df$M.C.ECO = df$M.C.AGB + df$M.C.SOC
  df$M.T.ECO = df$M.T.AGB + df$M.T.SOC
  
  df$V.C.ECO = df$V.C.AGB + df$V.C.SOC
  df$V.T.ECO = df$V.T.AGB + df$V.T.SOC
  
  return(df)
}

calc_eco_ind <- function(df) {
  df$C.ECO = df$C.AGB.gm2 + df$C.SOC.gm2
  df$T.ECO = df$T.AGB.gm2 + df$T.SOC.gm2
  
  df$V.C.ECO = (df$C.SD.AGB.gm2)^2 + (df$C.SD.SOC.gm2)^2
  df$V.T.ECO = (df$T.SD.AGB.gm2)^2 + (df$T.SD.SOC.gm2)^2
  
  return(df)
}

standardize_ecosystem_type <- function(df) {
  if ("Ecosystem.AGB" %in% colnames(df)){
    col = "Ecosystem.AGB"
  } else {
    col = "Ecosystem"
  }
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
 #... Any missing data has been filled in to the greatest extent possible

paired <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedSCNDatsaset11-13.xlsx') %>%
  mutate(
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
  ) %>%
  rename(Ecosystem = Vegetation)
soc <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/socDataset_complete11-7.xlsx')
agb <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/agbDataset_complete11-7.xlsx')

#==== Main Analysis =====

#..1. Convert SOC to g/m2

soc.gm2 <- soc %>% convert_to_g_m2()

#..2. Average SOC to depth
soc.gm2.avg <- soc.gm2 %>% site_level_avg()

#..3. Sum SOC depths

soc.gm2.avg.sum <- soc.gm2.avg %>% sum_depth()

#..4. Convert AGB to g/m2

agb.gm2 <- agb %>% convert_to_g_m2()

#..5. Average AGB to site

agb.gm2.avg <- agb.gm2 %>% site_level_avg()

#..6. Average paired data to site

#...6.1. Convert units

paired.gm2 <- paired %>% convert_to_g_m2()


#...6.2. Create separate datasets


commonVars <- c("Title", "Publication", "Latitude", "Longitude", "Elevation", "Ecosystem",
                "MAP", "MAT", "Method", "Sampled.Year", "Time", "Season", "Magnitude",
                "Others", "Duration", "InitialSOC", "avg_SCN", "SCN_source", "Air.DT", "Soil.DT", "Warming.Treatment.Group",
                "C.Num","T.Num", "C.Num.1", "T.Num.1")

agbAddtlVars <- c("C.AGB.gm2", "T.AGB.gm2", "C.SD.AGB.gm2", "T.SD.AGB.gm2")

socAddtlVars <- c("C.SOC.gm2", "T.SOC.gm2", "C.SD.SOC.gm2", "T.SD.SOC.gm2", "Depth",
                  "Depth_split", "Depth_lower_cm", "Depth_upper_cm", "Depth_cm")


agbpaired.gm2 <- paired.gm2 %>% dplyr::select(all_of(c(commonVars, agbAddtlVars))) 

socpaired.gm2 <- paired.gm2 %>% dplyr::select(all_of(c(commonVars, socAddtlVars)))

#...6.3. Average to site/depth

socpaired.gm2.avg <- socpaired.gm2 %>% site_level_avg()

agbpaired.gm2.avg <- agbpaired.gm2 %>% site_level_avg()

#...6.4. Sum

soc.paired.gm2.avg.sum <- socpaired.gm2.avg %>% sum_depth()

#...6.5. Pair site averages
 # Want to pair sites on their lat, lon, warming 
paired_matched <- agbpaired.gm2.avg %>% 
  inner_join(soc.paired.gm2.avg.sum, 
             by = c('Latitude', 'Longitude', 'Warming.Treatment.Group', 'Sampled.Year'),
             suffix = c(".AGB", ".SOC"))
unpaired_matched <- agb.gm2.avg %>%
  inner_join(soc.gm2.avg.sum,
             by = c('Latitude', 'Longitude', 'Warming.Treatment.Group', 'Sampled.Year'),
             suffix = c(".AGB", ".SOC"))

#..7. Create final dataframe

paired_full <- rbind(paired_matched, unpaired_matched) %>%
  calc_eco() %>%
  standardize_ecosystem_type()

paired_last <- paired_full %>% 
  get_last_obs() %>%
  subset(V.C.AGB > 0)

paired_matched_std <- paired_matched %>% 
  calc_eco() %>% 
  standardize_ecosystem_type()

paired.gm2.eco <- paired.gm2 %>% calc_eco_ind() %>% standardize_ecosystem_type()

safe_write_xlsx(paired_full, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedfull_11-22.xlsx')
safe_write_xlsx(paired_last, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedlast_11-22.xlsx')

safe_write_xlsx(paired_matched_std, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedmatched_11-23.xlsx')
safe_write_xlsx(paired.gm2.eco, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/paired.gm2.eco_11-23.xlsx')





