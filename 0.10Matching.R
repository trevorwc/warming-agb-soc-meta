#==== Data Matching ============================================================
 #    After identifying potential additional sites using 
 #    0.00Cleaning.R
 #    and filling in any missing year data,
 #    the goal of this script is to identify which sites can be matched

#==== Imports ==================================================================
library(openxlsx)

#==== Functions ================================================================

safe_write_xlsx <- function(data, path) {
  if (!file.exists(path)) {
    write_xlsx(data, path)
    message("File written to: ", path)
  } else {
    message("File already exists: ", path)
  }
}

#==== Main =====================================================================
# Read in the datasets
agb <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/biomassDataset11-7.xlsx')
soc <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/soilDataset11-7.xlsx')

siteVars <- c('Latitude', 'Longitude', 'Sampled.Year')

sitesYears <- agb %>% select(siteVars) %>% unique() %>%
  inner_join(soc %>% select(siteVars) %>% unique(), by = siteVars) %>%
  subset(Sampled.Year > 0)
 # 10 sites with AGB/ SOC measured in the same year

agb_complete <- agb %>% inner_join(sitesYears, by = siteVars)

soc_complete <- soc %>% inner_join(sitesYears, by = siteVars)
 # All complete cases

# Group by variables of interest, then perform fixed-effects meta-analysis 
# at the site level

safe_write_xlsx(agb_complete, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/agbDataset_complete11-7.xlsx')
safe_write_xlsx(soc_complete, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/socDataset_complete11-7.xlsx')

# After filling in missing data

agb_filled <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/agbDataset_complete11-7.xlsx')
soc_filled <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/socDataset_complete11-7.xlsx')

#....The goal is to first average/sum to "treatment" then pair to site
 #...because without going into original studies cannot "match"

# Fixed effects at the site-level (checked that each site has unique units)
table(soc_filled$Latitude, soc_filled$Units)
table(agb_filled$Latitude, agb_filled$Units)

scn <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/metaAGBSOC_6-7-24.xlsx')
pairedSites <- read.xlsx('/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedChenDatsaset11-7.xlsx')

pairedSCN <- pairedSites %>%
  mutate(
    C.SOC = as.numeric(C.SOC),
    T.SOC = as.numeric(T.SOC),
    Latitude = round(Latitude, 2),       
    Longitude = round(Longitude, 2)      
  ) %>%
  left_join(
    scn %>%
      mutate(
        SOC.C = as.numeric(SOC.C),
        SOC.T = as.numeric(SOC.T),
        Latitude = round(Latitude, 2),   
        Longitude = round(Longitude, 2) 
      ) %>%
      select(Latitude, Longitude, avg_SCN, SCN_source, SOC.C, SOC.T),
    by = c("Latitude", "Longitude", "C.SOC" = "SOC.C", "T.SOC" = "SOC.T")
  )

non_matches <- scn %>%
  mutate(
    SOC.C = as.numeric(SOC.C),
    SOC.T = as.numeric(SOC.T)
  ) %>%
  anti_join(
    pairedSCN %>%
      select(Latitude, Longitude, avg_SCN, SCN_source, C.SOC, T.SOC) %>%
      rename(SOC.C = C.SOC, SOC.T = T.SOC),  # Rename to match `scn` column names
    by = c("Latitude", "Longitude", "SOC.C", "SOC.T")
  )

bulkSites <- data.frame(
  Latitude = c(soc_site$Latitude, pairedSites$Latitude),
  Longitude = c(soc_site$Longitude, pairedSites$Longitude)
) %>% unique()

write.csv(bulkSites, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/bulkSites.csv')
safe_write_xlsx(pairedSCN, '/Users/trevor/Desktop/Research/Warming Ecosystem C/CleanedData/2Exported/pairedSCNDatsaset11-13.xlsx')

# Now ensure that soil C:N and bulk density are filled in (or input values)
# Fill in any additional missing values 


