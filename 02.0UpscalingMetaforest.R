# The goal of this script is to upscale the metaforest to the global scale
# Some processing in Google Earth Engine is reqd. 

library(ncdf4)
library(raster)
library(terra)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tiff)

pre_1991_2000 <- nc_open("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/pre/cru_ts4.08.1991.2000.pre.dat.nc")
pre_1991_2000_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/pre/cru_ts4.08.1991.2000.pre.dat.nc", varname = "pre")
pre_1991_2000_df <- as.data.frame(pre_1991_2000_raster, xy = TRUE)

pre_2001_2010_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/pre/cru_ts4.08.2001.2010.pre.dat.nc", varname = "pre")
pre_2001_2010_df <- as.data.frame(pre_2001_2010_raster, xy = TRUE)

pre_2011_2020_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/pre/cru_ts4.08.2011.2020.pre.dat.nc", varname = "pre")
pre_2011_2020_df <- as.data.frame(pre_2011_2020_raster, xy = TRUE)

pre_2021_2023_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/pre/cru_ts4.08.2021.2023.pre.dat.nc", varname = "pre")
pre_2021_2023_df <- as.data.frame(pre_2021_2023_raster, xy = TRUE)

pre_1991_2023 <- pre_1991_2000_df %>%
  full_join(pre_2001_2010_df, by = c("x","y")) %>%
  full_join(pre_2011_2020_df, by = c("x", "y")) %>%
  full_join(pre_2021_2023_df, by = c("x", "y"))

tmp_1991_2000 <- nc_open("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/tmp/cru_ts4.08.1991.2000.tmp.dat.nc")
tmp_1991_2000_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/tmp/cru_ts4.08.1991.2000.tmp.dat.nc", varname = "tmp")
tmp_1991_2000_df <- as.data.frame(tmp_1991_2000_raster, xy = TRUE)

tmp_2001_2010_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/tmp/cru_ts4.08.2001.2010.tmp.dat.nc", varname = "tmp")
tmp_2001_2010_df <- as.data.frame(tmp_2001_2010_raster, xy = TRUE)

tmp_2011_2020_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/tmp/cru_ts4.08.2011.2020.tmp.dat.nc", varname = "tmp")
tmp_2011_2020_df <- as.data.frame(tmp_2011_2020_raster, xy = TRUE)

tmp_2021_2023_raster <- brick("/Users/trevor/Desktop/Research/Warming Ecosystem C/cru/tmp/cru_ts4.08.2021.2023.tmp.dat.nc", varname = "tmp")
tmp_2021_2023_df <- as.data.frame(tmp_2021_2023_raster, xy = TRUE)

tmp_1991_2023 <- tmp_1991_2000_df %>%
  full_join(tmp_2001_2010_df, by = c("x","y")) %>%
  full_join(tmp_2011_2020_df, by = c("x", "y")) %>%
  full_join(tmp_2021_2023_df, by = c("x", "y"))

years = seq(1994, 2023, by = 1)

# sum the monthly precipitation to get yearly 
for(year in years){
  varname <- paste0("avg_pre_", year)
  pre_1991_2023 <- pre_1991_2023 %>%
    mutate(!!varname := rowSums(across(contains(as.character(year))), na.rm = TRUE))
}

# average the monthly temperature to get yearly 
for(year in years){
  varname <- paste0("avg_tmp_", year)
  tmp_1991_2023 <- tmp_1991_2023 %>%
    mutate(!!varname := rowMeans(across(contains(as.character(year))), na.rm = TRUE))
}


#######
# then calculate MAT (C) and MAP (mm) from 1994-2023

pre_1991_2023 <- pre_1991_2023 %>%
  mutate("MAP" := rowMeans(across(contains(as.character("avg"))), na.rm = TRUE))

tmp_1991_2023 <- tmp_1991_2023 %>%
  mutate("MAT" := rowMeans(across(contains(as.character("avg"))), na.rm = TRUE))

clim_1994_2023 <- pre_1991_2023 %>% dplyr::select(c(x,y, MAP)) %>% full_join(
  tmp_1991_2023 %>% dplyr::select(c(x,y,MAT)), by =c("x", "y")
)

###### now predict the results

# make a custom color palette

metafor1 <- readRDS("/Users/trevor/Desktop/Research/Warming Ecosystem C/Code/metaforest1.rds")

clim_2 <- clim_1994_2023 %>%
  mutate(DT = 2) %>%
  subset(MAT > -999 & MAP > -999)

clim_2 <- clim_2 %>% mutate(
  pct = 100 * (exp(prediction) - 1)
)

clim_2 <- clim_2 %>% mutate(
  "PredictedAGB" <- data.frame(predict(metafor1, clim_2))
) %>%
  mutate(prediction = ifelse(abs(y) <= 23.5, NA, prediction))

plot2c <- ggplot() +
  geom_raster(data = clim_2, aes(x = x, y = y, fill = prediction)) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "yellow",
    high = "darkgreen",
    midpoint = 0
  ) +
  coord_quickmap() +
  theme(
    panel.background = element_blank(),       
    plot.background = element_blank(),        
    panel.grid.major = element_blank(),       
    panel.grid.minor = element_blank(),       
    axis.line = element_blank(),               
    axis.ticks = element_blank(),              
    axis.text = element_blank(), 
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5) ,
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  ) +
  #ggtitle(expression(AGB ~ "response to" ~ 2 ~ degree ~ C ~ "warming"))
  labs(fill = "AGB C (ln RR)")

plot2c <- ggplot() +
  geom_raster(data = clim_2, aes(x = x, y = y, fill = pct)) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "yellow",
    high = "darkgreen",
    midpoint = 0
  ) +
  coord_quickmap() +
  theme(
    panel.background = element_blank(),       
    plot.background = element_blank(),        
    panel.grid.major = element_blank(),       
    panel.grid.minor = element_blank(),       
    axis.line = element_blank(),               
    axis.ticks = element_blank(),              
    axis.text = element_blank(), 
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5) ,
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  ) +
  #ggtitle(expression(AGB ~ "response to" ~ 2 ~ degree ~ C ~ "warming"))
  labs(fill = "AGB C (%)")

plot2c

predictionRaster <- rasterFromXYZ(clim_2 %>% dplyr::select(c(x,y,prediction)))

predictionRaster_pct <- rasterFromXYZ(clim_2 %>% dplyr::select(c(x,y,pct)))

plot(predictionRaster)

plot(predictionRaster_pct)

predictionRaster

predictionRaster_pct

crs(predictionRaster) <- "+proj=longlat +datum=WGS84 +no_defs"

crs(predictionRaster_pct) <- "+proj=longlat +datum=WGS84 +no_defs"

writeRaster(predictionRaster_pct, "/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/prediction_2c_pct.tif", format = "GTiff")

# Process in Google Earth Engine
# https://code.earthengine.google.com/f5136e3c264fbf86a82e412136173244
# but results should be available

d_agb <- raster("/Users/trevor/Desktop/Research/Warming Ecosystem C/Raw Data/exported_image_0_5_degree_pct.tif")

plot(d_agb)

d_agb_df <- as.data.frame(d_agb, xy = TRUE) 

d_agb_df <- d_agb_df %>% mutate(agb = agb/100)

ggplot() +
  geom_raster(data = d_agb_df, aes(x = x, y = y, fill = agb)) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "gold",
    high = "darkgreen",
    midpoint = 0,
    na.value = "white"
  ) +
  coord_quickmap() +
  theme(
    panel.background = element_blank(),       
    plot.background = element_blank(),        
    panel.grid.major = element_blank(),       
    panel.grid.minor = element_blank(),       
    axis.line = element_blank(),               
    axis.ticks = element_blank(),              
    axis.text = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  ) +
  labs(fill = "AGB C (Mg/ha)")

sum(subset(d_agb_df, agb > -999)$agb)

d_agb <- ggplot() +
  geom_raster(data = clim_2, aes(x = x, y = y, fill = prediction, color = "darkgray")) +
  coord_quickmap() +
  theme(
    panel.background = element_blank(),       
    plot.background = element_blank(),        
    panel.grid.major = element_blank(),       
    panel.grid.minor = element_blank(),       
    axis.line = element_blank(),               
    axis.ticks = element_blank(),              
    axis.text = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  )+
  geom_raster(data = d_agb_df %>% subset( agb > -999), aes(x = x, y = y, fill = agb)) +
  scale_fill_gradient2(
    low = "darkred",
    mid = adjustcolor("yellow"),
    high = "darkgreen",
    midpoint = 0
  ) +
  theme(
    panel.background = element_blank(),       
    plot.background = element_blank(),        
    panel.grid.major = element_blank(),       
    panel.grid.minor = element_blank(),       
    axis.line = element_blank(),               
    axis.ticks = element_blank(),              
    axis.text = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  ) +
  labs(fill = "AGB C (Mg/ha)")

grid.arrange(plot2c, d_agb, ncol = 1)

