rm(list = ls())
source("Scripts/setup.R")

farm_parcels <- read_sf("Data/Raw/GEO_499_SubFeatures_ANON.shp")

ireland <- read_sf("Data/Ireland_ITM.shp")
st_crs(farm_parcels)

ggplot() + 
  geom_sf(data = ireland) + 
  geom_sf(data = farm_parcels, fill = "orange") + 
  theme_bw()

