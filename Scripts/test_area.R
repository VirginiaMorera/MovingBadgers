test_data <- read.csv("Data/Raw/Location - SP051515-data-as-joinbyfield-2025-07-03 13_23_01.csv", 
                      row.names = NULL, header = T)

elev <- rast("Data/Env_vars/elev_Roscommon_study_area.tif")

test_sf <- test_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  select(-Time)

area <- st_convex_hull(st_union(test_sf)) 

farm <- data.frame(Longitude = c(-8.36405556,  -8.341085),   
                   Latitude = c(53.61894444,  53.676970), 
                   Location = c("Farm 1", "Farm 2")) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

elev <- project(elev, crs(area)) %>%
  crop(st_bbox(area))


captures <- data.frame(Longitude = c(-8.333063, -8.367243,  -8.365111),  
                   Latitude = c(53.61894444, 53.660320,  53.605194), 
                   Location = c("Sett 1", "Capture 1", "Capture 2")) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)



mapview(elev) + 
  mapview(list(test_sf, farm, captures),
        layer.name = c("Detections", "Farm", "Captures"), 
        col.regions = c("green", "red", "orange"), 
        map.types = c("OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))

st_write(test_sf , "test.kml", driver = "kml")


test_locations <- bind_rows(captures, farm)

write.csv(test_locations, "Data/test_locations.csv", row.names = F)
