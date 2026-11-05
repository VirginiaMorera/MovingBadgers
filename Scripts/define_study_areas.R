rm(list = ls())
source("Scripts/setup.R")

ireland <- read_sf("Data/Ireland_ITM.shp")

env_data <- rast("Data/Env_vars/all_covars_1km.grd")

env_data <- project(env_data, crs(ireland))

setts <- rast("Data/Raw/sett_pred_response.grd") %>% 
  project(ireland)

quart <- read_sf("Data/Raw/quartiles_turnover_date.shp") %>% 
  st_set_crs(29902) %>% 
  st_transform(st_crs(setts)) %>% 
  filter(!is.na(YEAR)) %>% 
  rename(TURNOVER_YEAR = YEAR)

removal <- readRDS("Data/Raw/culled_both_programmes.RDS") %>% 
  select(YEAR, PROGRAMME) %>% 
  mutate(DUM = 1, 
         YEAR = as.numeric(as.character(YEAR)))

removal_rast <- rasterize(removal, env_data, field = "DUM", fun = "sum") %>% 
  # subst(from = seq(60,99,1), to = rep(NA, 40), others=NULL) %>% 
  subst(NA, 0) %>% 
  mask(ireland)

plot(removal_rast)

f <- function(x) {
  length(unique(na.omit(x)))
}

removal_years <- rasterize(removal, env_data, field = "YEAR", fun = "f") %>% 
  subst(NA, 0) %>% 
  mask(ireland)

plot(removal_years)

removal_max <- rasterize(removal, env_data, 
                         field = "YEAR", fun = "max") %>% 
  subst(NA, 0) %>% 
  mask(ireland) 

removal_max_cull <- rasterize(removal %>% filter(PROGRAMME == "Culling"), env_data, 
                         field = "YEAR", fun = "max") %>% 
  subst(NA, 0) %>% 
  mask(ireland) 


y_table <- data.frame(id = c(0, 2005:2025), y = as.factor(c(0, 2005:2025)))

levels(removal_max) <-  y_table
levels(removal_max_cull) <- y_table


setts <- resample(setts, env_data)
sampling <- c(setts, removal_rast, removal_years, removal_max, removal_max_cull, 
              env_data$elevation, env_data$slope, env_data$tree_cover_density, 
              env_data$small_woody_features)

names(sampling) <- c("Sett density", "Badgers removed", "Years of removal", 
                     "Last removal", "Last culling programme removal", 
                     "Elevation", "Slope", "Tree cover density", "Small woody features")

plot(sampling)


writeRaster(sampling, file = "Data/culling_sett_env_data.grd")

# mapview will automatically create a layer toggle
# alpha = 0.5 gives 50% transparency

pal_continuous <- viridis(100)  # continuous for layers 1 & 2
pal_discrete <- viridis(21) # many discrete colors for layers 3-5

# Create mapview layers
layers <- list(
  mapview(sampling[[1]], layer.name = names(sampling)[1], alpha = 0.8, col.regions = pal_continuous),
  mapview(sampling[[2]], layer.name = names(sampling)[2], alpha = 0.8, col.regions = pal_continuous),
  mapview(sampling[[3]], layer.name = names(sampling)[3], alpha = 0.8, col.regions = pal_discrete),
  mapview(sampling[[4]], layer.name = names(sampling)[4], alpha = 0.8, col.regions = pal_discrete),
  mapview(sampling[[5]], layer.name = names(sampling)[5], alpha = 0.8, col.regions = pal_discrete), 
  mapview(sampling[[6]], layer.name = names(sampling)[6], alpha = 0.8, col.regions = pal_continuous),
  mapview(sampling[[7]], layer.name = names(sampling)[7], alpha = 0.8, col.regions = pal_continuous),
  mapview(sampling[[8]], layer.name = names(sampling)[8], alpha = 0.8, col.regions = pal_continuous), 
  mapview(sampling[[9]], layer.name = names(sampling)[9], alpha = 0.8, col.regions = pal_continuous)
)

polygon_layer <- mapview(ireland,
                         layer.name = "Counties",
                         col.regions = NA,      # transparent fill
                         alpha.regions = 0, 
                         aplha = 1,             # fully transparent fill
                         lwd = 1,               # line width
                         color = "black")       # outline color

# Combine all layers into one interactive map
Reduce(`+`, c(layers, list(polygon_layer)))






sum_setts <- terra::extract(setts, vect(ireland), fun = sum, na.rm = TRUE)
sum_culled <- terra::extract(removal_rast, vect(ireland), fun = sum, na.rm = TRUE)
max_years <- terra::extract(removal_years, vect(ireland), fun = max, na.rm = TRUE)
last_year <- terra::extract(removal_max_cull, vect(ireland), fun = max, na.rm = TRUE)



ireland_vals <- ireland %>% 
  select(NAME_TAG) %>% 
  mutate(tot_sett_dens = sum_setts$median, 
         tot_badgers_culled = sum_culled$sum, 
         n_years_culling = max_years$YEAR_f,
         most_recent_cull = last_year$y) %>% 
  mutate(tot_sett_dens = if_else(NAME_TAG %in% c("Antrim", "Tyrone", 
                                                 "Armagh", "Londonderry", "Down"), 
                                 NA, tot_sett_dens), 
         tot_badgers_culled = if_else(NAME_TAG %in% c("Antrim", "Tyrone", 
                                                 "Armagh", "Londonderry", "Down"), 
                                 NA, tot_badgers_culled), 
         n_years_culling = if_else(NAME_TAG %in% c("Antrim", "Tyrone", 
                                                      "Armagh", "Londonderry", "Down"), 
                                      NA, n_years_culling), 
         most_recent_cull = if_else(NAME_TAG %in% c("Antrim", "Tyrone", 
                                                      "Armagh", "Londonderry", "Down"), 
                                      NA, most_recent_cull))


ggplot() + 
  geom_sf(data = ireland_vals, aes(fill = tot_sett_dens)) + 
  # geom_sf(ireland, col = "red", fill = NA) + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  ggtitle("Sett density") + 
  
ggplot() + 
  geom_sf(data = ireland_vals, aes(fill = tot_badgers_culled)) + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  ggtitle("Badgers culled") + 
  
ggplot() + 
  geom_sf(data = ireland_vals, aes(fill = n_years_culling)) + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  ggtitle("Removal duration (years)") + 
  
ggplot() + 
  geom_sf(data = ireland_vals, aes(fill = most_recent_cull)) + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  ggtitle("Last removal")  


# per quartile

joined_data <- st_join(removal, quart, join = st_intersects)

# Drop geometry and count points per polygon
point_counts <- joined_data %>%
  st_drop_geometry() %>% 
  group_by(Q) %>% # Replace 'poly_id' with your polygon's unique ID column
  summarise(point_count = n(), 
            year_count = n_distinct(YEAR), 
            year_max = max(YEAR, na.rm = T))

# View the result
print(point_counts)

quart_data <- quart %>% 
  left_join(point_counts)


ggplot() + 
  geom_sf(data = quart_data, aes(fill = point_count), col = NA) + 
  geom_sf(data = ireland, fill = NA, col = "red") +
  scale_fill_viridis_c() + 
  ggtitle("Number of badgers culled") + 
  theme_bw() + 

ggplot() + 
  geom_sf(data = quart_data, aes(fill = year_count), col = NA) + 
  geom_sf(data = ireland, fill = NA, col = "red") +
  scale_fill_viridis_c() + 
  ggtitle("Total years of culling") + 
  theme_bw() + 
  
ggplot() + 
  geom_sf(data = quart_data, aes(fill = year_max), col = NA) + 
  geom_sf(data = ireland, fill = NA, col = "red") +
  scale_fill_viridis_c() + 
  ggtitle("Last year of culling") + 
  theme_bw() +
  
ggplot() + 
  geom_sf(data = quart_data, aes(fill = TURNOVER_YEAR), col = NA) + 
  geom_sf(data = ireland, fill = NA, col = "red") +
  scale_fill_viridis_c() + 
  ggtitle("Year entering vaccination programme") + 
  theme_bw() +

plot_layout(nrow = 2)
  
  


