# Housekeeping ####
rm(list = ls())
source("Scripts/setup.R")

# Load and prep data ####

## Load clean data ####
clean_data <- readRDS("Data/Clean_data.RDS")
metadata <- read_csv("G:/Shared drives/Badger (2 projects 2024) DAFM/DATA/UCD_Badger_Data/All_BAdgers/collar_metadata.csv") %>% 
  mutate(Deployment_Date = dmy(Deployment_Date)) 

## Remove first 2 days after capture ####
# (avoid capture effects on behaviour)

final_data <-  clean_data %>% 
  group_by(Collar_ID) %>% 
  filter(DateTimeUTC > Deployment_Date + 2) %>% 
  ungroup()

loc_count <- final_data %>% 
  group_by(Badger_ID) %>% 
  summarise(n_locs = n()) %>% 
  print(n = 30)

# Turn data into sf ####

final_data_sf <- final_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
  mutate(Month = as.factor(Month), 
  )

# transform into ITM (need planar crs for kernel measurements etc)
final_data_ITM <- final_data_sf %>% 
  st_transform(2157)

# Previsualisation ####

## Interactive plot ####
# mapview(final_data_ITM, zcol = "Disturbance")

## Static plot ####
ggplot(final_data) + 
  geom_path(aes(x = longitude, y = latitude, group = Bout), linewidth = 0.1, col = "gray") + 
  geom_point(aes(x = longitude, y = latitude, col = Hours_since_19)) + 
  scale_color_viridis_c() +
  # coord_equal() +
  facet_wrap(~County+Badger_ID, scales = "free") +
  theme_bw()

# Home range analysis ####

## Kernels by ID ####

### turn data into track object ####
trk <- final_data_ITM %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>% 
  mk_track(.x = x, .y = y,
           .t = DateTimeUTC,
           id = Badger_ID,
           crs = st_crs(final_data_ITM)$epsg, 
           all_cols = T)

### nest by ID ####
trk_list <- trk %>% 
  nest(data = -Badger_ID)

### calculate kernel surface ####

# (with automatically calculated h for each)
trk_kde <- trk_list %>%
  mutate(kde = purrr::map(data, ~ hr_kde(.x))) 

# # h values to obtain the average
# h_values <- purrr::map(trk_kde$kde, "h")
# 
# h_values <- sapply(h_values, "[[", 1)
# mean(h_values)

### calculate 50% and 95% isopleths ####
trk_kde <- trk_kde %>%
  mutate(kde95 = purrr::map(kde, ~ hr_isopleths(.x, levels = 0.95)), 
         kde50 = purrr::map(kde, ~ hr_isopleths(.x, levels = 0.50))) 

# unnest 50% kernels
kde50 <- trk_kde %>% 
  select(Badger_ID, kde50) %>% 
  unnest(kde50)

st_geometry(kde50) <- "geometry"

# unnest 95% kernels
kde95 <- trk_kde %>% 
  select(Badger_ID, kde95) %>% 
  unnest(kde95)

st_geometry(kde95) <- "geometry"

### put all kernels together ####
all_kernels <- bind_rows(kde50, kde95) %>% 
  select(-what) %>% 
  mutate(level = paste("kde", level, sep = "_"))  
  
### Join with metadata 
# but remove all collar-specific variables or we'll have duplicated rows 
# for badgers that hae been collared twice

all_kernels <- metadata %>% 
  select(Badger_ID, County, Disturbance) %>% 
  distinct() %>% 
  right_join(all_kernels) %>% 
  st_as_sf(., sf_column_name = "geometry")

### obtain kernel sizes ####
kernel_sizes <- all_kernels %>% 
  select(County, Disturbance, Badger_ID, area, level)  %>% 
  # st_drop_geometry() %>% 
  mutate(area = as.numeric(conv_unit(area, "m2", "km2"))) %>% 
  left_join(loc_count)

### Plot kernel size vs n fixes ####
ggplot(kernel_sizes %>% filter(n_locs<6000)) + 
  geom_point(aes(y = area, x = n_locs, col = level)) + 
  geom_smooth(aes(y = area, x = n_locs, col = level), method = "loess") +
  theme_bw()


### Plot kernels ####

bb_rc <- st_bbox(all_kernels %>% filter(County == "Roscommon" & level == "kde_0.95"))
bb_mn <- st_bbox(all_kernels %>% filter(County == "Monaghan" & level == "kde_0.95"))
bb_mh <- st_bbox(all_kernels %>% filter(County == "Meath" & level == "kde_0.95"))
bb_dl <- st_bbox(all_kernels %>% filter(County == "Donegal" & level == "kde_0.95"))
bb_kk <- st_bbox(all_kernels %>% filter(County == "Kilkenny" & level == "kde_0.95"))
bb_dy <- st_bbox(all_kernels %>% filter(County == "Derry" & level == "kde_0.95"))


ggplot() + 
  geom_sf(data = all_kernels %>% filter(County == "Roscommon" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels %>% filter(County == "Roscommon" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  coord_sf(xlim = c(bb_rc[1], bb_rc[1]+(bb_rc[4]-bb_rc[2])), 
           ylim = c(bb_rc[2], bb_rc[4])) + 
  ggtitle("Roscommon") + 
  
ggplot() + 
  geom_sf(data = all_kernels %>% filter(County == "Monaghan" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels %>% filter(County == "Monaghan" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  coord_sf(xlim = c(bb_mn[1], bb_mn[1]+(bb_mn[4]-bb_mn[2])), 
           ylim = c(bb_mn[2], bb_mn[4])) + 
  ggtitle("Monaghan") + 
  
  
  ggplot() + 
  geom_sf(data = all_kernels %>% filter(County == "Meath" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels %>% filter(County == "Meath" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  coord_sf(xlim = c(bb_mh[1], bb_mh[3]),
           ylim = c(bb_mh[2], bb_mh[2]+(bb_mh[3]-bb_mh[1]))) +
  ggtitle("Meath") +
  
  
  ggplot() + 
  geom_sf(data = all_kernels %>% filter(County == "Kilkenny" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels %>% filter(County == "Kilkenny" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  coord_sf(xlim = c(bb_kk[1], bb_kk[3]),
           ylim = c(bb_kk[2], bb_kk[2]+(bb_kk[3]-bb_kk[1]))) +
  ggtitle("Kilkenny") + 
  
  ggplot() + 
  geom_sf(data = all_kernels %>% filter(County == "Donegal" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels %>% filter(County == "Donegal" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  coord_sf(xlim = c(bb_dl[1], bb_dl[3]),
           ylim = c(bb_dl[2], bb_dl[2]+(bb_dl[3]-bb_dl[1]))) +
  ggtitle("Donegal") +  
  
  ggplot() + 
  geom_sf(data = all_kernels %>% filter(County == "Derry" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels %>% filter(County == "Derry" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  coord_sf(xlim = c(bb_dy[1], bb_dy[3]),
           ylim = c(bb_dy[2], bb_dy[2]+(bb_dy[3]-bb_dy[1])), 
           expand = T) +
  ggtitle("Derry") 

### Boxplots of kernel size ####
ggplot(kernel_sizes) + 
  geom_boxplot(aes(x = Disturbance, fill = Disturbance, y = area)) +
  facet_grid(level~County, scales = "free_x") + 
  theme_bw() + 
  ggtitle("Territory size")  

## Kernels by ID and month  #### 

### nest by ID and month ####
trk_list_m <- trk %>% 
  nest(data = c(-Badger_ID, -Year_Month_lab))

### remove months with less than 10 fixes ####
count <- trk_list_m %>% 
  group_by(Badger_ID, Year_Month_lab) %>% 
  summarise(n = map_dbl(data, nrow)) %>% 
  filter(n> 10) %>% 
  mutate(ID_month = paste0(Badger_ID, Year_Month_lab))

trk_list_m <- trk_list_m %>% 
  mutate(ID_month = paste0(Badger_ID, Year_Month_lab)) %>% 
  filter(ID_month %in% count$ID_month) %>% 
  select(-ID_month)

### calculate kernel surface ####

# (with automatically calculated h for each)
trk_kde_m <- trk_list_m %>%
  mutate(kde = purrr::map(data, ~ hr_kde(.x))) 


### calculate 50% and 95% isopleths ####
trk_kde_m <- trk_kde_m %>%
  mutate(kde95 = purrr::map(kde, ~ hr_isopleths(.x, levels = 0.95)), 
         kde50 = purrr::map(kde, ~ hr_isopleths(.x, levels = 0.50))) 

# unnest 50% kernels
kde50_m <- trk_kde_m %>% 
  select(Badger_ID, Year_Month_lab, kde50) %>% 
  unnest(kde50)

st_geometry(kde50_m) <- "geometry"

# unnest 95% kernels
kde95_m <- trk_kde_m %>% 
  select(Badger_ID, Year_Month_lab, kde95) %>% 
  unnest(kde95)

st_geometry(kde95_m) <- "geometry"

### put all kernels together ####
metadata <- metadata %>% 
  select(Badger_ID, County, Disturbance) %>% 
  distinct()
  
all_kernels_m <- bind_rows(kde50_m, kde95_m) %>% 
  select(-what) %>% 
  mutate(level = paste("kde", level, sep = "_")) %>% 
  left_join(metadata) %>% 
  separate(Year_Month_lab, into = c("Year", "Month_lab"), sep = "_") %>%
  mutate(Month_lab = factor(Month_lab, levels = c("Jan", "Feb", "Mar",
                                                  "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", 
                                                  "Oct", "Nov", "Dec")))


### obtain kernel sizes ####

loc_count_m <- final_data %>% 
  group_by(Badger_ID, Year_Month_lab) %>% 
  summarise(n_locs = n()) %>% 
  separate(Year_Month_lab, into = c("Year", "Month_lab"), sep = "_")


kernel_sizes_m <- all_kernels_m %>% 
  select(County, Disturbance, Badger_ID, Year, Month_lab, area, level)  %>% 
  st_drop_geometry() %>% 
  mutate(area = as.numeric(conv_unit(area, "m2", "km2"))) %>% 
  left_join(loc_count_m, by = c("Badger_ID", "Year", "Month_lab")) %>% 
  mutate(County = as.factor(County), 
         Disturbance = as.factor(Disturbance), 
         Badger_ID = as.factor(Badger_ID), 
         level = as.factor(level)) %>% 
  mutate(Month_lab = factor(Month_lab, levels = c("Jan", "Feb", "Mar",
                                                  "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", 
                                                  "Oct", "Nov", "Dec")))


### Plot kernel size vs n fixes ####
ggplot(kernel_sizes_m) + 
  geom_point(aes(y = area, x = n_locs, col = level)) + 
  geom_smooth(aes(y = area, x = n_locs, col = level), method = "loess") +
  # facet_wrap(~level) +
  theme_bw()

### Plot kernels spatially ####

ggplot() + 
  geom_sf(data = all_kernels_m %>% filter(County == "Roscommon" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% filter(County == "Roscommon" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Year+Month_lab, nrow = 2) +
  ggtitle("Roscommon") 

ggplot() + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Monaghan" & level == "kde_0.95" & Disturbance == "High"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Monaghan" & level == "kde_0.5"& Disturbance == "High"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) + 
  ggtitle("Monaghan high disturbance") +

ggplot() + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Monaghan" & level == "kde_0.95" & Disturbance == "Low"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Monaghan" & level == "kde_0.5"& Disturbance == "Low"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) + 
  ggtitle("Monaghan low disturbance") 

ggplot() + 
  geom_sf(data = all_kernels_m 
          %>% filter(County == "Kilkenny" & level == "kde_0.95" & Disturbance == "High"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Kilkenny" & level == "kde_0.5" & Disturbance == "High"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) +
  ggtitle("Kilkenny high disturbance") + 

ggplot() + 
  geom_sf(data = all_kernels_m 
          %>% filter(County == "Kilkenny" & level == "kde_0.95" & Disturbance == "Low"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Kilkenny" & level == "kde_0.5" & Disturbance == "Low"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) +
  ggtitle("Kilkenny low disturbance") 

ggplot() + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Donegal" & level == "kde_0.95" & Disturbance == "High"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Donegal" & level == "kde_0.5" & Disturbance == "High"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) +
  ggtitle("Donegal high disturbance")  

# ggplot() + 
#   geom_sf(data = all_kernels_m %>% 
#             filter(County == "Donegal" & level == "kde_0.95" & Disturbance == "Low"), 
#           aes(fill = Badger_ID), alpha = 0.5) + 
#   geom_sf(data = all_kernels_m %>% 
#             filter(County == "Donegal" & level == "kde_0.5" & Disturbance == "Low"), 
#           aes(fill = Badger_ID)) + 
#   theme_bw() + 
#   facet_wrap(~Month_lab) +
#   ggtitle("Donegal low disturbance") 

ggplot() + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Meath" & level == "kde_0.95", Disturbance == "High"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Meath" & level == "kde_0.5", Disturbance == "High"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) + 
  ggtitle("Meath high disturbance") +

ggplot() + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Meath" & level == "kde_0.95", Disturbance == "Low"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% 
            filter(County == "Meath" & level == "kde_0.5", Disturbance == "Low"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) + 
  ggtitle("Meath low disturbance")

ggplot() + 
  geom_sf(data = all_kernels_m %>% filter(County == "Derry" & level == "kde_0.95"), 
          aes(fill = Badger_ID), alpha = 0.5) + 
  geom_sf(data = all_kernels_m %>% filter(County == "Derry" & level == "kde_0.5"), 
          aes(fill = Badger_ID)) + 
  theme_bw() + 
  facet_wrap(~Month_lab) + 
  ggtitle("Derry") 

### Boxplots of kernel size ####
ggplot(kernel_sizes_m %>% filter(level == "kde_0.95")) + 
  geom_boxplot(aes(x = Month_lab, fill = Disturbance, y = area)) + 
  theme_bw() + 
  facet_wrap(~County, nrow = 1) +
  ggtitle("95% kernel") + 
  
  ggplot(kernel_sizes_m %>% filter(level == "kde_0.5")) + 
  geom_boxplot(aes(x = Month_lab, fill = Disturbance, y = area)) + 
  theme_bw() +
  facet_wrap(~County, nrow = 1) +
  ggtitle("50% kernel") +
  
  plot_layout(nrow = 2)


# Movement metrics ####

## By ID ####

tracks_with_mov_met <- trk_list %>% 
  mutate(straightness = map(data, ~straightness(.x)), 
         msd = map(data, ~ msd(.x)), 
         cum_dist = map(data, ~cum_dist(.x)),
         tot_dist = map(data, ~tot_dist(.x)), 
         intensity_use = map(data, ~intensity_use(.x)), 
         sinuosity = map(data, ~sinuosity(.x)), 
         angle_corr = map(data, ~tac(.x))) %>% 
  select(-data) %>% 
  unnest(c(straightness, msd, cum_dist, tot_dist, intensity_use, sinuosity, angle_corr))

class(tracks_with_mov_met) <- NULL
tracks_with_mov_met <- as.data.frame(tracks_with_mov_met) %>% 
  left_join(metadata) %>% 
  left_join(loc_count)

mov_met <- tracks_with_mov_met %>% 
  mutate(straightness = as.numeric(straightness),
         msd = as.numeric(msd),
         cum_dist = as.numeric(cum_dist), 
         tot_dist = as.numeric(tot_dist),
         intensity_use = as.numeric(tot_dist),
         sinuosity = as.numeric(sinuosity),
         angle_corr = as.numeric(angle_corr)) %>% 
  pivot_longer(cols = straightness:angle_corr, values_to = "Value", names_to = "Variable")


ggplot(mov_met %>% filter(n_locs < 2000)) +
  geom_point(aes(x = n_locs, y = Value, col = Disturbance)) +
  geom_smooth(aes(x = n_locs, y = Value, col = Disturbance)) + 
  facet_wrap(~Variable, scales = "free_y") + 
  theme_bw()


ggplot(mov_met) + 
  geom_boxplot(aes(x = County, y = Value, fill = Disturbance)) + 
  facet_wrap(~Variable, scale = "free_y") + 
  theme_bw()

## By ID and month ####

tracks_with_mov_met_m <- trk_list_m %>% 
  mutate(straightness = map(data, ~straightness(.x)), 
         msd = map(data, ~ msd(.x)), 
         cum_dist = map(data, ~cum_dist(.x)),
         tot_dist = map(data, ~tot_dist(.x)), 
         intensity_use = map(data, ~intensity_use(.x)), 
         sinuosity = map(data, ~sinuosity(.x)), 
         angle_corr = map(data, ~tac(.x))) %>% 
  select(-data) %>% 
  unnest(c(straightness, msd, cum_dist, tot_dist, intensity_use, sinuosity, angle_corr))

class(tracks_with_mov_met_m) <- NULL
tracks_with_mov_met_m <- as.data.frame(tracks_with_mov_met_m) %>% 
  separate(Year_Month_lab, into = c("Year", "Month_lab"), sep = "_") %>% 
  left_join(metadata) %>% 
  left_join(loc_count_m, by = c("Badger_ID", "Year", "Month_lab")) %>% 
  mutate(Month_lab = factor(Month_lab, levels = c("Jan", "Feb", "Mar",
                                                  "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", 
                                                  "Oct", "Nov", "Dec")))

mov_met_m <- tracks_with_mov_met_m %>% 
  mutate(straightness = as.numeric(straightness),
         msd = as.numeric(msd),
         cum_dist = as.numeric(cum_dist), 
         tot_dist = as.numeric(tot_dist),
         intensity_use = as.numeric(tot_dist),
         sinuosity = as.numeric(sinuosity),
         angle_corr = as.numeric(angle_corr)) %>% 
  pivot_longer(cols = straightness:angle_corr, values_to = "Value", names_to = "Variable")


ggplot(mov_met_m) +
  geom_point(aes(x = n_locs, y = Value, col = Disturbance)) +
  geom_smooth(aes(x = n_locs, y = Value, col = Disturbance)) + 
  facet_wrap(~Variable, scales = "free_y") + 
  theme_bw()


ggplot(mov_met_m %>% filter(County == "Kilkenny")) + 
  geom_boxplot(aes(x = Month_lab, y = Value, fill = Disturbance)) + 
  facet_wrap(~Variable, scale = "free") + 
  theme_bw()

ggplot(mov_met_m %>% filter(County == "Meath")) + 
  geom_boxplot(aes(x = Month_lab, y = Value, fill = Disturbance)) + 
  facet_wrap(~Variable, scale = "free") + 
  theme_bw()

ggplot(mov_met_m %>% filter(County == "Monaghan")) + 
  geom_boxplot(aes(x = Month_lab, y = Value, fill = Disturbance)) + 
  facet_wrap(~Variable, scale = "free") + 
  theme_bw()
