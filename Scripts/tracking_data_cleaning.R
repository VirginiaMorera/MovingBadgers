# Housekeeping ####
rm(list = ls())
source("Scripts/setup.R")

# Clean tracking data ####
df <- read_csv("Data/Tracking/all_collars_02_11_2026_plain_with_haccuracy.csv")

## select columns, format datetimes ####
all_data_clean <- df %>% 
  select(ID = DeviceName, Time, h_acc_est, Latitude, Longitude) %>% 
  mutate(DateTime = ymd_hms(Time),
         DateTimeLoc = force_tz(DateTime, tzone = "Europe/Dublin"), 
         DateTimeUTC = with_tz(DateTimeLoc, tzone = "UTC")) %>% 
  dplyr::arrange(ID, DateTimeUTC) %>% 
  mutate(RowN = 1:nrow(.))
  
## calculate p2p time ####
all_data_clean <- all_data_clean %>% 
  group_by(ID) %>% 
  mutate(
    TimeDif = DateTimeUTC - lag(DateTimeUTC), 
    TimeDif = if_else(is.na(TimeDif), as.difftime(0, units = "mins"), TimeDif)
  )

## separate data in bouts ####
all_data_clean2 <- all_data_clean %>% 
  group_by(ID) %>% 
  mutate(
    NewBout = as.numeric(TimeDif, units = "mins") > 400, # if time dif is bigger than 400 min make new bout
    Bout = cumsum(NewBout) + 1, # everytime newbout = TRUE, add a value to Bout (so a bout count)
    Hour = hour(DateTimeUTC),
    Minute = minute(DateTimeUTC),
    Date = as_date(DateTimeUTC),
    Time = hms::as_hms(DateTimeUTC),
    Week = week(DateTimeUTC), 
    Month = month(DateTimeUTC),
    Month_lab = month(DateTimeUTC, label = T),
    Year_Month_lab = paste(year(DateTimeUTC), Month_lab, sep = "_"),
    Hours_since_19 = if_else(Hour > 12, Hour - 18, Hour + 6), 
    Hours_around_midnight = if_else(Hour > 12, Hour-24, Hour), 
    TimeShift = hms::as_hms(DateTimeUTC + 12*60*60)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  %>%   
  group_by(ID, Bout) %>% 
  mutate(
    TimeDif_group = replace(TimeDif, 1, 0), 
    CumTime_group = cumsum(as.numeric(TimeDif_group, units = "mins")), # time dif but now the first one of each bout is 0
    Prev_acc = lag(h_acc_est), 
    p2pdist = st_distance(geometry, lag(geometry), by_element = T), 
    p2pspeed = (p2pdist/1000)/(TimeDif_group/60)) %>% # precision of the previous fix. This is to detect cold fixes happening. 
  ungroup() %>% 
  mutate(longitude = st_coordinates(.)[,"X"], latitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

######### check p2ptime and distance and speed here 


## filter devices with >10 positions ####
retain <- all_data_clean2 %>% 
  group_by(ID) %>% 
  tally() %>% 
  filter(n > 10) %>% 
  print(n = 40)  

final_data <- all_data_clean2 %>% 
  filter(ID %in% retain$ID) %>% 
  rename(Collar_ID = ID)


# Load and clean metadata ####
metadata <- read_csv("G:/Shared drives/Badger (2 projects 2024) DAFM/DATA/UCD_Badger_Data/All_BAdgers/collar_metadata.csv") %>% 
  mutate(Deployment_Date = dmy(Deployment_Date)) 

# merge data and metadata ####
final_data <- left_join(final_data, metadata)

## filter out all positions before deployment date ####

final_data <- final_data %>% 
  # mutate(Pre_deploymet = if_else(DateTimeUTC >= Deployment_Date, "Yes", "No"))
  filter(DateTimeUTC >= Deployment_Date) %>% 
  select(County, Disturbance, Sex, Status, Badger_ID, Collar_ID, Deployment_Date, 
         DateTimeUTC, longitude, latitude, h_acc_est, DateTimeLoc, Bout, Date, 
         Hour, Minute, Week, Month, Month_lab, Year_Month_lab, Hours_since_19, 
         Hours_around_midnight, TimeShift, TimeDif_group, CumTime_group, TimeDif, 
         Prev_acc, p2pdist, p2pspeed)

loc_count <- final_data %>% 
  group_by(Collar_ID) %>% 
  summarise(n_locs = n())

# Several plots ####

## Fix time ####
# at what minute do fixes happen? should have three peaks, as it should be every 20 min (+/- fix acquisition time)
ggplot(final_data) + 
  geom_bar(aes(x = Minute)) + 
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  theme_bw() + 
  facet_wrap(~County+Collar_ID, scales = "free_y")

## Time between fixes ####
# histogram of time between fixes. A device with many fixes with longer time between them may be malfunctioning
ggplot(final_data) +
  geom_histogram(aes(x = TimeDif_group/60)) +
  theme_bw() + 
  labs(title = "Time between fixes") + 
  facet_wrap(~County+Collar_ID, scales = "free")

## Effect of cold fixes? ####
# USe this to assess the length of bouts. Too many too short bouts may indicate an issue with hot/cold fixes
ggplot(final_data) + 
  geom_line(aes(x = CumTime_group, y = Bout, group = Bout)) + 
  geom_point(aes(x = CumTime_group, y = Bout, col = h_acc_est)) + 
  scale_color_viridis_c() +
  theme_bw() + 
  facet_wrap(~Collar_ID, scales = "free")

## Fixes througout the day ####
# timing of fixes throughout the day 
ggplot(final_data) + 
  geom_line(aes(x = TimeShift, y = Date, group = Bout)) + 
  geom_point(aes(x = TimeShift, y = Date, col = TimeShift)) + 
  scale_color_viridis_c(
    breaks = seq(7 * 3600, 21 * 3600, by = 7200),
    labels = function(x) format(hms::as_hms(x))
  ) +
  scale_x_time(date_labels = "%H:%M") +
  scale_y_date(date_labels = "%Y-%m-%d") + 
  theme_bw() + 
  labs(colour = "Horizontal accuracy") + 
  facet_wrap(~Collar_ID, scales = "free_y")

## Calendar ####

final_data %>% 
  group_by(Disturbance, Sex, County, Badger_ID) %>% 
  summarise(First_fix = min(Date), 
            Last_fix = max(Date)) %>% 
  ggplot() + 
  geom_segment(aes(x = First_fix, xend = Last_fix, y = Badger_ID, col = County, linetype = Sex), 
               linewidth = 1) + 
  facet_wrap(~Disturbance, ncol = 1, scales = "free_y") + 
  theme_bw()

saveRDS(final_data, file = "Data/Clean_data.RDS")
