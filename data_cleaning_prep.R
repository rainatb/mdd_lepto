# Precipitation data (max, mean, and min) clean up
max_precipitation <- read.csv('mdd_precipitation_monthly_max.csv')
max_precipitation <- max_precipitation[, -1]

colnames(max_precipitation) <- gsub("_total_precipitation_max", "", colnames(max_precipitation))
colnames(max_precipitation) <- gsub("X", "", colnames(max_precipitation))

max_precipitation <- max_precipitation %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Maximum Precipitation')
max_precipitation <- max_precipitation[, -2]
max_precipitation$Month <- substr(max_precipitation$Year, 5, 6)
max_precipitation$year <- substr(max_precipitation$Year, 1, 4)
max_precipitation <- max_precipitation[, -2]


min_precipitation <- read.csv("mdd_precipitation_monthly_min.csv")
min_precipitation <- min_precipitation[, -1]
colnames(min_precipitation) <- gsub("_total_precipitation_min", "", colnames(min_precipitation))
colnames(min_precipitation) <- gsub("X", "", colnames(min_precipitation))

min_precipitation <- min_precipitation %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Minimum Precipitation')
min_precipitation <- min_precipitation[, -2]
min_precipitation$Month <- substr(min_precipitation$Year, 5, 6)
min_precipitation$year <- substr(min_precipitation$Year, 1, 4)
min_precipitation <- min_precipitation[, -2]


mean_precipitation <- read.csv('mdd_precipitation_monthly_mean.csv')
mean_precipitation <- mean_precipitation[, -1]
colnames(mean_precipitation) <- gsub("_total_precipitation_sum", "", colnames(mean_precipitation))
colnames(mean_precipitation) <- gsub("X", "", colnames(mean_precipitation))

mean_precipitation <- mean_precipitation %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Mean Precipitation')

mean_precipitation <- mean_precipitation[, -2]
mean_precipitation$Month <- substr(mean_precipitation$Year, 5, 6)
mean_precipitation$year <- substr(mean_precipitation$Year, 1, 4)
mean_precipitation <- mean_precipitation[, -2]

# trying to check mean temperature too

# Temperature data (min, max, and mean) clean up
mean_temperature <- read.csv('mdd_temperature_monthly_mean.csv')
mean_temperature <- mean_temperature[, -1]
colnames(mean_temperature) <- gsub("_temperature_2m", "", colnames(mean_temperature))
colnames(mean_temperature) <- gsub("X", "", colnames(mean_temperature))

mean_temperature <- mean_temperature %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Mean Temperature')

mean_temperature <- mean_temperature[, -2]
mean_temperature$Month <- substr(mean_temperature$Year, 5, 6)
mean_temperature$year <- substr(mean_temperature$Year, 1, 4)
mean_temperature <- mean_temperature[, -2]

max_temperature <- read.csv('mdd_temperature_monthly_max.csv')
max_temperature <- max_temperature[, -1]
colnames(max_temperature) <- gsub("_temperature_2m_max", "", colnames(max_temperature))
colnames(max_temperature) <- gsub("X", "", colnames(max_temperature))

max_temperature <- max_temperature %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Max Temperature')

max_temperature <- max_temperature[, -2]
max_temperature$Month <- substr(max_temperature$Year, 5, 6)
max_temperature$year <- substr(max_temperature$Year, 1, 4)
max_temperature <- max_temperature[, -2]


min_temperature <- read.csv('mdd_temperature_monthly_min.csv')
min_temperature <- min_temperature[, -1]
colnames(min_temperature) <- gsub("_temperature_2m_min", "", colnames(min_temperature))
colnames(min_temperature) <- gsub("X", "", colnames(min_temperature))

min_temperature <- min_temperature %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Min Temperature')

min_temperature <- min_temperature[, -2]
min_temperature$Month <- substr(min_temperature$Year, 5, 6)
min_temperature$year <- substr(min_temperature$Year, 1, 4)
min_temperature <- min_temperature[, -2]

# checking runoff

# Runoff data clean up (both max and min)
runoff_max <- read.csv('mdd_runoff_monthly_max.csv')
runoff_max <- runoff_max[, -1]
colnames(runoff_max) <- gsub("_runoff_max", "", colnames(runoff_max))
colnames(runoff_max) <- gsub("X", "", colnames(runoff_max))

runoff_max <- runoff_max %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Maximum Runoff')

runoff_max <- runoff_max[, -2]
runoff_max$Month <- substr(runoff_max$Year, 5, 6)
runoff_max$year <- substr(runoff_max$Year, 1, 4)
runoff_max <- runoff_max[, -2]

runoff_min <- read.csv('mdd_runoff_monthly_min.csv')
runoff_min <- runoff_min[, -1]
colnames(runoff_min) <- gsub("_runoff_min", "", colnames(runoff_min))
colnames(runoff_min) <- gsub("X", "", colnames(runoff_min))

runoff_min <- runoff_min %>%
  pivot_longer(cols = 1:120,
               names_to = 'Year',
               values_to = 'Minimum Runoff')

runoff_min <- runoff_min[, -2]
runoff_min$Month <- substr(runoff_min$Year, 5, 6)
runoff_min$year <- substr(runoff_min$Year, 1, 4)
runoff_min <- runoff_min[, -2]


# pdps to include in poster- max runoff, year, urban and agricultural
# make the highway black not red
# 100 hc centers were clustered into 70 spatial units of similar geographic 
# clustered 100 hc centers into 70 clusters based on geographic proximity.

# first run spatial-temporal cross-validation 
# to report auc avg plus range

# do 25 iterations instead of 50

lulc_data <- read.csv('lulc_mdd_hc_final.csv')
lulc_data <- lulc_data[, -1]
lulc_data <- lulc_data[, -5]
lulc_wider <- lulc_data %>%
  pivot_wider(names_from = class, values_from = area)

lulc_wider[is.na(lulc_wider)] <- 0
lulc_wider <- lulc_wider %>%
  rename(miscellaneous = "0",
         forest_area = "3",
         pasture = "15",
         mosaic = "21",
         farming = "18",
         urban_area = "24",
         mining_area = "30")
lulc_wider$agricultural_area <- lulc_wider$pasture + lulc_wider$mosaic + lulc_wider$farming
lulc_wider <- lulc_wider[, -5]
lulc_wider <- lulc_wider[, -5]
lulc_wider <- lulc_wider[, -7]

lepto_cases <- read.csv('mdd_lepto_data copy 2.csv')

temperatures_merging <- left_join(x = min_temperature,
                                  y = max_temperature,
                                  by = c("layer" = "layer",
                                         "Month" = "Month",
                                         "year" = "year"))
temperatures_merging <- left_join(x = temperatures_merging,
                                  y = mean_temperature,
                                  by = c("layer" = "layer",
                                         "Month" = "Month",
                                         "year" = "year"))

runoff_merging <- left_join(x = runoff_max,
                            y = runoff_min,
                            by = c("layer" = "layer",
                                   "Month" = "Month",
                                   "year" = "year"))
runoff_plus_temps <- left_join(x = temperatures_merging,
                               y = runoff_merging,
                               by = c("layer" = "layer",
                                      "Month" = "Month",
                                      "year" = "year"))
precipitations_merging <- left_join(x = max_precipitation,
                                    y = min_precipitation,
                                    by = c("layer" = "layer",
                                           "Month" = "Month",
                                           "year" = "year"))
precipitations_merging <- left_join(x = precipitations_merging,
                                    y = mean_precipitation,
                                    by = c("layer" = "layer",
                                           "Month" = "Month",
                                           "year" = "year"))
precipitations_merging$`Minimum Precipitation` <- precipitations_merging$`Minimum Precipitation` *-1

precipitations_merging$Month <- as.numeric(precipitations_merging$Month)
precipitations_merging$year <- as.numeric(precipitations_merging$year)

runoff_plus_temps$Month <- as.numeric(runoff_plus_temps$Month)
runoff_plus_temps$year <- as.numeric(runoff_plus_temps$year)

dataset_setup <- left_join(x = runoff_plus_temps,
                           y = precipitations_merging,
                           by = c("layer" = "layer",
                                  "Month" = "Month",
                                  "year" = "year"))

dataset_setup <- left_join(x = dataset_setup,
                           y = lulc_wider,
                           by = c("layer" = "layer",
                                  "year" = "year"))

e_salud_key <- read.csv('linking_clusterid_esaludkey.csv')
e_salud_key <- e_salud_key[, -1]

lepto_cases <- left_join(x = lepto_cases,
                         y = e_salud_key,
                         by = c("E_SALUD" = "e_salud"))

river_distances <- read.csv('Copy of healthcare_center_distances_actual_final.csv')
river_distances <- river_distances[, -1]
river_distances <- river_distances[, -3]


names(river_distances)[names(river_distances) == 'mean'] <- 'Avg_distance_to_river'


lepto_monthly_clust <- lepto_cases %>%
  mutate(FECHA_INI = ymd_hms(FECHA_INI)) %>%
  group_by(month = floor_date(FECHA_INI, unit = "month"), 
           location = clust) %>%
  summarize(monthlycount = n(), .groups = 'drop')

lepto_monthly_clust <- lepto_monthly_clust %>%
  dplyr::mutate(year = lubridate::year(month), 
                month = lubridate::month(month)) 
lepto_monthly_clust <- subset(lepto_monthly_clust, year > 2012)
lepto_monthly_clust <- na.omit(lepto_monthly_clust)

dataset_setup <- left_join(x = dataset_setup,
                           y = lepto_monthly_clust,
                           by = c("layer" = "location",
                                  "Month" = "month",
                                  "year" = "year"))

dataset_setup[is.na(dataset_setup)] <- 0
dataset_setup <- dataset_setup %>%
  mutate(occurrence = if_else(monthlycount > 0, 1, 0))

dataset_setup <- left_join(x = dataset_setup,
                           y = river_distances,
                           by = c("layer" = "layer"))

clust_thing <- read.csv('cluster_centroids_70.csv')
clust_thing <- clust_thing[, -2]
library(sf)

analysis_thing <- st_as_sf(clust_thing, coords = c("longitude", "latitude"), 
                           crs  =  "+proj=longlat +datum=WGS84 +ellps=WGS84")

spatial_clust_model <- left_join(x = dataset_setup,
                                 y = analysis_thing,
                                 by = c("layer" = "clust"))
spatial_clust_2021 <- subset(spatial_clust_model, year == 2021)
spatial_clust_2021$geometry <- st_as_sf(spatial_clust_2021$geometry)
print(spatial_clust_2021$geometry$x)
print(spatial_clust_2021$occurrence)
spatial_clust_2021$occurrence <- as.character(spatial_clust_2021$occurrence)

library(blockCV)


set_up_for_new_spatial <- cv_spatial(
  x = spatial_clust_2021$geometry$x,
  column = 'occurrence',
  k = 3,
  hexagon = FALSE,
  flat_top = FALSE,
  size = 20000,
  selection = 'random',
  iteration = 100,
  seed = 1,
  plot = TRUE
)

spatial_clust_2021$fold_ids <- set_up_for_new_spatial$folds_ids

important <- unique(spatial_clust_2021[c('layer', 'fold_ids')])

updated_spatial_model_prep <- left_join(x = spatial_clust_model,
                                        y = important,
                                        by = c("layer" = "layer"))

write.csv(updated_spatial_model_prep, 'dataset_with_spatial_folds_13_22.csv')
