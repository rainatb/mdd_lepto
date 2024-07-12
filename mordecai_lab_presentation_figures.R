# Figures made for presenting research at a Mordecai Lab meeting
# mordecai lab presentation figures

library(blockCV)
library(tmap)
library(ggplot2)
library(xgboost)
library(pdp)
library(sf)

theme_update(plot.title = element_text(hjust = 0.5))
districts <- read_sf("/Users/rashitalwarbhatia/Downloads/folder/Madre_de_Dios.shp")
districts <- st_as_sf(districts)
districts$geometry <- st_transform(districts$geometry, 4326)
set.seed(1)
n <- 100
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

blocks <- st_as_sf(set_up_for_new_spatial$blocks)

spatial_clust_2021$fold_id <- set_up_for_new_spatial$foldID

# Count the number of points in each fold
fold_counts <- spatial_clust_2021 %>%
  group_by(fold_ids) %>%
  summarize(count = n())

# Merge fold counts with blocks
blocks <- blocks %>%
  left_join(fold_counts, by = c("folds" = "fold_ids"))

# Plot the districts and the spatial blocks
xab <- ggplot() +
  geom_sf(data = districts, fill = "white", color = "black") +
  geom_sf(data = blocks, aes(fill = factor(folds)), color = "black", size = 0.5) +
  labs(title = "Spatial Blocks in Madre de Dios", fill = "Fold ID")+
  theme_minimal()
print(xab)





new_replica <- model_confidence
new_replica$Feature[new_replica$Feature == "Avg_distance_to_river"] <- "Distance To River"
new_replica$Feature[new_replica$Feature == 'Max.Temperature'] <- "Max Temperature"
new_replica$Feature[new_replica$Feature == 'urban_area'] <- 'Urban Area'
new_replica$Feature[new_replica$Feature == 'mining_area'] <- 'Mining Area'
new_replica$Feature[new_replica$Feature == 'Min.Temperature'] <- 'Min Temperature'
new_replica$Feature[new_replica$Feature == 'agricultural_area'] <- 'Agricultural Area'
new_replica$Feature[new_replica$Feature == 'forest_area'] <- 'Forest Area'

new_replica$Feature[new_replica$Feature == 'year'] <- 'Year'
new_replica$Feature[new_replica$Feature == 'Minimum.Runoff'] <- 'Min Runoff'
new_replica$Feature[new_replica$Feature == 'Maximum.Precipitation'] <- 'Max Precipitation'
library(dplyr)
library(stringr)

new_replica <- new_replica %>%
  mutate(
    Group = case_when(
      stringr::str_detect(Feature, "Area") ~ "Land-Use",
      stringr::str_detect(Feature, "[Mm]in|[Mm]ax") ~ "Climate",
      TRUE ~ "Other" # Default group for any that do not match above
    )
  )
new_replica$Group <- as.factor(new_replica$Group)

new_replica <- new_replica %>%
  rename(importance = boostrapping_mean)

group_colors <- c("Land-Use" = "#1f77b4", "Climate" = "#ff7f0e", "Other" = "#2ca02c")

fi_opt_plot <- ggplot(new_replica, 
                      aes(x = reorder(Feature, +importance), y = importance, fill = Group)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(x = 'Variable') + 
  geom_errorbar(aes(x = Feature, ymin = lowerbound, ymax = upperbound)) +
  ggtitle("Occurrence Feature Importance Plot for Tuned Model")
print(fi_opt_plot)
fi_opt_plot + coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                   panel.background = element_blank(), axis.line = element_line(colour = "black"))

subset_boi <- subset(pdps_optimal_model, variable == 'urban_area' | variable == 'agricultural_area' |
                       variable == 'Avg_distance_to_river' | variable == 'Minimum.Runoff')

subset_dude <- subset(pdps_optimal_model_mean, variable == 'urban_area' | variable == 'agricultural_area' |
                        variable == 'Avg_distance_to_river' | variable == 'Minimum.Runoff')

subset_boi$variable[subset_boi$variable == "urban_area"] <- "Urban Area"
subset_dude$variable[subset_dude$variable == 'urban_area'] <- 'Urban Area'
subset_boi$variable[subset_boi$variable == 'agricultural_area'] <- 'Agricultural Area'
subset_dude$variable[subset_dude$variable == 'agricultural_area'] <- 'Agricultural Area'
subset_boi$variable[subset_boi$variable == 'Avg_distance_to_river'] <- 'Avg Distance to River'
subset_dude$variable[subset_dude$variable == 'Avg_distance_to_river'] <- "Avg Distance to River"
subset_boi$variable[subset_boi$variable == "Minimum.Runoff"] <- "Minimum Runoff"
subset_dude$variable[subset_dude$variable == "Minimum.Runoff"] <- "Minimum Runoff"

classify_variable <- function(variable) {
  case_when(
    variable %in% c("Urban Area", "Agricultural Area") ~ "Land-Use",
    variable == "Avg Distance to River" ~ "Other",
    variable == "Minimum Runoff" ~ "Climate",
    TRUE ~ "Other"  # Default case if none of the above conditions are met
  )
}

subset_boi <- subset_boi %>%
  mutate(Variable = classify_variable(variable))

subset_dude <- subset_dude %>%
  mutate(Variable = classify_variable(variable))


bsurp_dudes <- ggplot(subset_boi, aes(x = value, y = yhat, group = model)) +
  stat_smooth(geom = 'line', alpha = 0.15, se = FALSE) +
  geom_smooth(data = subset_dude, aes(x = value, y = mean_yhat, color = Variable),
              linewidth = 1.5, 
              se = FALSE) +
  ylab("Occurrence") + xlab("Value") +
  facet_wrap(~variable, scales = "free", nrow = 2) +
  scale_color_manual(values = group_colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


bsurp_dudes <- ggplot(subset_boi, aes(x = value, 
                                      y = yhat, group = model)) +
  stat_smooth(geom = 'line', alpha = 0.15, se = FALSE) +
  geom_smooth(data = subset_dude, aes(x = value, y = mean_yhat, color = Variable),
              linewidth = 1.5, 
              se = FALSE)+
  ylab("Occurrence") + xlab("Value") +
  facet_wrap(~variable, scales = "free", nrow = 2)  
print(bsurp_dudes)
bsurp_dudes <- bsurp_dudes + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 
print(bsurp_dudes)
bsurp_dudes+ theme(text = element_text(size = 12)) 


# trying smt
# Install necessary packages if not already installed
install.packages("sf")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

# Load the libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Load the world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for Peru
peru <- subset(world, admin == "Peru")

# Define the coordinates for Madre de Dios region
madre_de_dios_coords <- matrix(c(
  -70.97, -12.16, -70.90, -11.88, -69.74, -11.01, -69.26, -11.12, 
  -68.65, -10.96, -68.65, -12.57, -69.48, -12.85, -70.47, -13.56, 
  -70.97, -12.16
), ncol = 2, byrow = TRUE)

# Create a polygon for Madre de Dios
madre_de_dios <- st_polygon(list(madre_de_dios_coords)) %>%
  st_sfc(crs = st_crs(peru))

peru_centroid <- st_centroid(st_geometry(peru))
peru_centroid_df <- as.data.frame(st_coordinates(peru_centroid))
peru_centroid_df$label <- "Peru"

# Plotting the map
ggplot() +
  geom_sf(data = peru, fill = "lightgrey", color = "black") +
  geom_sf(data = madre_de_dios, fill = "red", color = "black") +
  geom_text(data = peru_centroid_df, aes(x = X-1, y = Y, label = "Peru"), size = 8, fontface = "bold") +
  labs(title = "Madre de Dios Region in Peru") +
  theme_void()

# plotting all pdps nicely

new_subset_one <- pdps_optimal_model
new_subset_two <- pdps_optimal_model_mean

new_subset_one$variable[new_subset_one$variable == "Avg_distance_to_river"] <- "Distance To River"
new_subset_one$variable[new_subset_one$variable == 'Max.Temperature'] <- "Max Temperature"
new_subset_one$variable[new_subset_one$variable == 'urban_area'] <- 'Urban Area'
new_subset_one$variable[new_subset_one$variable == 'mining_area'] <- 'Mining Area'
new_subset_one$variable[new_subset_one$variable == 'Min.Temperature'] <- 'Min Temperature'
new_subset_one$variable[new_subset_one$variable == 'agricultural_area'] <- 'Agricultural Area'
new_subset_one$variable[new_subset_one$variable == 'forest_area'] <- 'Forest Area'
new_subset_one$variable[new_subset_one$variable == 'year'] <- 'Year'
new_subset_one$variable[new_subset_one$variable == 'Minimum.Runoff'] <- 'Min Runoff'
new_subset_one$variable[new_subset_one$variable == 'Maximum.Precipitation'] <- 'Max Precipitation'


new_subset_two$variable[new_subset_two$variable == "Avg_distance_to_river"] <- "Distance To River"
new_subset_two$variable[new_subset_two$variable == 'Max.Temperature'] <- "Max Temperature"
new_subset_two$variable[new_subset_two$variable == 'urban_area'] <- 'Urban Area'
new_subset_two$variable[new_subset_two$variable == 'mining_area'] <- 'Mining Area'
new_subset_two$variable[new_subset_two$variable == 'Min.Temperature'] <- 'Min Temperature'
new_subset_two$variable[new_subset_two$variable == 'agricultural_area'] <- 'Agricultural Area'
new_subset_two$variable[new_subset_two$variable == 'forest_area'] <- 'Forest Area'
new_subset_two$variable[new_subset_two$variable == 'year'] <- 'Year'
new_subset_two$variable[new_subset_two$variable == 'Minimum.Runoff'] <- 'Min Runoff'
new_subset_two$variable[new_subset_two$variable == 'Maximum.Precipitation'] <- 'Max Precipitation'

new_classify_variable <- function(variable) {
  case_when(
    variable %in% c("Urban Area", "Agricultural Area", "Forest Area", "Mining Area") ~ "Land-Use",
    variable %in% c("Avg Distance to River","Year") ~ "Other",
    variable %in% c("Min Runoff","Max Precipitation", "Max Temperature", "Min Temperature")  ~ "Climate",
    TRUE ~ "Other"  # Default case if none of the above conditions are met
  )
}

new_subset_one <- new_subset_one %>%
  mutate(Variable = new_classify_variable(variable))

new_subset_two <- new_subset_two %>%
  mutate(Variable = new_classify_variable(variable))


mordecai_lab_fig4 <- ggplot(new_subset_one, aes(x = value, 
                                                y = yhat, group = model)) +
  stat_smooth(geom = 'line', alpha = 0.15, se = FALSE) +
  geom_smooth(data = new_subset_two, aes(x = value, y = mean_yhat, color = Variable),
              linewidth = 1.5, 
              se = FALSE)+
  ylab("Occurrence") + xlab("Value") +
  facet_wrap(~variable, scales = "free", nrow = 4)  
print(mordecai_lab_fig4)
mordecai_lab_fig4 <- mordecai_lab_fig4 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                               panel.background = element_blank(), axis.line = element_line(colour = "black")) 
print(mordecai_lab_fig4)
mordecai_lab_fig4+ theme(text = element_text(size = 12)) 



incidence_feature_importance_boi <- read.csv('new_incidence_tuned_model_FI_50_its.csv')
incidence_feature_importance_boi <- na.omit(incidence_feature_importance_boi)




# now developing the figures for Incidence Model with new variables

mordecai_lab_fi_incidence <- inc_model_confidence
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == "Avg_distance_to_river"] <- "Distance To River"
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'Max.Temperature'] <- "Max Temperature"
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'urban_area'] <- 'Urban Area'
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'mining_area'] <- 'Mining Area'
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'Min.Temperature'] <- 'Min Temperature'
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'agricultural_area'] <- 'Agricultural Area'
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'forest_area'] <- 'Forest Area'

mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'year'] <- 'Year'
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'Minimum.Runoff'] <- 'Min Runoff'
mordecai_lab_fi_incidence$Feature[mordecai_lab_fi_incidence$Feature == 'Maximum.Precipitation'] <- 'Max Precipitation'


library(dplyr)
library(stringr)

mordecai_lab_fi_incidence <- mordecai_lab_fi_incidence %>%
  mutate(
    Group = case_when(
      stringr::str_detect(Feature, "Area") ~ "Land-Use",
      stringr::str_detect(Feature, "[Mm]in|[Mm]ax") ~ "Climate",
      TRUE ~ "Other" # Default group for any that do not match above
    )
  )
mordecai_lab_fi_incidence$Group <- as.factor(mordecai_lab_fi_incidence$Group)

group_colors <- c("Land-Use" = "#1f77b4", "Climate" = "#ff7f0e", "Other" = "#2ca02c")

mordecai_lab_fi_incidence <- mordecai_lab_fi_incidence %>%
  rename(importance = boostrapping_mean)

new_fi_plot_for_lab_present <- ggplot(mordecai_lab_fi_incidence, 
                                      aes(x = reorder(Feature, +importance), y = importance, fill = Group)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(x = 'Variable') + 
  geom_errorbar(aes(x = Feature, ymin = lowerbound, ymax = upperbound)) +
  ggtitle("Incidence Feature Importance Plot for Tuned Model")
print(new_fi_plot_for_lab_present)
new_fi_plot_for_lab_present <- new_fi_plot_for_lab_present + coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(new_fi_plot_for_lab_present)


incidence_pdp_tinkering_all <- pdps_incidence_model
incidence_pdp_tinkering_mean <- pdps_incidence_model_mean

incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == "Avg_distance_to_river"] <- "Distance To River"
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'Max.Temperature'] <- "Max Temperature"
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'urban_area'] <- 'Urban Area'
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'mining_area'] <- 'Mining Area'
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'Min.Temperature'] <- 'Min Temperature'
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'agricultural_area'] <- 'Agricultural Area'
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'forest_area'] <- 'Forest Area'
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'year'] <- 'Year'
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'Minimum.Runoff'] <- 'Min Runoff'
incidence_pdp_tinkering_all$variable[incidence_pdp_tinkering_all$variable == 'Maximum.Precipitation'] <- 'Max Precipitation'


incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == "Avg_distance_to_river"] <- "Distance To River"
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'Max.Temperature'] <- "Max Temperature"
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'urban_area'] <- 'Urban Area'
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'mining_area'] <- 'Mining Area'
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'Min.Temperature'] <- 'Min Temperature'
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'agricultural_area'] <- 'Agricultural Area'
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'forest_area'] <- 'Forest Area'
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'year'] <- 'Year'
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'Minimum.Runoff'] <- 'Min Runoff'
incidence_pdp_tinkering_mean$variable[incidence_pdp_tinkering_mean$variable == 'Maximum.Precipitation'] <- 'Max Precipitation'

incidence_pdp_tinkering_all <- incidence_pdp_tinkering_all %>%
  mutate(Variable = new_classify_variable(variable))

incidence_pdp_tinkering_mean <- incidence_pdp_tinkering_mean %>%
  mutate(Variable = new_classify_variable(variable))


mordecai_lab_fig10_idk <- ggplot(incidence_pdp_tinkering_all, aes(x = value, 
                                                                  y = yhat, group = model)) +
  stat_smooth(geom = 'line', alpha = 0.15, se = FALSE) +
  geom_smooth(data = incidence_pdp_tinkering_mean, aes(x = value, y = mean_yhat, color = Variable),
              linewidth = 1.5, 
              se = FALSE)+
  ylab("Incidence") + xlab("Value") +
  facet_wrap(~variable, scales = "free", nrow = 4)  
print(mordecai_lab_fig10_idk)
mordecai_lab_fig10_idk <- mordecai_lab_fig10_idk + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black")) 
print(mordecai_lab_fig10_idk)
mordecai_lab_fig4+ theme(text = element_text(size = 12)) 
