# king center symposium figures
##King Center Symposium Poster figures
theme_update(plot.title = element_text(hjust = 0.5))

# more figures
locations <- read.csv('cluster_centroids_70.csv')
locations <- data.frame(locations)
locations <- locations %>%
  mutate(latitude= as.numeric(latitude),longitude= as.numeric(longitude) ) %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))
locations <- st_transform(locations$geometry, 4326)
mapview(locations, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
districts <- read_sf("/Users/rashitalwarbhatia/Downloads/folder/Madre_de_Dios.shp")
districts <- st_as_sf(districts)
districts$geometry <- st_transform(districts$geometry, 4326)
highway <- read_sf("/Users/rashitalwarbhatia/peru_roads/peru_roads_vimportant.shp")
highway <- highway[which(highway$ref=="PE-30C"),]
highway <- st_as_sf(highway)
st_crs(highway) <- 4326
highway$geometry <- st_transform(highway$geometry, 4326)
mdd_region <- st_union(districts$geometry)
highway_mdd <- st_covers(mdd_region,highway$geometry, sparse = FALSE)
highway_mdd <- highway[highway_mdd[1,],]
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())
fig2 <- ggplot() +
  geom_sf(data=districts, fill="#FFFFFF", color="#626262", size=.15, show.legend = FALSE) +
  geom_sf(data = highway_mdd, aes(geometry = geometry), colour="#d3d3d3", linewidth=1.0) +
  geom_sf(data = locations, aes(geometry = geometry), colour="#476C9B", size = 1.5, alpha=1) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.position="bottom") +
  guides(fill=guide_legend(override.aes=list(shape=21)))
fig2 <- fig2 + ggtitle("Healthcare Center Clusters") +   theme(plot.title = element_text(hjust = 0.5))

fig2trial <- fig2 + ggspatial::annotation_north_arrow(
  location = "tr", which_north = "true",
  pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
  style = ggspatial::north_arrow_fancy_orienteering(
    line_width = 1,
    line_col = "black",
    fill = c("white", "black"),
    text_col = "black",
    text_family = "",
    text_face = NULL,
    text_size = 10,
    text_angle = 0
  )) + 
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  )



print(fig2trial)

replica <- model_confidence

replica$Feature[replica$Feature == "Avg_distance_to_river"] <- "Distance To River"
replica$Feature[replica$Feature == 'Max.Temperature'] <- "Max Temperature"
replica$Feature[replica$Feature == 'urban_area'] <- 'Urban Area'
replica$Feature[replica$Feature == 'mining_area'] <- 'Mining Area'
replica$Feature[replica$Feature == 'Min.Temperature'] <- 'Min Temperature'
replica$Feature[replica$Feature == 'agricultural_area'] <- 'Agricultural Area'
replica$Feature[replica$Feature == 'forest_area'] <- 'Forest Area'

replica$Feature[replica$Feature == 'year'] <- 'Year'
replica$Feature[replica$Feature == 'Maximum Runoff'] <- 'Max Runoff'
replica$Feature[replica$Feature == 'Minimum.Precipitation'] <- 'Min Precipitation'
replica$Feature[replica$Feature == 'Minimum.Runoff'] <- 'Min Runoff'

library(dplyr)
library(stringr)

replica <- replica %>%
  mutate(
    Group = case_when(
      stringr::str_detect(Feature, "Area") ~ "Land-Use",
      stringr::str_detect(Feature, "[Mm]in|[Mm]ax") ~ "Climate",
      TRUE ~ "Other" # Default group for any that do not match above
    )
  )
replica$Group <- as.factor(replica$Group)

group_colors <- c("Land-Use" = "#1f77b4", "Climate" = "#ff7f0e", "Other" = "#2ca02c")

# Ensure the levels of your 'Group' factor match the names used in your color vector.
replica$Group <- factor(replica$Group, levels = c("Land-Use", "Climate", "Other"))

fi_for_poster <- ggplot(replica, aes(x = reorder(Feature, +boostrapping_mean), y = boostrapping_mean)) + 
  geom_point(aes(color = Group), size = 2) +  # Increase the size of the points
  labs(x = 'Variable', y = 'Importance') + 
  geom_errorbar(aes(ymin = lowerbound, ymax = upperbound, color = Group), width = 0.4) +  # Make error bars ends smaller
  scale_color_manual(values = group_colors) +
  ggtitle("Variable Importance") + 
  theme_minimal(base_size = 15) +  # Increase base text size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20), # Make title text bigger
    axis.title = element_text(size = 14), # Make axis titles bigger
    axis.text.x = element_text(size = 12), # Make x axis labels bigger
    axis.text.y = element_text(size = 12), # Make y axis labels bigger
    legend.position = "right", # Move legend to right
    legend.title = element_text(size = 10), # Increase legend title size
    legend.text = element_text(size = 8) # Increase legend text size
  )

print(fi_for_poster)
fi_for_poster + coord_flip()
pretty_hopefully <- fi_for_poster + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
pretty_hopefully <- pretty_hopefully + coord_flip()
print(pretty_hopefully)
ggsave("Variable_Importance_Plot_KCS.png", pretty_hopefully, width = 12, height = 8, dpi = 300)


subset_boi <- subset(pdps_optimal_model, variable == 'urban_area' | variable == 'agricultural_area' |
                       variable == 'year' | variable == 'Minimum.Precipitation')

subset_dude <- subset(pdps_optimal_model_mean, variable == 'urban_area' | variable == 'agricultural_area' |
                        variable == 'year' | variable == 'Minimum.Precipitation')

subset_boi$variable[subset_boi$variable == "urban_area"] <- "Urban Area"
subset_dude$variable[subset_dude$variable == 'urban_area'] <- 'Urban Area'
subset_boi$variable[subset_boi$variable == 'agricultural_area'] <- 'Agricultural Area'
subset_dude$variable[subset_dude$variable == 'agricultural_area'] <- 'Agricultural Area'
subset_boi$variable[subset_boi$variable == 'year'] <- 'Year'
subset_dude$variable[subset_dude$variable == 'year'] <- "Year"
subset_boi$variable[subset_boi$variable == "Minimum.Precipitation"] <- "Minimum Precipitation"
subset_dude$variable[subset_dude$variable == "Minimum.Precipitation"] <- "Minimum Precipitation"

classify_variable <- function(variable) {
  case_when(
    variable %in% c("Urban Area", "Agricultural Area") ~ "Land-Use",
    variable == "Year" ~ "Other",
    variable == "Minimum Precipitation" ~ "Climate",
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
bsurp_dudes+ theme(text = element_text(size = 15))                    # All font sizes

mean_area_under_curve <- mean(auc_per_run$auc)
areaundercurve_sd <- sd(auc_per_run$auc)
areaundercurve_se <- areaundercurve_sd/sqrt(50)
alpha = 0.05

t_score <- qt(p = alpha/2, df = 49, lower.tail = F)

auc_margin_error <- t_score * areaundercurve_se
auc_lb_ <- mean_area_under_curve - auc_margin_error
auc_ub <- mean_area_under_curve + auc_margin_error
