# simple data analysis post-Mordecai lab meeting & feedback
# plotting min temperature versus time for each cluster
theme_update(plot.title = element_text(hjust = 0.5))

library(ggplot2)
library(dplyr)

plots_dataframe <- updated_spatial_model_prep

plots_dataframe <- plots_dataframe %>%
  mutate(Date = as.Date(paste(year, Month, "01", sep = "-"), format = "%Y-%m-%d"))


monthly_cases_plots <- ggplot(plots_dataframe, aes(x = Date, y = monthlycount, color = as.factor(layer))) +
  stat_smooth(aes(group = layer), method = "loess", se = FALSE, alpha = 0.5) +
  facet_grid(~ fold_ids, scales = "free_y") +
  labs(title = "Monthly Count Cases Over Time by Fold_Ids",
       x = "Date",
       y = "Monthly Count Cases",
       color = "Layer") +
  theme_minimal() +
  theme(legend.position = "right") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(monthly_cases_plots)

new_name <- plots_dataframe[plots_dataframe$layer != 1, ]

another_monthly_cases <- ggplot(new_name, aes(x = Date, y = monthlycount, color = as.factor(layer))) +
  #stat_smooth(aes(group = layer), method = "loess", se = FALSE, alpha = 0.5) +
  geom_line(aes(group = layer), alpha = 0.5) +
  facet_grid(~ fold_ids, scales = "free_y") +
  labs(title = "Monthly Count Cases Over Time by Fold_Ids",
       x = "Date",
       y = "Monthly Count Cases",
       color = "Layer") +
  theme_minimal() +
  theme(legend.position = "right") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(another_monthly_cases)

max(plots_dataframe$monthlycount)

urban_area_plots <- ggplot(plots_dataframe,  aes(x = Date, y = urban_area, color = as.factor(layer))) +
  geom_line(aes(group = layer), alpha = 0.5) +
  #stat_smooth(aes(group = layer), method = "loess", se = FALSE, alpha = 0.5) +
  facet_grid(~ fold_ids, scales = "free_y") +
  labs(title = "Urban Area Over Time by Fold_Ids",
       x = "Date",
       y = "Urban Area",
       color = "Layer") +
  theme_minimal() +
  theme(legend.position = "right") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(urban_area_plots)

forest_area_plots <-  ggplot(plots_dataframe,  aes(x = Date, y = forest_area, color = as.factor(layer))) +
  stat_smooth(aes(group = layer), method = "loess", se = FALSE, alpha = 0.5) +
  facet_grid(~ fold_ids, scales = "free_y") +
  labs(title = "Forest Area Over Time by Fold_Ids",
       x = "Date",
       y = "Forest Area",
       color = "Layer") +
  theme_minimal() +
  theme(legend.position = "right") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(forest_area_plots)

agricultural_area_plots <-  ggplot(plots_dataframe,  aes(x = Date, y = agricultural_area, color = as.factor(layer))) +
  stat_smooth(aes(group = layer), method = "loess", se = FALSE, alpha = 0.5) +
  facet_grid(~ fold_ids, scales = "free_y") +
  labs(title = "Agricultural Area Over Time by Fold_Ids",
       x = "Date",
       y = "Agricultural Area",
       color = "Layer") +
  theme_minimal() +
  theme(legend.position = "right") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(agricultural_area_plots)



another_plot_df <- new_model_w_inc

another_plot_df <- another_plot_df %>%
  mutate(Date = as.Date(paste(year, Month, "01", sep = "-"), format = "%Y-%m-%d"))

another_plot_df <- another_plot_df[another_plot_df$incidence > 0, ]

another_plot_df$incidence_log <- log(another_plot_df$incidence)
incidence_plots <- ggplot(another_plot_df, aes(x = Date, y = incidence, color = as.factor(layer))) +
  geom_line(aes(group = layer), alpha = 0.5) +
  #stat_smooth(aes(group = layer), method = "loess", se = FALSE, alpha = 0.5) +
  facet_grid(~ fold_ids, scales = "free_y") +
  labs(title = "Incidence Over Time by Fold_Ids",
       x = "Date",
       y = "Incidence ",
       color = "Layer") +
  theme_minimal() +
  theme(legend.position = "right") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(incidence_plots)


incidence_plots_log <- ggplot(another_plot_df, aes(x = Date, y = incidence_log, color = as.factor(layer))) +
  geom_line(aes(group = layer), alpha = 0.5) +
  #stat_smooth(aes(group = layer), method = "loess", se = FALSE, alpha = 0.5) +
  facet_grid(~ fold_ids, scales = "free_y") +
  labs(title = "Incidence Over Time by Fold_Ids",
       x = "Date",
       y = "Incidence ",
       color = "Layer") +
  theme_minimal() +
  theme(legend.position = "right") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(incidence_plots_log)

first_lepto_df <- plots_dataframe %>%
  group_by(layer) %>%
  filter(occurrence == 1) %>%
  summarize(first_date = min(Date), .groups = 'drop')

merged_df <- left_join(x = first_lepto_df,
                       y = another_plot_df,
                       by = c("layer", "first_date" = "Date"))

plot_urban <- ggplot(merged_df, aes(x = first_date, y = urban_area)) +
  geom_point() +
  labs(title = "First Lepto Occurrence vs Urban Area",
       x = "Date",
       y = "Urban Area") +
  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(plot_urban)

plot_forest <- ggplot(merged_df, aes(x = first_date, y = forest_area)) +
  geom_point() +
  labs(title = "Forest Area vs First Lepto Ocurrence Date",
       x = "Date",
       y = "Forest Area") +
  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(plot_forest)

plot_agriculture <- ggplot(merged_df, aes(x = first_date, y = agricultural_area)) +
  geom_point() +
  labs(title = "Agricultural Area vs First Lepto Ocurrence Date",
       x = "Date",
       y = "Agriculture Area") +
  theme_minimal()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                         panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(plot_agriculture)

plot_temperature <- ggplot(merged_df, aes(x = first_date, y = Min.Temperature)) +
  geom_point() +
  labs(title = "Minimum Temperature vs First Lepto Ocurrence Date",
       x = "Date",
       y = "Minimum Temperature") +
  theme_minimal()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                         panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(plot_temperature)

table(merged_df$Month)
