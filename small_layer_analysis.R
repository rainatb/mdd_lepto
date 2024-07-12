# layer analysis for 0s and 1s distribution
# layers checking
checking_clust_66_and_16 <- read.csv('final_dataset_for_spatio_temporal_tuning.csv')


# Check for layer 16
layer_16_occurrences = sum(checking_clust_66_and_16$layer == 16 & checking_clust_66_and_16$occurrence == 1)

# Check for layer 66
layer_66_occurrences = sum(checking_clust_66_and_16$layer == 66 & checking_clust_66_and_16$occurrence == 1)

# Output results
if (layer_16_occurrences > 0) {
  print("Layer 16 has occurrences where value is 1.")
} else {
  print("Layer 16 has no occurrences where value is 1.")
}

if (layer_66_occurrences > 0) {
  print("Layer 66 has occurrences where value is 1.")
} else {
  print("Layer 66 has no occurrences where value is 1.")
}
