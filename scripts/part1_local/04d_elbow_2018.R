# =============================================================================
# 04d_elbow_2018.R
# -----------------------------------------------------------------------------
# 2018: elbow analysis on a random sample of 16,000 items, k = 1..500
# (k-means nstart = 3, iter.max = 60). Creates mat_sample, k_values,
# wss_values used by 04e.
#
# Run the 04 scripts in order in ONE R session: each uses objects created
# by the previous one (data, clean_data, mat_pca, mat_sample, ...).
# =============================================================================

# Elbow Analysis for 2018 Brazil

# 1. Representative sample (10% of total 160k)
set.seed(123)
sample_idx <- sample(1:nrow(mat_pca), 16000)
mat_sample <- mat_pca[sample_idx, ]

# 2. Define range (Starting lower to see the 'drop')
k_values <- seq(1, 500, by = 1) 
wss_values <- numeric(length(k_values))

# 3. Loop
for (i in seq_along(k_values)) {
  # Reduced nstart slightly to speed up the check
  km_test <- kmeans(mat_sample, centers = k_values[i], nstart = 3, iter.max = 60)
  wss_values[i] <- km_test$tot.withinss
  cat("Tested K =", k_values[i], " | WSS:", km_test$tot.withinss, "\n")
}

# 4. Plot the results
plot(k_values, wss_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters K",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method: Brazil 2018 Political Discourse",
     col = "#2E7D32")