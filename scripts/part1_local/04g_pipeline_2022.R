# =============================================================================
# 04g_pipeline_2022.R
# -----------------------------------------------------------------------------
# 2022: same steps as 2018 (unpack, L2, truncated SVD to 250 components),
# Bayesian optimisation of k in [2, 250], final k-means with k = 200 kept
# for comparability with 2018, diagnostics, and export of both years
# (brazil_url_shares_clusters_combined.rds, used by steps 05 and 06).
#
# Run the 04 scripts in order in ONE R session: each uses objects created
# by the previous one (data, clean_data, mat_pca, mat_sample, ...).
# =============================================================================

# ==============================================================================
# 2022 REPRODUCIBILITY PIPELINE
# ==============================================================================
# This script mirrors the 2018 process. Even with a smaller N, we maintain
# k=200 and PCA variance thresholds to ensure comparative validity.
# ==============================================================================

# --- 1. SETUP & DATA PREPARATION ---
library(Matrix)
library(irlba)
library(tidyverse)
library(rBayesianOptimization)

# Filter for 2022 and clean NAs
data_2022 <- clean_data %>% filter(year == 2022)
embeddings_raw_22 <- data_2022$embedding_vector
n_rows_22 <- length(embeddings_raw_22)

# Pre-allocate matrix (smaller footprint than 2018)
mat_2022 <- matrix(0, nrow = n_rows_22, ncol = 3072)

# --- 2. VECTOR UNPACKING (CHUNKED) ---
chunk_size <- 5000
for (i in seq(1, n_rows_22, by = chunk_size)) {
  end_idx <- min(i + chunk_size - 1, n_rows_22)
  chunk_unpacked <- lapply(embeddings_raw_22[i:end_idx], function(x) {
    as.numeric(strsplit(gsub("\\[|\\]", "", x), ",")[[1]])
  })
  mat_2022[i:end_idx, ] <- do.call(rbind, chunk_unpacked)
  rm(chunk_unpacked); gc()
  cat(sprintf("Unpacking 2022: %.1f%% completed\n", (end_idx/n_rows_22)*100))
}

# --- 3. L2 NORMALIZATION ---
# Critical to maintain "Cosine Similarity" logic across both years
row_norms_22 <- sqrt(rowSums(mat_2022^2))
row_norms_22[row_norms_22 == 0] <- 1
mat_norm_22 <- mat_2022 / row_norms_22
rm(mat_2022); gc()

# --- 4. PCA REDUCTION ---
# Note: We aim for 250 components to match 2018, but check variance explained.
set.seed(123)
pca_22 <- irlba(mat_norm_22, nv = 250)
mat_pca_22 <- pca_22$u %*% diag(pca_22$d)

var_explained_22 <- sum(pca_22$d^2) / sum(rowSums(mat_norm_22^2))
cat(sprintf("\n2022 PCA: 250 components explain %.2f%% of variance.\n", var_explained_22 * 100))

## Run this BEFORE the Bayesian Optimization step if your environment was cleared
clustering_objective <- function(k_candidate) {
  k_int <- as.integer(round(k_candidate))
  set.seed(123)
  # Using mat_pca_22 specifically for the 2022 run
  km_model <- kmeans(mat_pca_22, centers = k_int, nstart = 3, iter.max = 50)
  score_val <- -km_model$tot.withinss
  return(list(Score = score_val, Pred = 0))
}

# --- 5. BAYESIAN OPTMIZATION ---

# Use the mat_pca_22 you already created
cat("Starting Bayesian Optimization for 2022...\n")

opt_results_22 <- BayesianOptimization(
  FUN = clustering_objective, # Uses the function from your 2018 script
  bounds = list(k_candidate = c(2, 250)), # We decided to maintain k=200 also for 2022
  init_points = 10,
  n_iter = 20,
  acq = "ei",
  verbose = TRUE
)

# Check if the Best_Par is close to 200
print(opt_results_22$Best_Par) # k=250, but maintaing k=200 we have a loss of only 2,49%


# --- 5. FINAL K-MEANS (k=200) ---
# We keep k=200 for direct thematic comparison with the 2018 results.
cat("Running Final 2022 K-means...\n")
set.seed(123)
final_km_2022 <- kmeans(mat_pca_22, centers = 200, nstart = 20, 
                        iter.max = 100, algorithm = "Lloyd")

# Assign the finalized cluster labels to your 2022 dataframe
data_2022$cluster <- final_km_2022$cluster

# --- 2. STATISTICAL SUMMARY ---
# Extract sizes as a vector for easy calculation
cluster_sizes_22 <- as.vector(table(data_2022$cluster))

cat("\n======================================\n")
cat("   2022 CLUSTER SIZE DIAGNOSTICS\n")
cat("======================================\n")
print(summary(cluster_sizes_22))

# --- 3. DISTRIBUTION VISUALIZATION ---
# This histogram shows if your 200 themes are evenly spread
hist(cluster_sizes_22, 
     breaks = 50, 
     main = "Distribution of Cluster Sizes (2022, K=200)",
     xlab = "Number of Posts per Cluster", 
     col = "steelblue",
     border = "white")

# Add a vertical line for the median to help the visual analysis
abline(v = median(cluster_sizes_22), col = "red", lwd = 2, lty = 2)

# --- 4. NOISE IDENTIFICATION ---
# With N=62k, clusters < 50-80 posts might be very niche outliers
small_clusters <- sum(cluster_sizes_22 < 100)
cat("\nNumber of clusters with fewer than 100 posts:", small_clusters, "\n")

# --- 6. EXPORT ---
saveRDS(data_2022, "data_2022_final_clustered_200.rds")
save(data_2022, final_km_2022, file = "2022_clustering_backup.RData")
write.csv(data_2018, file = "data_2018_final_clustered.csv", row.names = FALSE)
write.csv(data_2022, file = "data_2022_final_clustered.csv", row.names = FALSE)

## Save everything ##
# Save the absolute entire environment (all years, models, and variables)
save.image(file = "brazil_url_shares_clusters_combined.RData")

# Save only the specific objects needed for the thesis/paper
save(
  # 2018 Data & Models
  data_2018, mat_pca, final_km_200, 
  # 2022 Data & Models
  data_2022, mat_pca_22, final_km_2022,
  # Comparison Stats
  validation_results, 
  file = "Thesis_Core_Data_18_22.RData"
)

# Standard format for sharing processed data
saveRDS(data_2018, "data_2018_clusters.rds")
saveRDS(data_2022, "data_2022_clusters.rds")

# Merge dataframes 'data_2018' and 'data_2022'
final_clean_data <- rbind(data_2018, data_2022)

# 2. Save as a single RDS (Compressed and Fast)
saveRDS(final_clean_data, "brazil_url_shares_clusters_combined.rds")

# 3. Optional: Save as an RData to keep the separate objects too
save(data_2018, data_2022, final_clean_data, 
     file = "Thesis_Workspace_Final.RData")
