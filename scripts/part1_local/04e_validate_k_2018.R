# =============================================================================
# 04e_validate_k_2018.R
# -----------------------------------------------------------------------------
# 2018: compares candidate k values (elbow by perpendicular distance and by
# second derivative, 85% of WSS reduction, and k = 200) on the 16,000-item
# sample with silhouette, Davies-Bouldin and Calinski-Harabasz indices,
# then checks the cluster-size balance of k = 200.
#
# Run the 04 scripts in order in ONE R session: each uses objects created
# by the previous one (data, clean_data, mat_pca, mat_sample, ...).
# =============================================================================

# --- Method 1: perpendicular distance ---
find_knee <- function(x, y) {
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (y - min(y)) / (max(y) - min(y))
  x1 <- x_norm[1]; y1 <- y_norm[1]
  x2 <- x_norm[length(x_norm)]; y2 <- y_norm[length(y_norm)]
  distances <- abs((y2-y1)*x_norm - (x2-x1)*y_norm + x2*y1 - y2*x1) / 
    sqrt((y2-y1)^2 + (x2-x1)^2)
  return(x[which.max(distances)])
}

elbow_perp <- find_knee(k_values, wss_values)

# --- Method 2: second derivative ---
second_deriv <- diff(diff(wss_values))
elbow_2nd <- k_values[which.max(second_deriv) + 2]

# --- Method 3: share of WSS reduction ---
total_var <- wss_values[1] - wss_values[length(wss_values)]
var_explained <- (wss_values[1] - wss_values) / total_var * 100
elbow_85pct <- k_values[which(var_explained >= 85)[1]]
elbow_90pct <- k_values[which(var_explained >= 90)[1]]

# --- Elbow summary ---
cat("\n======================================\n")
cat("   ELBOW ANALYSIS - RESULTS\n")
cat("======================================\n")
cat("Perpendicular distance:    k =", elbow_perp, "\n")
cat("Second derivative:         k =", elbow_2nd, "\n")
cat("85% of WSS reduction:      k =", elbow_85pct, "\n")
cat("90% of WSS reduction:      k =", elbow_90pct, "\n")
cat("Bayesian optimisation:     k = 200 (upper bound of the search range)\n")
cat("======================================\n\n")

# --- Validation: compare candidate k ---
library(cluster)

# Candidate k values (elbow methods and k = 200)
candidate_ks <- c(elbow_perp, elbow_2nd, elbow_85pct, 200)
candidate_ks <- unique(sort(candidate_ks))

cat("Testing k values:", paste(candidate_ks, collapse = ", "), "\n\n")

validation_results <- data.frame(
  k = candidate_ks,
  wss = NA,
  silhouette = NA,
  davies_bouldin = NA,
  calinski_harabasz = NA
)

for (i in seq_along(candidate_ks)) {
  k <- candidate_ks[i]
  cat("Testing k =", k, "...")
  
  set.seed(123)
  km <- kmeans(mat_sample, centers = k, nstart = 20, iter.max = 100)
  
  # WSS
  validation_results$wss[i] <- km$tot.withinss
  
  # Silhouette Score (higher is better, range -1 to 1)
  sil <- silhouette(km$cluster, dist(mat_sample))
  validation_results$silhouette[i] <- mean(sil[, 3])
  
  # Calinski-Harabasz Index (higher is better)
  between_ss <- km$betweenss
  within_ss <- km$tot.withinss
  n <- nrow(mat_sample)
  ch_index <- (between_ss / (k - 1)) / (within_ss / (n - k))
  validation_results$calinski_harabasz[i] <- ch_index
  
  # Davies-Bouldin Index (lower is better)
  # Davies-Bouldin index computed by hand
  centers <- km$centers
  clusters <- km$cluster
  db_values <- numeric(k)
  
  for (j in 1:k) {
    if (sum(clusters == j) > 0) {
      cluster_j <- mat_sample[clusters == j, , drop = FALSE]
      avg_dist_j <- mean(sqrt(rowSums((sweep(cluster_j, 2, centers[j, ]))^2)))
      
      max_ratio <- 0
      for (l in 1:k) {
        if (l != j && sum(clusters == l) > 0) {
          cluster_l <- mat_sample[clusters == l, , drop = FALSE]
          avg_dist_l <- mean(sqrt(rowSums((sweep(cluster_l, 2, centers[l, ]))^2)))
          center_dist <- sqrt(sum((centers[j, ] - centers[l, ])^2))
          
          if (center_dist > 0) {
            ratio <- (avg_dist_j + avg_dist_l) / center_dist
            if (ratio > max_ratio) max_ratio <- ratio
          }
        }
      }
      db_values[j] <- max_ratio
    }
  }
  validation_results$davies_bouldin[i] <- mean(db_values[is.finite(db_values)])
  
  cat(" Done!\n")
}

summary(validation_results$k)

# Rescale each index to 0-1
validation_results$wss_score <- 1 - (validation_results$wss - min(validation_results$wss)) / 
  (max(validation_results$wss) - min(validation_results$wss))
validation_results$sil_score <- (validation_results$silhouette - min(validation_results$silhouette)) / 
  (max(validation_results$silhouette) - min(validation_results$silhouette))
validation_results$db_score <- 1 - (validation_results$davies_bouldin - min(validation_results$davies_bouldin)) / 
  (max(validation_results$davies_bouldin) - min(validation_results$davies_bouldin))
validation_results$ch_score <- (validation_results$calinski_harabasz - min(validation_results$calinski_harabasz)) / 
  (max(validation_results$calinski_harabasz) - min(validation_results$calinski_harabasz))

# Combined score (mean of the four rescaled indices)
validation_results$combined_score <- (validation_results$wss_score + 
                                        validation_results$sil_score + 
                                        validation_results$db_score + 
                                        validation_results$ch_score) / 4

# Sort by combined score
validation_results <- validation_results[order(-validation_results$combined_score), ]

cat("\n======================================\n")
cat("   VALIDATION RESULTS\n")
cat("======================================\n")
print(validation_results[, c("k", "silhouette", "davies_bouldin", 
                             "calinski_harabasz", "combined_score")])

best_k <- validation_results$k[1]
cat("\nHighest combined score: k =", best_k, "\n")

# Check whether k = 200 is among the two best candidates
if (200 %in% validation_results$k[1:min(2, nrow(validation_results))]) {
  cat("\nk = 200 is among the two best candidates.\n")
} else {
  cat("\nk = 200 is not among the two best candidates; best k =", best_k, "\n")
}

# --- Cluster size distribution (k = 200) ---
cat("\n======================================\n")
cat("   CLUSTER SIZE DISTRIBUTION (k = 200)\n")
cat("======================================\n")

set.seed(123)
km_200 <- kmeans(mat_sample, centers = 200, nstart = 20)
cluster_sizes <- table(km_200$cluster)

cat("Min cluster size:", min(cluster_sizes), "\n")
cat("Max cluster size:", max(cluster_sizes), "\n")
cat("Median:", median(cluster_sizes), "\n")
cat("Mean:", mean(cluster_sizes), "\n")
cat("Std dev:", sd(cluster_sizes), "\n")

# Check balance
imbalance_ratio <- max(cluster_sizes) / median(cluster_sizes)
cat("Imbalance ratio (max/median):", round(imbalance_ratio, 2), "\n")

if (imbalance_ratio > 5) {
  cat("\nWarning: clusters are highly unbalanced (max/median > 5).\n")
} else {
  cat("\nCluster sizes are reasonably balanced (max/median <= 5).\n")
}

# Plot
hist(cluster_sizes, breaks = 30, col = "#2E7D32",
     main = "Cluster size distribution (k = 200)",
     xlab = "Posts per cluster", ylab = "Frequency")
abline(v = median(cluster_sizes), col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Median", col = "red", lty = 2, lwd = 2)

### Save the workspace ###
save.image(file = "2018_pca_validation_k.RData")
