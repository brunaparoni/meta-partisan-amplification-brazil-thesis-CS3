# =============================================================================
# 04f_final_kmeans_2018.R
# -----------------------------------------------------------------------------
# 2018: final k-means with k = 200 (Lloyd, nstart = 20, iter.max = 100,
# seed 123) on the 250 SVD components; cluster-size diagnostics.
#
# Run the 04 scripts in order in ONE R session: each uses objects created
# by the previous one (data, clean_data, mat_pca, mat_sample, ...).
# =============================================================================

# 1. Final k-means, k = 200 (Lloyd)
set.seed(123)
final_km_200 <- kmeans(mat_pca, 
                       centers = 200, 
                       nstart = 20, 
                       iter.max = 100,      # Aumentiamo i giri massimi
                       algorithm = "Lloyd") # Cambiamo algoritmo per stabilità


# 2. Add to your data
data_2018$cluster <- final_km_200$cluster

# 3. Check the sizes - are they tiny or balanced?
cluster_sizes <- table(data_2018$cluster)
print(summary(as.vector(cluster_sizes)))

# Check the distribution of cluster sizes
cluster_sizes <- table(final_km_200$cluster)
hist(cluster_sizes, breaks = 50, main = "Distribution of Cluster Sizes (K=200)",
     xlab = "Number of Posts per Cluster", col = "steelblue")

# Identify very small clusters (potential noise)
small_clusters <- sum(cluster_sizes < 100)
cat("Number of clusters with fewer than 100 posts:", small_clusters)

### Save the workspace ###
save.image(file = "2018_final_with_clusters.RData")
save(data_2018, final_km_200, file = "backup_clustering_completo.RData")
