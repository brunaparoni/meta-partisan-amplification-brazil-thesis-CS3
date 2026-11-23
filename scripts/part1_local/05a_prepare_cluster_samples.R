# =============================================================================
# 05a_prepare_cluster_samples.R
# -----------------------------------------------------------------------------
# Cluster labelling, step 1: density-based sampling of representative texts.
#
# For each cluster, every text gets a local density score = mean cosine
# similarity to its 20 nearest neighbours in the cluster; the 150 densest
# texts are kept (all texts when the cluster has 150 or fewer). Unlike
# centroid-based sampling, this picks texts from where most of the cluster
# actually is, even when the centroid falls between sub-topics.
#
# Input : rawdata/brazil_url_shares_clusters_combined.rds   (from step 04)
# Output: output/cluster_samples.rds   (59,047 texts, 400 cluster-years)
# =============================================================================

library(jsonlite)

INPUT_FILE  <- "rawdata/brazil_url_shares_clusters_combined.rds"
OUTPUT_FILE <- "output/cluster_samples.rds"
TOP_N       <- 150
K_NEIGHBORS <- 20

dir.create("output", showWarnings = FALSE)
d <- readRDS(INPUT_FILE)
results <- list()

for (yr in sort(unique(d$year))) {
  d_year <- d[d$year == yr, ]

  # Parse embeddings into a matrix and L2-normalise (dot product = cosine)
  emb_mat <- do.call(rbind, lapply(d_year$embedding_vector, fromJSON))
  gc(verbose = FALSE)
  emb_mat <- emb_mat / sqrt(rowSums(emb_mat^2))

  for (cl in sort(unique(d_year$cluster))) {
    idx_cl <- which(d_year$cluster == cl)
    n_cl   <- length(idx_cl)
    k_use  <- min(K_NEIGHBORS, n_cl - 1)

    if (n_cl <= TOP_N) {
      selected <- idx_cl
    } else {
      sim_mat <- tcrossprod(emb_mat[idx_cl, , drop = FALSE])
      diag(sim_mat) <- -Inf
      density_scores <- vapply(seq_len(n_cl), function(i) {
        mean(sort(sim_mat[i, ], decreasing = TRUE)[1:k_use])
      }, numeric(1))
      selected <- idx_cl[order(density_scores, decreasing = TRUE)[1:TOP_N]]
    }

    results[[length(results) + 1]] <- data.frame(
      year = yr, cluster = cl, n_urls = n_cl,
      alltext = d_year$alltext[selected],
      stringsAsFactors = FALSE
    )
  }
  rm(emb_mat, d_year); gc(verbose = FALSE)
}

samples <- do.call(rbind, results)
saveRDS(samples, OUTPUT_FILE)
cat(sprintf("Sampled texts: %s | cluster-years: %d\n",
            format(nrow(samples), big.mark = ","),
            nrow(unique(samples[, c("year", "cluster")]))))
