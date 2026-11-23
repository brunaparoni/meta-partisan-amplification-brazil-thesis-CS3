# =============================================================================
# 05c_enrich_cluster_labels.R
# -----------------------------------------------------------------------------
# Cluster labelling, step 3: per-cluster metadata.
#
#   centroid (mean 3,072-d embedding); cohesion = mean / median / sd cosine
#   similarity of members to the centroid; mean pairwise cosine similarity
#   (all pairs up to 500 members, else 500 random pairs); top 5 source domains;
#   number of distinct domains; distribution of predicted categories;
#   first and last share date.
#
# Input : rawdata/brazil_url_shares_clusters_combined.rds, output/cluster_labels.csv
# Output: output/cluster_labels_enriched.rds  (with centroids)
#         output/cluster_labels.csv           (without centroids)
# =============================================================================

library(jsonlite)

d      <- readRDS("rawdata/brazil_url_shares_clusters_combined.rds")
labels <- read.csv("output/cluster_labels.csv", stringsAsFactors = FALSE)
enriched <- vector("list", nrow(labels))

for (yr in sort(unique(d$year))) {
  d_year   <- d[d$year == yr, ]
  emb_mat  <- do.call(rbind, lapply(d_year$embedding_vector, fromJSON))
  emb_norm <- emb_mat / sqrt(rowSums(emb_mat^2))
  gc(verbose = FALSE)

  for (cl in sort(unique(d_year$cluster))) {
    idx     <- which(d_year$cluster == cl)
    cl_data <- d_year[idx, ]
    cl_norm <- emb_norm[idx, , drop = FALSE]
    n       <- length(idx)

    centroid        <- colMeans(emb_mat[idx, , drop = FALSE])
    cos_to_centroid <- as.numeric(cl_norm %*% (centroid / sqrt(sum(centroid^2))))

    if (n <= 500) {
      sim_mat <- tcrossprod(cl_norm)
      pairwise_sims <- sim_mat[upper.tri(sim_mat)]
    } else {
      set.seed(yr * 1000 + cl)
      i1 <- sample(n, 500, replace = TRUE)
      i2 <- sample(n, 500, replace = TRUE)
      pairwise_sims <- rowSums(cl_norm[i1, ] * cl_norm[i2, ])
    }

    top_5 <- head(sort(table(cl_data$parent_domain), decreasing = TRUE), 5)
    cats  <- sort(table(cl_data$predicted_category), decreasing = TRUE)
    lab_idx <- which(labels$year == yr & labels$cluster == cl)

    enriched[[lab_idx]] <- list(
      year = yr, cluster = cl, label = labels$label[lab_idx],
      n_urls = n,
      n_unique_domains = length(unique(cl_data$parent_domain)),
      top_domains = paste(sprintf("%s (%d)", names(top_5), as.integer(top_5)), collapse = "; "),
      predicted_categories = paste(sprintf("%s (%d)", names(cats), as.integer(cats)), collapse = "; "),
      date_start = as.character(min(cl_data$first_post_time, na.rm = TRUE)),
      date_end   = as.character(max(cl_data$first_post_time, na.rm = TRUE)),
      mean_cos_sim_to_centroid   = round(mean(cos_to_centroid), 4),
      median_cos_sim_to_centroid = round(median(cos_to_centroid), 4),
      sd_cos_sim_to_centroid     = round(sd(cos_to_centroid), 4),
      mean_pairwise_cos_sim      = round(mean(pairwise_sims), 4),
      centroid = centroid
    )
  }
  rm(emb_mat, emb_norm, d_year); gc(verbose = FALSE)
}

flat <- do.call(rbind, lapply(enriched, function(x) {
  data.frame(x[setdiff(names(x), "centroid")], stringsAsFactors = FALSE)
}))
flat$centroid <- do.call(rbind, lapply(enriched, function(x) x$centroid))

saveRDS(flat, "output/cluster_labels_enriched.rds")
write.csv(flat[, setdiff(names(flat), "centroid")], "output/cluster_labels.csv", row.names = FALSE)
