# =============================================================================
# 05_prepare_modeling_data_BR.R
# -----------------------------------------------------------------------------
# Merge URL-level engagement (script 03 output) with cluster-level MPAS
# (uploaded clean file) and prepare a single model-ready dataset for RQ1.
#
# Pipeline position
#   03_combine_aggregate_BR.R    ->  data/urls_BR_with_clusters.csv
#   (external: MPAS pipeline)    ->  rawdata/clusters_mpas_clean.csv
# > 05_prepare_modeling_data_BR.R  ->  data/urls_BR_modeling.csv
#   06_activation_cluster_intensity_BR.R         ->  results/rq1_models_BR.RData
#
# Decisions baked in (per RQ1 plan)
#   - No URL-level social-media filter; pct_social_media enters as a control.
#   - All 353 clusters with any MPAS coverage are kept; a high_mpas_coverage
#     flag (pct_matched_urls > 50) is added for the robustness sample.
#   - Cluster-level controls are z-scored POOLED across years so that
#     coefficients in the year-stratified models are directly comparable.
#   - mpas_abs is kept on its native 0-1 scale (already interpretable).
#   - URL-level engagement (url_views, url_shares, ...) stays on raw counts
#     so lmdp() can apply its DP bias correction directly.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

URLS_FILE   <- "./data/urls_BR_with_clusters.csv"
MPAS_FILE   <- "./rawdata/clusters_mpas_clean.csv"
OUTPUT_FILE <- "./data/urls_BR_modeling.csv"

cat(strrep("=", 65), "\n", sep = "")
cat(" Prepare RQ1 modeling dataset (BR)\n")
cat(strrep("=", 65), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 1. Load URL-level engagement
# -----------------------------------------------------------------------------

cat("Loading URL-level engagement from", URLS_FILE, "\n")
urls <- read_csv(URLS_FILE, show_col_types = FALSE)
cat("  URL-level rows:", format(nrow(urls), big.mark = ","), "\n")
cat("  By year:\n"); print(table(urls$election_year))

# -----------------------------------------------------------------------------
# 2. Load cluster MPAS and prepare for merge
# -----------------------------------------------------------------------------

cat("\nLoading cluster MPAS from", MPAS_FILE, "\n")
mpas <- read_csv(MPAS_FILE, show_col_types = FALSE)
cat("  Cluster MPAS rows:", nrow(mpas), "\n")
cat("  By year:\n"); print(table(mpas$year))

# Rename to match URL-level keys; keep only the columns we'll use downstream.
mpas_for_merge <- mpas %>%
  rename(election_year = year, cluster_id = cluster) %>%
  select(election_year, cluster_id,
         label, topic, predicted_categories,
         duration_days, total_urls, n_urls_no_social, n_domains,
         n_matched_urls, n_matched_domains,
         pct_matched_urls, pct_matched_domains,
         weighted_mpas, mpas_abs,
         pct_social_media, gini_domains,
         mean_cos_sim_to_centroid)

# -----------------------------------------------------------------------------
# 3. Inner join (drops URLs in clusters without MPAS)
# -----------------------------------------------------------------------------

merged <- urls %>%
  inner_join(mpas_for_merge, by = c("election_year", "cluster_id"))

n_lost_urls  <- nrow(urls) - nrow(merged)
clusters_in_urls   <- urls   %>% distinct(election_year, cluster_id)
clusters_in_merged <- merged %>% distinct(election_year, cluster_id)
n_lost_clust <- nrow(clusters_in_urls) - nrow(clusters_in_merged)

cat("\n--- Merge results ---\n")
cat("URLs after merge:", format(nrow(merged), big.mark = ","),
    " (lost ", format(n_lost_urls, big.mark = ","),
    " URLs in ", n_lost_clust, " clusters without MPAS)\n", sep = "")
cat("Year breakdown after merge:\n")
print(table(merged$election_year))

# -----------------------------------------------------------------------------
# 4. Add z-scored cluster controls (pooled across years) and robustness flag
# -----------------------------------------------------------------------------
# Why pooled standardization: coefficients in the per-year models share a
# common scale, so cross-year coefficient comparison is meaningful.

cat("\nStandardizing cluster-level controls (pooled z-scores)...\n")

merged <- merged %>%
  mutate(
    z_duration_days     = as.numeric(scale(duration_days)),
    z_total_urls        = as.numeric(scale(total_urls)),
    z_pct_social_media  = as.numeric(scale(pct_social_media)),
    z_gini_domains      = as.numeric(scale(gini_domains)),
    z_pct_matched_urls  = as.numeric(scale(pct_matched_urls)),
    high_mpas_coverage  = pct_matched_urls > 50,
    year_2022           = as.integer(election_year == 2022)  # numeric dummy for pooled model
  )

# -----------------------------------------------------------------------------
# 5. Descriptive checks
# -----------------------------------------------------------------------------

cat("\n--- mpas_abs distribution by year ---\n")
mpas_smry <- merged %>%
  group_by(election_year) %>%
  summarise(
    n_urls       = n(),
    n_clusters   = n_distinct(cluster_id),
    median_mpas  = round(median(mpas_abs), 3),
    iqr_mpas     = round(IQR(mpas_abs), 3),
    mean_mpas    = round(mean(mpas_abs), 3),
    .groups = "drop"
  )
print(mpas_smry)

cat("\n--- Engagement summary by year (medians; full distribution in script 04) ---\n")
eng_smry <- merged %>%
  group_by(election_year) %>%
  summarise(
    n_urls         = n(),
    median_views   = median(url_views),
    median_shares  = median(url_shares),
    pct_pos_views  = round(100 * mean(url_views  > 0), 1),
    pct_pos_shares = round(100 * mean(url_shares > 0), 1),
    .groups        = "drop"
  )
print(eng_smry)

cat("\n--- High MPAS coverage flag (pct_matched_urls > 50%) ---\n")
hc_smry <- merged %>%
  group_by(election_year, high_mpas_coverage) %>%
  summarise(n_urls     = n(),
            n_clusters = n_distinct(cluster_id),
            .groups    = "drop")
print(hc_smry)

# -----------------------------------------------------------------------------
# 6. Save
# -----------------------------------------------------------------------------

write_csv(merged, OUTPUT_FILE)
cat("\nSaved:", OUTPUT_FILE,
    " (", format(nrow(merged), big.mark = ","), " rows)\n", sep = "")
cat("Done.\n")
