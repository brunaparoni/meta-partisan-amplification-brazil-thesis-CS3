# =============================================================================
# 03_combine_aggregate_BR.R
# -----------------------------------------------------------------------------
# Combine BR engagement chunks, join with cluster assignments, and compute
# per-cluster means with DP-corrected standard errors.
#
# Pipeline position
#   01_load_split_clusters_BR.R   ->  data/urlrids_BR_clusters_split.csv
#   02_extract_engagement_BR.py   ->  chunks_BR/chunk_YYYY_NNN.tsv
# > 03_combine_aggregate_BR.R     ->  data/urls_BR_with_clusters.csv  (URL-level)
#                                     data/cluster_engagement_BR.csv  (long)
#                                     data/cluster_engagement_BR_wide.csv (wide)
#
# Methodology
# -----------
# Each per-URL engagement total is the SQL sum of N_i noisy breakdown cells
# (Evans & King 2022; Messing et al. 2020). Each cell carries mean-zero
# Gaussian noise with action-specific sigma:
#
#     metric                    sigma   (per cell)
#     ----------------------    -----
#     url_views                  2228
#     url_clicks                   40
#     url_likes                    22
#     url_shares                   14
#     url_share_without_clicks     10
#     url_comments / reactions     10
#
# For URL i with N_i cells summed:
#     E[X_i_obs] = X_i_true               (mean unbiased)
#     Var(X_i_obs | DP) = N_i * sigma^2   (variance grows with cells summed)
#
# For a cluster of K URLs, the empirical mean X-bar is unbiased for the
# cluster's true mean. Two standard errors are reported:
#
#   se_dp = sigma * sqrt(sum N_i) / K
#       Uncertainty in the cluster's TRUE mean given DP noise alone.
#       Treat as descriptive: "How well do we know THIS cluster's mean?"
#
#   se_inf = sd_emp / sqrt(K)
#       Standard textbook SE; valid for inference about a hypothetical
#       population the cluster URLs were drawn from. The DP contribution
#       to Var(X-bar) is approximately equal to the DP-induced bias in
#       s_emp^2, so the two cancel and the standard formula remains valid
#       (cf. Evans & King 2022, Sec. 3 for the lmdp() analog).
#
# A moment-corrected within-cluster SD is also reported, matching
# PrivacyUnbiased::descriptiveDP():
#     sd_dp_corr = sqrt(max(s_emp^2 - sigma^2 * mean(N_i), 0))
# Clipping at zero handles the case where DP noise dominates the empirical
# variance (true within-cluster variability is smaller than the noise floor).
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

CHUNKS_DIR    <- "./chunks_BR"
CLUSTERS_FILE <- "./data/urlrids_BR_clusters_split.csv"

OUTPUT_URLS   <- "./data/urls_BR_with_clusters.csv"
OUTPUT_LONG   <- "./data/cluster_engagement_BR.csv"
OUTPUT_WIDE   <- "./data/cluster_engagement_BR_wide.csv"

# Per-cell DP noise SDs from Evans & King (2022), Messing et al. (2020).
SIGMA <- c(
  url_views                = 2228,
  url_clicks               = 40,
  url_likes                = 22,
  url_shares               = 14,
  url_share_without_clicks = 10,
  url_comments             = 10,
  url_loves                = 10,
  url_hahas                = 10,
  url_wows                 = 10,
  url_sorrys               = 10,
  url_angers               = 10
)
METRICS <- names(SIGMA)

# Column types for the chunk TSVs (matches script 02 SQL output).
COL_TYPES <- cols(
  url_rid                  = col_character(),
  url_views                = col_double(),
  url_clicks               = col_double(),
  url_shares               = col_double(),
  url_share_without_clicks = col_double(),
  url_comments             = col_double(),
  url_likes                = col_double(),
  url_loves                = col_double(),
  url_hahas                = col_double(),
  url_wows                 = col_double(),
  url_sorrys               = col_double(),
  url_angers               = col_double(),
  url_total_rows           = col_integer(),
  months_active            = col_integer()
)

cat(strrep("=", 65), "\n", sep = "")
cat(" BR cluster engagement aggregation\n")
cat(strrep("=", 65), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 2. Read chunk TSVs
# -----------------------------------------------------------------------------
# Year is encoded in the filename (chunk_YYYY_NNN.tsv) and is the ground
# truth for which election window each row belongs to. A url_rid that
# appears in both 2018 and 2022 clusters will surface here as two rows,
# one per year, with separate engagement totals - which is exactly what
# we want.

chunk_files <- list.files(
  CHUNKS_DIR,
  pattern    = "^chunk_(2018|2022)_\\d+\\.tsv$",
  full.names = TRUE
)
cat("Chunk files found:", length(chunk_files), "\n")
if (length(chunk_files) == 0) {
  stop("No chunk files found in ", CHUNKS_DIR)
}

read_chunk_with_year <- function(path) {
  yr <- as.integer(str_extract(basename(path), "(2018|2022)"))
  read_tsv(path, col_types = COL_TYPES, progress = FALSE) %>%
    mutate(election_year = yr, .after = url_rid)
}

cat("Reading chunks...\n")
url_engagement <- map_dfr(chunk_files, read_chunk_with_year)
cat("  URL-level rows loaded:", format(nrow(url_engagement), big.mark = ","), "\n")
cat("  Unique url_rids:      ", format(n_distinct(url_engagement$url_rid), big.mark = ","), "\n")
cat("  Rows by year:\n")
print(table(url_engagement$election_year))

# -----------------------------------------------------------------------------
# 3. Join with cluster assignments
# -----------------------------------------------------------------------------

clusters <- read_csv(CLUSTERS_FILE, col_types = cols(
  url_rid       = col_character(),
  election_year = col_integer(),
  cluster_id    = col_integer(),
  year_cluster  = col_character()
))
cat("\nCluster CSV rows:", format(nrow(clusters), big.mark = ","), "\n")

merged <- url_engagement %>%
  inner_join(clusters, by = c("url_rid", "election_year"))

n_lost <- nrow(url_engagement) - nrow(merged)
cat("Merged URL x cluster rows:", format(nrow(merged), big.mark = ","),
    " (lost", format(n_lost, big.mark = ","), "in join)\n\n")

# -----------------------------------------------------------------------------
# 4. Save URL-level merged table (input for downstream regression scripts)
# -----------------------------------------------------------------------------
# This is the natural building block for any per-URL analysis (H1, RQ1-3):
# one row per (url_rid, election_year) with engagement totals + cluster info.

write_csv(merged, OUTPUT_URLS)
cat("Saved URL-level merged table: ", OUTPUT_URLS,
    " (", format(nrow(merged), big.mark = ","), " rows)\n", sep = "")

# -----------------------------------------------------------------------------
# 5. Per-cluster aggregates: long format, one row per (year_cluster x metric)
# -----------------------------------------------------------------------------

cat("\nComputing per-cluster means with DP-corrected SEs...\n")

aggregate_one_metric <- function(df, metric, sigma) {
  df %>%
    group_by(election_year, cluster_id, year_cluster) %>%
    summarise(
      n_urls       = n(),
      sum_cells    = sum(url_total_rows, na.rm = TRUE),
      mean_N_cells = sum_cells / n_urls,
      mean         = mean(.data[[metric]], na.rm = TRUE),
      sd_emp       = sd(.data[[metric]], na.rm = TRUE),
      n_negative   = sum(.data[[metric]] < 0, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    mutate(
      metric     = metric,
      sigma_cell = sigma,
      # SE for the cluster's true mean (DP noise only)
      se_dp      = sigma * sqrt(sum_cells) / n_urls,
      # SE for inference about a population the cluster was sampled from
      se_inf     = sd_emp / sqrt(n_urls),
      # Moment-corrected within-cluster SD (cf. PrivacyUnbiased::descriptiveDP)
      sd_dp_corr = sqrt(pmax(sd_emp^2 - sigma^2 * mean_N_cells, 0)),
      # Signal-to-noise: |mean| / se_dp; <2 means cluster mean is
      # statistically indistinguishable from zero given DP noise.
      snr_mean   = ifelse(se_dp > 0, mean / se_dp, NA_real_)
    ) %>%
    select(election_year, cluster_id, year_cluster,
           metric, sigma_cell,
           n_urls, sum_cells, mean_N_cells,
           mean, se_dp, se_inf, snr_mean,
           sd_emp, sd_dp_corr, n_negative)
}

per_cluster_long <- map_dfr(
  METRICS,
  ~ aggregate_one_metric(merged, .x, SIGMA[[.x]])
)

cat("  Per-cluster x metric rows:", format(nrow(per_cluster_long), big.mark = ","), "\n")

# -----------------------------------------------------------------------------
# 6. Wide format: one row per cluster, mean of each metric as columns
# -----------------------------------------------------------------------------

per_cluster_wide <- per_cluster_long %>%
  select(election_year, cluster_id, year_cluster, n_urls, sum_cells, metric, mean) %>%
  mutate(metric_clean = str_remove(metric, "^url_")) %>%
  select(-metric) %>%
  pivot_wider(
    names_from   = metric_clean,
    values_from  = mean,
    names_prefix = "mean_"
  )

# -----------------------------------------------------------------------------
# 7. Diagnostics
# -----------------------------------------------------------------------------

cat("\n--- Per-year cluster summary ---\n")
year_smry <- per_cluster_wide %>%
  group_by(election_year) %>%
  summarise(
    n_clusters  = n(),
    median_size = median(n_urls),
    min_size    = min(n_urls),
    max_size    = max(n_urls),
    .groups     = "drop"
  )
print(year_smry)

cat("\n--- DP noise quality check (per metric) ---\n")
cat("|mean| < 2 * SE_DP means the cluster mean is indistinguishable from 0\n")
cat("given DP noise.  High % indicates noise-dominated metric for cluster sizes here.\n\n")

quality <- per_cluster_long %>%
  group_by(metric) %>%
  summarise(
    n_cells          = n(),
    n_dp_dominated   = sum(abs(snr_mean) < 2, na.rm = TRUE),
    pct_dp_dominated = round(100 * n_dp_dominated / n_cells, 1),
    median_se_dp     = round(median(se_dp, na.rm = TRUE), 1),
    median_mean      = round(median(mean, na.rm = TRUE), 1),
    .groups          = "drop"
  ) %>%
  arrange(desc(pct_dp_dominated))
print(quality)

# Optional: peek at the top-10 largest clusters for views
cat("\n--- Top 10 clusters by mean views (sanity check) ---\n")
top10 <- per_cluster_wide %>%
  arrange(desc(mean_views)) %>%
  select(election_year, cluster_id, n_urls, sum_cells,
         mean_views, mean_shares, mean_clicks, mean_likes) %>%
  head(10)
print(top10)

# -----------------------------------------------------------------------------
# 8. Save
# -----------------------------------------------------------------------------

write_csv(per_cluster_long, OUTPUT_LONG)
write_csv(per_cluster_wide, OUTPUT_WIDE)

cat("\nSaved:\n")
cat("  ", OUTPUT_LONG, "  (", format(nrow(per_cluster_long), big.mark = ","),
    " rows, long format)\n", sep = "")
cat("  ", OUTPUT_WIDE, "  (", format(nrow(per_cluster_wide), big.mark = ","),
    " rows, wide format)\n", sep = "")

cat("\nDone.\n")

# -----------------------------------------------------------------------------
# Validation note
# -----------------------------------------------------------------------------
# To sanity-check that our manual moment correction matches PrivacyUnbiased,
# pick any cluster and metric and run:
#
#   library(PrivacyUnbiased)
#   one_cluster <- merged %>%
#     filter(year_cluster == "2018_125") %>%
#     mutate(metric_val = url_views) %>%
#     select(metric_val)
#   descriptiveDP(metric_val, one_cluster, noise = 2228^2)
#
# The "Before DP noise (estimated)" Mean should match our `mean` column,
# and the SD should match our `sd_dp_corr` for that (cluster, metric) row.
# -----------------------------------------------------------------------------
