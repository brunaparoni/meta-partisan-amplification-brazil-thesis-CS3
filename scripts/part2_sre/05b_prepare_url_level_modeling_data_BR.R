# =============================================================================
# 05b_prepare_url_level_modeling_data_BR.R
# -----------------------------------------------------------------------------
# URL-level (source ideology) modeling data prep.
#
# Produces a model-ready URL-level dataset where the focal ideology variable
# is the URL's OWN domain MPAS (Santos 2023) rather than the cluster's
# weighted average. This re-frames the question from
#   "Does the ideological *environment* matter for sharing?" (cluster-level)
# to
#   "Does the ideology of the *URL itself* matter?" (URL-level)
#
# Pipeline position
#   05_prepare_modeling_data_BR.R  ->  data/urls_BR_modeling.csv   (cluster-level MPAS)
#                                  +
#   (external)                     ->  rawdata/URLs_info_mathed_only.csv (URL-level MPAS)
# > 05b_prepare_url_level_modeling_data_BR.R
#                                  ->  data/urls_BR_modeling_url_level.csv
#   06d_activation_url_BR.R         ->  results/rq1_url_level_BR.RData
#   06e_amplification_url_BR.R -> results/rq1_url_level_us_direction_BR.RData
#
# Decisions baked in
#   - Inner-join on url_rid (which is globally unique per the codebook).
#     election_year/year is checked as a sanity match but not part of the key.
#   - The URL-level MPAS file is already restricted to URLs whose parent_domain
#     matches the Santos (2023) reference list. No additional filtering here.
#   - URL-level mpas       -> renamed to weighted_mpas (signed, in [-1, +1])
#     URL-level mpas_abs   -> kept as mpas_abs        (intensity, in [0, 1])
#     Cluster-level versions are RETAINED but renamed:
#         weighted_mpas -> cluster_weighted_mpas
#         mpas_abs      -> cluster_mpas_abs
#     This preserves both for optional side-by-side comparison.
#   - Cluster-level controls (z_*) are kept unchanged. Per decision 5.8, the
#     BR analysis includes them in all specifications.
#   - high_mpas_coverage is kept as the cluster-level flag (pct_matched_urls
#     > 50). The URL-level analysis already filters URLs to matched ones, but
#     the cluster-level flag still tracks measurement-environment quality
#     (clusters where a high share of URLs are in the Santos list).
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

CLUSTER_LEVEL_FILE <- "./data/urls_BR_modeling.csv"
URL_LEVEL_FILE     <- "./rawdata/URLs_info_mathed_only.csv"
OUTPUT_FILE        <- "./data/urls_BR_modeling_url_level.csv"

cat(strrep("=", 65), "\n", sep = "")
cat(" Prepare URL-level RQ1 modeling dataset (BR)\n")
cat(strrep("=", 65), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 1. Load cluster-level modeling file (engagement + cluster controls)
# -----------------------------------------------------------------------------

cat("Loading cluster-level modeling file from\n  ", CLUSTER_LEVEL_FILE, "\n", sep = "")
cl <- read_csv(CLUSTER_LEVEL_FILE, show_col_types = FALSE)
cat("  Rows:", format(nrow(cl), big.mark = ","), "\n")
cat("  By year:\n"); print(table(cl$election_year))

# Rename cluster-level ideology to make room for URL-level versions
cl <- cl %>%
  rename(
    cluster_weighted_mpas = weighted_mpas,
    cluster_mpas_abs      = mpas_abs
  )

# -----------------------------------------------------------------------------
# 2. Load URL-level MPAS file
# -----------------------------------------------------------------------------

cat("\nLoading URL-level MPAS file from\n  ", URL_LEVEL_FILE, "\n", sep = "")
url_mpas <- read_csv(URL_LEVEL_FILE, show_col_types = FALSE)
cat("  Rows:", format(nrow(url_mpas), big.mark = ","), "\n")
cat("  By year:\n"); print(table(url_mpas$year))

# Prepare for merge: rename keys + ideology, drop columns that already exist
# in the cluster-level file (to avoid suffix collisions on join).
url_mpas_for_merge <- url_mpas %>%
  rename(
    election_year = year,
    weighted_mpas = mpas
  ) %>%
  # Keep only the URL-level columns we need; cluster_id, parent_domain,
  # full_domain, gini_domains, etc. are already in cl (or not needed here).
  select(url_rid, election_year,
         weighted_mpas, mpas_abs,
         url_parent_domain = parent_domain,
         url_full_domain   = full_domain,
         url_is_social_media = is_social_media,
         url_faixa = faixa,
         url_tipo  = tipo)

# -----------------------------------------------------------------------------
# 3. Inner join (drops URLs without URL-level MPAS match)
# -----------------------------------------------------------------------------

merged <- cl %>%
  inner_join(url_mpas_for_merge, by = c("url_rid", "election_year"))

n_lost_urls <- nrow(cl) - nrow(merged)
n_kept_pct  <- 100 * nrow(merged) / nrow(cl)

cat("\n--- URL-level merge results ---\n")
cat("URLs after URL-level MPAS merge: ", format(nrow(merged), big.mark = ","),
    "\n  (lost ", format(n_lost_urls, big.mark = ","),
    " URLs without a Santos-list domain match;\n   retained ",
    sprintf("%.1f", n_kept_pct), "%% of cluster-level sample)\n", sep = "")
cat("Year breakdown after URL-level merge:\n")
print(table(merged$election_year))

# -----------------------------------------------------------------------------
# 4. Descriptive checks on the URL-level ideology variable
# -----------------------------------------------------------------------------

cat("\n--- URL-level weighted_mpas distribution by year ---\n")
print(merged %>%
  group_by(election_year) %>%
  summarise(
    n_urls    = n(),
    median_w  = round(median(weighted_mpas), 3),
    iqr_w     = round(IQR(weighted_mpas), 3),
    p10       = round(quantile(weighted_mpas, 0.10), 3),
    p90       = round(quantile(weighted_mpas, 0.90), 3),
    pct_left  = round(100 * mean(weighted_mpas < 0), 1),
    pct_right = round(100 * mean(weighted_mpas > 0), 1),
    .groups   = "drop"
  ))

cat("\n--- URL-level mpas_abs distribution by year ---\n")
print(merged %>%
  group_by(election_year) %>%
  summarise(
    n_urls      = n(),
    median_abs  = round(median(mpas_abs), 3),
    iqr_abs     = round(IQR(mpas_abs), 3),
    mean_abs    = round(mean(mpas_abs), 3),
    .groups     = "drop"
  ))

cat("\n--- High-coverage subsample (pct_matched_urls > 50, cluster-level flag) ---\n")
print(merged %>%
  group_by(election_year, high_mpas_coverage) %>%
  summarise(n_urls     = n(),
            n_clusters = n_distinct(cluster_id),
            .groups    = "drop"))

# -----------------------------------------------------------------------------
# 5. Cross-tab: URL vs cluster ideology (sanity / interest)
# -----------------------------------------------------------------------------
# Correlation between URL-level and cluster-level mpas should be substantial
# (URLs in left-leaning clusters tend to come from left-leaning sources) but
# far from 1 (otherwise URL-level adds nothing). A correlation in [0.4, 0.8]
# is the expected range and tells us the URL-level analysis is informative.

cat("\n--- Correlation: URL-level vs cluster-level ideology ---\n")
cor_smry <- merged %>%
  group_by(election_year) %>%
  summarise(
    cor_signed_pearson  = round(cor(weighted_mpas, cluster_weighted_mpas,
                                    method = "pearson"), 3),
    cor_signed_spearman = round(cor(weighted_mpas, cluster_weighted_mpas,
                                    method = "spearman"), 3),
    cor_abs_pearson     = round(cor(mpas_abs, cluster_mpas_abs,
                                    method = "pearson"), 3),
    .groups = "drop"
  )
print(cor_smry)

# -----------------------------------------------------------------------------
# 6. Save
# -----------------------------------------------------------------------------

write_csv(merged, OUTPUT_FILE)
cat("\nSaved:", OUTPUT_FILE,
    " (", format(nrow(merged), big.mark = ","), " rows)\n", sep = "")
cat("Done.\n")
