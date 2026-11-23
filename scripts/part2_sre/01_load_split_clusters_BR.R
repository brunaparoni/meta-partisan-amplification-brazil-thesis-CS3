# =============================================================================
# Load Brazilian URL Cluster Data and Split year_cluster Column
# =============================================================================
#
# Project : URL Shares Dataset - Brazilian elections (2018, 2022)
# Step    : 01 - Load raw cluster assignments and split composite key
#
# Description:
#   Loads the CSV exported from Python containing url_rid and year_cluster
#   (e.g., "2018_125", "2022_22") and splits year_cluster into two clean
#   columns: election_year (integer, 2018 or 2022) and cluster_id (integer).
#
#   Clusters are nested within election year, so a given numeric cluster_id
#   only has meaning within its election_year. We therefore also keep the
#   original year_cluster label as a globally unique cluster identifier.
#
# Input:
#   ./rawdata/urlrids_2018_2022_clusters.csv
#     - url_rid       : unique URL identifier (string)
#     - year_cluster  : composite key "YYYY_<cluster>"
#
# Output:
#   ./data/urlrids_BR_clusters_split.csv
#     - url_rid       : unique URL identifier
#     - election_year : 2018 or 2022 (integer)
#     - cluster_id    : within-year cluster id (integer)
#     - year_cluster  : original composite key (kept for joins/uniqueness)
# =============================================================================

# ---- 1. Setup ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
})

# I/O paths
INPUT_FILE  <- "./rawdata/urlrids_2018_2022_clusters.csv"
OUTPUT_DIR  <- "./data"
OUTPUT_FILE <- file.path(OUTPUT_DIR, "urlrids_BR_clusters_split.csv")

# Make sure output directory exists
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat("Created output directory:", OUTPUT_DIR, "\n")
}

cat("=============================================================\n")
cat(" BR URL Clusters - load & split year_cluster\n")
cat("=============================================================\n\n")

# ---- 2. Load CSV ------------------------------------------------------------

cat("Reading:", INPUT_FILE, "\n")

clusters_raw <- read_csv(
  INPUT_FILE,
  col_types = cols(
    url_rid      = col_character(),
    year_cluster = col_character()
  )
)

cat("Rows loaded :", nrow(clusters_raw), "\n")
cat("Cols loaded :", paste(names(clusters_raw), collapse = ", "), "\n\n")

# Quick sanity check on the format of year_cluster
bad_format <- clusters_raw %>%
  filter(!grepl("^[0-9]{4}_[0-9]+$", year_cluster))

if (nrow(bad_format) > 0) {
  cat("WARNING:", nrow(bad_format),
      "rows do NOT match the expected 'YYYY_<int>' pattern.\n")
  cat("Showing up to 5 examples:\n")
  print(head(bad_format, 5))
  cat("\n")
} else {
  cat("All year_cluster values match the expected 'YYYY_<int>' pattern.\n\n")
}

# ---- 3. Split year_cluster into election_year + cluster_id ------------------

# Use tidyr::separate on the underscore. We keep the original column with
# `remove = FALSE` so it stays available as a globally unique cluster key.
clusters <- clusters_raw %>%
  separate(
    col    = year_cluster,
    into   = c("election_year", "cluster_id"),
    sep    = "_",
    remove = FALSE,        # keep original year_cluster column
    convert = FALSE        # we'll cast types ourselves below
  ) %>%
  mutate(
    election_year = as.integer(election_year),
    cluster_id    = as.integer(cluster_id)
  ) %>%
  # Restore a tidy column order
  select(url_rid, election_year, cluster_id, year_cluster)

# ---- 4. Validation & summary ------------------------------------------------

cat("--- Validation ---\n")

# Did anything fail to parse to integer?
n_bad_year    <- sum(is.na(clusters$election_year))
n_bad_cluster <- sum(is.na(clusters$cluster_id))
cat("Rows with NA election_year :", n_bad_year, "\n")
cat("Rows with NA cluster_id    :", n_bad_cluster, "\n")

# Election years actually present
years_present <- sort(unique(clusters$election_year))
cat("Election years present     :", paste(years_present, collapse = ", "), "\n")

# Flag anything outside the expected {2018, 2022}
unexpected_years <- setdiff(years_present, c(2018, 2022))
if (length(unexpected_years) > 0) {
  cat("WARNING: unexpected years detected:",
      paste(unexpected_years, collapse = ", "), "\n")
}

cat("\n--- Per-year summary ---\n")
year_summary <- clusters %>%
  group_by(election_year) %>%
  summarise(
    n_urls     = n(),
    n_clusters = n_distinct(cluster_id),
    min_clu    = min(cluster_id, na.rm = TRUE),
    max_clu    = max(cluster_id, na.rm = TRUE),
    .groups    = "drop"
  )
print(year_summary)

cat("\n--- Top 10 largest clusters (by n URLs) ---\n")
top_clusters <- clusters %>%
  count(election_year, cluster_id, name = "n_urls") %>%
  arrange(desc(n_urls)) %>%
  head(10)
print(top_clusters)

cat("\n--- First few rows of the split table ---\n")
print(head(clusters, 5))

# ---- 5. Save ----------------------------------------------------------------

write_csv(clusters, OUTPUT_FILE)
cat("\nSaved:", OUTPUT_FILE, "(", nrow(clusters), "rows )\n")
cat("Done.\n")
