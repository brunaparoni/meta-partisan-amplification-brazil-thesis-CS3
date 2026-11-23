# =============================================================================
# 07a_cluster_agenda.R
# -----------------------------------------------------------------------------
# Cluster-level characterization of the political agenda (CS3, descriptive part
# of SQ3). Produces one row per year-cluster with volume, ideological position,
# social-media share, and coverage, then prints the aggregate statistics used to
# describe how the agenda's ideological composition shifted between the 2018 and
# 2022 Brazilian elections.
#
# Sources (single source of truth for each quantity):
#   - Volumes and cluster membership:
#       brazil_url_shares_clusters_combined.rds
#       (same source as the modeling sample; 2022 = 45,930 after the clean_url
#        filter, matching the cluster-level sample described in the methods).
#   - Domain MPAS scores:
#       proportions.csv (Santos, 2023; 500 most-cited domains). This is the list
#       used in the models; an exploratory extended list is deliberately NOT used
#       here, so that the description and the models rest on the same measure.
#   - Cluster labels and dates:
#       cluster_labels_enriched.rds
#
# Output:
#   output/cluster_agenda.csv  (one row per year-cluster)
#
# Note on labels: the cluster numbering in cluster_labels_enriched.rds and in the
# combined file coincide (verified: median volume discrepancy of 9 URLs, due to
# the clean_url filter), so labels are merged directly on (year, cluster).
# =============================================================================

library(data.table)

# --- Paths (edit these two to reproduce) -------------------------------------
BASE       <- "."
SANTOS_DIR <- "rawdata/santos_2023"

combined_path <- "rawdata/brazil_url_shares_clusters_combined.rds"
labels_path   <- "output/cluster_labels_enriched.rds"
prop_path <- "rawdata/santos_2023/proportions.csv"
out_path      <- "output/cluster_agenda.csv"

# --- Parameters --------------------------------------------------------------
# A cluster's MPAS is treated as reliable only when it rests on at least this
# many URLs whose domain is in the Santos list.
MIN_MATCHED_FOR_RELIABLE <- 5

# Social-media parent domains: links to these carry no editorial line to
# classify and fall outside a domain-based partisanship measure by construction.
SOCIAL_MEDIA <- c(
  "youtube.com", "youtu.be", "instagram.com",
  "facebook.com", "fb.com", "fb.watch", "tiktok.com",
  "twitter.com", "x.com", "t.co", "linkedin.com",
  "pinterest.com", "reddit.com", "snapchat.com",
  "telegram.org", "t.me", "whatsapp.com",
  "vimeo.com", "twitch.tv", "threads.net"
)

# =============================================================================
# 1. Load the aligned source and apply the modeling-sample filter
# =============================================================================
rds <- as.data.table(readRDS(combined_path))
rds[, full_domain := sub("^www[0-9]*[.\\-]", "", full_domain)]
rds <- rds[!is.na(clean_url)]

cat("=== Source check (2022 should be 45,930) ===\n")
print(rds[, .(n_urls = .N), by = year])
cat("Total URLs:", nrow(rds), "\n\n")

# =============================================================================
# 2. Attach domain-level MPAS (Santos 2023, 500 domains)
#    Exact match on full_domain, then fall back to parent_domain.
# =============================================================================
prop <- fread(prop_path)
prop[, domain := trimws(domain)]
prop_cols <- unique(prop[, .(domain, mpas)], by = "domain")

rds[, mpas := NA_real_]
rds[prop_cols, on = .(full_domain = domain), mpas := i.mpas]

unmatched <- which(is.na(rds$mpas))
if (length(unmatched) > 0) {
  fallback <- prop_cols[rds[unmatched], on = .(domain = parent_domain)]
  rds[unmatched, mpas := fallback$mpas]
}

rds[, is_social := parent_domain %in% SOCIAL_MEDIA]

# =============================================================================
# 3. Aggregate to the cluster level
# =============================================================================
cluster <- rds[, .(
  total_urls       = .N,
  n_social         = sum(is_social),
  pct_social_media = round(mean(is_social) * 100, 1),
  n_matched_urls   = sum(!is.na(mpas)),
  pct_matched_urls = round(mean(!is.na(mpas)) * 100, 1),
  # URL-frequency-weighted mean over classified domains = directional MPAS
  weighted_mpas    = round(sum(mpas, na.rm = TRUE) / sum(!is.na(mpas)), 4)
), by = .(year, cluster)]

cluster[, mpas_abs      := round(abs(weighted_mpas), 4)]      # partisan intensity
cluster[, mpas_reliable := n_matched_urls >= MIN_MATCHED_FOR_RELIABLE]

# =============================================================================
# 4. Attach labels and dates (numbering coincides; see header note)
# =============================================================================
labels <- as.data.table(readRDS(labels_path))
labels <- labels[, .(year, cluster, label, top_domains, predicted_categories, date_start, date_end)]
labels[, topic := trimws(sub("\\s*\\(.*\\)", "", predicted_categories))]

cluster <- merge(cluster, labels, by = c("year", "cluster"), all.x = TRUE)
setorder(cluster, year, -total_urls)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
fwrite(cluster, out_path)
cat("Saved:", out_path, "(", nrow(cluster), "clusters )\n")

# =============================================================================
# 5. Aggregate statistics for the agenda description
# =============================================================================
wmean <- function(x, w) sum(x * w) / sum(w)
wsd   <- function(x, w) sqrt(sum(w * (x - wmean(x, w))^2) / sum(w))

for (y in c(2018, 2022)) {
  cl  <- cluster[year == y]
  rel <- cl[mpas_reliable == TRUE & !is.na(weighted_mpas)]   # reliable MPAS only
  
  cat(sprintf("\n================  %d  ================\n", y))
  cat(sprintf("Clusters: %d total | %d with reliable MPAS\n", nrow(cl), nrow(rel)))
  cat(sprintf("URLs: %d total\n", sum(cl$total_urls)))
  cat(sprintf("Directional MPAS   (vol-weighted): mean %.3f | sd %.3f\n",
              wmean(rel$weighted_mpas, rel$total_urls),
              wsd(rel$weighted_mpas, rel$total_urls)))
  cat(sprintf("Partisan intensity (vol-weighted): mean %.3f\n",
              wmean(rel$mpas_abs, rel$total_urls)))
  
  # Left / center / right = thirds of the [-1, 1] scale, by share of volume
  rel[, band := fcase(
    weighted_mpas < -1/3, "left",
    weighted_mpas >  1/3, "right",
    default               = "center"
  )]
  band_by_vol <- rel[, .(vol = sum(total_urls)), by = band]
  band_by_vol[, pct_vol := round(vol / sum(vol) * 100, 1)]
  cat("Ideological split (share of classified volume):\n")
  print(band_by_vol[order(factor(band, levels = c("left", "center", "right")))])
  
  social_only_vol <- sum(cl[n_matched_urls == 0]$total_urls)
  cat(sprintf("Volume in social-only clusters (no classifiable domain): %d (%.1f%%)\n",
              social_only_vol, social_only_vol / sum(cl$total_urls) * 100))
}

# =============================================================================
# 6. Top-10 clusters by volume, for illustration
# =============================================================================
for (y in c(2018, 2022)) {
  cat(sprintf("\n--- Top 10 clusters by volume, %d ---\n", y))
  print(head(cluster[year == y], 10)[, .(
    cluster, total_urls, weighted_mpas, mpas_reliable, pct_social_media, label
  )])
}

# =============================================================================
# 7. Reliable clusters by pole, for illustrative examples
#    Only clusters with a reliable MPAS enter here; labels and top domains
#    characterize the content, while the weighted MPAS (not the top domain)
#    determines the pole.
# =============================================================================
rel_all <- cluster[mpas_reliable == TRUE & !is.na(weighted_mpas)]
rel_all[, band := fcase(
  weighted_mpas < -1/3, "left",
  weighted_mpas >  1/3, "right",
  default               = "center"
)]

for (y in c(2018, 2022)) {
  for (b in c("left", "right")) {
    cat(sprintf("\n--- %d | %s pole, top 5 reliable clusters by volume ---\n", y, b))
    sub <- rel_all[year == y & band == b][order(-total_urls)]
    if (nrow(sub) == 0) { cat("  (none)\n"); next }
    print(head(sub, 5)[, .(cluster, total_urls, weighted_mpas,
                           n_matched_urls, pct_social_media, label, top_domains)])
  }
}
###################à
# 1. Filter reliable clusters for ALL years
rel_all <- cluster[mpas_reliable == TRUE & !is.na(weighted_mpas)]

# 2. Define 10 decile bins across [-1, 1]
breaks_10 <- seq(-1, 1, by = 0.2)
decile_labels <- paste0("D", 1:10)

# 3. Assign decile bins
rel_all[, decile := cut(weighted_mpas, 
                        breaks = breaks_10, 
                        include.lowest = TRUE, 
                        labels = decile_labels)]

# 4. Aggregate volume by year AND decile
decile_by_year <- rel_all[, .(vol = sum(total_urls)), by = .(year, decile)]

# 5. Calculate percentage share PER YEAR
decile_by_year[, pct_vol := round(vol / sum(vol) * 100, 1), by = year]

# 6. Reshape to show 2018 and 2022 side-by-side with change
decile_summary <- dcast(decile_by_year, decile ~ year, value.var = "pct_vol")

# Optional: Fill NAs with 0 if a decile has no volume in a year
decile_summary[is.na(`2018`), `2018` := 0]
decile_summary[is.na(`2022`), `2022` := 0]

# Calculate percentage point change (2022 minus 2018)
decile_summary[, change_pp := `2022` - `2018`]

print(decile_summary)


###################
# 5-band semantic classification of the agenda
# ---------------------------------------------------------------------------
# Bands follow the qualitative meaning of MPAS positions rather than equal
# width: the center retains the thirds cutoff (mainstream, shareable across
# poles), while each side is split into "predominant" and "exclusive"
# (hyperpartisan, near ±1) sources.
# ---------------------------------------------------------------------------

# 1. Filter reliable clusters for ALL years
rel_all <- cluster[mpas_reliable == TRUE & !is.na(weighted_mpas)]

# 2. Define 5 semantic bands across [-1, 1]
breaks_5 <- c(-1, -0.8, -1/3, 1/3, 0.8, 1)
band_labels <- c("Exclusively left",
                 "Predominantly left",
                 "Mainstream / center",
                 "Predominantly right",
                 "Exclusively right")

# 3. Assign bands
rel_all[, band5 := cut(weighted_mpas,
                       breaks = breaks_5,
                       include.lowest = TRUE,
                       labels = band_labels)]

# 4. Aggregate volume by year AND band
band_by_year <- rel_all[, .(vol = sum(total_urls)), by = .(year, band5)]

# 5. Percentage share PER YEAR
band_by_year[, pct_vol := round(vol / sum(vol) * 100, 1), by = year]

# 6. Reshape to show 2018 and 2022 side-by-side with change
band_summary <- dcast(band_by_year, band5 ~ year, value.var = "pct_vol")
band_summary[is.na(`2018`), `2018` := 0]
band_summary[is.na(`2022`), `2022` := 0]
band_summary[, change_pp := `2022` - `2018`]

# Keep the semantic order (not alphabetical)
setorder(band_summary, band5)
print(band_summary)