# =============================================================================
# 06d_build_cluster_dataset.R
# -----------------------------------------------------------------------------
# One row per election year x cluster with partisanship and structure:
#   weighted_mpas  URL-weighted mean MPAS of the cluster's non-social-media
#                  URLs (exact full-domain match, fallback on parent domain)
#   mpas_abs       |weighted_mpas|, partisan intensity
#   coverage       matched URLs / domains (n and %)
#   pct_social_media, gini_domains (source concentration), duration_days,
#   label, topic and cohesion from the labelling step (05)
#
# Input : output/extended_mpas.csv (06a), output/domain_cluster_with_social.csv (06c),
#         output/cluster_labels_enriched.rds (05)
# Output: output/cluster_characteristics.csv
#         -> uploaded to the SRE as clusters_mpas_clean.csv (Part 2)
# =============================================================================

library(data.table)

# --- sources ---
extended  <- fread("output/extended_mpas.csv")
prop_cols <- unique(extended[, .(domain, mpas)], by = "domain")

labels <- as.data.table(readRDS("output/cluster_labels_enriched.rds"))
labels <- labels[, .(year, cluster, label, predicted_categories,
                     date_start, date_end,
                     mean_cos_sim_to_centroid, median_cos_sim_to_centroid)]

dc_all <- fread("output/domain_cluster_with_social.csv")

social_media <- c(
  "youtube.com", "youtu.be", "instagram.com",
  "facebook.com", "fb.com", "fb.watch", "tiktok.com",
  "twitter.com", "x.com", "t.co", "linkedin.com",
  "pinterest.com", "reddit.com", "snapchat.com",
  "telegram.org", "t.me", "whatsapp.com",
  "vimeo.com", "twitch.tv", "threads.net"
)

dc_no_social <- dc_all[!parent_domain %in% social_media]
dc_social    <- dc_all[parent_domain  %in% social_media]

# --- MPAS join (no social) ---
dc_no_social[, mpas_prop := NA_real_]
dc_no_social[prop_cols, on = .(full_domain = domain), mpas_prop := i.mpas]
unmatched <- which(is.na(dc_no_social$mpas_prop))
if (length(unmatched) > 0) {
  fallback <- prop_cols[dc_no_social[unmatched], on = .(domain = parent_domain)]
  dc_no_social[unmatched, mpas_prop := fallback$mpas]
}

# --- ideology per cluster ---
ideology <- dc_no_social[, .(
  n_urls_no_social   = sum(n),
  n_domains          = uniqueN(full_domain),
  n_matched_urls     = sum(n[!is.na(mpas_prop)]),
  n_matched_domains  = uniqueN(full_domain[!is.na(mpas_prop)]),
  pct_matched_urls   = round(sum(n[!is.na(mpas_prop)]) / sum(n) * 100, 1),
  pct_matched_domains = round(uniqueN(full_domain[!is.na(mpas_prop)]) / uniqueN(full_domain) * 100, 1),
  weighted_mpas      = round(sum(mpas_prop * n, na.rm = TRUE) / sum(n[!is.na(mpas_prop)]), 4)
), by = .(year, cluster)]
ideology[, mpas_abs := round(abs(weighted_mpas), 4)]  # partisan intensity

# --- social media share per cluster ---
sm  <- dc_social[, .(sm_urls = sum(n)), by = .(year, cluster)]
tot <- dc_all[,    .(total_urls = sum(n)), by = .(year, cluster)]
sm_share <- merge(tot, sm, by = c("year", "cluster"), all.x = TRUE)
sm_share[is.na(sm_urls), sm_urls := 0]
sm_share[, pct_social_media := round(sm_urls / total_urls * 100, 1)]
sm_share[, sm_urls := NULL]

# --- Gini index of domain concentration (no social) ---
gini <- function(x) {
  x <- sort(x[x > 0])
  n <- length(x)
  if (n == 0) return(NA_real_)
  round((2 * sum(seq_len(n) * x)) / (n * sum(x)) - (n + 1) / n, 4)
}
gini_dt <- dc_no_social[, .(gini_domains = gini(tapply(n, full_domain, sum))), by = .(year, cluster)]

# --- cluster duration (days) ---
labels[, date_start := as.POSIXct(date_start)]
labels[, date_end   := as.POSIXct(date_end)]
labels[, duration_days := as.numeric(difftime(date_end, date_start, units = "days"))]

# --- assemble ---
df <- merge(ideology,    sm_share,  by = c("year", "cluster"), all.x = TRUE)
df <- merge(df,          gini_dt,   by = c("year", "cluster"), all.x = TRUE)
df <- merge(df,          labels,    by = c("year", "cluster"), all.x = TRUE)

# clean topic: strip count from predicted_categories e.g. "politics (812)" -> "politics"
df[, topic := trimws(sub("\\s*\\(.*\\)", "", predicted_categories))]

setcolorder(df, c("year", "cluster", "label", "topic",
                  "date_start", "date_end", "duration_days",
                  "total_urls", "n_urls_no_social",
                  "n_domains", "n_matched_urls", "n_matched_domains",
                  "pct_matched_urls", "pct_matched_domains",
                  "weighted_mpas", "mpas_abs",
                  "pct_social_media",
                  "gini_domains",
                  "mean_cos_sim_to_centroid", "median_cos_sim_to_centroid"))

setorder(df, year, cluster)

fwrite(df, "output/cluster_characteristics.csv")
cat("Saved: outputs/cluster_characteristics.csv\n")
cat("Rows:", nrow(df), "\n")
cat("Cols:", paste(names(df), collapse = ", "), "\n")
