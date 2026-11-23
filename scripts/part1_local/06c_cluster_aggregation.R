library(data.table)

# =============================================================================
# 06c_cluster_aggregation.R
# -----------------------------------------------------------------------------
# MPAS coverage and cluster-level aggregation (Santos 2023 scores, domains
# with at least 10 citations). Reports URL and domain coverage by year and
# ideology band, per-cluster weighted MPAS (with a reliability flag for
# clusters with fewer than 5 matched URLs), and the domain x cluster tables
# with and without social-media platforms used by 06d.
#
# Input : rawdata/brazil_url_shares_clusters_combined.rds (04),
#         rawdata/santos_2023/proportions_n10.rds, output/cluster_labels_enriched.rds (05)
# Output: output/urls_per_cluster.csv, output/domains_per_cluster.csv,
#         output/domains_per_cluster_no_social.csv,
#         output/domain_cluster_with_social.csv, output/domain_cluster_no_social.csv,
#         output/social_media_only_clusters.csv
# =============================================================================

# --- reliability threshold for cluster-level weighted_mpas ---
# Decided via sensitivity analysis: n >= 5 matched URLs per cluster
# retains 83.2% of clusters while reducing extreme (|MPAS|>0.7) clusters
# from 5.6% to 2.4%.
MIN_MATCHED_FOR_RELIABLE_MPAS <- 5

rds_comb <- as.data.table(readRDS("rawdata/brazil_url_shares_clusters_combined.rds"))
rds_comb[, full_domain := sub("^www[0-9]*[.\\-]", "", full_domain)]

# --- check whether the file is already filtered to political posts ---
cat("=== is_politics check ===\n")
print(table(rds_comb$is_politics, useNA = "ifany"))

# If not already filtered, restrict to political posts here (uncomment):
# rds_comb <- rds_comb[is_politics == TRUE]

df <- rds_comb[!is.na(clean_url), .(url_rid, year, cluster, clean_url, full_domain, parent_domain)]

cat("\n=== Source check against thesis-reported political corpus (222,558) ===\n")
cat("Total URLs in df:", nrow(df), "\n")
print(df[, .N, by = year])

# --- MPAS source (n >= 10 threshold) ---
proportions <- as.data.table(readRDS("rawdata/santos_2023/proportions_n10.rds"))
proportions[, domain := trimws(domain)]
prop_cols <- unique(proportions[, .(domain, mpas, faixa, tipo)], by = "domain")

# join ideology: exact match on full_domain, fallback to parent_domain
df[prop_cols, on = .(full_domain = domain), `:=`(mpas = i.mpas, faixa = i.faixa, tipo = i.tipo)]
unmatched_idx <- which(is.na(df$mpas))
if (length(unmatched_idx) > 0) {
  fallback <- prop_cols[df[unmatched_idx], on = .(domain = parent_domain)]
  df[unmatched_idx, `:=`(mpas = fallback$mpas, faixa = fallback$faixa, tipo = fallback$tipo)]
}

# --- URL coverage ---
cat("=== URL coverage ===\n")
cat("Total URLs:    ", nrow(df), "\n")
cat("Matched:       ", sum(!is.na(df$mpas)), sprintf("(%.1f%%)\n", mean(!is.na(df$mpas)) * 100))
cat("Unmatched:     ", sum(is.na(df$mpas)),  sprintf("(%.1f%%)\n", mean(is.na(df$mpas))  * 100))

cat("\n=== URL coverage by year ===\n")
url_year <- df[, .(
  total       = .N,
  matched     = sum(!is.na(mpas)),
  pct_matched = round(mean(!is.na(mpas)) * 100, 1)
), by = year]
print(url_year)

# --- Domain coverage ---
domains <- unique(df[, .(full_domain, parent_domain, mpas)])

cat("\n=== Domain coverage ===\n")
cat("Total unique domains: ", nrow(domains), "\n")
cat("Matched:              ", sum(!is.na(domains$mpas)), sprintf("(%.1f%%)\n", mean(!is.na(domains$mpas)) * 100))
cat("Unmatched:            ", sum(is.na(domains$mpas)),  sprintf("(%.1f%%)\n", mean(is.na(domains$mpas))  * 100))

cat("\n=== Domain coverage by year ===\n")
dom_year <- df[, .(
  total_domains   = uniqueN(full_domain),
  matched_domains = uniqueN(full_domain[!is.na(mpas)]),
  pct_matched     = round(uniqueN(full_domain[!is.na(mpas)]) / uniqueN(full_domain) * 100, 1)
), by = year]
print(dom_year)

# --- breakdown by faixa ---
cat("\n=== Matched URLs by ideology (faixa) ===\n")
faixa_breakdown <- df[!is.na(faixa), .(n_urls = .N), by = .(year, faixa)]
setorder(faixa_breakdown, year, -n_urls)
print(faixa_breakdown)

# --- URLs per cluster ---
cat("\n=== URLs per cluster ===\n")
cluster_urls <- df[, .(
  n_urls          = .N,
  n_matched       = sum(!is.na(mpas)),
  pct_matched     = round(mean(!is.na(mpas)) * 100, 1),
  weighted_mpas   = round(mean(mpas, na.rm = TRUE), 4)
), by = .(year, cluster)]

# reliability flag: NA out weighted_mpas when based on too few matched URLs
cluster_urls[, weighted_mpas_reliable := ifelse(
  n_matched >= MIN_MATCHED_FOR_RELIABLE_MPAS, weighted_mpas, NA_real_
)]

cat("Clusters with reliable weighted_mpas (n_matched >=", MIN_MATCHED_FOR_RELIABLE_MPAS, "):",
    sum(!is.na(cluster_urls$weighted_mpas_reliable)),
    sprintf("(%.1f%%)\n", mean(!is.na(cluster_urls$weighted_mpas_reliable)) * 100))

# faixa breakdown per cluster (wide format)
faixa_wide <- dcast(
  df[!is.na(faixa), .(n = .N), by = .(year, cluster, faixa)],
  year + cluster ~ faixa,
  value.var = "n",
  fill = 0
)

cluster_urls <- merge(cluster_urls, faixa_wide, by = c("year", "cluster"), all.x = TRUE)
setorder(cluster_urls, year, -n_urls)
print(cluster_urls)

fwrite(cluster_urls, "output/urls_per_cluster.csv")
cat("Saved: outputs/urls_per_cluster.csv\n\n")

# --- domain-level breakdown per cluster ---
cat("=== Domain-level ideology per cluster ===\n")

domain_cluster <- df[, .(n = .N), by = .(year, cluster, full_domain, parent_domain, mpas, faixa)]

cluster_domains <- domain_cluster[, .(
  n_domains       = uniqueN(full_domain),
  n_matched       = uniqueN(full_domain[!is.na(mpas)]),
  pct_matched     = round(uniqueN(full_domain[!is.na(mpas)]) / uniqueN(full_domain) * 100, 1),
  n_matched_urls  = sum(n[!is.na(mpas)]),
  weighted_mpas   = round(sum(mpas * n, na.rm = TRUE) / sum(n[!is.na(mpas)]), 4)
), by = .(year, cluster)]

# reliability flag: same logic, using matched URL volume (not domain count)
cluster_domains[, weighted_mpas_reliable := ifelse(
  n_matched_urls >= MIN_MATCHED_FOR_RELIABLE_MPAS, weighted_mpas, NA_real_
)]

# faixa breakdown by unique domains per cluster
faixa_domains_wide <- dcast(
  domain_cluster[!is.na(faixa), .(n_domains = uniqueN(full_domain)), by = .(year, cluster, faixa)],
  year + cluster ~ faixa,
  value.var = "n_domains",
  fill = 0
)

cluster_domains <- merge(cluster_domains, faixa_domains_wide, by = c("year", "cluster"), all.x = TRUE)
setorder(cluster_domains, year, -n_domains)
print(cluster_domains)

fwrite(cluster_domains, "output/domains_per_cluster.csv")
cat("Saved: outputs/domains_per_cluster.csv\n\n")

# --- same but without social media ---
social_media <- c(
  "youtube.com", "youtu.be", "instagram.com",
  "facebook.com", "fb.com", "fb.watch", "tiktok.com",
  "twitter.com", "x.com", "t.co", "linkedin.com",
  "pinterest.com", "reddit.com", "snapchat.com",
  "telegram.org", "t.me", "whatsapp.com",
  "vimeo.com", "twitch.tv", "threads.net"
)

domain_cluster_no_social <- domain_cluster[!parent_domain %in% social_media]

cluster_domains_no_social <- domain_cluster_no_social[, .(
  n_domains      = uniqueN(full_domain),
  n_matched      = uniqueN(full_domain[!is.na(mpas)]),
  pct_matched    = round(uniqueN(full_domain[!is.na(mpas)]) / uniqueN(full_domain) * 100, 1),
  n_matched_urls = sum(n[!is.na(mpas)]),
  weighted_mpas  = round(sum(mpas * n, na.rm = TRUE) / sum(n[!is.na(mpas)]), 4)
), by = .(year, cluster)]

cluster_domains_no_social[, weighted_mpas_reliable := ifelse(
  n_matched_urls >= MIN_MATCHED_FOR_RELIABLE_MPAS, weighted_mpas, NA_real_
)]

faixa_domains_no_social_wide <- dcast(
  domain_cluster_no_social[!is.na(faixa), .(n_domains = uniqueN(full_domain)), by = .(year, cluster, faixa)],
  year + cluster ~ faixa,
  value.var = "n_domains",
  fill = 0
)

cluster_domains_no_social <- merge(cluster_domains_no_social, faixa_domains_no_social_wide,
                                   by = c("year", "cluster"), all.x = TRUE)
setorder(cluster_domains_no_social, year, -n_domains)

fwrite(cluster_domains_no_social, "output/domains_per_cluster_no_social.csv")
cat("Saved: outputs/domains_per_cluster_no_social.csv\n\n")

# --- two full domain-cluster files: with and without social media ---
fwrite(domain_cluster, "output/domain_cluster_with_social.csv")
cat("Saved: outputs/domain_cluster_with_social.csv\n")

fwrite(domain_cluster[!parent_domain %in% social_media],
       "output/domain_cluster_no_social.csv")
cat("Saved: outputs/domain_cluster_no_social.csv\n\n")

# --- clusters that disappear after removing social media ---
social_only_clusters <- cluster_domains[
  !paste(year, cluster) %in% paste(cluster_domains_no_social$year, cluster_domains_no_social$cluster),
  .(year, cluster, n_domains)
]

labels <- as.data.table(readRDS("output/cluster_labels_enriched.rds"))
labels <- labels[, .(year, cluster, label, n_urls)]

social_only_clusters <- merge(social_only_clusters, labels, by = c("year", "cluster"), all.x = TRUE)

social_only_domains <- domain_cluster[
  paste(year, cluster) %in% paste(social_only_clusters$year, social_only_clusters$cluster),
  .(domains = paste(sort(unique(full_domain)), collapse = "; ")),
  by = .(year, cluster)
]
social_only_clusters <- merge(social_only_clusters, social_only_domains, by = c("year", "cluster"), all.x = TRUE)
setorder(social_only_clusters, year, -n_urls)

cat("Clusters with only social media domains:", nrow(social_only_clusters), "\n")
print(social_only_clusters)

fwrite(social_only_clusters, "output/social_media_only_clusters.csv")
cat("Saved: output/social_media_only_clusters.csv\n")