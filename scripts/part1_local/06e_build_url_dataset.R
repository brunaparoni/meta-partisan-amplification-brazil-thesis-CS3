# =============================================================================
# 06e_build_url_dataset.R
# -----------------------------------------------------------------------------
# URL-level partisanship: each URL gets the MPAS of its own domain (exact
# full-domain match, fallback on parent domain), its absolute value, the
# Santos category (faixa, tipo), a social-media flag and its cluster's Gini.
#
# Input : output/extended_mpas.csv (06a), output/cluster_characteristics.csv (06d),
#         rawdata/brazil_url_shares_clusters_combined.rds (04),
#         rawdata/url_rid_year_cluster_url.csv
# Output: output/url_mpas.csv  (all URLs; mpas is NA when the domain is unmatched)
#         -> uploaded to the SRE as url_mpas_matched_only_v2.csv (Part 2)
# =============================================================================

library(data.table)

# --- sources ---
extended  <- fread("output/extended_mpas.csv")
prop_cols <- unique(extended[, .(domain, mpas, faixa, tipo)], by = "domain")

clusters  <- fread("output/cluster_characteristics.csv")
gini_ref  <- unique(clusters[, .(year, cluster, gini_domains)])

rds <- as.data.table(readRDS("rawdata/brazil_url_shares_clusters_combined.rds"))
rds[, full_domain := sub("^www[0-9]*[.\\-]", "", full_domain)]

urls <- fread("rawdata/url_rid_year_cluster_url.csv")
urls[, year    := as.integer(sub("_.*", "", year_cluster))]
urls[, cluster := as.integer(sub(".*_",  "", year_cluster))]
urls[, year_cluster := NULL]

# --- merge url metadata ---
df <- merge(urls[, .(url_rid, year, cluster)],
            rds[, .(url_rid, clean_url, full_domain, parent_domain, first_post_time)],
            by = "url_rid", all.x = TRUE)

# --- join MPAS: exact on full_domain, fallback to parent_domain ---
df[, mpas  := NA_real_]
df[, faixa := NA_character_]
df[, tipo  := NA_character_]

df[prop_cols, on = .(full_domain = domain),
   `:=`(mpas = i.mpas, faixa = i.faixa, tipo = i.tipo)]

unmatched <- which(is.na(df$mpas))
if (length(unmatched) > 0) {
  fallback <- prop_cols[df[unmatched], on = .(domain = parent_domain)]
  df[unmatched, `:=`(mpas = fallback$mpas, faixa = fallback$faixa, tipo = fallback$tipo)]
}

# --- join cluster-level gini ---
df <- merge(df, gini_ref, by = c("year", "cluster"), all.x = TRUE)

# --- derived columns ---
df[, mpas_abs       := round(abs(mpas), 4)]
df[, is_social_media := parent_domain %in% c(
  "youtube.com", "youtu.be", "instagram.com",
  "facebook.com", "fb.com", "fb.watch", "tiktok.com",
  "twitter.com", "x.com", "t.co", "linkedin.com",
  "pinterest.com", "reddit.com", "snapchat.com",
  "telegram.org", "t.me", "whatsapp.com",
  "vimeo.com", "twitch.tv", "threads.net"
)]

# --- column order mirrors clusters_mpas_cor.csv at URL level ---
setcolorder(df, c("url_rid", "year", "cluster", "clean_url",
                  "full_domain", "parent_domain",
                  "first_post_time", "is_social_media",
                  "mpas", "mpas_abs", "faixa", "tipo",
                  "gini_domains"))
setorder(df, year, cluster, url_rid)

# --- summary ---
cat("=== URL-level dataset ===\n")
cat("Total URLs:          ", nrow(df), "\n")
cat("Matched (mpas):      ", sum(!is.na(df$mpas)),
    sprintf("(%.1f%%)\n", mean(!is.na(df$mpas)) * 100))
cat("Social media URLs:   ", sum(df$is_social_media),
    sprintf("(%.1f%%)\n", mean(df$is_social_media) * 100))
cat("Non-social matched:  ")
ns <- df[is_social_media == FALSE]
cat(sum(!is.na(ns$mpas)), sprintf("(%.1f%% of non-social)\n", mean(!is.na(ns$mpas)) * 100))
cat("\nBy year:\n")
print(df[, .(
  total       = .N,
  matched     = sum(!is.na(mpas)),
  pct_matched = round(mean(!is.na(mpas)) * 100, 1)
), by = year])

fwrite(df, "output/url_mpas.csv")
cat("\nSaved: outputs/url_mpas.csv\n")
