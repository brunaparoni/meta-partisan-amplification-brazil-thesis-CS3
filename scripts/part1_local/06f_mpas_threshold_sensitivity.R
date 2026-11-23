# =============================================================================
# 06f_mpas_threshold_sensitivity.R
# -----------------------------------------------------------------------------
# Sensitivity of MPAS coverage to the minimum number of citations per domain
# (n >= 1, n >= 10 [chosen], n >= 500 [Santos 2023]): share of URLs and
# domains matched, overall and by year.
# =============================================================================

library(data.table)

run_mpas_join <- function(proportions_path, rds_path, csv_path) {
  
  rds <- as.data.table(readRDS(rds_path))
  rds[, full_domain := sub("^www[0-9]*[.\\-]", "", full_domain)]
  
  csv <- fread(csv_path)
  csv[, `:=`(
    year    = as.integer(sub("_.*", "", year_cluster)),
    cluster = as.integer(sub(".*_",  "", year_cluster))
  )]
  
  df <- merge(rds[, .(url_rid, clean_url, full_domain, parent_domain)],
              csv[, .(url_rid, year, cluster)],
              by = "url_rid")
  df <- df[!is.na(clean_url)]
  
  proportions <- as.data.table(readRDS(proportions_path))
  proportions[, domain := trimws(domain)]
  prop_cols <- unique(proportions[, .(domain, mpas, faixa, tipo)], by = "domain")
  
  df[prop_cols, on = .(full_domain = domain), `:=`(mpas = i.mpas, faixa = i.faixa, tipo = i.tipo)]
  unmatched_idx <- which(is.na(df$mpas))
  if (length(unmatched_idx) > 0) {
    fallback <- prop_cols[df[unmatched_idx], on = .(domain = parent_domain)]
    df[unmatched_idx, `:=`(mpas = fallback$mpas, faixa = fallback$faixa, tipo = fallback$tipo)]
  }
  
  list(
    n_domains_source = nrow(proportions),
    url_match_pct    = round(mean(!is.na(df$mpas)) * 100, 2),
    url_match_by_year = df[, .(pct_matched = round(mean(!is.na(mpas)) * 100, 2)), by = year],
    domain_match_pct = round(uniqueN(df$full_domain[!is.na(df$mpas)]) / uniqueN(df$full_domain) * 100, 2),
    df = df   # tieni il dataframe completo se ti serve ispezionarlo
  )
}

res_n1 <- run_mpas_join(
  "rawdata/santos_2023/proportions.rds",
  "data/political_embeddings_complete_join_method.rds",
  "rawdata/url_rid_year_cluster_url.csv"
)

res_n10 <- run_mpas_join(
  "rawdata/santos_2023/proportions_n10.rds",
  "data/political_embeddings_complete_join_method.rds",
  "rawdata/url_rid_year_cluster_url.csv"
)

res_n500 <- run_mpas_join(
  "rawdata/santos_2023/proportions_n500.rds",
  "data/political_embeddings_complete_join_method.rds",
  "rawdata/url_rid_year_cluster_url.csv"
)

comparison <- rbind(
  data.table(version = "n>=1 (unfiltered)", n_domains = res_n1$n_domains_source,
             url_match_pct = res_n1$url_match_pct, domain_match_pct = res_n1$domain_match_pct),
  data.table(version = "n>=10 (chosen)",     n_domains = res_n10$n_domains_source,
             url_match_pct = res_n10$url_match_pct, domain_match_pct = res_n10$domain_match_pct),
  data.table(version = "n>=500 (Santos 2023)", n_domains = res_n500$n_domains_source,
             url_match_pct = res_n500$url_match_pct, domain_match_pct = res_n500$domain_match_pct)
)
print(comparison)

#Per year
year_comparison <- rbind(
  cbind(version = "n>=1",   res_n1$url_match_by_year),
  cbind(version = "n>=10",  res_n10$url_match_by_year),
  cbind(version = "n>=500", res_n500$url_match_by_year)
)
print(year_comparison)