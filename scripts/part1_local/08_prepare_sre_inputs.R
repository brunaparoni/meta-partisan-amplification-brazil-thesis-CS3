# =============================================================================
# 08_prepare_sre_inputs.R
# -----------------------------------------------------------------------------
# Runs OUTSIDE the SRE, before upload. Cleans the two externally built MPAS
# files so that the SRE database can parse them (the SRE CSV reader fails on
# empty cells in numeric columns).
#
# Inputs (from 06d and 06e):
#   output/cluster_characteristics.csv  one row per year x cluster (400 rows)
#   output/url_mpas.csv                 one row per URL (222,486 rows)
#
# Outputs, in output/ (to upload with scripts/part2_sre/00_upload_inputs_SRE.py):
#   clusters_mpas_clean_353.csv   clusters with an MPAS score (353 rows)
#   url_mpas_matched_only_v3.csv  URLs whose domain is in the Santos (2023)
#                                 list (110,851 rows), columns used by 05b only
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

INPUT_DIR <- "output"

# --- 1. Cluster-level MPAS: keep clusters with a score ------------------------
m <- read_csv(file.path(INPUT_DIR, "cluster_characteristics.csv"), show_col_types = FALSE)
m353 <- m %>% filter(!is.na(weighted_mpas), !is.na(mpas_abs))

cat("Clusters in input:", nrow(m), "| with MPAS:", nrow(m353), "(expected 353)\n")
print(colSums(is.na(m353)))
write_csv(m353, file.path(INPUT_DIR, "clusters_mpas_clean_353.csv"), na = "")

# --- 2. URL-level MPAS: matched URLs, columns used downstream -----------------
u <- read_csv(file.path(INPUT_DIR, "url_mpas.csv"), show_col_types = FALSE)
u_m <- u %>%
  filter(!is.na(mpas), !is.na(mpas_abs)) %>%
  select(url_rid, year, mpas, mpas_abs, parent_domain, full_domain,
         is_social_media, faixa, tipo) %>%
  mutate(faixa = ifelse(is.na(faixa), "nd", as.character(faixa)))

cat("URLs in input:", nrow(u), "| matched:", nrow(u_m), "(expected 110,851)\n")
cat("Duplicated url_rid x year:", sum(duplicated(u_m[, c("url_rid", "year")])), "\n")
write_csv(u_m, file.path(INPUT_DIR, "url_mpas_matched_only_v3.csv"), na = "")
