# =============================================================================
# 06a_extend_mpas.R
# -----------------------------------------------------------------------------
# Domain-level partisanship lookup. Starts from the Santos (2023) MPAS
# scores (proportions.csv) and adds the domains listed by party in Santos and
# Albuquerque (2024) that have no MPAS: each gets the median MPAS of the
# domains of the same party present in both sources.
#
# Input : rawdata/santos_2023/proportions.csv
#         rawdata/santos_2023/santos e albuquerque 2024_sources_by_party.csv
# Output: output/extended_mpas.csv  (domain, mpas, faixa, tipo, mpas_source)
# =============================================================================

library(data.table)

prop <- fread("rawdata/santos_2023/proportions.csv")
new  <- fread("rawdata/santos_2023/santos e albuquerque 2024_sources_by_party.csv")

prop_cols <- unique(prop[, .(domain, mpas, faixa, tipo)], by = "domain")
prop_cols[, mpas_source := "proportions"]

# party-median MPAS from domains present in both files
both <- merge(new, prop_cols[, .(domain, mpas)], by = "domain")
party_scores <- both[, .(
  party_mpas    = round(median(mpas), 4),
  party_mpas_sd = round(sd(mpas), 4),
  n_domains     = .N
), by = adjudication]
cat("Party-level MPAS scores (from overlap):\n")
print(party_scores[order(party_mpas)])

# new domains not in proportions.csv
new_only <- new[!domain %in% prop_cols$domain]
new_only <- merge(new_only, party_scores[, .(adjudication, party_mpas, party_mpas_sd)],
                  by = "adjudication")
new_only[, mpas        := party_mpas]
new_only[, mpas_source := paste0("party_median_2024 (", adjudication, ", sd=", party_mpas_sd, ")")]
new_only[, faixa       := fcase(
  mpas >=  0.75,  "pred_dir",
  mpas >=  0.1,   "medio",
  mpas <= -0.75,  "pred_esq",
  mpas <= -0.1,   "medio",
  default         = "medio"
)]
new_only[, tipo := "Mídia Alternativa"]

# combine
extended <- rbind(
  prop_cols[, .(domain, mpas, faixa, tipo, mpas_source)],
  new_only[, .(domain, mpas, faixa, tipo, mpas_source)]
)

cat("\n=== Extended MPAS lookup ===\n")
cat("Total domains:          ", nrow(extended), "\n")
cat("From proportions.csv:   ", sum(extended$mpas_source == "proportions"), "\n")
cat("From party median 2024: ", sum(extended$mpas_source != "proportions"), "\n")

fwrite(extended, "output/extended_mpas.csv")
cat("Saved: outputs/extended_mpas.csv\n")
