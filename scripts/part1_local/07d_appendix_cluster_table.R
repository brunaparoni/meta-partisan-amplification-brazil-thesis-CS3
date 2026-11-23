# =============================================================================
# 07d_appendix_cluster_table.R
# -----------------------------------------------------------------------------
# Appendix table: for each of the 400 clusters, label, number of URLs,
# share of the year, MPAS and coverage, and whether it enters the MPAS
# sample (353 clusters).
# =============================================================================

library(readr); library(dplyr); library(tidyr)

# 1. URLs per cluster
urls <- read_csv("rawdata/url_rid_year_cluster.csv",
                 col_types = cols(url_rid = col_character(),
                                  year_cluster = col_character()))

n_by_cluster <- urls %>%
  separate(year_cluster, into = c("year", "cluster"), sep = "_", convert = TRUE) %>%
  count(year, cluster, name = "n_urls") %>%
  group_by(year) %>%
  mutate(pct_year = round(100 * n_urls / sum(n_urls), 2)) %>%
  ungroup()

# 2. Labels and MPAS (all 400 clusters)
info <- read_csv("output/cluster_characteristics.csv", show_col_types = FALSE)
names(info)   # check column names

# 3. Appendix table
appendix <- n_by_cluster %>%
  left_join(info %>% select(year, cluster, label, topic, predicted_categories,
                            weighted_mpas, mpas_abs, pct_matched_urls),
            by = c("year", "cluster")) %>%
  mutate(in_mpas_sample = !is.na(weighted_mpas),
         across(c(weighted_mpas, mpas_abs), ~ round(.x, 3)),
         pct_matched_urls = round(pct_matched_urls, 1)) %>%
  arrange(year, desc(n_urls)) %>%
  select(year, cluster, label, topic, predicted_categories, n_urls, pct_year,
         weighted_mpas, mpas_abs, pct_matched_urls, in_mpas_sample)

# Checks
appendix %>% group_by(year) %>%
  summarise(n_clusters = n(), n_urls = sum(n_urls),
            with_mpas = sum(in_mpas_sample), no_label = sum(is.na(label)))

write_csv(appendix, "output/appendix_clusters_2018_2022.csv")