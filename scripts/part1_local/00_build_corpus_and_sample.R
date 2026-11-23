# =============================================================================
# 00_build_corpus_and_sample.R
# -----------------------------------------------------------------------------
# Builds the text field used throughout the pipeline and draws a reproducible
# random sample for annotation.
#
# Input : ./rawdata/2018.csv, ./rawdata/2022.csv
#         Brazilian URLs from the public URL Shares release (one row per URL,
#         with share_title and share_main_blurb). Not included in this repository.
# Output: ./data/sample_data_60obs_<date>.csv  (30 URLs per election year)
#
# alltext = "<title>. <blurb>", cleaned of leading/trailing and repeated periods.
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

df_2018 <- read_csv("./rawdata/2018.csv", show_col_types = FALSE)
df_2022 <- read_csv("./rawdata/2022.csv", show_col_types = FALSE)

df_combined <- bind_rows(
  df_2018 %>% mutate(year = "2018"),
  df_2022 %>% mutate(year = "2022")
) %>%
  mutate(
    alltext = paste(coalesce(share_title, ""), coalesce(share_main_blurb, ""), sep = ". "),
    alltext = str_trim(alltext),
    alltext = gsub("^\\. |\\. $", "", alltext),   # leading / trailing periods
    alltext = gsub("\\.{2,}", ".", alltext),      # repeated periods
    alltext = str_trim(alltext)
  )

set.seed(1974)
df_sample <- df_combined %>%
  group_by(year) %>%
  slice_sample(n = 30) %>%
  ungroup()

dir.create("data", showWarnings = FALSE)
write_csv(df_sample, paste0("data/sample_data_60obs_", format(Sys.Date(), "%Y%m%d"), ".csv"))
