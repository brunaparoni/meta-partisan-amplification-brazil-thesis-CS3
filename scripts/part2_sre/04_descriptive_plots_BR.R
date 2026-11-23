# =============================================================================
# 04_descriptive_plots_BR.R
# -----------------------------------------------------------------------------
# Six descriptive figures for sharing initial BR cluster results with co-authors.
# Designed to run as a single Jupyter cell, or split into 6 cells (one per
# section header below). Each figure renders inline at hi-res AND is saved
# as a 300-dpi PNG to ./figures/ for downstream use.
#
# Inputs (from script 03):
#   ./data/cluster_engagement_BR.csv       (long format)
#   ./data/cluster_engagement_BR_wide.csv  (wide format)
#
# Outputs:
#   inline plots in the notebook (hi-res via repr.plot.res = 200)
#   ./figures/fig0[1-6]_*.png              (300 dpi)
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(forcats)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

INPUT_LONG  <- "./data/cluster_engagement_BR.csv"
INPUT_WIDE  <- "./data/cluster_engagement_BR_wide.csv"
FIGURES_DIR <- "./figures"
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

# Hi-res inline plots in Jupyter R kernel
options(repr.plot.width = 12, repr.plot.height = 8, repr.plot.res = 200)

# Okabe-Ito palette (colorblind-safe)
COL_2018 <- "#0072B2"   # blue
COL_2022 <- "#E69F00"   # orange
year_pal <- c("2018" = COL_2018, "2022" = COL_2022)

# Shared theme
theme_report <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor      = element_blank(),
      strip.text            = element_text(face = "bold", size = base + 1),
      plot.title            = element_text(face = "bold", size = base + 4),
      plot.subtitle         = element_text(size = base + 1, color = "grey30"),
      plot.caption          = element_text(size = base - 3, color = "grey50",
                                           hjust = 0, lineheight = 1.1),
      legend.position       = "top",
      legend.title          = element_blank()
    )
}

save_fig <- function(plot, name, w = 12, h = 8) {
  ggsave(file.path(FIGURES_DIR, paste0(name, ".png")),
         plot, width = w, height = h, dpi = 300, bg = "white")
}

# -----------------------------------------------------------------------------
# Load
# -----------------------------------------------------------------------------

clusters_long <- read_csv(INPUT_LONG, show_col_types = FALSE) %>%
  mutate(election_year = factor(election_year))
clusters_wide <- read_csv(INPUT_WIDE, show_col_types = FALSE) %>%
  mutate(election_year = factor(election_year))

cat("Loaded long format:", nrow(clusters_long), "rows\n")
cat("Loaded wide format:", nrow(clusters_wide), "rows\n\n")


# =============================================================================
# FIG 1 - Coverage by election year
# =============================================================================

coverage <- clusters_wide %>%
  group_by(election_year) %>%
  summarise(n_urls = sum(n_urls), .groups = "drop")

fig1 <- ggplot(coverage, aes(x = election_year, y = n_urls, fill = election_year)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = format(n_urls, big.mark = ",")),
            vjust = -0.6, fontface = "bold", size = 5.5) +
  scale_fill_manual(values = year_pal) +
  scale_y_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Brazilian URL coverage in the pre-election window",
    subtitle = "URLs with engagement data, 1 Jul - 31 Oct of each election year",
    x = "Election year", y = "URLs (n)",
    caption  = paste(
      "Source: Facebook URL Shares Dataset (BR subset).",
      "99.7% recovery from 222,486 URLs in cluster CSV;\n637 URLs (all in 2018) had no activity in the window."
    )
  ) +
  theme_report() + theme(legend.position = "none")

print(fig1)
save_fig(fig1, "fig01_coverage", w = 9, h = 6)


# =============================================================================
# FIG 2 - Cluster size distribution
# =============================================================================

cluster_sizes <- clusters_wide %>% select(election_year, cluster_id, n_urls)

medians <- cluster_sizes %>%
  group_by(election_year) %>%
  summarise(median_size = median(n_urls), .groups = "drop")

fig2 <- ggplot(cluster_sizes, aes(x = election_year, y = n_urls, fill = election_year)) +
  geom_violin(alpha = 0.45, color = NA, width = 0.85, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.9, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.06, alpha = 0.25, size = 0.7, color = "grey30") +
  scale_fill_manual(values = year_pal) +
  scale_y_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0.02, 0.06))) +
  labs(
    title    = "Cluster sizes differ markedly between elections",
    subtitle = sprintf(
      "Median cluster size: 2018 = %s URLs, 2022 = %s URLs  (200 clusters per year)",
      format(medians$median_size[1], big.mark = ","),
      format(medians$median_size[2], big.mark = ",")
    ),
    x = "Election year", y = "Cluster size (n URLs)",
    caption = paste(
      "Each grey point is one cluster. Violin shows the full distribution shape;",
      "box shows median + interquartile range; whiskers extend to 1.5x IQR."
    )
  ) +
  theme_report() + theme(legend.position = "none")

print(fig2)
save_fig(fig2, "fig02_cluster_sizes", w = 10, h = 6)


# =============================================================================
# FIG 3 - Top 10 clusters by mean views and shares (2x2 grid via facet_grid)
# =============================================================================
# Native ggplot2 approach (no patchwork dependency): rank-on-y, cluster ID
# rendered as a per-bar text label so each facet can show its own top 10.

top_one <- function(year_chr, metric_col, metric_label) {
  clusters_wide %>%
    filter(election_year == year_chr) %>%
    arrange(desc(.data[[metric_col]])) %>%
    head(10) %>%
    mutate(metric_label = metric_label,
           value        = .data[[metric_col]],
           rank         = row_number())
}

top_data <- bind_rows(
  top_one("2018", "mean_views",  "Mean views per URL"),
  top_one("2022", "mean_views",  "Mean views per URL"),
  top_one("2018", "mean_shares", "Mean shares per URL"),
  top_one("2022", "mean_shares", "Mean shares per URL")
) %>%
  mutate(
    metric_label = factor(metric_label,
                          levels = c("Mean views per URL", "Mean shares per URL")),
    rank         = factor(rank, levels = 10:1)   # rank 1 at TOP of horizontal bars
  )

fig3 <- ggplot(top_data, aes(x = value, y = rank, fill = election_year)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("#%s  (n = %s)",
                                cluster_id, format(n_urls, big.mark = ","))),
            hjust = -0.06, size = 3.5, color = "black") +
  facet_grid(metric_label ~ election_year,
             scales = "free_x", switch = "y") +
  scale_x_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0, 0.36))) +
  scale_fill_manual(values = year_pal) +
  labs(
    title    = "Top 10 clusters by mean views and shares per URL",
    subtitle = "Brazilian URL Shares Dataset, pre-election windows (Jul - Oct)",
    x = NULL, y = NULL,
    caption  = paste(
      "Each bar is one cluster, ranked within its (metric x year) panel; rank 1 is at top.",
      "Labels show cluster ID and cluster size.\nNote: independent x-axes - 2018 magnitudes are roughly an order of magnitude larger than 2022."
    )
  ) +
  theme_report() +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    strip.background   = element_rect(fill = "grey92", color = NA),
    strip.text         = element_text(face = "bold"),
    strip.placement    = "outside"
  )

# Need extra height for 2x2 grid
options(repr.plot.width = 13, repr.plot.height = 11)
print(fig3)
options(repr.plot.width = 12, repr.plot.height = 8)
save_fig(fig3, "fig03_top_clusters", w = 13, h = 11)


# =============================================================================
# FIG 4 - Distribution of cluster means (5 main metrics, log scale)
# =============================================================================

main_metrics <- c("url_views", "url_shares", "url_clicks", "url_likes", "url_comments")

dist_data <- clusters_long %>%
  filter(metric %in% main_metrics) %>%
  mutate(
    metric_label = factor(
      str_to_title(str_remove(metric, "^url_")),
      levels = c("Views", "Shares", "Clicks", "Likes", "Comments")
    )
  )

n_dropped <- dist_data %>%
  group_by(metric_label) %>%
  summarise(n_neg = sum(mean <= 0), .groups = "drop")

dist_data_pos <- dist_data %>% filter(mean > 0)

fig4 <- ggplot(dist_data_pos, aes(x = election_year, y = mean, fill = election_year)) +
  geom_violin(alpha = 0.4, color = NA, width = 0.9, trim = FALSE) +
  geom_boxplot(width = 0.18, alpha = 0.9, outlier.alpha = 0.4,
               outlier.size = 0.7, color = "black") +
  facet_wrap(~ metric_label, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = year_pal) +
  scale_y_log10(labels = label_comma()) +
  labs(
    title    = "Distribution of cluster-mean engagement",
    subtitle = "200 clusters per election year, log scale on y",
    x = "Election year", y = "Cluster mean per URL  (log scale)",
    caption  = sprintf(
      "Each box represents the distribution of cluster-means within a year. Violin shows full shape. %s clusters with non-positive means filtered for log scale.",
      sum(n_dropped$n_neg)
    )
  ) +
  theme_report() +
  theme(legend.position = "none",
        axis.text.x     = element_text(face = "bold"))

options(repr.plot.width = 14, repr.plot.height = 6)
print(fig4)
options(repr.plot.width = 12, repr.plot.height = 8)
save_fig(fig4, "fig04_distribution_cluster_means", w = 14, h = 6)


# =============================================================================
# FIG 5 - DP quality at the cluster level
# =============================================================================

dp_quality <- clusters_long %>%
  group_by(election_year, metric) %>%
  summarise(
    n_clusters = n(),
    n_dp_dom   = sum(abs(snr_mean) < 2, na.rm = TRUE),
    pct_dp_dom = 100 * n_dp_dom / n_clusters,
    .groups    = "drop"
  ) %>%
  mutate(
    metric_label = str_replace_all(str_remove(metric, "^url_"), "_", " ")
  )

# Order metrics by max DP-dominated % across years (worst on top)
metric_order <- dp_quality %>%
  group_by(metric_label) %>%
  summarise(max_pct = max(pct_dp_dom), .groups = "drop") %>%
  arrange(max_pct) %>%
  pull(metric_label)

dp_quality <- dp_quality %>%
  mutate(metric_label = factor(metric_label, levels = metric_order))

fig5 <- ggplot(dp_quality,
               aes(x = pct_dp_dom, y = metric_label, fill = election_year)) +
  geom_col(position = position_dodge(0.7), width = 0.62) +
  geom_text(aes(label = paste0(formatC(pct_dp_dom, format = "f", digits = 1), "%")),
            position = position_dodge(0.7), hjust = -0.15, size = 3.4) +
  scale_fill_manual(values = year_pal) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Some reaction metrics are too rare to estimate at the cluster level",
    subtitle = "Percent of clusters where the empirical mean is statistically indistinguishable from zero",
    x = "% of clusters DP-dominated  (|mean| < 2 x SE_DP)", y = NULL,
    caption  = paste(
      "DP-dominated cluster: empirical mean within 2 SE of zero, given DP noise on the underlying counts.",
      "Note: this caveat applies to cluster-level descriptive statistics only.\nURL-level regression with PrivacyUnbiased::lmdp() is unaffected."
    )
  ) +
  theme_report() +
  theme(panel.grid.major.y = element_blank())

options(repr.plot.width = 12, repr.plot.height = 7)
print(fig5)
options(repr.plot.width = 12, repr.plot.height = 8)
save_fig(fig5, "fig05_dp_quality", w = 12, h = 7)


# =============================================================================
# FIG 6 - Cluster-level views vs shares (sneak peek)
# =============================================================================

vs_data <- clusters_wide %>%
  filter(mean_views > 0, mean_shares > 0)

n_excluded <- nrow(clusters_wide) - nrow(vs_data)

fig6 <- ggplot(vs_data, aes(x = mean_shares, y = mean_views, color = election_year)) +
  geom_point(alpha = 0.55, size = 1.9) +
  geom_smooth(aes(fill = election_year),
              method = "loess", se = TRUE, alpha = 0.18, span = 0.9) +
  scale_color_manual(values = year_pal) +
  scale_fill_manual(values  = year_pal) +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10(labels = label_comma()) +
  labs(
    title    = "Cluster-level views vs shares: a positive log-log relationship",
    subtitle = "Each point is one topic cluster; smoother is LOESS (span = 0.9)",
    x = "Mean shares per URL  (log scale)",
    y = "Mean views per URL  (log scale)",
    color = NULL, fill = NULL,
    caption  = sprintf(paste0(
      "n = %s clusters (%s excluded with non-positive means).\n",
      "Descriptive sneak peek only - formal modeling at the URL level should use lmdp() to handle DP measurement error."),
      nrow(vs_data), n_excluded
    )
  ) +
  theme_report()

options(repr.plot.width = 11, repr.plot.height = 7)
print(fig6)
options(repr.plot.width = 12, repr.plot.height = 8)
save_fig(fig6, "fig06_views_vs_shares", w = 11, h = 7)


# =============================================================================
# Wrap up
# =============================================================================

cat("\nAll six figures generated and saved to:", FIGURES_DIR, "\n")
cat("Files:\n")
cat("  fig01_coverage.png\n")
cat("  fig02_cluster_sizes.png\n")
cat("  fig03_top_clusters.png\n")
cat("  fig04_distribution_cluster_means.png\n")
cat("  fig05_dp_quality.png\n")
cat("  fig06_views_vs_shares.png\n")
