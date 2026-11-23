# =============================================================================
# 07c_fig_mpas_bands_shift.R
# -----------------------------------------------------------------------------
# Figure: share of classified volume in five MPAS bands, 2018 vs 2022, and
# the change in percentage points. Values are the band shares printed by
# 07a_cluster_agenda.R (section 5-band classification).
# =============================================================================

# =============================================================================
# Figure X.2 — Shift in source alignment across MPAS bands (2018 vs 2022)
# =============================================================================
library(ggplot2)
library(data.table)
library(patchwork)

# =============================================================================
# 1. Prepare Summary Data — 5 semantic bands
# =============================================================================
band_labels <- c(
  "Exclusively left\n[-1.0, -0.8]",
  "Predominantly left\n(-0.8, -0.33]",
  "Mainstream / center\n(-0.33, +0.33]",
  "Predominantly right\n(+0.33, +0.8]",
  "Exclusively right\n(+0.8, +1.0]"
)

df_summary <- data.frame(
  band   = factor(paste0("B", 1:5), levels = paste0("B", 1:5)),
  label  = factor(band_labels, levels = band_labels),
  y2018  = c(2.2, 20.8, 74.2,  2.8, 0.0),
  y2022  = c(0.3, 23.1, 59.2, 16.9, 0.5),
  change = c(-1.9,  2.3, -15.0, 14.1, 0.5)
)

df_long <- reshape2::melt(
  df_summary,
  id.vars = c("band", "label"),
  measure.vars = c("y2018", "y2022"),
  variable.name = "year",
  value.name = "pct_vol"
)
levels(df_long$year) <- c("2018", "2022")

# =============================================================================
# 2. Main Grouped Bar Chart (2018 vs 2022 Volume Share)
# =============================================================================
p1 <- ggplot(df_long, aes(x = label, y = pct_vol, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c("2018" = "#d870e6", "2022" = "#a3b134")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
  labs(
    title = "Figure X.2: Shift in Source Alignment Distribution Across MPAS Bands (2018 vs. 2022)",
    x = NULL,
    y = "Proportion of Classified Volume (%)",
    fill = "Election Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, margin = margin(b = 10)),
    axis.text.x = element_text(size = 9),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =============================================================================
# 3. Percentage Point Change Chart (Net Shift)
# =============================================================================
df_summary$change_type <- ifelse(df_summary$change >= 0, "Increase", "Decrease")

p2 <- ggplot(df_summary, aes(x = label, y = change, fill = change_type)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+.1f pp", change),
        vjust = ifelse(change >= 0, -0.4, 1.3)),
    size = 3, fontface = "bold"
  ) +
  scale_fill_manual(values = c("Increase" = "#7a8a21", "Decrease" = "#b340c2")) +
  scale_y_continuous(limits = c(-18, 18), labels = function(x) paste0(x, " pp")) +
  labs(
    x = "Source Alignment Band (MPAS Range)",
    y = "Change (pp)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =============================================================================
# 4. Combine and Display Panels
# =============================================================================
combined_plot <- p1 / p2 + plot_layout(heights = c(2, 1))
print(combined_plot)

ggsave(
  filename = "figure_2_mpas_bands_shift.png",
  plot = combined_plot,
  width = 10,
  height = 7,
  dpi = 300
)