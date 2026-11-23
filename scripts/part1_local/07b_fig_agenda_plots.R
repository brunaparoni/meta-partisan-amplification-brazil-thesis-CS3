# =============================================================================
# 07b_fig_agenda_plots.R  (input from 07a_cluster_agenda.R)
# -----------------------------------------------------------------------------
# Two complementary figures for CS3 section 5.3.3.1, both sized for an A4 page
# with 2 cm margins (usable width ~17 cm). Palette matches CS1/CS2.
#
#   Figure 1 (violin) — the SHAPE of the agenda: how the volume-weighted
#     distribution of cluster ideology widens and grows a right shoulder
#     between 2018 and 2022.
#   Figure 2 (scatter) — the GRAIN: where individual clusters sit and how much
#     they weigh. Facets are stacked vertically so each panel uses the full
#     page width.
# =============================================================================

library(ggplot2)
library(data.table)
library(scales)

agenda <- fread("output/cluster_agenda.csv")
agenda <- agenda[mpas_reliable == TRUE & !is.na(weighted_mpas)]
agenda[, year := factor(year, levels = c("2022", "2018"))]   # 2018 on top in violin

year_pal <- c("2018" = "#D860F0", "2022" = "#909000")   # CS1/CS2 palette

# =============================================================================
# FIGURE 1 — Violin: distribution of cluster ideology, weighted by volume
# -----------------------------------------------------------------------------
# Each cluster contributes to the density in proportion to its URL volume, so
# the shape reflects where the *agenda* sits, not just where clusters sit.
# =============================================================================
p_violin <- ggplot(agenda,
                   aes(x = weighted_mpas,
                       y = factor(year, levels = c("2022", "2018")),  # 2018 in alto
                       fill = factor(year))) +
  geom_vline(xintercept = 0,            linetype = "dashed", color = "grey60", linewidth = 0.35) +
  geom_vline(xintercept = c(-1/3, 1/3), linetype = "dotted", color = "grey75", linewidth = 0.35) +
  geom_violin(aes(weight = total_urls),          # volume-weighted density
              color = NA, alpha = 0.65, scale = "area", trim = FALSE) +
  geom_boxplot(aes(weight = total_urls),
               width = 0.12, outlier.size = 0.4, color = "grey25",
               fill = "white", alpha = 0.8) +
  scale_fill_manual(values = year_pal, guide = "none") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  labs(x = "Cluster MPAS (left \u2013 right)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y        = element_text(face = "bold"))

ggsave("figures/fig_agenda_violin.png", p_violin,
       width = 17, height = 7, units = "cm", dpi = 300, bg = "white")

# =============================================================================
# FIGURE 2 — Scatter: cluster ideology vs volume, one panel per year
# -----------------------------------------------------------------------------
# Facets STACKED (ncol = 1) so each panel spans the full page width.
# =============================================================================
agenda[, year := factor(year, levels = c("2018", "2022"))]

p_scatter <- ggplot(agenda, aes(x = weighted_mpas, y = total_urls)) +
  geom_vline(xintercept = 0,            linetype = "dashed", color = "grey60", linewidth = 0.35) +
  geom_vline(xintercept = c(-1/3, 1/3), linetype = "dotted", color = "grey75", linewidth = 0.35) +
  geom_point(aes(color = year, size = total_urls),
             alpha = 0.4, stroke = 0) +
  scale_color_manual(values = year_pal, guide = "none") +
  scale_size_continuous(name = "Cluster volume (URLs)",
                        range = c(0.6, 6),
                        breaks = c(250, 500, 1000, 1500)) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  facet_wrap(~ year, ncol = 1) +
  labs(x = "Cluster MPAS (left \u2013 right)",
       y = "Cluster volume (URLs, log scale)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.spacing     = unit(1, "lines"),
        strip.text        = element_text(face = "bold", size = 11),
        legend.position   = "right")

ggsave("figures/fig_agenda_p_scatter.png", p_scatter,
       width = 17, height = 7, units = "cm", dpi = 300, bg = "white")

print(p_violin)
print(p_scatter)
