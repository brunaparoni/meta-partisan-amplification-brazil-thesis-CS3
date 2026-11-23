# =============================================================================
# 07_all_figures_BR.R
# -----------------------------------------------------------------------------
# All RQ1 coefficient figures, built DIRECTLY from the model results saved in
# the SRE (no hand-typed numbers). Same style as Fig A-C (purple/gold, no
# titles, filled vs open markers).
#
#   ACTIVATION (shares ~ views), cluster level
#     Fig A  - slope on url_views (centrist reference, signed model)   <- 06b
#     Fig B  - intensity: level (mpas_abs) + interaction               <- 06
#     Fig C  - direction: interaction (url_views x weighted_mpas)      <- 06b
#     Fig C2 - direction: level + interaction (optional, new)          <- 06b
#   AMPLIFICATION (views ~ shares)
#     Fig D  - URL level, M1 vs M2, intensity + direction              <- 06e
#     Fig D1 - URL level, intensity only                               <- 06e
#     Fig E  - cluster level, M2, primary vs high-coverage             <- 06c
#     Fig E_M1 - cluster level, M1 (appendix)                          <- 06c
#
# Run in an R notebook, from the project folder (the one with ./results).
# A figure whose .RData does not exist yet is skipped with a message.
# Output: inline plots + 300-dpi PNGs in ./figures, and one CSV with every
# number plotted (./figures/all_figure_estimates.csv) to export with them.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tibble)
  library(stringr)
})

RESULTS_DIR <- "./results"
FIGURES_DIR <- "./figures"
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

options(repr.plot.width = 11, repr.plot.height = 5, repr.plot.res = 200)

FILES <- c(
  act_int = "rq1_models_BR.RData",                   # 06
  act_sgn = "rq1_signed_models_BR.RData",            # 06b
  amp_cl  = "rq1_us_direction_BR.RData",             # 06c
  amp_url = "rq1_url_level_us_direction_BR.RData"    # 06e
)

# --- Palette and theme (same as Fig A-C) --------------------------------------
year_pal <- c("2018" = "#9C60F0", "2022" = "#F0D860")

theme_report <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text         = element_text(face = "bold", size = base + 1),
      plot.caption       = element_text(size = base - 3, color = "grey50",
                                        hjust = 0, lineheight = 1.1),
      legend.position    = "top",
      legend.box         = "vertical",
      legend.spacing.y   = unit(0, "pt")
    )
}

# --- Helpers -----------------------------------------------------------------
load_focal <- function(key) {
  f <- file.path(RESULTS_DIR, FILES[[key]])
  if (!file.exists(f)) {
    message("SKIP: ", f, " not found (run the corresponding 06 script first).")
    return(NULL)
  }
  e <- new.env(); load(f, envir = e)
  tibble::as_tibble(e$results$focal_table)
}

p_label <- function(p) ifelse(p < 0.001, "p < 0.001", sprintf("p = %.2g", p))

# d needs: panel (factor), row (factor), year, shape (factor), Estimate, SE, p
make_forest <- function(d, shape_values, shape_name, caption, show_p = FALSE) {
  d <- d %>% mutate(lower = Estimate - 1.96 * SE, upper = Estimate + 1.96 * SE,
                    p_lab = p_label(p))
  g <- ggplot(d, aes(x = Estimate, y = row, color = year, shape = shape)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, linewidth = 0.7) +
    geom_point(size = 3.2, fill = "white", stroke = 1.1)
  if (show_p) {
    g <- g + geom_text(aes(label = p_lab), color = "grey35", size = 3.3,
                       vjust = -1.3, show.legend = FALSE)
  }
  g +
    facet_wrap(~ panel, scales = "free_x", nrow = 1) +
    scale_x_continuous(labels = label_comma(accuracy = NULL),
                       expand = expansion(mult = 0.15)) +
    scale_color_manual(values = year_pal, name = "Election year") +
    scale_shape_manual(values = shape_values, name = shape_name) +
    guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
    labs(x = "Estimate", y = NULL, caption = caption) +
    theme_report()
}

save_show <- function(g, name, w, h) {
  ggsave(file.path(FIGURES_DIR, paste0(name, ".png")), g,
         width = w, height = h, dpi = 300, bg = "white")
  options(repr.plot.width = w, repr.plot.height = h)
  print(g)
  cat("Saved", file.path(FIGURES_DIR, paste0(name, ".png")), "\n")
}

all_estimates <- list()

# =============================================================================
# ACTIVATION (06 / 06b): model labels "2018 primary", "2022 high-coverage", ...
# =============================================================================
prep_activation <- function(tbl) {
  tbl %>%
    mutate(
      year   = str_sub(model, 1, 4),
      sample = if_else(str_detect(model, "primary"), "Primary", "High-coverage"),
      row    = factor(paste(year, sample),
                      levels = c("2022 High-coverage", "2022 Primary",
                                 "2018 High-coverage", "2018 Primary")),
      shape  = factor(sample, levels = c("Primary", "High-coverage"))
    )
}
shape_sample <- c("Primary" = 16, "High-coverage" = 21)
cap_act <- "95% CIs (+/- 1.96 x bootstrap SE). Dashed line at zero."

act_int <- load_focal("act_int")
act_sgn <- load_focal("act_sgn")

# --- Fig A: slope on url_views (signed model = centrist reference) -----------
if (!is.null(act_sgn)) {
  dA <- prep_activation(act_sgn) %>% filter(term == "url_views") %>%
    mutate(panel = factor("Slope (url_views)"))
  all_estimates$A <- dA %>% mutate(figure = "A")
  save_show(make_forest(dA, shape_sample, "Sample",
                        paste(cap_act, "Signed model, evaluated at weighted_mpas = 0.")),
            "Fig_A_Slope", 7, 4.8)
}

# --- Fig B: intensity level + interaction ------------------------------------
if (!is.null(act_int)) {
  dB <- prep_activation(act_int) %>%
    filter(term %in% c("mpas_abs", "url_views:mpas_abs")) %>%
    mutate(panel = factor(term, levels = c("mpas_abs", "url_views:mpas_abs"),
                          labels = c("Partisan-intensity level (mpas_abs)",
                                     "Interaction (url_views x mpas_abs)")))
  all_estimates$B <- dB %>% mutate(figure = "B")
  save_show(make_forest(dB, shape_sample, "Sample", cap_act),
            "Fig_B_Intensity", 10.5, 4.8)
}

# --- Fig C: direction interaction; Fig C2: level + interaction ---------------
if (!is.null(act_sgn)) {
  dC <- prep_activation(act_sgn) %>%
    filter(term %in% c("weighted_mpas", "url_views:weighted_mpas")) %>%
    mutate(panel = factor(term, levels = c("weighted_mpas", "url_views:weighted_mpas"),
                          labels = c("Ideological-direction level (weighted_mpas)",
                                     "Interaction (url_views x weighted_mpas)")))
  all_estimates$C <- dC %>% mutate(figure = "C/C2")
  save_show(make_forest(dC %>% filter(term == "url_views:weighted_mpas"),
                        shape_sample, "Sample", cap_act),
            "Fig_C_Signed", 7, 4.8)
  save_show(make_forest(dC, shape_sample, "Sample", cap_act),
            "Fig_C2_Signed_level_interaction", 10.5, 4.8)
}

# =============================================================================
# AMPLIFICATION (06c / 06e): model labels "M2_intensity / primary_2018", ...
# =============================================================================
prep_amplification <- function(tbl) {
  tbl %>%
    filter(term %in% c("mpas_abs", "weighted_mpas")) %>%
    mutate(
      spec   = str_extract(model, "^M[12]"),
      year   = str_extract(model, "\\d{4}$"),
      sample = if_else(str_detect(model, "high_coverage"), "High-coverage", "Primary"),
      panel  = factor(term, levels = c("mpas_abs", "weighted_mpas"),
                      labels = c("Partisan-intensity level (mpas_abs)",
                                 "Ideological-direction level (weighted_mpas)"))
    )
}

amp_url <- load_focal("amp_url")
amp_cl  <- load_focal("amp_cl")

# --- Fig D / D1: URL level, M1 vs M2 -----------------------------------------
if (!is.null(amp_url)) {
  spec_lab <- c("M1" = "M1 (no clicks)", "M2" = "M2 (+ clicks)")
  dD <- prep_amplification(amp_url) %>%
    mutate(row   = factor(paste(year, spec_lab[spec]),
                          levels = c("2022 M1 (no clicks)", "2022 M2 (+ clicks)",
                                     "2018 M1 (no clicks)", "2018 M2 (+ clicks)")),
           shape = factor(spec_lab[spec], levels = spec_lab[c("M2", "M1")]))
  all_estimates$D <- dD %>% mutate(figure = "D/D1")
  shape_spec <- c("M2 (+ clicks)" = 16, "M1 (no clicks)" = 21)
  cap_D <- "95% CIs (+/- 1.96 x SE). Dashed line at zero.\nURL level, primary sample."
  save_show(make_forest(dD, shape_spec, "Specification", cap_D, show_p = TRUE),
            "Fig_D_Amplification", 10.5, 4.8)
  save_show(make_forest(dD %>% filter(term == "mpas_abs"), shape_spec,
                        "Specification", cap_D, show_p = TRUE),
            "Fig_D1_Amplification_Intensity", 7, 4.8)
}

# --- Fig E / E_M1: cluster level, primary vs high-coverage --------------------
if (!is.null(amp_cl)) {
  dE <- prep_amplification(amp_cl) %>%
    mutate(row   = factor(paste(year, sample),
                          levels = c("2022 High-coverage", "2022 Primary",
                                     "2018 High-coverage", "2018 Primary")),
           shape = factor(sample, levels = c("Primary", "High-coverage")))
  all_estimates$E <- dE %>% mutate(figure = "E/E_M1")
  for (s in c("M2", "M1")) {
    note <- if (s == "M2") "M2 (url_views ~ url_shares + ideology + url_clicks + controls)"
            else "M1 (url_views ~ url_shares + ideology + controls)"
    save_show(make_forest(dE %>% filter(spec == s), shape_sample, "Sample",
                          paste0("95% CIs (+/- 1.96 x SE). Dashed line at zero.\n",
                                 "Cluster level, ", note, "."), show_p = TRUE),
              if (s == "M2") "Fig_E_Amplification_Cluster_M2" else "Fig_E_Amplification_Cluster_M1",
              10.5, 4.8)
  }
}

# =============================================================================
# One table with every plotted number (aggregate coefficients only)
# =============================================================================
if (length(all_estimates) > 0) {
  out <- bind_rows(lapply(all_estimates, function(x)
    x %>% transmute(figure, model, term, year,
                    Estimate = signif(Estimate, 5), SE = signif(SE, 4),
                    p = signif(p, 3))))
  write.csv(out, file.path(FIGURES_DIR, "all_figure_estimates.csv"), row.names = FALSE)
  print(out, n = Inf)
}

cat("\nDone. Figures in", FIGURES_DIR, "\n")
print(list.files(FIGURES_DIR))
