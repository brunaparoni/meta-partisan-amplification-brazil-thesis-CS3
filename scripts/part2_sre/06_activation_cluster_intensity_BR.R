# =============================================================================
# 06_activation_cluster_intensity_BR.R
# -----------------------------------------------------------------------------
# RQ1: For widely circulated URLs shared on Facebook, how does the relationship
#      between viewing and sharing vary based on the partisan intensity of the
#      audience (operationalised via cluster-level mpas_abs)?
#
# Primary specification (linear interaction, per RQ1 plan):
#   url_shares ~ url_views * mpas_abs +
#                z_duration_days + z_total_urls + z_pct_social_media +
#                z_gini_domains + z_pct_matched_urls
#
# Models fitted
#   1. Primary, 2018 only
#   2. Primary, 2022 only
#   3. Pooled with year x mpas_abs interaction (robustness on temporal stability)
#   4. High-coverage sensitivity (pct_matched_urls > 50), per year
#
# Methodology notes
#   - lmdp() is the bias-corrected estimator of Evans & King (2022) for linear
#     models on differentially private data. It does NOT support random effects,
#     so we treat cluster-level controls as URL-level fixed predictors. SEs do
#     not adjust for within-cluster correlation; this is a known limitation
#     of the current PrivacyUnbiased framework.
#   - Per-cell DP noise SDs are passed via a 'noise row' prepended to the data
#     (matches the convention of the US amplification scripts). Note that the
#     URL-level totals are sums of N_i cells, so the *true* per-URL noise SD
#     is sqrt(N_i) * sigma. We follow the US convention of passing per-cell
#     sigma; the implication is a slightly under-corrected bias on the noisy
#     coefficients. Robustness via the high-coverage subset addresses this
#     partially (less heterogeneity in N_i).
#   - Models with interaction terms force lmdp into bootstrap-variance mode;
#     robust_lmdp() handles that automatically with 1000 simulations.
# =============================================================================

suppressPackageStartupMessages({
  library(PrivacyUnbiased)
  library(readr)
  library(dplyr)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

DATA_FILE    <- "./data/urls_BR_modeling.csv"
RESULTS_DIR  <- "./results"
RESULTS_FILE <- file.path(RESULTS_DIR, "rq1_models_BR.RData")

dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Per-cell DP noise SDs (Evans & King 2022)
NOISE_PARAMETERS <- list(
  # DP-noisy engagement metrics
  url_views                 = 2228,
  url_shares                = 14,
  url_clicks                = 40,
  url_likes                 = 22,
  url_share_without_clicks  = 10,
  url_comments              = 10,
  url_loves                 = 10,
  url_hahas                 = 10,
  url_wows                  = 10,
  url_sorrys                = 10,
  url_angers                = 10,
  # Cluster-level / derived predictors (NOT DP-noisy)
  mpas_abs                  = 0,
  weighted_mpas             = 0,
  z_duration_days           = 0,
  z_total_urls              = 0,
  z_pct_social_media        = 0,
  z_gini_domains            = 0,
  z_pct_matched_urls        = 0,
  pct_matched_urls          = 0,
  pct_social_media          = 0,
  year_2022                 = 0
)

get_noise_sd <- function(variable_name) {
  noise <- NOISE_PARAMETERS[[variable_name]]
  if (is.null(noise)) {
    warning(paste("No noise parameter defined for", variable_name,
                  "- assuming 0."))
    return(0)
  }
  return(noise)
}

# -----------------------------------------------------------------------------
# Robust lmdp wrapper - simulation first, bootstrap fallback
# -----------------------------------------------------------------------------

robust_lmdp <- function(formula, data, model_name = "Model",
                        n_boot = 1000) {

  cat(sprintf("Fitting %s using lmdp() ...\n", model_name))

  # First try simulation-based variance
  m <- NULL
  sim_failed <- FALSE
  tryCatch({
    m <- lmdp(formula, data = data, bootstrap_var = FALSE)
    if (!is.null(m$vc_pos_def) && !m$vc_pos_def) {
      sim_failed <- TRUE
      cat("  ! VC matrix not positive definite; switching to bootstrap.\n")
    } else {
      cat("  Simulation method succeeded.\n")
      return(m)
    }
  }, warning = function(w) {
    if (grepl("VC matrix not positive definite", w$message)) {
      sim_failed <<- TRUE
      cat("  ! VC matrix not PD (warning); switching to bootstrap.\n")
    }
  }, error = function(e) {
    sim_failed <<- TRUE
    cat("  ! Simulation method errored:", e$message, "- using bootstrap.\n")
  })

  if (sim_failed) {
    cat(sprintf("  Running %d-iteration bootstrap (this can take several minutes)...\n",
                n_boot))
    t0 <- Sys.time()
    m  <- lmdp(formula, data = data,
               bootstrap_var = TRUE, nsims_var = n_boot)
    cat("  Bootstrap done in",
        round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
  }
  return(m)
}

# -----------------------------------------------------------------------------
# Fit + summarise wrapper
# -----------------------------------------------------------------------------

fit_and_summarize_lmdp <- function(formula, model_name, data, n_boot = 1000) {

  cat(strrep("-", 65), "\n", sep = "")
  cat(sprintf("=== %s ===\n", model_name))
  cat("Formula: ", deparse(formula), "\n")

  # Pull only the variables in the formula and drop incomplete rows
  vars <- all.vars(formula)
  d    <- data %>% select(all_of(vars)) %>% na.omit()

  # Build a one-row 'noise' frame with each variable's per-cell SD,
  # then prepend it to the data. lmdp reads SDs from row 1 of the input.
  noise_row <- as.data.frame(
    setNames(lapply(vars, get_noise_sd), vars)
  )
  final_data <- bind_rows(noise_row, d)

  cat(sprintf("Sample size: %s URLs (after na.omit)\n",
              format(nrow(d), big.mark = ",")))

  # Fit
  m <- robust_lmdp(formula, final_data, model_name, n_boot = n_boot)

  cat("\nSummary:\n")
  print(summary(m))

  # Reporting metrics
  cat(sprintf("\nSigma_sq_hat: %.4f  (lower = better fit, Evans & King primary metric)\n",
              m$Sigma_sq_hat))
  cat("Variance method: ",
      ifelse(!is.null(m$boot) && m$boot,
             sprintf("bootstrap (%d simulations)", n_boot),
             "simulation"),
      "\n", sep = "")
  cat("\n")

  return(m)
}

# -----------------------------------------------------------------------------
# Cross-year coefficient comparison (Z-test on independent samples)
# -----------------------------------------------------------------------------
# PrivacyUnbiased's lmdp() permits at most ONE interaction term per model, so
# a single pooled model with both url_views:mpas_abs AND year_2022:mpas_abs is
# not allowed. The 2018 and 2022 URL samples are disjoint and independent,
# however, so cross-year coefficient equality can be tested directly:
#
#     Z = (b_2018 - b_2022) / sqrt(SE_2018^2 + SE_2022^2)
#     two-sided p = 2 * pnorm(-|Z|)
#
# This is statistically equivalent to a year-x-coefficient interaction test
# in a hypothetical pooled model, without violating lmdp()'s constraints.

compare_coefs_across_years <- function(m_a, m_b,
                                       label_a = "2018", label_b = "2022") {
  pull_summary <- function(m) {
    s <- as.data.frame(summary(m))
    s$term <- rownames(s)
    rownames(s) <- NULL
    s
  }
  s_a <- pull_summary(m_a) %>%
    select(term, est_a = `Estimate`, se_a = `Std. Error`)
  s_b <- pull_summary(m_b) %>%
    select(term, est_b = `Estimate`, se_b = `Std. Error`)

  out <- inner_join(s_a, s_b, by = "term") %>%
    mutate(
      diff    = est_a - est_b,
      se_diff = sqrt(se_a^2 + se_b^2),
      z       = diff / se_diff,
      p_two   = 2 * pnorm(-abs(z))
    )
  # Rename a/b columns to use the year labels
  names(out)[names(out) == "est_a"] <- paste0("est_", label_a)
  names(out)[names(out) == "se_a"]  <- paste0("se_",  label_a)
  names(out)[names(out) == "est_b"] <- paste0("est_", label_b)
  names(out)[names(out) == "se_b"]  <- paste0("se_",  label_b)
  out
}

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

cat(strrep("=", 65), "\n", sep = "")
cat(" RQ1 modeling: shares ~ views * mpas_abs (BR)\n")
cat(strrep("=", 65), "\n\n", sep = "")

df <- read_csv(DATA_FILE, show_col_types = FALSE)
cat("Loaded:", format(nrow(df), big.mark = ","), "URL-level rows\n\n")

# Primary specification used by all year-stratified models
formula_primary <- url_shares ~ url_views * mpas_abs +
  z_duration_days + z_total_urls + z_pct_social_media +
  z_gini_domains + z_pct_matched_urls

# -----------------------------------------------------------------------------
# Model 1 & 2 - Primary, per year
# -----------------------------------------------------------------------------

df_2018 <- df %>% filter(election_year == 2018)
df_2022 <- df %>% filter(election_year == 2022)

m_2018 <- fit_and_summarize_lmdp(
  formula_primary,
  "RQ1 / 2018 / primary",
  df_2018
)

m_2022 <- fit_and_summarize_lmdp(
  formula_primary,
  "RQ1 / 2022 / primary",
  df_2022
)

# -----------------------------------------------------------------------------
# Pooled year-x-mpas_abs interaction is NOT fitted as a single lmdp model
# -----------------------------------------------------------------------------
# PrivacyUnbiased::lmdp() supports a maximum of ONE interaction term per
# model. A pooled spec with both url_views:mpas_abs and year_2022:mpas_abs
# triggers the error: "Model cannot contain more than one interaction
# variable". We therefore use the cross-year Z-test (defined above and
# applied below the high-coverage block) on the disjoint per-year fits,
# which is statistically equivalent to that interaction test.

# -----------------------------------------------------------------------------
# Model 4 & 5 - Sensitivity: high MPAS coverage (pct_matched_urls > 50)
# -----------------------------------------------------------------------------

df_2018_hc <- df_2018 %>% filter(high_mpas_coverage)
df_2022_hc <- df_2022 %>% filter(high_mpas_coverage)

cat(sprintf(
  "\nHigh-coverage subsample sizes: 2018 = %s URLs, 2022 = %s URLs\n",
  format(nrow(df_2018_hc), big.mark = ","),
  format(nrow(df_2022_hc), big.mark = ",")
))

m_2018_hc <- fit_and_summarize_lmdp(
  formula_primary,
  "RQ1 / 2018 / pct_matched > 50%",
  df_2018_hc
)

m_2022_hc <- fit_and_summarize_lmdp(
  formula_primary,
  "RQ1 / 2022 / pct_matched > 50%",
  df_2022_hc
)

# -----------------------------------------------------------------------------
# Side-by-side coefficient table for the focal terms
# -----------------------------------------------------------------------------

extract_focal <- function(m, label) {
  s   <- summary(m)
  cof <- as.data.frame(s)
  cof$term  <- rownames(cof)
  rownames(cof) <- NULL
  cof$model <- label
  # Keep only the terms most relevant to RQ1
  focal <- c("(Intercept)", "url_views", "mpas_abs", "url_views:mpas_abs")
  cof %>% filter(term %in% focal)
}

focal_tbl <- bind_rows(
  extract_focal(m_2018,    "2018 primary"),
  extract_focal(m_2022,    "2022 primary"),
  extract_focal(m_2018_hc, "2018 high-coverage"),
  extract_focal(m_2022_hc, "2022 high-coverage")
) %>%
  select(model, term, Estimate = `Estimate`, SE = `Std. Error`,
         t = `t value`, p = `Pr(>|t|)`)

cat(strrep("=", 65), "\n", sep = "")
cat(" RQ1 focal-term comparison across models\n")
cat(strrep("=", 65), "\n", sep = "")
print(tibble::as_tibble(focal_tbl), n = Inf)

# -----------------------------------------------------------------------------
# Cross-year tests (replaces the pooled model)
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests on PRIMARY samples (2018 vs 2022)\n")
cat(strrep("=", 65), "\n", sep = "")
xyear_primary <- compare_coefs_across_years(m_2018, m_2022, "2018", "2022")
print(tibble::as_tibble(xyear_primary), n = Inf)

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests on HIGH-COVERAGE samples (2018 vs 2022)\n")
cat(strrep("=", 65), "\n", sep = "")
xyear_hc <- compare_coefs_across_years(m_2018_hc, m_2022_hc, "2018", "2022")
print(tibble::as_tibble(xyear_hc), n = Inf)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

results <- list(
  primary_2018       = m_2018,
  primary_2022       = m_2022,
  high_coverage_2018 = m_2018_hc,
  high_coverage_2022 = m_2022_hc,
  focal_table        = focal_tbl,
  xyear_primary      = xyear_primary,
  xyear_high_cov     = xyear_hc
)

save(results, NOISE_PARAMETERS, file = RESULTS_FILE)
cat("\nSaved all models + focal table + cross-year tests to:", RESULTS_FILE, "\n")
cat("Done.\n")
