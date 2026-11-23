# =============================================================================
# 06b_activation_cluster_signed_BR.R
# -----------------------------------------------------------------------------
# Sensitivity / complementary analysis to script 06.
# Substitutes the SIGNED weighted_mpas (-1 = left, +1 = right) for the
# absolute mpas_abs used in the primary RQ1 analysis.
#
# Question shifts from
#   "Do partisan (= intense) clusters share more than centrist ones?"
# to
#   "Do right-wing clusters share more or less than left-wing ones?"
#
# Same four-model design (2018/2022 x primary/high-coverage), same controls,
# same lmdp() pipeline, same cross-year Z-tests. Output saved separately to
# ./results/rq1_signed_models_BR.RData so the primary results are preserved.
#
# A more complete analysis would include BOTH weighted_mpas and mpas_abs
# (direction + intensity), but PrivacyUnbiased::lmdp() limits the model to a
# single interaction term, so we run separate single-focal models and
# compare side by side.
# =============================================================================

suppressPackageStartupMessages({
  library(PrivacyUnbiased)
  library(readr)
  library(dplyr)
  library(tibble)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

DATA_FILE    <- "./data/urls_BR_modeling.csv"
RESULTS_DIR  <- "./results"
RESULTS_FILE <- file.path(RESULTS_DIR, "rq1_signed_models_BR.RData")
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Per-cell DP noise SDs (same as script 06)
NOISE_PARAMETERS <- list(
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
  # Cluster-level / derived predictors (no DP noise)
  mpas_abs                  = 0,
  weighted_mpas             = 0,    # focal variable in this script
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
# Robust lmdp wrapper (same as script 06)
# -----------------------------------------------------------------------------

robust_lmdp <- function(formula, data, model_name = "Model", n_boot = 1000) {
  cat(sprintf("Fitting %s using lmdp() ...\n", model_name))
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
    cat(sprintf("  Running %d-iteration bootstrap...\n", n_boot))
    t0 <- Sys.time()
    m  <- lmdp(formula, data = data,
               bootstrap_var = TRUE, nsims_var = n_boot)
    cat("  Bootstrap done in",
        round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
  }
  return(m)
}

fit_and_summarize_lmdp <- function(formula, model_name, data, n_boot = 1000) {
  cat(strrep("-", 65), "\n", sep = "")
  cat(sprintf("=== %s ===\n", model_name))
  cat("Formula: ", deparse(formula), "\n")

  vars <- all.vars(formula)
  d    <- data %>% select(all_of(vars)) %>% na.omit()
  noise_row <- as.data.frame(setNames(lapply(vars, get_noise_sd), vars))
  final_data <- bind_rows(noise_row, d)

  cat(sprintf("Sample size: %s URLs (after na.omit)\n",
              format(nrow(d), big.mark = ",")))

  m <- robust_lmdp(formula, final_data, model_name, n_boot = n_boot)
  cat("\nSummary:\n"); print(summary(m))
  cat(sprintf("\nSigma_sq_hat: %.4f\n", m$Sigma_sq_hat))
  cat("Variance method: ",
      ifelse(!is.null(m$boot) && m$boot,
             sprintf("bootstrap (%d simulations)", n_boot),
             "simulation"),
      "\n", sep = "")
  cat("\n")
  return(m)
}

compare_coefs_across_years <- function(m_a, m_b,
                                       label_a = "2018", label_b = "2022") {
  pull_summary <- function(m) {
    s <- as.data.frame(summary(m))
    s$term <- rownames(s); rownames(s) <- NULL
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
  names(out)[names(out) == "est_a"] <- paste0("est_", label_a)
  names(out)[names(out) == "se_a"]  <- paste0("se_",  label_a)
  names(out)[names(out) == "est_b"] <- paste0("est_", label_b)
  names(out)[names(out) == "se_b"]  <- paste0("se_",  label_b)
  out
}

# -----------------------------------------------------------------------------
# Load
# -----------------------------------------------------------------------------

cat(strrep("=", 65), "\n", sep = "")
cat(" RQ1 modeling (SIGNED): shares ~ views * weighted_mpas (BR)\n")
cat(strrep("=", 65), "\n\n", sep = "")

df <- read_csv(DATA_FILE, show_col_types = FALSE)
cat("Loaded:", format(nrow(df), big.mark = ","), "URL-level rows\n")

# Quick descriptive on the directional variable
cat("\n--- weighted_mpas distribution by year ---\n")
df %>%
  group_by(election_year) %>%
  summarise(
    n_urls    = n(),
    median_w  = round(median(weighted_mpas), 3),
    iqr_w     = round(IQR(weighted_mpas), 3),
    p10       = round(quantile(weighted_mpas, 0.10), 3),
    p90       = round(quantile(weighted_mpas, 0.90), 3),
    pct_left  = round(100 * mean(weighted_mpas < 0), 1),
    pct_right = round(100 * mean(weighted_mpas > 0), 1),
    .groups   = "drop"
  ) %>%
  print()

cat("\n")

# Primary specification (signed)
formula_signed <- url_shares ~ url_views * weighted_mpas +
  z_duration_days + z_total_urls + z_pct_social_media +
  z_gini_domains + z_pct_matched_urls

# -----------------------------------------------------------------------------
# Models 1 & 2 - Primary, per year
# -----------------------------------------------------------------------------

df_2018 <- df %>% filter(election_year == 2018)
df_2022 <- df %>% filter(election_year == 2022)

m_2018 <- fit_and_summarize_lmdp(
  formula_signed, "RQ1 / 2018 / signed / primary", df_2018
)
m_2022 <- fit_and_summarize_lmdp(
  formula_signed, "RQ1 / 2022 / signed / primary", df_2022
)

# -----------------------------------------------------------------------------
# Models 3 & 4 - Sensitivity: high MPAS coverage
# -----------------------------------------------------------------------------

df_2018_hc <- df_2018 %>% filter(high_mpas_coverage)
df_2022_hc <- df_2022 %>% filter(high_mpas_coverage)

cat(sprintf(
  "\nHigh-coverage subsample sizes: 2018 = %s URLs, 2022 = %s URLs\n",
  format(nrow(df_2018_hc), big.mark = ","),
  format(nrow(df_2022_hc), big.mark = ",")
))

m_2018_hc <- fit_and_summarize_lmdp(
  formula_signed, "RQ1 / 2018 / signed / pct_matched > 50%", df_2018_hc
)
m_2022_hc <- fit_and_summarize_lmdp(
  formula_signed, "RQ1 / 2022 / signed / pct_matched > 50%", df_2022_hc
)

# -----------------------------------------------------------------------------
# Focal-term comparison + cross-year Z-tests
# -----------------------------------------------------------------------------

extract_focal <- function(m, label) {
  s   <- summary(m)
  cof <- as.data.frame(s)
  cof$term  <- rownames(cof); rownames(cof) <- NULL
  cof$model <- label
  focal <- c("(Intercept)", "url_views", "weighted_mpas",
             "url_views:weighted_mpas")
  cof %>% filter(term %in% focal)
}

focal_tbl <- bind_rows(
  extract_focal(m_2018,    "2018 primary"),
  extract_focal(m_2022,    "2022 primary"),
  extract_focal(m_2018_hc, "2018 high-coverage"),
  extract_focal(m_2022_hc, "2022 high-coverage")
) %>%
  select(model, term,
         Estimate = `Estimate`, SE = `Std. Error`,
         t        = `t value`,  p  = `Pr(>|t|)`)

cat(strrep("=", 65), "\n", sep = "")
cat(" RQ1 (signed) focal-term comparison across models\n")
cat(strrep("=", 65), "\n", sep = "")
print(as_tibble(focal_tbl), n = Inf)

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests on PRIMARY samples (2018 vs 2022)\n")
cat(strrep("=", 65), "\n", sep = "")
xyear_primary <- compare_coefs_across_years(m_2018, m_2022, "2018", "2022")
print(as_tibble(xyear_primary), n = Inf)

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests on HIGH-COVERAGE samples\n")
cat(strrep("=", 65), "\n", sep = "")
xyear_hc <- compare_coefs_across_years(m_2018_hc, m_2022_hc, "2018", "2022")
print(as_tibble(xyear_hc), n = Inf)

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
cat("\nSaved all models + focal table + cross-year tests to:\n  ",
    RESULTS_FILE, "\n", sep = "")
cat("Done.\n")
