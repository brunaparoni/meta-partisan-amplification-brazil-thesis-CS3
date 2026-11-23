# =============================================================================
# 06c_amplification_cluster_BR.R
# -----------------------------------------------------------------------------
# US-paper specifications applied to the Brazilian data, for cross-country
# comparability in the methods section.
#
# The US analysis estimated:
#   M1: url_views ~ url_shares + std_audience_alignment_strength
#   M2: url_views ~ url_shares + std_audience_alignment_strength + url_clicks
#   M3: url_views ~ url_shares + std_audience_alignment_strength + ng_score
#   M4: url_views ~ url_shares + std_audience_alignment_strength + ng_score
#                   + url_clicks
#
# This script runs M1 and M2 only - M3 and M4 require a NewsGuard-equivalent
# source-quality score, which has not yet been computed for Brazilian sources.
# A future script (06f or similar) can add M3-M4 once such a measure exists.
#
# Substantive differences from scripts 06 and 06b
#   - DV / IV inversion: shares is the regressor here (US "amplification"
#     framing: how big is the audience reached per share?), where the BR
#     activation analysis used views as the regressor.
#   - Main effect only: no interaction with the ideology measure, mirroring
#     the US specifications. Slope-moderation hypotheses are tested in 06/06b.
#   - Both ideology specifications (intensity and signed) run side by side.
#   - Cluster-level controls are still included; the US analysis lacked them
#     because its data was not nested in clusters. This is a justified BR-
#     specific addition.
#
# Eight model families (4 specs x 2 ideology) x 4 (year x sample) = 16 fits.
# All are main-effect models, so lmdp's simulation method should typically
# succeed and bootstrap fallback should rarely trigger - much faster than
# 06/06b which forced bootstrap throughout.
# =============================================================================

suppressPackageStartupMessages({
  library(PrivacyUnbiased)
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

DATA_FILE    <- "./data/urls_BR_modeling.csv"
RESULTS_DIR  <- "./results"
RESULTS_FILE <- file.path(RESULTS_DIR, "rq1_us_direction_BR.RData")
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Per-cell DP noise SDs (Evans & King 2022)
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
# Helpers (reused from 06 / 06b)
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
# Specifications
# -----------------------------------------------------------------------------
# All four specs include the same cluster-level controls (z-scored, pooled).
# Differences: ideology variable (mpas_abs vs weighted_mpas), and whether
# url_clicks is included (M1 vs M2).

CTRL <- "z_duration_days + z_total_urls + z_pct_social_media + z_gini_domains + z_pct_matched_urls"

spec_formulas <- list(
  M1_intensity = list(
    label   = "M1 / intensity (no clicks)",
    formula = as.formula(paste("url_views ~ url_shares + mpas_abs +", CTRL)),
    focal   = "mpas_abs"
  ),
  M2_intensity = list(
    label   = "M2 / intensity (+ clicks)",
    formula = as.formula(paste("url_views ~ url_shares + mpas_abs + url_clicks +", CTRL)),
    focal   = "mpas_abs"
  ),
  M1_signed = list(
    label   = "M1 / signed (no clicks)",
    formula = as.formula(paste("url_views ~ url_shares + weighted_mpas +", CTRL)),
    focal   = "weighted_mpas"
  ),
  M2_signed = list(
    label   = "M2 / signed (+ clicks)",
    formula = as.formula(paste("url_views ~ url_shares + weighted_mpas + url_clicks +", CTRL)),
    focal   = "weighted_mpas"
  )
)

# -----------------------------------------------------------------------------
# Load
# -----------------------------------------------------------------------------

cat(strrep("=", 65), "\n", sep = "")
cat(" RQ1 modeling (US DIRECTION): views ~ shares + ideology (BR)\n")
cat(strrep("=", 65), "\n\n", sep = "")

df <- read_csv(DATA_FILE, show_col_types = FALSE)
cat("Loaded:", format(nrow(df), big.mark = ","), "URL-level rows\n\n")

df_2018    <- df %>% filter(election_year == 2018)
df_2022    <- df %>% filter(election_year == 2022)
df_2018_hc <- df_2018 %>% filter(high_mpas_coverage)
df_2022_hc <- df_2022 %>% filter(high_mpas_coverage)

# -----------------------------------------------------------------------------
# Fit all 16 models in a structured loop
# -----------------------------------------------------------------------------

fits <- list()
sample_data <- list(
  "primary_2018"       = df_2018,
  "primary_2022"       = df_2022,
  "high_coverage_2018" = df_2018_hc,
  "high_coverage_2022" = df_2022_hc
)

for (spec_name in names(spec_formulas)) {
  spec <- spec_formulas[[spec_name]]
  for (samp_name in names(sample_data)) {
    label <- sprintf("%s / %s", spec$label,
                     str_replace(samp_name, "_", " "))
    fits[[paste(spec_name, samp_name, sep = "::")]] <- list(
      model = fit_and_summarize_lmdp(spec$formula, label, sample_data[[samp_name]]),
      spec  = spec_name,
      sample = samp_name,
      focal = spec$focal
    )
  }
}

# -----------------------------------------------------------------------------
# Focal-term comparison across all 16 models
# -----------------------------------------------------------------------------
# Focal terms of interest: ideology variable (mpas_abs or weighted_mpas) and
# url_shares (the structural amplification slope, parallel to url_views in 06).

extract_focal <- function(fit, label) {
  s   <- summary(fit$model)
  cof <- as.data.frame(s)
  cof$term  <- rownames(cof); rownames(cof) <- NULL
  cof$model <- label
  focal_set <- c("(Intercept)", "url_shares", "url_clicks",
                 "mpas_abs", "weighted_mpas")
  cof %>% filter(term %in% focal_set)
}

focal_tbl <- bind_rows(lapply(names(fits), function(key) {
  parts <- strsplit(key, "::")[[1]]
  spec  <- parts[1]; samp <- parts[2]
  label <- sprintf("%s / %s", spec, samp)
  extract_focal(fits[[key]], label)
})) %>%
  select(model, term,
         Estimate = `Estimate`, SE = `Std. Error`,
         t        = `t value`,  p  = `Pr(>|t|)`)

cat(strrep("=", 65), "\n", sep = "")
cat(" RQ1 / US-direction focal-term comparison across all 16 models\n")
cat(strrep("=", 65), "\n", sep = "")
print(as_tibble(focal_tbl), n = Inf)

# -----------------------------------------------------------------------------
# Cross-year Z-tests, organized by (spec, sample) pair
# -----------------------------------------------------------------------------
# 4 specs x 2 samples = 8 cross-year comparisons total.

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests on US-direction models\n")
cat(strrep("=", 65), "\n", sep = "")

xyear_results <- list()
for (spec_name in names(spec_formulas)) {
  for (samp_base in c("primary", "high_coverage")) {
    key_2018 <- paste(spec_name, paste0(samp_base, "_2018"), sep = "::")
    key_2022 <- paste(spec_name, paste0(samp_base, "_2022"), sep = "::")
    label    <- sprintf("%s / %s sample", spec_name, samp_base)
    cat("\n--- ", label, " ---\n", sep = "")
    xyear <- compare_coefs_across_years(
      fits[[key_2018]]$model, fits[[key_2022]]$model, "2018", "2022"
    )
    print(as_tibble(xyear), n = Inf)
    xyear_results[[paste(spec_name, samp_base, sep = "::")]] <- xyear
  }
}

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

results <- list(
  fits           = fits,
  focal_table    = focal_tbl,
  xyear          = xyear_results,
  spec_formulas  = lapply(spec_formulas, function(s) {
    list(label = s$label, formula = deparse(s$formula), focal = s$focal)
  })
)

save(results, NOISE_PARAMETERS, file = RESULTS_FILE)
cat("\nSaved 16 models + focal table + 8 cross-year tests to:\n  ",
    RESULTS_FILE, "\n", sep = "")
cat("Done.\n")
