# =============================================================================
# 06e_amplification_url_BR.R
# -----------------------------------------------------------------------------
# URL-level robustness check on the US (amplification) direction of RQ1.
# Combines 06c's specifications (M1, M2 x intensity, signed) with 06d's
# URL-level ideology variable (Santos 2023 source MPAS) instead of the
# cluster-level weighted mean.
#
# Mirrors the US paper's direction:
#   M1 / intensity:  url_views ~ url_shares + mpas_abs                + cluster controls
#   M2 / intensity:  url_views ~ url_shares + mpas_abs + url_clicks   + cluster controls
#   M1 / signed   :  url_views ~ url_shares + weighted_mpas           + cluster controls
#   M2 / signed   :  url_views ~ url_shares + weighted_mpas + url_clicks + cluster controls
#
# Substantive interpretation
#   Where 06c asked "do URLs in partisan CLUSTERS reach larger audiences per
#   share?", 06e asks "do URLs from partisan SOURCES reach larger audiences
#   per share, controlling for cluster context?". Source-effect rather than
#   environment-effect amplification.
#
# Design parallels 06c, with one simplification
#   - 4 specs x 2 years = 8 model fits (primary samples only)
#   - High-coverage robustness sample omitted at URL level: it was a
#     measurement-noise check on cluster-aggregated MPAS, but URL-level MPAS
#     is the source's own score with no aggregation noise. Cluster-coverage
#     heterogeneity is still absorbed by the z_pct_matched_urls covariate.
#   - All main-effect models, so lmdp's faster simulation method should
#     succeed in most cells; bootstrap fallback rare.
#   - Same per-cell DP noise SDs; same robust_lmdp wrapper.
#   - Cross-year Z-tests on the 4 spec pairs.
#
# Sample-size note: the URL-level merge drops ~46% of the cluster-level sample.
# 2022 will be the smallest cell (~17k URLs); 2018 (~93k URLs) should still
# have good power for the structural views/shares slope.
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

DATA_FILE    <- "./data/urls_BR_modeling_url_level.csv"
RESULTS_DIR  <- "./results"
RESULTS_FILE <- file.path(RESULTS_DIR, "rq1_url_level_us_direction_BR.RData")
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Per-cell DP noise SDs (Evans & King 2022; identical to 06/06b/06c/06d)
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
  cluster_mpas_abs          = 0,
  cluster_weighted_mpas     = 0,
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
# Helpers (reused from 06/06b/06c/06d)
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
cat(" RQ1 modeling (URL-LEVEL US DIRECTION): views ~ shares + ideology (BR)\n")
cat(strrep("=", 65), "\n\n", sep = "")

df <- read_csv(DATA_FILE, show_col_types = FALSE)
cat("Loaded:", format(nrow(df), big.mark = ","), "URL-level rows\n\n")

df_2018 <- df %>% filter(election_year == 2018)
df_2022 <- df %>% filter(election_year == 2022)

cat("Sample sizes:\n")
cat(sprintf("  2018 : %s URLs\n", format(nrow(df_2018), big.mark = ",")))
cat(sprintf("  2022 : %s URLs\n", format(nrow(df_2022), big.mark = ",")))
cat("\n")

# -----------------------------------------------------------------------------
# Fit all 8 models in a structured loop
# -----------------------------------------------------------------------------

fits <- list()
sample_data <- list(
  "primary_2018" = df_2018,
  "primary_2022" = df_2022
)

for (spec_name in names(spec_formulas)) {
  spec <- spec_formulas[[spec_name]]
  for (samp_name in names(sample_data)) {
    label <- sprintf("URL-level / %s / %s", spec$label,
                     str_replace(samp_name, "_", " "))
    fits[[paste(spec_name, samp_name, sep = "::")]] <- list(
      model = fit_and_summarize_lmdp(spec$formula, label,
                                     sample_data[[samp_name]]),
      spec  = spec_name,
      sample = samp_name,
      focal = spec$focal
    )
  }
}

# -----------------------------------------------------------------------------
# Focal-term comparison across all 8 models
# -----------------------------------------------------------------------------

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
cat(" RQ1 / URL-level US-direction focal-term comparison (all 8 models)\n")
cat(strrep("=", 65), "\n", sep = "")
print(as_tibble(focal_tbl), n = Inf)

# -----------------------------------------------------------------------------
# Cross-year Z-tests, one per spec
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests on URL-level US-direction models\n")
cat(strrep("=", 65), "\n", sep = "")

xyear_results <- list()
for (spec_name in names(spec_formulas)) {
  key_2018 <- paste(spec_name, "primary_2018", sep = "::")
  key_2022 <- paste(spec_name, "primary_2022", sep = "::")
  cat("\n--- ", spec_name, " ---\n", sep = "")
  xyear <- compare_coefs_across_years(
    fits[[key_2018]]$model, fits[[key_2022]]$model, "2018", "2022"
  )
  print(as_tibble(xyear), n = Inf)
  xyear_results[[spec_name]] <- xyear
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
cat("\nSaved 8 URL-level models + focal table + 4 cross-year tests to:\n  ",
    RESULTS_FILE, "\n", sep = "")
cat("Done.\n")
