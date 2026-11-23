# =============================================================================
# 06d_activation_url_BR.R
# -----------------------------------------------------------------------------
# URL-level robustness check on the activation framing of RQ1.
# Parallels scripts 06 (intensity) + 06b (signed) merged into one script,
# but with the URL's OWN ideology score (Santos 2023) as the focal variable
# instead of the cluster-level weighted mean.
#
# Question shifts from
#   "Do partisan clusters share more than centrist clusters?"
# to
#   "Are URLs from partisan SOURCES shared more than URLs from centrist sources,
#    once we control for the cluster context they're embedded in?"
#
# Two specifications, run in parallel (per the single-interaction constraint
# of lmdp(); see decision 5.5):
#   Intensity:    url_shares ~ url_views * mpas_abs       + cluster controls
#   Directional:  url_shares ~ url_views * weighted_mpas  + cluster controls
#
# Four-fit design (2 ideology x 2 years, primary sample only). The
# high-coverage robustness sample is omitted at URL level: cluster-level
# high-coverage was a measurement-noise check on the WEIGHTED-AVERAGE MPAS,
# but URL-level MPAS is the source's own score with no aggregation noise.
# Cluster-coverage heterogeneity is still absorbed by the z_pct_matched_urls
# covariate.
#
# Sample-size warning: the URL-level merge drops ~46% of the cluster-level
# sample. The 2022 cell will be the smallest (~17k URLs); expect somewhat
# wider standard errors than in 06/06b, especially in 2022.
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
RESULTS_FILE <- file.path(RESULTS_DIR, "rq1_url_level_BR.RData")
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Per-cell DP noise SDs (Evans & King 2022; identical to 06/06b/06c)
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
  # URL-level ideology (no DP noise; from Santos 2023 reference list)
  mpas_abs                  = 0,
  weighted_mpas             = 0,
  # Cluster-level ideology kept for comparison (no DP noise)
  cluster_mpas_abs          = 0,
  cluster_weighted_mpas     = 0,
  # Cluster-level controls (no DP noise)
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
# Helpers (reused from 06/06b/06c)
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

formula_intensity <- as.formula(paste(
  "url_shares ~ url_views * mpas_abs +", CTRL))
formula_signed    <- as.formula(paste(
  "url_shares ~ url_views * weighted_mpas +", CTRL))

# -----------------------------------------------------------------------------
# Load
# -----------------------------------------------------------------------------

cat(strrep("=", 65), "\n", sep = "")
cat(" RQ1 modeling (URL-LEVEL): shares ~ views * ideology (BR)\n")
cat(strrep("=", 65), "\n\n", sep = "")

df <- read_csv(DATA_FILE, show_col_types = FALSE)
cat("Loaded:", format(nrow(df), big.mark = ","), "URL-level rows\n\n")

df_2018 <- df %>% filter(election_year == 2018)
df_2022 <- df %>% filter(election_year == 2022)

cat("Sample sizes:\n")
cat(sprintf("  2018 : %s URLs\n", format(nrow(df_2018), big.mark = ",")))
cat(sprintf("  2022 : %s URLs\n", format(nrow(df_2022), big.mark = ",")))
cat("\n")

# =============================================================================
# Block 1 - INTENSITY MODELS (parallels script 06)
# =============================================================================

cat(strrep("#", 65), "\n", sep = "")
cat("# INTENSITY: shares ~ views * mpas_abs (URL-level)\n")
cat(strrep("#", 65), "\n\n", sep = "")

m_int_2018 <- fit_and_summarize_lmdp(
  formula_intensity, "URL-level / 2018 / intensity", df_2018)
m_int_2022 <- fit_and_summarize_lmdp(
  formula_intensity, "URL-level / 2022 / intensity", df_2022)

# =============================================================================
# Block 2 - SIGNED MODELS (parallels script 06b)
# =============================================================================

cat(strrep("#", 65), "\n", sep = "")
cat("# SIGNED: shares ~ views * weighted_mpas (URL-level)\n")
cat(strrep("#", 65), "\n\n", sep = "")

m_sgn_2018 <- fit_and_summarize_lmdp(
  formula_signed, "URL-level / 2018 / signed", df_2018)
m_sgn_2022 <- fit_and_summarize_lmdp(
  formula_signed, "URL-level / 2022 / signed", df_2022)

# -----------------------------------------------------------------------------
# Focal-term tables, separately for each spec
# -----------------------------------------------------------------------------

extract_focal <- function(m, label, focal_set) {
  s   <- summary(m)
  cof <- as.data.frame(s)
  cof$term  <- rownames(cof); rownames(cof) <- NULL
  cof$model <- label
  cof %>% filter(term %in% focal_set)
}

focal_int <- c("(Intercept)", "url_views", "mpas_abs", "url_views:mpas_abs")
focal_sgn <- c("(Intercept)", "url_views", "weighted_mpas", "url_views:weighted_mpas")

focal_tbl_int <- bind_rows(
  extract_focal(m_int_2018, "2018", focal_int),
  extract_focal(m_int_2022, "2022", focal_int)
) %>%
  select(model, term,
         Estimate = `Estimate`, SE = `Std. Error`,
         t        = `t value`,  p  = `Pr(>|t|)`)

focal_tbl_sgn <- bind_rows(
  extract_focal(m_sgn_2018, "2018", focal_sgn),
  extract_focal(m_sgn_2022, "2022", focal_sgn)
) %>%
  select(model, term,
         Estimate = `Estimate`, SE = `Std. Error`,
         t        = `t value`,  p  = `Pr(>|t|)`)

cat(strrep("=", 65), "\n", sep = "")
cat(" URL-LEVEL INTENSITY focal-term comparison\n")
cat(strrep("=", 65), "\n", sep = "")
print(as_tibble(focal_tbl_int), n = Inf)

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" URL-LEVEL SIGNED focal-term comparison\n")
cat(strrep("=", 65), "\n", sep = "")
print(as_tibble(focal_tbl_sgn), n = Inf)

# -----------------------------------------------------------------------------
# Cross-year Z-tests
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests (URL-level intensity)\n")
cat(strrep("=", 65), "\n", sep = "")
xyear_int <- compare_coefs_across_years(m_int_2018, m_int_2022)
print(as_tibble(xyear_int), n = Inf)

cat("\n", strrep("=", 65), "\n", sep = "")
cat(" Cross-year Z-tests (URL-level signed)\n")
cat(strrep("=", 65), "\n", sep = "")
xyear_sgn <- compare_coefs_across_years(m_sgn_2018, m_sgn_2022)
print(as_tibble(xyear_sgn), n = Inf)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

results <- list(
  intensity = list(
    primary_2018 = m_int_2018,
    primary_2022 = m_int_2022,
    focal_table  = focal_tbl_int,
    xyear        = xyear_int
  ),
  signed = list(
    primary_2018 = m_sgn_2018,
    primary_2022 = m_sgn_2022,
    focal_table  = focal_tbl_sgn,
    xyear        = xyear_sgn
  )
)

save(results, NOISE_PARAMETERS, file = RESULTS_FILE)
cat("\nSaved 4 URL-level models + focal tables + cross-year tests to:\n  ",
    RESULTS_FILE, "\n", sep = "")
cat("Done.\n")
