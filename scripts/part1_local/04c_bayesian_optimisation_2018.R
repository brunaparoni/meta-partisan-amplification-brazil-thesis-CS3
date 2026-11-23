# =============================================================================
# 04c_bayesian_optimisation_2018.R
# -----------------------------------------------------------------------------
# 2018: Bayesian optimisation of k (rBayesianOptimization, expected
# improvement, 10 initial points + 30 iterations, k in [100, 200]).
# Objective: minus the total within-cluster sum of squares of k-means
# (nstart = 3, iter.max = 50).
#
# Run the 04 scripts in order in ONE R session: each uses objects created
# by the previous one (data, clean_data, mat_pca, mat_sample, ...).
# =============================================================================

library(rBayesianOptimization)
library(progress)

# Define the scoring function for rBayesianOptimization
scoring_function <- function(k_input) {
  # k_input comes from the optimizer
  k_int <- as.integer(round(k_input))
  
  # Run K-means 
  set.seed(123)
  km <- kmeans(mat_pca, centers = k_int, nstart = 3, iter.max = 50)
  
  # The objective: Minimize Within-Cluster Sum of Squares
  # rBayesianOptimization MAXIMIZES, so we use negative WSS
  s_val <- -km$tot.withinss 
  
  # UPDATE THE PROGRESS BAR
  pb$tick()
  
  return(list(Score = s_val, Pred = 0))
}

# 1. Setup the Progress Bar
# Total iterations = n_iter + n_init
total_iters <- 40 # Let's do 10 + 30 for a deeper search
pb <- progress_bar$new(
  format = "  Optimizing Brazilian 2018 K [:bar] :percent in :elapsed (ETA: :eta)",
  total = total_iters, clear = FALSE, width = 60)

# 2. Match the points in the function
opt_results_brazil <- BayesianOptimization(
  FUN = scoring_function,
  bounds = list(k_input = c(100, 200)), 
  init_points = 10,  # First 10 ticks
  n_iter = 30,       # Next 30 ticks
  acq = "ei",
  verbose = FALSE
)

# 3. Extract the winner
best_k <- round(opt_results$Best_Par["k_input"])
cat("\nOptimization finished! The best K is:", best_k)