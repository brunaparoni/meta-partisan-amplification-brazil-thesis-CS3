# =============================================================================
# 04h_k200_vs_k250_2022.R
# -----------------------------------------------------------------------------
# 2022: within-cluster sum of squares at k = 200 compared with the k = 250
# proposed by the optimisation (difference: 2.49%).
#
# Run the 04 scripts in order in ONE R session: each uses objects created
# by the previous one (data, clean_data, mat_pca, mat_sample, ...).
# =============================================================================

# Manual check of k=200 quality
set.seed(123)
km_200_check <- kmeans(mat_pca_22, centers = 200, nstart = 10, iter.max = 50)

# Calculate the Score
score_200 <- -km_200_check$tot.withinss
score_250 <- -18704.9752

# Calculate the 'Accuracy Loss'
pct_diff <- abs((score_250 - score_200) / score_250) * 100

cat("Score for k=200:", score_200, "\n")
cat("Score for k=250:", score_250, "\n")
cat("Percentage difference:", round(pct_diff, 2), "%\n")
