# =============================================================================
# 04b_svd_2018.R
# -----------------------------------------------------------------------------
# 2018: unpack the embedding strings (chunks of 5,000), L2-normalise each
# row, truncated SVD with irlba to 250 components (scores = U %*% diag(d)).
# The 250 components keep 80.56% of the total sum of squares.
# Note: irlba is applied to the uncentred matrix (truncated SVD, not a
# centred PCA).
#
# Run the 04 scripts in order in ONE R session: each uses objects created
# by the previous one (data, clean_data, mat_pca, mat_sample, ...).
# =============================================================================

library(dplyr)
# --- 1. SETUP & DATA PREPARATION ---
library(Matrix)
library(irlba)

data <- readRDS("data/political_embeddings_complete_join_method.rds")

# Filter out the 72 NAs first
clean_data <- data %>% filter(!is.na(embedding_vector))
remove(data)

# Filter for 2018
data_2018 <- clean_data[clean_data$year == 2018, ]
embeddings_text <- data_2018$embedding_vector
n_rows <- length(embeddings_text)

# Pre-allocate the master matrix (approx 3.8GB in RAM)
mat_2018 <- matrix(0, nrow = n_rows, ncol = 3072)

# --- 2. UNPACKING STRINGS INTO NUMERIC MATRIX ---
chunk_size <- 5000
for (i in seq(1, n_rows, by = chunk_size)) {
  end_idx <- min(i + chunk_size - 1, n_rows)
  
  # Extract and clean strings
  chunk_strings <- embeddings_text[i:end_idx]
  unpacked <- lapply(chunk_strings, function(x) {
    as.numeric(strsplit(gsub("\\[|\\]", "", x), ",")[[1]])
  })
  
  # Fill the master matrix
  mat_2018[i:end_idx, ] <- do.call(rbind, unpacked)
  
  # Garbage collection to prevent RAM spikes
  rm(chunk_strings, unpacked)
  if (i %% 10000 == 1) gc() 
  
  cat(sprintf("Unpacking Progress: %.1f%% completed\n", (end_idx/n_rows)*100))
}

# Assign IDs
rownames(mat_2018) <- rownames(data_2018)

### Save the workspace ###
save.image(file = "2018_pca.RData")

# --- 3. NORMALIZATION (L2 / Unit Sphere) ---
# We do this so PCA focuses on the angle (meaning) rather than vector length
row_norms <- sqrt(rowSums(mat_2018^2))
row_norms[row_norms == 0] <- 1
mat_norm <- mat_2018 / row_norms

# --- 4. DIMENSIONALITY REDUCTION (PCA) ---
# Running irlba on the normalized matrix
set.seed(123) # For reproducibility. By setting the seed to a specific number (like 123),
#you force R to use the exact same sequence of "random" numbers every time.
pca_decomp <- irlba(mat_norm, nv = 250) #Let's try 250. It's the number of vectors | In this case, 80.56% of the variance is explained

# Project the data into the new n-dimensional space
mat_pca <- pca_decomp$u %*% diag(pca_decomp$d)
rownames(mat_pca) <- rownames(data_2018)

# --- 5. VALIDATION ---
# Calculate the percentage of info we kept
# (Sum of eigenvalues of our 200 PCs / Total variance of normalized matrix)
var_explained <- sum(pca_decomp$d^2) / sum(rowSums(mat_norm^2))

cat("\n--- FINAL RESULTS ---\n")
cat("Dimensions reduced from 3072 to 250.\n")
cat("Variance explained by these 250 components:", round(var_explained * 100, 2), "%\n") # With 250 components, we have 80.56% of the variance explained

### Save the workspace ###
save.image(file = "2018_pca.RData")

# Free memory
rm(mat_norm)
rm(mat_2018)
rm(embeddings_text)

### Save the workspace ###
save.image(file = "2018_pca_clean.RData")
