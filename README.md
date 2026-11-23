# Partisan amplification of political URLs on Brazilian Facebook (2018 and 2022)

R and Python code for **Case Study 3** of the PhD thesis *The Algorithmic Reconfiguration of Visibility in Brazilian Political Communication within Meta's Political Content Reduction Policy* (Bruna Almeida Paroni, Università degli Studi di Urbino Carlo Bo, PhD Programme in Studi Umanistici, cycle 39, academic year 2025/2026), Chapter 5, section 5.3.

The pipeline does three things:

1. It identifies the political URLs shared on Facebook in Brazil during the 2018 and 2022 general-election campaigns (July–October of each year) and groups them into topical clusters. The steps are a fine-tuned GPT-4o mini classifier, `text-embedding-3-large` embeddings, truncated SVD and k-means, and cluster labels written by gpt-4.1-mini, following the LLM-in-the-loop design of Marino and Giglietto (2024).
2. It scores the partisanship of each URL's source and of each cluster with the Media Partisan Alignment Score (MPAS) of Santos (2023).
3. Inside Meta's Secure Research Environment (SRE), it estimates how partisanship shapes the relationship between views and shares, in both directions, with bias-corrected regressions for differentially private data.

## Data availability

**No data are included.** Part 1 uses the public metadata (titles and descriptions) of the Brazilian URLs in Meta's URL Shares release. Part 2 uses the privacy-protected Condor URL Shares engagement data (Messing et al., 2020), which can only be accessed and analysed inside the SRE; raw records cannot be exported. Intermediate files, model objects and API batch files are excluded by `.gitignore`.

The OpenAI scripts read credentials from environment variables. The SRE scripts authenticate through the SRE session. No API keys or passwords are stored in the code.

## Analysed sample

| | 2018 | 2022 | Total |
|---|---|---|---|
| URLs classified | 350,388 | 115,585 | 465,973 |
| Share classified as politics | 45.6% | 54.2% | |
| Political corpus (after de-duplication) | 159,888 | 62,670 | 222,558 |
| Clustered URLs (with an embedding) | 159,836 | 62,650 | 222,486 |
| Clusters | 200 | 200 | 400 |
| Clusters with an MPAS score | 199 | 154 | 353 |
| URLs in scored clusters, with engagement (environment level) | 158,486 | 47,675 | 206,161 |
| URLs with a source MPAS score (source level) | 92,998 | 17,624 | 110,622 |

- 72 political items have no embedding (`embedding_vector` missing) and are left out of the clustering.
- 221,849 clustered URLs return engagement data in the SRE.

## Pipeline

Part 1 runs locally, from the repository root, with the inputs in `rawdata/` and `data/`; outputs go to `output/`. Part 2 runs inside the SRE (private environment, Jupyter) from a project folder containing `rawdata/`, `data/`, `chunks_BR/`, `results/` and `figures/`.

### Part 1: corpus, clusters and partisanship (`scripts/part1_local/`)

| Script | What it does |
|---|---|
| `00_build_corpus_and_sample.R` | Builds `alltext` (title + description) and draws the annotation sample (seed 1974). |
| — | Annotation and fine-tuning of the classifier, and classification of the corpus (not included; see below). |
| `03_embeddings_batch.R` | `text-embedding-3-large` embeddings of the political items through the OpenAI Batch API (3,072 dimensions). |
| `04b_svd_2018.R` | 2018: L2 normalisation and truncated SVD (`irlba`) to 250 components. |
| `04c_bayesian_optimisation_2018.R` | 2018: Bayesian optimisation of k in [100, 200]. |
| `04d_elbow_2018.R` | 2018: elbow curve, k = 1–500, on a sample of 16,000 items. |
| `04e_validate_k_2018.R` | 2018: silhouette, Davies–Bouldin and Calinski–Harabasz for the candidate k values; size balance at k = 200. |
| `04f_final_kmeans_2018.R` | 2018: final k-means with k = 200. |
| `04g_pipeline_2022.R` | 2022: the same steps, final k-means with k = 200, and export of both years. |
| `04h_k200_vs_k250_2022.R` | 2022: within-cluster sum of squares at k = 200 versus 250. |
| `05a_prepare_cluster_samples.R` | Keeps the 150 densest texts per cluster (mean cosine similarity to the 20 nearest neighbours). |
| `05b_generate_cluster_labels.R` | One descriptive label per cluster-year with gpt-4.1-mini. |
| `05c_enrich_cluster_labels.R` | Centroids, cohesion, top domains and date range per cluster. |
| `05d_add_short_labels.R` | Short topic labels (5–10 words) with gpt-4.1-mini. |
| `06a_extend_mpas.R` | Extends the Santos (2023) domain list with party-median scores (Santos & Albuquerque, 2024). |
| `06c_cluster_aggregation.R` | URLs and domains per cluster, with and without social media. |
| `06d_build_cluster_dataset.R` | Cluster dataset: `weighted_mpas`, `mpas_abs`, coverage, social-media share, Gini, cohesion. |
| `06e_build_url_dataset.R` | URL dataset with the source MPAS. |
| `06f_mpas_threshold_sensitivity.R` | MPAS coverage with n ≥ 1, 10 and 500 citations per domain. |
| `07a_cluster_agenda.R` | Cluster agenda by ideology band, 2018 versus 2022 (section 5.3.3.1). |
| `07b_fig_agenda_plots.R` | Violin and scatter of cluster MPAS weighted by volume (section 5.3.3.1). |
| `07c_fig_mpas_bands_shift.R` | Share of volume in five MPAS bands and the change between years. |
| `07d_appendix_cluster_table.R` | Appendix table of the 400 clusters. |
| `08_prepare_sre_inputs.R` | Cleans the cluster and URL files for upload (353 clusters; 110,851 matched URLs). |

Scripts 04b–04h must run in order in **one R session**, because each uses objects created by the previous one.

### Part 2: models inside the SRE (`scripts/part2_sre/`)

| Script | What it does |
|---|---|
| `00_upload_inputs_SRE.py` | Stages the three input files in the SRE database and writes them to `rawdata/`. |
| `01_load_split_clusters_BR.R` | Splits `year_cluster` (e.g. `2018_125`) into election year and cluster id. |
| `02_extract_engagement_BR.py` | Chunked SQL extraction of per-URL engagement totals, July–October of each year. |
| `03_combine_aggregate_BR.R` | Combines the chunks, joins the clusters and builds the cluster summaries. |
| `04_descriptive_plots_BR.R` | Descriptive figures (optional). |
| `05_prepare_modeling_data_BR.R` | Environment-level file: cluster MPAS and standardised controls. |
| `05b_prepare_url_level_modeling_data_BR.R` | Source-level file: the URL's own MPAS. |
| `06_activation_cluster_intensity_BR.R` | Activation (`url_shares ~ url_views`) × `mpas_abs`, cluster level. |
| `06b_activation_cluster_signed_BR.R` | Activation × `weighted_mpas`, cluster level. |
| `06c_amplification_cluster_BR.R` | Amplification (`url_views ~ url_shares`), M1 and M2, cluster level. |
| `06d_activation_url_BR.R` | Activation, source level. |
| `06e_amplification_url_BR.R` | Amplification, M1 and M2, source level. |
| `07_all_figures_BR.R` | All coefficient figures, read from the saved model results. |

Run order in the SRE: `00` → `01` → `02` → `03` (`04` optional) → `05` → `05b` → `06`–`06e` → `07`.

## Key analytic choices

- **Classifier:** two coders reached κ = 0.852 on 200 posts and annotated 2,000 posts, stratified by month and election year. `gpt-4o-mini-2024-07-18` was fine-tuned on 1,597 posts and validated on 399 (accuracy 0.910; F1 0.902 for politics, 0.917 for other). The prompt is in `prompts/classifier_prompt.md`.
- **Dimensionality reduction:** embeddings are L2-normalised and reduced with a truncated SVD of the uncentred matrix to 250 components. The components keep 80.56% (2018) and 82.19% (2022) of the total sum of squares.
- **Number of clusters:** k-means (Lloyd, 20 starts, up to 100 iterations, seed 123), k = 200 in each year. The within-cluster sum of squares always falls as k grows, so the Bayesian optimisation returns the upper bound of its range. k = 200 therefore rests on the elbow curve, the validation indices, cluster-size balance and comparability between years; at k = 250 the 2022 sum of squares is only 2.49% lower.
- **Cluster labels:** gpt-4.1-mini, temperature 0.3 for the descriptions and 0.2 for the short labels, from the 150 densest texts per cluster.
- **Partisanship:** MPAS from −1 (left) to +1 (right). URLs are matched on the full domain, falling back to the parent domain. Social-media platforms have no score and are excluded from the cluster scores. `weighted_mpas` is the URL-weighted mean of a cluster; `mpas_abs` is its absolute value (intensity).
- **Two levels:** the *source* level uses the URL's own outlet score; the *environment* level uses the score of the URL's cluster. Cluster models are also fitted on a high-coverage sample (`pct_matched_urls` > 50).
- **Two directions:** activation (`url_shares ~ url_views`, shares gained per view) and amplification (`url_views ~ url_shares`, views reached per share). M2 adds `url_clicks` as a control and is the reference specification.
- **Differential privacy:** all models use `PrivacyUnbiased::lmdp()` (Evans & King, 2022) with the per-cell noise standard deviations of the release (views 2,228; clicks 40; likes 22; shares 14; other counts 10). When the variance–covariance matrix is not positive definite, `lmdp()` falls back to a 1,000-draw bootstrap. Because `lmdp()` allows one interaction per model, intensity and direction are fitted separately.
- **Years** are fitted separately and compared with z = (b₂₀₁₈ − b₂₀₂₂) / √(SE₂₀₁₈² + SE₂₀₂₂²).
- **Limitations:** a URL total sums many noisy cells, so passing the per-cell σ under-corrects the noise; cluster-level predictors are constant within 353 clusters while `lmdp()` treats URLs as independent, so their standard errors are likely too small; views, shares and clicks reflect users, ranking and moderation together. The models describe associations and do not identify causal effects.

## Not included

- The annotation and fine-tuning script and the corpus classification script (steps 01–02). Their settings and results are reported above.
- The script that builds the n ≥ 10 version of the Santos (2023) list (`proportions_n10.rds`), read by `06c` and `06f`.

## Software

Part 1 ran in R locally. Main packages: `tidyverse`, `data.table`, `httr2`, `jsonlite`, `Matrix`, `irlba`, `rBayesianOptimization`, `cluster`, `ggplot2`, `patchwork` and `scales`. Part 2 ran in R 4.3 and Python inside the Meta SRE, with `PrivacyUnbiased`, `tidyverse` and `scales`; SQL extraction uses the SRE Python client.

## Configuration

- `OPENAI_API_KEY` and `OPENAI_ORG_ID` (scripts 03, 05b, 05d); `CLASSIFIER_ID` (script 03, optional).
- In `scripts/part2_sre/00_upload_inputs_SRE.py`, the database name (`private_user_<YOUR_USER_ID>`) and the upload mapping ids (`<TIMESTAMP>`) are placeholders. Replace them with the values from your own SRE session.

## License

The code is released under the MIT License (see `LICENSE`).

## Citation

If you use this code, please cite the thesis: Almeida Paroni, B. *The Algorithmic Reconfiguration of Visibility in Brazilian Political Communication within Meta's Political Content Reduction Policy*. PhD thesis, Università degli Studi di Urbino Carlo Bo. The clustering pipeline follows Marino and Giglietto (2024); the partisanship scores come from Santos (2023).
