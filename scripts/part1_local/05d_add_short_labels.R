# =============================================================================
# 05d_add_short_labels.R
# -----------------------------------------------------------------------------
# Cluster labelling, step 4: short topic labels (5-10 words) distilled from the
# long descriptions by gpt-4.1-mini (temperature 0.2). The long label becomes
# `description`; the short one becomes `label`.
#
# Input/output: output/cluster_labels_enriched.rds, output/cluster_labels.csv
# Needs       : OPENAI_API_KEY environment variable
# =============================================================================

library(httr2)

api_key <- Sys.getenv("OPENAI_API_KEY")
if (api_key == "") stop("Set the OPENAI_API_KEY environment variable")

d <- readRDS("output/cluster_labels_enriched.rds")
short_labels <- character(nrow(d))

for (i in seq_len(nrow(d))) {
  messages <- list(
    list(role = "system", content = paste(
      "You generate very short topic labels (5-10 words max) from longer descriptions.",
      "The labels should be concise noun phrases suitable for use in a table or chart.",
      "Return ONLY the short label, nothing else. No quotes, no period."
    )),
    list(role = "user", content = paste0(
      "Shorten this cluster description into a brief topic label (5-10 words):\n\n", d$label[i]
    ))
  )
  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers("Authorization" = paste("Bearer", api_key),
                "Content-Type" = "application/json") |>
    req_body_json(list(model = "gpt-4.1-mini", messages = messages,
                       temperature = 0.2, max_tokens = 30)) |>
    req_timeout(30) |>
    req_retry(max_tries = 5) |>
    req_perform()
  short_labels[i] <- trimws(resp_body_json(resp)$choices[[1]]$message$content)
}

d$description <- d$label
d$label       <- short_labels

saveRDS(d, "output/cluster_labels_enriched.rds")
write.csv(d[, setdiff(names(d), "centroid")], "output/cluster_labels.csv", row.names = FALSE)
