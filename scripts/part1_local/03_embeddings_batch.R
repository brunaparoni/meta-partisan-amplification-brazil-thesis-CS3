#!/usr/bin/env Rscript
# =============================================================================
# 03_embeddings_batch.R
# -----------------------------------------------------------------------------
# Creates text-embedding-3-large embeddings (3,072 dimensions) for every item
# classified as "politics" by the fine-tuned classifier (step 02), using the
# OpenAI Batch API.
#
#   - filters: politics only, non-missing / non-empty text, >= 10 characters,
#     >= 2 words;
#   - embeds each distinct text once and re-attaches the vector to duplicates;
#   - resumable: progress is written to disk after every batch;
#   - logs every filtering step (./logs, ./data/filter_report_*.csv).
#
# Input : ./data/classified_dataset_batchapi_<classifier id>.csv
#         (columns: row_id, alltext, predicted_category)
# Output: ./data/political_embeddings_complete_<date>.csv
#
# Credentials are read from environment variables, never from the code:
#   OPENAI_API_KEY, OPENAI_ORG_ID
#
# Usage:  Rscript 03_embeddings_batch.R            (new run)
#         Rscript 03_embeddings_batch.R --resume   (continue an interrupted run)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(cli)
  library(glue)
})

# ==============================================
# CONFIGURATION
# ==============================================

config <- list(
  EMBEDDING_MODEL = "text-embedding-3-large",
  BATCH_SIZE = 25000,                       # max requests per batch job
  EMBEDDING_DIMENSIONS = 3072,
  POLLING_INTERVAL = 300,                   # seconds
  MAX_POLLING_CYCLES = 50,
  MIN_TEXT_LENGTH = 10,
  MIN_WORD_COUNT = 2,
  BATCH_API_PRICE_PER_1K_TOKENS = 0.000065, # check current pricing
  REGULAR_API_PRICE_PER_1K_TOKENS = 0.00013,
  CHARS_PER_TOKEN_ESTIMATE = 4,
  MAX_RETRIES = 3,
  RETRY_DELAY = 5
)

# Classifier id used to name the step-02 output file
CLASSIFIER_ID <- Sys.getenv(
  "CLASSIFIER_ID",
  "ft_gpt-4o-mini-2024-07-18_br-url-politics-classifier-v1"
)

# ==============================================
# LOGGING AND ERROR HANDLING
# ==============================================

dir.create("./logs", showWarnings = FALSE, recursive = TRUE)
log_file <- file.path("./logs", glue("embedding_pipeline_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log"))

log_message <- function(message, level = "INFO", console = TRUE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- glue("[{timestamp}] [{level}] {message}")
  cat(log_entry, "\n", file = log_file, append = TRUE)
  if (console) {
    switch(level,
      "INFO" = cli_alert_info(message),
      "SUCCESS" = cli_alert_success(message),
      "WARNING" = cli_alert_warning(message),
      "ERROR" = cli_alert_danger(message),
      cat(log_entry, "\n")
    )
  }
}

with_retry <- function(expr, max_retries = config$MAX_RETRIES, delay = config$RETRY_DELAY,
                       desc = "operation") {
  for (attempt in 1:max_retries) {
    result <- tryCatch(
      list(success = TRUE, value = expr),
      error = function(e) list(success = FALSE, error = e)
    )
    if (result$success) {
      return(result$value)
    }
    if (attempt < max_retries) {
      log_message(glue("Attempt {attempt}/{max_retries} failed for {desc}: {result$error$message}. Retrying in {delay}s..."), "WARNING")
      Sys.sleep(delay)
    } else {
      log_message(glue("All {max_retries} attempts failed for {desc}: {result$error$message}"), "ERROR")
      stop(result$error)
    }
  }
}

# ==============================================
# FILTER TRACKING
# ==============================================

FilterTracker <- R6::R6Class("FilterTracker",
  public = list(
    steps = list(),
    start_time = NULL,

    initialize = function() {
      self$start_time <- Sys.time()
      self$steps <- list()
    },

    add_step = function(step_name, data_before, data_after, description, filter_details = NULL) {
      n_before <- if (is.data.frame(data_before)) nrow(data_before) else data_before
      n_after <- if (is.data.frame(data_after)) nrow(data_after) else data_after
      n_removed <- n_before - n_after
      removal_rate <- if (n_before > 0) round((n_removed / n_before) * 100, 2) else 0

      step_info <- list(
        step = length(self$steps) + 1,
        name = step_name,
        description = description,
        items_before = n_before,
        items_after = n_after,
        items_removed = n_removed,
        removal_rate = removal_rate,
        timestamp = Sys.time(),
        filter_details = filter_details
      )
      self$steps <- append(self$steps, list(step_info))

      log_message(glue("Filter Step {step_info$step}: {step_name}"), "INFO")
      log_message(glue("  Before: {format(n_before, big.mark=',')} | After: {format(n_after, big.mark=',')} | Removed: {format(n_removed, big.mark=',')} ({removal_rate}%)"), "INFO")
      if (!is.null(filter_details)) {
        for (detail_name in names(filter_details)) {
          log_message(glue("  {detail_name}: {filter_details[[detail_name]]}"), "INFO", console = FALSE)
        }
      }
    },

    get_summary = function() {
      if (length(self$steps) == 0) return(NULL)
      data.frame(
        Step = sapply(self$steps, function(x) x$step),
        Filter_Name = sapply(self$steps, function(x) x$name),
        Items_Before = sapply(self$steps, function(x) x$items_before),
        Items_After = sapply(self$steps, function(x) x$items_after),
        Items_Removed = sapply(self$steps, function(x) x$items_removed),
        Removal_Rate = sapply(self$steps, function(x) paste0(x$removal_rate, "%")),
        stringsAsFactors = FALSE
      )
    },

    save_report = function(output_dir) {
      if (length(self$steps) == 0) {
        log_message("No filter steps to save", "WARNING")
        return(NULL)
      }
      report_file <- file.path(output_dir, glue("filter_report_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv"))
      write_csv(self$get_summary(), report_file)
      log_message(glue("Filter report saved to: {basename(report_file)}"), "SUCCESS")
      report_file
    }
  )
)

# ==============================================
# VALIDATION
# ==============================================

validate_environment <- function() {
  log_message("Validating environment...", "INFO")
  api_key <- Sys.getenv("OPENAI_API_KEY")
  org_id <- Sys.getenv("OPENAI_ORG_ID")
  if (api_key == "" || org_id == "") {
    stop("Please set the OPENAI_API_KEY and OPENAI_ORG_ID environment variables")
  }

  test_response <- with_retry({
    GET(
      "https://api.openai.com/v1/models",
      add_headers("Authorization" = paste("Bearer", api_key), "OpenAI-Organization" = org_id),
      timeout(10)
    )
  }, desc = "API connection test")

  if (http_status(test_response)$category != "Success") {
    stop("Failed to connect to OpenAI API. Please check your credentials.")
  }
  log_message("Environment validation successful", "SUCCESS")
  list(api_key = api_key, org_id = org_id)
}

validate_data_schema <- function(df, required_cols = c("row_id", "alltext", "predicted_category")) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  df %>% mutate(across(all_of(required_cols), as.character))
}

show_memory_usage <- function(label = "") {
  used_mb <- sum(gc(verbose = FALSE)[, 2])
  if (label != "") {
    log_message(glue("[{label}] Memory used: ~{round(used_mb, 1)} MB"), "INFO", console = FALSE)
  }
  if (used_mb > 1000) {
    log_message("High memory usage detected. Running garbage collection...", "WARNING")
    gc(verbose = FALSE, full = TRUE)
  }
}

# ==============================================
# OPENAI API
# ==============================================

create_embedding_batch_request <- function(custom_id, text_content, model_id) {
  if (is.na(text_content) || nchar(text_content) == 0) {
    stop(glue("Invalid text content for custom_id: {custom_id}"))
  }
  jsonlite::toJSON(list(
    custom_id = as.character(custom_id),
    method = "POST",
    url = "/v1/embeddings",
    body = list(model = model_id, input = text_content, encoding_format = "float")
  ), auto_unbox = TRUE)
}

upload_openai_file <- function(file_path, api_key, org_id) {
  log_message(glue("Uploading batch file: {basename(file_path)}"), "INFO")
  if (!file.exists(file_path)) stop(glue("File not found: {file_path}"))
  log_message(glue("File size: {round(file.info(file_path)$size / 1024 / 1024, 2)} MB"), "INFO", console = FALSE)

  response <- with_retry({
    POST(
      "https://api.openai.com/v1/files",
      add_headers("Authorization" = paste("Bearer", api_key), "OpenAI-Organization" = org_id),
      body = list(purpose = "batch", file = upload_file(file_path)),
      encode = "multipart",
      timeout(300)
    )
  }, desc = "file upload")

  if (http_status(response)$category == "Success") {
    result <- content(response, "parsed")
    log_message(glue("File uploaded successfully. File ID: {result$id}"), "SUCCESS")
    return(result$id)
  }
  log_message(glue("File Upload Error: {http_status(response)$message}"), "ERROR")
  NULL
}

create_openai_batch <- function(input_file_id, api_key, org_id, batch_metadata = NULL) {
  log_message(glue("Creating batch job for file ID: {input_file_id}"), "INFO")
  body <- list(input_file_id = input_file_id, endpoint = "/v1/embeddings", completion_window = "24h")
  if (!is.null(batch_metadata)) body$metadata <- batch_metadata

  response <- with_retry({
    POST(
      "https://api.openai.com/v1/batches",
      add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key),
        "OpenAI-Organization" = org_id
      ),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json",
      timeout(60)
    )
  }, desc = "batch creation")

  if (http_status(response)$category == "Success") {
    result <- content(response, "parsed")
    log_message(glue("Batch job created successfully. Batch ID: {result$id}"), "SUCCESS")
    return(result)
  }
  log_message(glue("Batch Creation Error: {content(response, 'text', encoding = 'UTF-8')}"), "ERROR")
  NULL
}

get_openai_batch_status <- function(batch_id, api_key, org_id) {
  response <- tryCatch(
    GET(
      paste0("https://api.openai.com/v1/batches/", batch_id),
      add_headers("Authorization" = paste("Bearer", api_key), "OpenAI-Organization" = org_id),
      timeout(30)
    ),
    error = function(e) {
      log_message(glue("Error checking batch status: {e$message}"), "WARNING")
      NULL
    }
  )
  if (!is.null(response) && http_status(response)$category == "Success") {
    return(content(response, "parsed"))
  }
  NULL
}

download_openai_file_content <- function(file_id, api_key, org_id) {
  log_message(glue("Downloading embeddings file ID: {file_id}"), "INFO")
  response <- with_retry({
    GET(
      paste0("https://api.openai.com/v1/files/", file_id, "/content"),
      add_headers("Authorization" = paste("Bearer", api_key), "OpenAI-Organization" = org_id),
      timeout(600)
    )
  }, desc = "file download")

  if (http_status(response)$category == "Success") {
    log_message("Embeddings file downloaded successfully", "SUCCESS")
    return(content(response, "text", encoding = "UTF-8"))
  }
  log_message(glue("File Download Error: {content(response, 'text', encoding = 'UTF-8')}"), "ERROR")
  NULL
}

# ==============================================
# DATA PROCESSING
# ==============================================

process_political_items <- function(classified_data_file, filter_tracker) {
  log_message("=== Processing Political Items for Embedding ===", "INFO")
  log_message(glue("Loading classified data from: {classified_data_file}"), "INFO")

  data_df <- with_retry({
    read_csv(classified_data_file, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  }, desc = "data loading")
  data_df <- validate_data_schema(data_df)
  show_memory_usage("After loading classified data")

  filter_tracker$add_step(
    "Initial Dataset Load", nrow(data_df), data_df,
    "Loaded classified dataset from CSV file",
    list(
      File_path = basename(classified_data_file),
      Columns = paste(names(data_df), collapse = ", "),
      File_size_MB = round(file.info(classified_data_file)$size / 1024 / 1024, 2)
    )
  )

  filtered_df <- data_df

  # 1. Political category
  before_count <- nrow(filtered_df)
  filtered_df <- filtered_df %>% filter(predicted_category == "politics")
  filter_tracker$add_step(
    "Political Category Filter", before_count, filtered_df,
    "Keep only items classified as 'politics'",
    list(
      Politics_items = sum(data_df$predicted_category == "politics", na.rm = TRUE),
      Other_items = sum(data_df$predicted_category != "politics", na.rm = TRUE)
    )
  )

  # 2. Missing text
  before_count <- nrow(filtered_df)
  filtered_df <- filtered_df %>% filter(!is.na(alltext))
  filter_tracker$add_step("Remove NA Text Content", before_count, filtered_df,
                          "Remove items with missing text content")

  # 3. Empty text
  before_count <- nrow(filtered_df)
  filtered_df <- filtered_df %>% filter(str_trim(alltext) != "")
  filter_tracker$add_step("Remove Empty Text", before_count, filtered_df,
                          "Remove items with only whitespace or empty text")

  # 4. Minimum length
  before_count <- nrow(filtered_df)
  filtered_df <- filtered_df %>% filter(str_length(alltext) >= config$MIN_TEXT_LENGTH)
  filter_tracker$add_step("Minimum Length Filter", before_count, filtered_df,
                          glue("Remove texts shorter than {config$MIN_TEXT_LENGTH} characters"))

  # 5. Minimum word count
  before_count <- nrow(filtered_df)
  filtered_df <- filtered_df %>% filter(str_count(alltext, "\\S+") >= config$MIN_WORD_COUNT)
  filter_tracker$add_step("Minimum Word Count Filter", before_count, filtered_df,
                          glue("Remove texts with fewer than {config$MIN_WORD_COUNT} words"))

  duplicate_info <- handle_duplicates(filtered_df, filter_tracker)

  political_items <- duplicate_info$unique_items %>%
    mutate(
      embedding_id = paste0("embed_", row_number()),
      batch_job_id_embed = NA_character_,
      embedding_completed = FALSE,
      embedding_error = NA_character_,
      embedding_tokens = NA_integer_,
      embedding_timestamp = NA_character_,
      embedding_vector = NA_character_
    )
  filter_tracker$add_step("Add Embedding Metadata", nrow(duplicate_info$unique_items),
                          political_items, "Add columns for tracking embedding progress")

  stats <- calculate_embedding_stats(political_items)
  log_message(glue("Items ready for embedding: {format(nrow(political_items), big.mark=',')}"), "SUCCESS")
  log_message(glue("Estimated cost: ${round(stats$estimated_cost, 2)}"), "INFO")

  list(items = political_items, duplicate_mapping = duplicate_info$mapping, stats = stats)
}

handle_duplicates <- function(df, filter_tracker) {
  before_count <- nrow(df)

  duplicate_mapping <- df %>%
    group_by(alltext) %>%
    summarise(
      count = n(),
      representative_row_id = first(row_id),
      all_row_ids = list(row_id),
      .groups = "drop"
    ) %>%
    arrange(desc(count)) %>%
    filter(count > 1)

  unique_items <- df %>% distinct(alltext, .keep_all = TRUE)

  filter_tracker$add_step(
    "Deduplication (Temporary)", before_count, unique_items,
    "Temporarily remove duplicates to save embedding costs",
    list(
      Duplicate_groups = nrow(duplicate_mapping),
      Total_duplicates = before_count - nrow(unique_items),
      Savings_factor = if (before_count > 0) round(before_count / nrow(unique_items), 2) else 1
    )
  )
  list(unique_items = unique_items, mapping = duplicate_mapping)
}

calculate_embedding_stats <- function(items) {
  avg_chars <- mean(str_length(items$alltext), na.rm = TRUE)
  estimated_tokens <- nrow(items) * ceiling(avg_chars / config$CHARS_PER_TOKEN_ESTIMATE)
  estimated_cost <- (estimated_tokens / 1000) * config$BATCH_API_PRICE_PER_1K_TOKENS
  regular_cost <- (estimated_tokens / 1000) * config$REGULAR_API_PRICE_PER_1K_TOKENS
  list(
    avg_chars = avg_chars,
    avg_words = mean(str_count(items$alltext, "\\S+"), na.rm = TRUE),
    estimated_tokens = estimated_tokens,
    estimated_cost = estimated_cost,
    regular_cost = regular_cost,
    savings = regular_cost - estimated_cost
  )
}

# ==============================================
# BATCH SUBMISSION
# ==============================================

submit_embedding_batches <- function(political_items, api_key, org_id, output_file, filter_tracker) {
  log_message("=== Submitting Embedding Batch Jobs ===", "INFO")

  items_to_embed <- political_items %>% filter(!embedding_completed & is.na(batch_job_id_embed))
  if (nrow(items_to_embed) == 0) {
    log_message("No items need embedding submission", "INFO")
    return(political_items)
  }

  batch_temp_dir <- file.path(tempdir(), glue("embed_batch_{format(Sys.time(), '%Y%m%d%H%M%S')}"))
  dir.create(batch_temp_dir, showWarnings = FALSE, recursive = TRUE)

  num_chunks <- ceiling(nrow(items_to_embed) / config$BATCH_SIZE)
  successful_submissions <- 0
  cli_progress_bar(glue("Submitting {num_chunks} batch jobs"), total = num_chunks)

  for (chunk_idx in 1:num_chunks) {
    cli_progress_update()
    start_idx <- (chunk_idx - 1) * config$BATCH_SIZE + 1
    end_idx <- min(chunk_idx * config$BATCH_SIZE, nrow(items_to_embed))
    current_chunk <- items_to_embed[start_idx:end_idx, ]

    batch_result <- create_and_submit_batch(current_chunk, chunk_idx, num_chunks,
                                            batch_temp_dir, api_key, org_id)
    if (!is.null(batch_result$batch_id)) {
      update_indices <- which(political_items$embedding_id %in% current_chunk$embedding_id)
      political_items$batch_job_id_embed[update_indices] <- batch_result$batch_id
      successful_submissions <- successful_submissions + 1
      write_csv(political_items, output_file)   # save progress
    }
    Sys.sleep(2)   # rate limiting
  }
  cli_progress_done()
  unlink(batch_temp_dir, recursive = TRUE, force = TRUE)

  filter_tracker$add_step(
    "Batch Job Submission", nrow(items_to_embed), successful_submissions * config$BATCH_SIZE,
    "Submit items to OpenAI Batch API",
    list(
      Total_chunks = num_chunks,
      Successful_submissions = successful_submissions,
      Failed_submissions = num_chunks - successful_submissions
    )
  )
  political_items
}

create_and_submit_batch <- function(chunk_data, chunk_idx, total_chunks, temp_dir, api_key, org_id) {
  batch_file <- file.path(temp_dir, glue("batch_{chunk_idx}.jsonl"))
  jsonl_lines <- sapply(seq_len(nrow(chunk_data)), function(i) {
    create_embedding_batch_request(
      custom_id = chunk_data$embedding_id[i],
      text_content = chunk_data$alltext[i],
      model_id = config$EMBEDDING_MODEL
    )
  })
  writeLines(jsonl_lines, batch_file)

  file_id <- upload_openai_file(batch_file, api_key, org_id)
  if (is.null(file_id)) return(list(batch_id = NULL))

  batch_info <- create_openai_batch(file_id, api_key, org_id, list(
    description = glue("Political embeddings chunk {chunk_idx}/{total_chunks}"),
    chunk = as.character(chunk_idx),
    total_chunks = as.character(total_chunks),
    timestamp = as.character(Sys.time())
  ))
  list(batch_id = if (!is.null(batch_info)) batch_info$id else NULL, file_id = file_id)
}

# ==============================================
# POLLING AND RESULTS
# ==============================================

poll_and_download_embeddings <- function(political_items, api_key, org_id, output_file, filter_tracker) {
  log_message("=== Polling for Embedding Results ===", "INFO")

  pending_batches <- political_items %>%
    filter(!is.na(batch_job_id_embed) & !embedding_completed) %>%
    distinct(batch_job_id_embed) %>%
    pull(batch_job_id_embed)

  if (length(pending_batches) == 0) {
    log_message("No pending batch jobs found", "INFO")
    return(political_items)
  }
  log_message(glue("Monitoring {length(pending_batches)} batch jobs"), "INFO")

  for (cycle in 1:config$MAX_POLLING_CYCLES) {
    log_message(glue("Polling cycle {cycle}/{config$MAX_POLLING_CYCLES}"), "INFO")
    completed_in_cycle <- 0
    failed_in_cycle <- 0

    for (batch_id in pending_batches) {
      batch_status <- get_openai_batch_status(batch_id, api_key, org_id)
      if (is.null(batch_status)) {
        log_message(glue("Could not get status for batch {batch_id}"), "WARNING")
        next
      }
      log_message(
        glue("Batch {batch_id}: {batch_status$status} | ",
             "Completed: {batch_status$request_counts$completed %||% 0}/{batch_status$request_counts$total %||% 0}"),
        "INFO", console = FALSE
      )

      if (batch_status$status == "completed") {
        completed_in_cycle <- completed_in_cycle + 1
        if (!is.null(batch_status$output_file_id)) {
          results_content <- download_openai_file_content(batch_status$output_file_id, api_key, org_id)
          if (!is.null(results_content)) {
            political_items <- process_embedding_results(political_items, results_content, batch_id)
            write_csv(political_items, output_file)
            log_message(glue("Processed results for batch {batch_id}"), "SUCCESS")
          }
        }
      } else if (batch_status$status %in% c("failed", "cancelled", "expired")) {
        failed_in_cycle <- failed_in_cycle + 1
        failed_indices <- which(political_items$batch_job_id_embed == batch_id & !political_items$embedding_completed)
        political_items$embedding_error[failed_indices] <- paste("Batch", batch_status$status)
        write_csv(political_items, output_file)
        log_message(glue("Batch {batch_id} failed with status: {batch_status$status}"), "ERROR")
      }
      Sys.sleep(1)
    }

    pending_batches <- political_items %>%
      filter(!is.na(batch_job_id_embed) & !embedding_completed & is.na(embedding_error)) %>%
      distinct(batch_job_id_embed) %>%
      pull(batch_job_id_embed)

    if (length(pending_batches) == 0) {
      log_message("All batch jobs completed!", "SUCCESS")
      break
    }
    log_message(glue("Completed: {completed_in_cycle} | Failed: {failed_in_cycle} | Pending: {length(pending_batches)}"), "INFO")
    log_message(glue("Waiting {config$POLLING_INTERVAL} seconds before next check..."), "INFO")
    Sys.sleep(config$POLLING_INTERVAL)
  }

  final_stats <- list(
    completed = sum(political_items$embedding_completed, na.rm = TRUE),
    failed = sum(!is.na(political_items$embedding_error)),
    pending = sum(!political_items$embedding_completed & is.na(political_items$embedding_error) &
                    !is.na(political_items$batch_job_id_embed))
  )
  filter_tracker$add_step(
    "Embedding Completion", length(pending_batches), final_stats$completed,
    "Process completed embedding batch results",
    list(
      Successfully_embedded = final_stats$completed,
      Failed_embeddings = final_stats$failed,
      Still_pending = final_stats$pending,
      Polling_cycles = cycle
    )
  )
  political_items
}

process_embedding_results <- function(political_items, results_content, batch_id) {
  result_lines <- strsplit(results_content, "\n")[[1]]
  result_lines <- result_lines[nzchar(result_lines)]
  processed_count <- 0
  error_count <- 0
  successful_embeddings <- 0

  for (line in result_lines) {
    tryCatch({
      result_item <- jsonlite::fromJSON(line)
      row_idx <- which(political_items$embedding_id == result_item$custom_id &
                         political_items$batch_job_id_embed == batch_id)
      if (length(row_idx) > 0) {
        if (!is.null(result_item$response) && result_item$response$status_code == 200) {
          embedding_vector <- result_item$response$body$data[[1]]$embedding
          political_items$embedding_vector[row_idx] <- jsonlite::toJSON(embedding_vector, auto_unbox = TRUE)
          political_items$embedding_completed[row_idx] <- TRUE
          political_items$embedding_tokens[row_idx] <- result_item$response$body$usage$total_tokens %||% NA
          political_items$embedding_timestamp[row_idx] <- as.character(Sys.time())
          successful_embeddings <- successful_embeddings + 1
        } else if (!is.null(result_item$error)) {
          political_items$embedding_error[row_idx] <- result_item$error$message %||% "Unknown error"
          error_count <- error_count + 1
        }
        processed_count <- processed_count + 1
      }
    }, error = function(e) {
      log_message(glue("Error processing embedding result: {e$message}"), "WARNING")
      error_count <<- error_count + 1
    })
  }
  log_message(glue("Processed {processed_count} results | Success: {successful_embeddings} | Errors: {error_count}"), "INFO")
  rm(result_lines, results_content)
  gc(verbose = FALSE)
  political_items
}

# ==============================================
# DUPLICATE RE-INSERTION
# ==============================================

reinsert_duplicates <- function(embedded_items, duplicate_mapping, original_data_file, filter_tracker) {
  log_message("=== Re-inserting Duplicates with Shared Embeddings ===", "INFO")
  if (is.null(duplicate_mapping) || nrow(duplicate_mapping) == 0) {
    log_message("No duplicates to re-insert", "INFO")
    return(embedded_items)
  }

  # Same filters as process_political_items(), before deduplication
  before_dedup_data <- read_csv(original_data_file, show_col_types = FALSE) %>%
    filter(
      predicted_category == "politics",
      !is.na(alltext),
      str_trim(alltext) != "",
      str_length(alltext) >= config$MIN_TEXT_LENGTH,
      str_count(alltext, "\\S+") >= config$MIN_WORD_COUNT
    )

  before_count <- nrow(embedded_items)
  expanded_rows <- list()

  for (i in seq_len(nrow(embedded_items))) {
    embedded_item <- embedded_items[i, ]
    matching_rows <- before_dedup_data %>% filter(alltext == embedded_item$alltext)
    for (j in seq_len(nrow(matching_rows))) {
      expanded_row <- matching_rows[j, ]
      expanded_row$embedding_id <- paste0("embed_expanded_", length(expanded_rows) + 1)
      expanded_row$batch_job_id_embed <- embedded_item$batch_job_id_embed
      expanded_row$embedding_completed <- embedded_item$embedding_completed
      expanded_row$embedding_error <- embedded_item$embedding_error
      expanded_row$embedding_tokens <- embedded_item$embedding_tokens
      expanded_row$embedding_timestamp <- embedded_item$embedding_timestamp
      expanded_row$embedding_vector <- embedded_item$embedding_vector
      expanded_row$is_duplicate <- (j > 1)
      expanded_row$original_embedding_id <- embedded_item$embedding_id
      expanded_rows <- append(expanded_rows, list(expanded_row))
    }
  }

  expanded_df <- bind_rows(expanded_rows)
  duplicates_reinserted <- nrow(expanded_df) - before_count
  filter_tracker$add_step(
    "Duplicate Re-insertion", before_count, expanded_df,
    "Re-insert duplicate items with shared embeddings",
    list(
      Duplicates_reinserted = duplicates_reinserted,
      Unique_embeddings = before_count,
      Total_items = nrow(expanded_df),
      Reuse_efficiency = round((duplicates_reinserted / nrow(expanded_df)) * 100, 2)
    )
  )
  expanded_df
}

# ==============================================
# MAIN PIPELINE
# ==============================================

run_embedding_pipeline <- function(resume = FALSE) {
  log_message("=== Political Content Embedding Pipeline ===", "INFO")
  log_message(glue("Start time: {Sys.time()}"), "INFO")

  filter_tracker <- FilterTracker$new()
  show_memory_usage("Initial")
  creds <- validate_environment()

  classified_file <- file.path("./data", glue("classified_dataset_batchapi_{CLASSIFIER_ID}.csv"))
  embeddings_file <- file.path("./data", glue("political_embeddings_{format(Sys.Date(), '%Y%m%d')}.csv"))
  dir.create("./data", showWarnings = FALSE, recursive = TRUE)

  if (!file.exists(classified_file)) stop(glue("Classified dataset not found: {classified_file}"))
  log_message(glue("Input: {basename(classified_file)}"), "INFO")
  log_message(glue("Output: {basename(embeddings_file)}"), "INFO")

  if (file.exists(embeddings_file) && resume) {
    log_message("Resuming from existing embeddings file", "INFO")
    political_items <- read_csv(embeddings_file, show_col_types = FALSE)
    for (col in c("embedding_vector", "embedding_id", "batch_job_id_embed", "embedding_completed",
                  "embedding_error", "embedding_tokens", "embedding_timestamp")) {
      if (!col %in% names(political_items)) {
        political_items[[col]] <- if (col == "embedding_completed") FALSE else NA_character_
      }
    }
    filter_tracker$add_step(
      "Resume from Existing", nrow(political_items), political_items,
      "Loaded existing embeddings file",
      list(
        Completed = sum(political_items$embedding_completed, na.rm = TRUE),
        In_progress = sum(!is.na(political_items$batch_job_id_embed) & !political_items$embedding_completed, na.rm = TRUE),
        Errors = sum(!is.na(political_items$embedding_error))
      )
    )
    duplicate_mapping <- NULL   # rebuilt only on a fresh run
  } else {
    process_result <- process_political_items(classified_file, filter_tracker)
    political_items <- process_result$items
    duplicate_mapping <- process_result$duplicate_mapping
    write_csv(political_items, embeddings_file)
    log_message(glue("Initial data saved to {basename(embeddings_file)}"), "SUCCESS")
  }

  political_items <- submit_embedding_batches(political_items, creds$api_key, creds$org_id,
                                              embeddings_file, filter_tracker)
  political_items <- poll_and_download_embeddings(political_items, creds$api_key, creds$org_id,
                                                  embeddings_file, filter_tracker)

  complete_file <- embeddings_file
  if (!is.null(duplicate_mapping) && nrow(duplicate_mapping) > 0) {
    political_items <- reinsert_duplicates(political_items, duplicate_mapping, classified_file, filter_tracker)
    complete_file <- file.path("./data", glue("political_embeddings_complete_{format(Sys.Date(), '%Y%m%d')}.csv"))
    write_csv(political_items, complete_file)
    log_message(glue("Complete dataset saved to {basename(complete_file)}"), "SUCCESS")
  }

  filter_report_file <- filter_tracker$save_report("./data")
  print_final_summary(political_items, filter_tracker)
  summary_file <- save_summary_json(political_items, filter_tracker)
  log_message(glue("Pipeline completed at {Sys.time()}"), "SUCCESS")

  list(
    data = political_items,
    filter_summary = filter_tracker$get_summary(),
    files = list(embeddings = embeddings_file, complete = complete_file,
                 filter_report = filter_report_file, summary = summary_file, log = log_file)
  )
}

# ==============================================
# REPORTING
# ==============================================

print_final_summary <- function(political_items, filter_tracker) {
  cli_h1("Pipeline Summary")
  total_items <- nrow(political_items)
  completed <- sum(political_items$embedding_completed, na.rm = TRUE)
  errors <- sum(!is.na(political_items$embedding_error))
  pending <- total_items - completed - errors

  cli_alert_info(glue("Total political items: {format(total_items, big.mark=',')}"))
  cli_alert_success(glue("Embeddings completed: {format(completed, big.mark=',')} ({round(completed / total_items * 100, 1)}%)"))
  if (errors > 0) cli_alert_warning(glue("Errors: {errors} ({round(errors / total_items * 100, 1)}%)"))
  if (pending > 0) cli_alert_info(glue("Still pending: {pending} ({round(pending / total_items * 100, 1)}%)"))

  if (completed > 0) {
    total_tokens <- sum(political_items$embedding_tokens[political_items$embedding_completed], na.rm = TRUE)
    actual_cost <- (total_tokens / 1000) * config$BATCH_API_PRICE_PER_1K_TOKENS
    regular_cost <- (total_tokens / 1000) * config$REGULAR_API_PRICE_PER_1K_TOKENS
    cli_h2("Cost Analysis")
    cli_alert_info(glue("Total tokens used: {format(total_tokens, big.mark=',')}"))
    cli_alert_success(glue("Batch API cost: ${round(actual_cost, 2)}"))
    cli_alert_info(glue("Regular API would cost: ${round(regular_cost, 2)} (saved ${round(regular_cost - actual_cost, 2)})"))
  }

  if (length(filter_tracker$steps) > 0) {
    cli_h2("Filter Impact")
    first_step <- filter_tracker$steps[[1]]
    last_step <- filter_tracker$steps[[length(filter_tracker$steps)]]
    reduction <- first_step$items_before - last_step$items_after
    cli_alert_info(glue("Pipeline reduced data by {round(reduction / first_step$items_before * 100, 1)}%"))
    cli_alert_info(glue("From {format(first_step$items_before, big.mark=',')} to {format(last_step$items_after, big.mark=',')} items"))
  }

  total_time <- difftime(Sys.time(), filter_tracker$start_time, units = "mins")
  cli_alert_info(glue("Total processing time: {round(as.numeric(total_time), 1)} minutes"))
}

save_summary_json <- function(political_items, filter_tracker) {
  summary <- list(
    timestamp = Sys.time(),
    config = config,
    statistics = list(
      total_items = nrow(political_items),
      completed_embeddings = sum(political_items$embedding_completed, na.rm = TRUE),
      failed_embeddings = sum(!is.na(political_items$embedding_error)),
      pending_embeddings = sum(!political_items$embedding_completed & is.na(political_items$embedding_error) &
                                 !is.na(political_items$batch_job_id_embed)),
      total_tokens = sum(political_items$embedding_tokens[political_items$embedding_completed], na.rm = TRUE),
      unique_texts = length(unique(political_items$alltext))
    ),
    filter_steps = filter_tracker$steps,
    processing_time_minutes = as.numeric(difftime(Sys.time(), filter_tracker$start_time, units = "mins"))
  )
  summary_file <- file.path("./data", glue("embedding_summary_{format(Sys.time(), '%Y%m%d_%H%M%S')}.json"))
  jsonlite::write_json(summary, summary_file, pretty = TRUE, auto_unbox = TRUE)
  log_message(glue("Summary saved to {basename(summary_file)}"), "SUCCESS")
  summary_file
}

# ==============================================
# UTILITIES
# ==============================================

verify_embeddings <- function(embeddings_file) {
  log_message("=== Verifying Embeddings ===", "INFO")
  if (!file.exists(embeddings_file)) stop(glue("Embeddings file not found: {embeddings_file}"))
  df <- read_csv(embeddings_file, show_col_types = FALSE)

  has_vector <- sum(!is.na(df$embedding_vector))
  cli_h2("Embedding Verification")
  cli_alert_info(glue("Total items: {nrow(df)}"))
  cli_alert_info(glue("Marked as completed: {sum(df$embedding_completed, na.rm = TRUE)}"))
  cli_alert_info(glue("Have embedding vectors: {has_vector}"))

  if (has_vector > 0) {
    sample_vector <- jsonlite::fromJSON(df$embedding_vector[which(!is.na(df$embedding_vector))[1]])
    cli_alert_info(glue("Sample vector dimensions: {length(sample_vector)} (expected {config$EMBEDDING_DIMENSIONS})"))
    if (length(sample_vector) != config$EMBEDDING_DIMENSIONS) cli_alert_warning("Dimension mismatch detected!")
  }
  if (any(!is.na(df$embedding_error))) {
    cli_h3("Error Summary")
    print(df %>% filter(!is.na(embedding_error)) %>% count(embedding_error, sort = TRUE))
  }
  df
}

export_embeddings <- function(embeddings_file, format = c("parquet", "feather", "rds")) {
  format <- match.arg(format)
  df <- read_csv(embeddings_file, show_col_types = FALSE) %>%
    mutate(embedding_parsed = map(embedding_vector, ~ if (!is.na(.x)) jsonlite::fromJSON(.x) else NA))
  base_name <- tools::file_path_sans_ext(embeddings_file)
  output_file <- switch(format,
    "parquet" = { out <- glue("{base_name}.parquet"); arrow::write_parquet(df, out); out },
    "feather" = { out <- glue("{base_name}.feather"); arrow::write_feather(df, out); out },
    "rds"     = { out <- glue("{base_name}.rds"); saveRDS(df, out, compress = "xz"); out }
  )
  log_message(glue("Exported to {basename(output_file)}"), "SUCCESS")
  output_file
}

# ==============================================
# MAIN
# ==============================================

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  run_embedding_pipeline(resume = length(args) > 0 && args[1] == "--resume")
} else {
  cli_h1("Batch embeddings for political content")
  cli_code("results <- run_embedding_pipeline()")
  cli_code("results <- run_embedding_pipeline(resume = TRUE)  # continue an interrupted run")
  cli_code("verify_embeddings('path/to/embeddings.csv')")
  cli_code("export_embeddings('path/to/embeddings.csv', format = 'parquet')")
  cli_alert_info("Set OPENAI_API_KEY and OPENAI_ORG_ID before running")
}
