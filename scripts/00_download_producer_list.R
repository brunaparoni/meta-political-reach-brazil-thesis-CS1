# =============================================================================
# 00 - Download posts from Meta Content Library (MCL) producer lists
# Case Study 1 (thesis section 5.1): Brazilian political actors on Facebook
# =============================================================================
#
# Queries one or more MCL producer lists through the MCL API (version 6.0),
# records which list(s) each account belongs to (provenance), and batches the
# accounts so that no single query exceeds the 100K results-per-query limit.
#
# Runs only inside Meta's Secure Research Environment (SRE): the API client
# authenticates through the SRE session, so no API key is needed or stored.
#
# In this version PRODUCER_LISTS contains only the "Prominent Politicians"
# extra list (BR_FB_prominent_politicians1). The other input files merged in
# script 01 were downloaded separately and are not produced by this script.
#
# Output (in OUTPUT_DIR):
#   posts_with_provenance_<date>.rds / .csv
#   producer_provenance_mapping_<date>.csv
#   posts_by_list_over_time.png
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Libraries
# -----------------------------------------------------------------------------
library(reticulate)  # calls the Python MCL client from R
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# -----------------------------------------------------------------------------
# 2. Initialise the MCL API client (Python package available in the SRE)
# -----------------------------------------------------------------------------
cat("Initializing Meta Content Library API client (MCL 6.0)...\n")
flush.console()

client <- import("metacontentlibraryapi")$MetaContentLibraryAPIClient
client$set_default_version(client$LATEST_VERSION)

cat("✓ API client initialized successfully\n\n")
flush.console()

# -----------------------------------------------------------------------------
# 3. Configuration
# -----------------------------------------------------------------------------

# Facebook post fields requested from the API
SELECTED_POST_FIELDS <- c(
  "id",
  "creation_time",
  "text",
  "statistics",
  "content_type",
  "surface",
  "post_owner",
  "is_verified",
  "is_branded_content"
)
API_FIELDS_STRING <- paste(SELECTED_POST_FIELDS, collapse = ",")

# Producer lists to query. Replace the placeholder with the ID of a list you
# created in the MCL UI. The IDs of the lists used in the thesis are
# available from the author on request (MCL lists cannot be exported).
PRODUCER_LISTS <- list(
  list(
    list_id = "<PRODUCER_LIST_ID_PROMINENT_EXTRA>",
    list_name = "BR_FB_prominent_politicians1",
    description = "Prominent Politicians (non-MPs) extra list"
  )
)

# Observation window and batch size
START_DATE <- "2021-01-01"
END_DATE <- "2025-12-31"
MAX_RESULTS_PER_BATCH <- 90000L  # 10K safety margin below the 100K API limit

# Quota handling (MCL 6.0: 500,000 records per 7-day rolling window)
ENABLE_QUOTA_CHECKS <- TRUE
ASSUMED_AVAILABLE_QUOTA <- 500000L  # used only if quota checks are disabled

# Metadata attached to the MCL collection that groups the queries
PROJECT_NAME <- "Political Content Analysis"
PROJECT_DESCRIPTION <- paste0(
  "PURPOSE: Multi-list analysis of political content on Facebook. ",
  "SCOPE: Posts from defined producer lists. ",
  "DATE RANGE: ", START_DATE, " to ", END_DATE, ". ",
  "METHODOLOGY: Batch queries with provenance tracking. ",
  "PI: [Your Name], IRB: [Your IRB Number]"
)

# Output directory. Set MCL_OUTPUT_DIR to choose a location inside the SRE;
# defaults to a folder relative to the working directory.
OUTPUT_DIR <- Sys.getenv("MCL_OUTPUT_DIR", unset = "raw_downloads")
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 4. Helper functions
# -----------------------------------------------------------------------------

# Return the current query budget (GET /budgets) and print a short report.
mcl_check_quota <- function() {
  cat("Checking API query budget...\n")
  flush.console()

  tryCatch({
    response <- client$get(path = "budgets")
    budgets <- fromJSON(response$text, flatten = TRUE)

    queries <- budgets$queries
    queries_used <- as.numeric(queries$total_usage)
    queries_limit <- as.numeric(queries$max_usage_limit)
    queries_avail <- queries_limit - queries_used
    in_progress <- as.numeric(queries$preallocated_rows_for_running_queries)
    usage_pct <- (queries_used / queries_limit) * 100

    cat(strrep("─", 41), "\n", sep = "")
    cat("QUOTA STATUS:\n")
    cat(strrep("─", 41), "\n", sep = "")
    cat(sprintf("Used:      %12s / %s records (%.1f%%)\n",
                format(queries_used, big.mark = ","),
                format(queries_limit, big.mark = ","),
                usage_pct))
    cat(sprintf("Available: %12s records\n",
                format(queries_avail, big.mark = ",")))
    if (in_progress > 0) {
      cat(sprintf("In Progress: %10s records\n",
                  format(in_progress, big.mark = ",")))
    }
    cat(strrep("─", 41), "\n\n", sep = "")
    flush.console()

    if (queries_avail <= 0) {
      cat("WARNING: QUERY QUOTA EXCEEDED\n")
      cat("Wait for quota to reset (7-day rolling window)\n\n")
      flush.console()
    }

    return(list(
      available = queries_avail,
      total_usage = queries_used,
      max_limit = queries_limit,
      in_progress = in_progress,
      usage_pct = usage_pct,
      over_quota = (queries_avail <= 0)
    ))
  }, error = function(e) {
    cat("ERROR: Cannot check quota -", e$message, "\n\n")
    flush.console()
    stop("Quota check failed: ", e$message)
  })
}

# Estimate how many posts a query would return (GET /facebook/posts/estimate).
# The count is read from $estimated_results.
mcl_get_estimate <- function(surface_ids, start_date, end_date) {
  params <- list(
    surface_ids = as.list(as.character(surface_ids)),
    since = as.character(start_date),
    until = as.character(end_date)
  )

  tryCatch({
    response <- client$get(path = "facebook/posts/estimate", params = params)
    estimate_data <- fromJSON(response$text, flatten = TRUE)

    count <- estimate_data$estimated_results
    complete <- estimate_data$expected_complete

    if (is.null(count) || length(count) == 0) {
      return(list(estimated_results = NA_real_, expected_complete = NA, over_limit = NA))
    }

    return(list(
      estimated_results = as.numeric(count),
      expected_complete = isTRUE(complete),
      over_limit = !isTRUE(complete)
    ))
  }, error = function(e) {
    return(list(estimated_results = NA_real_, expected_complete = NA, over_limit = NA))
  })
}

# Retrieve the account (surface) IDs contained in one producer list.
mcl_get_producer_list <- function(list_name, list_id) {
  cat("Retrieving producer list '", list_name, "' (", list_id, ")...\n", sep = "")
  flush.console()

  tryCatch({
    response <- client$get(path = paste0("lists/producers/", list_id))
    producers_json <- fromJSON(response$text, flatten = TRUE)
    producer_data <- producers_json$producers

    if (is.null(producer_data) || nrow(producer_data) == 0) {
      cat("No producers found in list '", list_name, "'\n", sep = "")
      flush.console()
      return(character(0))
    }

    all_ids <- as.character(producer_data$id)
    valid_ids <- all_ids[!is.na(all_ids) & nchar(all_ids) > 0]

    cat("Retrieved", length(valid_ids), "producer IDs from '", list_name, "'\n", sep = "")
    flush.console()
    return(valid_ids)
  }, error = function(e) {
    cat("ERROR: Failed to retrieve producer list '", list_name, "' - ",
        e$message, "\n", sep = "")
    flush.console()
    return(character(0))
  })
}

# Create an MCL collection that groups all queries of this run.
mcl_create_collection <- function(name, description, set_as_default = TRUE) {
  cat("Creating collection:", name, "...\n")
  flush.console()

  tryCatch({
    response <- client$post(
      path = "async/collections",
      body = list(name = name, description = description, default = set_as_default)
    )
    collection_data <- fromJSON(response$text, flatten = TRUE)
    collection_id <- collection_data$id

    cat("Collection created successfully\n")
    cat("  Collection ID:", collection_id, "\n")
    cat("  Name:", name, "\n")
    if (set_as_default) {
      cat("  Set as default: Yes (new queries will go here)\n")
    }
    cat("\n")
    flush.console()

    return(collection_id)
  }, error = function(e) {
    cat("Error creating collection:", e$message, "\n")
    flush.console()
    return(NULL)
  })
}

# Split the accounts into batches whose estimated result count stays below
# max_results. Batches grow one account at a time; if the first test batch is
# already too large, a binary search finds the largest admissible size.
mcl_create_smart_batches <- function(surface_ids, start_date, end_date, max_results = 90000L) {

  cat("\n", strrep("═", 70), "\n", sep = "")
  cat("CREATING SMART BATCHES (Result-based, not count-based)\n")
  cat(strrep("═", 70), "\n\n", sep = "")
  flush.console()

  total_surfaces <- length(surface_ids)
  cat("Total surface IDs:", total_surfaces, "\n")
  cat("Max results per batch:", format(max_results, big.mark = ","), "\n")
  cat("(Safety margin: 10K below API's 100K limit)\n\n")
  flush.console()

  # Phase 1: estimate the first (up to) 20 accounts individually to gauge
  # average activity and choose an initial batch size
  cat("Phase 1: Estimating sample surfaces to gauge activity levels...\n")
  flush.console()

  sample_size <- min(20L, total_surfaces)
  individual_estimates <- numeric(sample_size)

  for (i in 1:sample_size) {
    sid <- surface_ids[i]
    cat("  Estimating", i, "/", sample_size, ":", substr(sid, 1, 20), "...")
    flush.console()

    estimate <- mcl_get_estimate(list(sid), start_date, end_date)

    if (!is.na(estimate$estimated_results)) {
      individual_estimates[i] <- estimate$estimated_results
      cat(" ", format(estimate$estimated_results, big.mark = ","), "posts\n")
    } else {
      individual_estimates[i] <- 0
      cat(" 0 posts (or error)\n")
    }
    flush.console()
  }

  avg_per_surface <- mean(individual_estimates)
  cat("\nAverage posts per surface:", format(round(avg_per_surface), big.mark = ","), "\n")
  flush.console()

  # Initial batch size: capped between 1 and 50 accounts
  if (avg_per_surface > 0) {
    initial_batch_size <- floor(max_results / avg_per_surface)
    initial_batch_size <- max(1L, min(initial_batch_size, 50L))
  } else {
    initial_batch_size <- 25L
  }

  cat("Initial batch size:", initial_batch_size, "surface IDs\n\n")
  flush.console()

  # Phase 2: build and verify batches sequentially
  cat("Phase 2: Creating and verifying batches...\n\n")
  flush.console()

  batches <- list()
  batch_num <- 1L
  processed_count <- 0L

  while (processed_count < total_surfaces) {

    remaining_surfaces <- surface_ids[(processed_count + 1L):total_surfaces]
    remaining_count <- length(remaining_surfaces)

    cat("  --- Starting batch", batch_num, "---\n")
    cat("  Surfaces remaining:", remaining_count, "\n")
    flush.console()

    test_size <- min(initial_batch_size, remaining_count)
    test_batch <- remaining_surfaces[1:test_size]
    cat("  Testing with", test_size, "surfaces...")
    flush.console()

    test_estimate <- mcl_get_estimate(test_batch, start_date, end_date)

    # If the estimate fails, fall back to a single account; skip it if that
    # also fails
    if (is.na(test_estimate$estimated_results)) {
      cat(" estimate failed, trying smaller batch\n")
      flush.console()
      test_size <- 1L
      test_batch <- remaining_surfaces[1]
      test_estimate <- mcl_get_estimate(test_batch, start_date, end_date)

      if (is.na(test_estimate$estimated_results)) {
        cat("  Cannot estimate even single surface, skipping\n")
        flush.console()
        processed_count <- processed_count + 1L
        next
      }
    }

    cat(" ", format(test_estimate$estimated_results, big.mark = ","), "results")
    flush.console()

    if (test_estimate$estimated_results <= max_results) {
      cat(" OK\n")
      flush.console()

      current_batch <- test_batch
      current_estimate <- test_estimate$estimated_results
      current_size <- test_size

      # Grow the batch one account at a time while it stays under the limit
      while (current_size < remaining_count) {
        next_size <- current_size + 1L
        next_batch <- remaining_surfaces[1:next_size]

        cat("  Testing with", next_size, "surfaces...")
        flush.console()

        next_estimate <- mcl_get_estimate(next_batch, start_date, end_date)

        if (is.na(next_estimate$estimated_results)) {
          cat(" estimate failed, keeping current batch\n")
          flush.console()
          break
        }

        cat(" ", format(next_estimate$estimated_results, big.mark = ","), "results")
        flush.console()

        if (next_estimate$estimated_results <= max_results) {
          cat(" OK\n")
          flush.console()
          current_batch <- next_batch
          current_estimate <- next_estimate$estimated_results
          current_size <- next_size
        } else {
          cat(" EXCEEDS LIMIT - stopping growth\n")
          flush.console()
          break
        }
      }

      batches[[batch_num]] <- list(
        surface_ids = current_batch,
        estimated_results = current_estimate
      )
      cat("  Saving batch", batch_num, "with", current_size, "surfaces,",
          format(current_estimate, big.mark = ","), "estimated results\n\n")
      flush.console()

      processed_count <- processed_count + current_size
      batch_num <- batch_num + 1L

    } else {
      cat(" EXCEEDS LIMIT\n")
      flush.console()

      if (test_size == 1L) {
        # A single account above the limit is queried on its own anyway
        cat("  Single surface exceeds limit - including anyway\n")
        flush.console()

        batches[[batch_num]] <- list(
          surface_ids = test_batch,
          estimated_results = test_estimate$estimated_results
        )
        cat("  Saving batch", batch_num, "with 1 surface,",
            format(test_estimate$estimated_results, big.mark = ","), "estimated results\n\n")
        flush.console()

        processed_count <- processed_count + 1L
        batch_num <- batch_num + 1L

      } else {
        # Binary search for the largest batch below the limit
        cat("  Searching for optimal batch size...\n")
        flush.console()

        low <- 1L
        high <- test_size - 1L
        best_batch <- NULL
        best_estimate <- 0
        best_size <- 0L

        while (low <= high) {
          mid <- (low + high) %/% 2L
          mid_batch <- remaining_surfaces[1:mid]

          cat("    Testing", mid, "surfaces...")
          flush.console()

          mid_estimate <- mcl_get_estimate(mid_batch, start_date, end_date)

          if (is.na(mid_estimate$estimated_results)) {
            cat(" estimate failed\n")
            flush.console()
            high <- mid - 1L
            next
          }

          cat(" ", format(mid_estimate$estimated_results, big.mark = ","), "results")
          flush.console()

          if (mid_estimate$estimated_results <= max_results) {
            cat(" OK\n")
            flush.console()
            best_batch <- mid_batch
            best_estimate <- mid_estimate$estimated_results
            best_size <- mid
            low <- mid + 1L
          } else {
            cat(" too large\n")
            flush.console()
            high <- mid - 1L
          }
        }

        if (!is.null(best_batch)) {
          batches[[batch_num]] <- list(
            surface_ids = best_batch,
            estimated_results = best_estimate
          )
          cat("  Saving batch", batch_num, "with", best_size, "surfaces,",
              format(best_estimate, big.mark = ","), "estimated results\n\n")
          flush.console()

          processed_count <- processed_count + best_size
          batch_num <- batch_num + 1L
        } else {
          cat("  Could not create valid batch, skipping surface\n\n")
          flush.console()
          processed_count <- processed_count + 1L
        }
      }
    }

    cat("  Progress:", processed_count, "/", total_surfaces, "surfaces processed\n\n")
    flush.console()
  }

  # Batch summary
  cat(strrep("═", 70), "\n", sep = "")
  cat("BATCH CREATION COMPLETE\n")
  cat(strrep("═", 70), "\n\n", sep = "")
  flush.console()

  total_batched_surfaces <- sum(sapply(batches, function(b) length(b$surface_ids)))
  total_estimated_results <- sum(sapply(batches, function(b) b$estimated_results))

  cat("Total batches created:", length(batches), "\n")
  cat("Total surface IDs processed:", total_batched_surfaces, "/", total_surfaces, "\n")
  cat("Total estimated results:", format(total_estimated_results, big.mark = ","), "\n\n")
  flush.console()

  if (total_batched_surfaces != total_surfaces) {
    cat("WARNING: Not all surfaces were batched!\n")
    cat("   Expected:", total_surfaces, "\n")
    cat("   Actual:", total_batched_surfaces, "\n")
    cat("   Missing:", total_surfaces - total_batched_surfaces, "\n\n")
  } else {
    cat("All surfaces successfully batched!\n\n")
  }
  flush.console()

  cat("Batch details:\n")
  for (i in seq_along(batches)) {
    cat("  Batch", i, ":",
        length(batches[[i]]$surface_ids), "surfaces,",
        format(batches[[i]]$estimated_results, big.mark = ","), "estimated results\n")
  }
  cat("\n")
  flush.console()

  return(batches)
}

# Submit an asynchronous search job (SNAPSHOT mode), wait for completion
# (polling every 10 s, timeout after 60 min), write the result to JSON and
# read it back as a data frame.
mcl_execute_async_search <- function(surface_ids,
                                     start_date,
                                     end_date,
                                     fields,
                                     name,
                                     description,
                                     collection_id = NULL,
                                     output_dir = "mcl_results") {

  cat("Submitting async search for", length(surface_ids), "surfaces...\n")
  flush.console()

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  params <- list(
    surface_ids = as.list(as.character(surface_ids)),
    since = as.character(start_date),
    until = as.character(end_date),
    fields = fields,
    sort = "newest_to_oldest",
    mode = "SNAPSHOT",
    name = name,
    description = description
  )

  tryCatch({
    response <- client$post(path = "facebook/posts/job", params = params)
    initial_response <- fromJSON(response$text, flatten = TRUE)

    job_id <- initial_response$id
    query_id <- initial_response$query_id

    cat("  Job ID:", job_id, "\n")
    cat("  Query ID:", query_id, "\n")
    flush.console()

    # Attach the query to the run's collection
    if (!is.null(collection_id)) {
      cat("  Assigning to collection...\n")
      flush.console()
      client$post(
        path = paste0("async/queries/", query_id),
        body = list(collection_id = collection_id)
      )
      cat("  Assigned to collection:", collection_id, "\n")
      flush.console()
    }

    cat("  Monitoring job status...\n")
    flush.console()

    job <- client$get_async_job(job_id = job_id)
    status <- job$get_status()
    start_time <- Sys.time()

    while (status != "COMPLETE") {
      Sys.sleep(10)
      status <- job$get_status()
      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)

      cat("  Status:", status, "(", elapsed, "min elapsed)\n")
      flush.console()

      if (status == "FAILED") {
        cat("  Job failed!\n")
        flush.console()
        return(list(data = NULL, job_id = job_id, query_id = query_id, status = "FAILED"))
      }

      if (elapsed > 60) {
        cat("  Timeout: Job took longer than 60 minutes\n")
        flush.console()
        return(list(data = NULL, job_id = job_id, query_id = query_id, status = "TIMEOUT"))
      }
    }

    cat("  Job complete!\n")
    flush.console()

    # Write results to disk, then read them back
    filename <- paste0("job_", job_id, ".json")
    cat("  Saving results to file...\n")
    flush.console()
    job$write_data_to_file(directory = output_dir, filename = filename)

    filepath <- file.path(output_dir, filename)
    cat("  Reading data from file...\n")
    flush.console()

    if (file.exists(filepath)) {
      query_data <- fromJSON(filepath, flatten = TRUE)

      if (!is.data.frame(query_data)) {
        query_data <- tryCatch({
          as.data.frame(query_data, stringsAsFactors = FALSE)
        }, error = function(e) {
          bind_rows(query_data)
        })
      }

      if (is.data.frame(query_data) && nrow(query_data) > 0) {
        cat("  Retrieved", format(nrow(query_data), big.mark = ","), "records\n")
        flush.console()
        return(list(
          data = query_data,
          job_id = job_id,
          query_id = query_id,
          collection_id = collection_id,
          status = "COMPLETE"
        ))
      }
    }

    cat("  No data returned\n")
    flush.console()
    return(list(data = NULL, job_id = job_id, query_id = query_id, status = "EMPTY"))

  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    flush.console()
    return(list(data = NULL, job_id = NA, query_id = NA, status = "ERROR", error = e$message))
  })
}

# =============================================================================
# 5. Main workflow
# =============================================================================

cat(strrep("═", 70), "\n", sep = "")
cat("MULTI-LIST QUERY WITH PROVENANCE TRACKING - MCL 6.0\n")
cat(strrep("═", 70), "\n", sep = "")
cat("Project:", PROJECT_NAME, "\n")
cat("Analysis period:", START_DATE, "to", END_DATE, "\n")
cat("Number of producer lists:", length(PRODUCER_LISTS), "\n\n")
flush.console()

# STEP 1: Check that quota is available -------------------------------------
cat("STEP 1: Checking API quota availability...\n")
flush.console()

if (ENABLE_QUOTA_CHECKS) {
  initial_quota <- mcl_check_quota()

  if (initial_quota$over_quota) {
    cat("CANNOT PROCEED - QUOTA EXCEEDED\n")
    cat("Please wait for quota reset (7-day rolling window)\n")
    flush.console()
    stop("Analysis cannot proceed - query quota exceeded.")
  }
} else {
  cat("QUOTA CHECKS DISABLED - Using assumed quota\n")
  cat("Assumed available quota:", format(ASSUMED_AVAILABLE_QUOTA, big.mark = ","), "records\n\n")
  flush.console()

  initial_quota <- list(
    available = ASSUMED_AVAILABLE_QUOTA,
    total_usage = 0,
    max_limit = 500000L,
    over_quota = FALSE
  )
}

# STEP 2: Create a collection for this run ----------------------------------
cat("STEP 2: Creating collection for organizing queries...\n")
flush.console()

collection_id <- mcl_create_collection(
  name = paste0(PROJECT_NAME, " - ", Sys.Date()),
  description = PROJECT_DESCRIPTION,
  set_as_default = TRUE
)

if (is.null(collection_id)) {
  cat("Could not create collection - queries will not be organized\n")
  flush.console()
}

# STEP 3: Retrieve every producer list (one row per account x list) ---------
cat("STEP 3: Retrieving producer lists and building provenance mapping...\n")
flush.console()

all_producers_with_lists <- data.frame()

for (i in seq_along(PRODUCER_LISTS)) {
  list_info <- PRODUCER_LISTS[[i]]
  list_id <- list_info$list_id
  list_name <- list_info$list_name
  description <- list_info$description

  cat("\nProcessing list", i, "/", length(PRODUCER_LISTS), ":\n")
  cat("  Name:", list_name, "\n")
  cat("  Description:", description, "\n")
  flush.console()

  surface_ids <- mcl_get_producer_list(list_name, list_id)

  if (length(surface_ids) > 0) {
    list_df <- data.frame(
      surface_id = surface_ids,
      list_id = list_id,
      list_name = list_name,
      description = description,
      stringsAsFactors = FALSE
    )
    all_producers_with_lists <- bind_rows(all_producers_with_lists, list_df)
  }
}

if (nrow(all_producers_with_lists) == 0) {
  stop("CRITICAL ERROR: No valid producers retrieved from any list.")
}

cat("\nPRODUCER LIST SUMMARY:\n")
cat("Total producer-list combinations:", nrow(all_producers_with_lists), "\n")
cat("Unique producers:", n_distinct(all_producers_with_lists$surface_id), "\n\n")
flush.console()

# STEP 4: Provenance mapping (one row per account) --------------------------
cat("STEP 4: Creating provenance mapping...\n")
flush.console()

producer_provenance <- all_producers_with_lists %>%
  group_by(surface_id) %>%
  summarise(
    n_lists = n(),
    list_ids = paste(list_id, collapse = "; "),
    list_names = paste(list_name, collapse = "; "),
    .groups = "drop"
  )

# One logical column per list: is the account a member of that list?
for (i in seq_along(PRODUCER_LISTS)) {
  list_name <- PRODUCER_LISTS[[i]]$list_name
  col_name <- paste0("in_", gsub("[^[:alnum:]]", "_", list_name))

  producer_provenance[[col_name]] <- sapply(
    producer_provenance$surface_id,
    function(sid) {
      list_name %in% (all_producers_with_lists %>%
                        filter(surface_id == sid) %>%
                        pull(list_name))
    }
  )
}

producer_provenance <- producer_provenance %>%
  mutate(
    list_overlap = case_when(
      n_lists == 1 ~ "Single list",
      n_lists == 2 ~ "Two lists",
      n_lists >= 3 ~ "Three+ lists",
      TRUE ~ "Unknown"
    )
  )

cat("Provenance mapping created\n\n")
cat("PROVENANCE STATISTICS:\n")
cat("Producers in multiple lists:", sum(producer_provenance$n_lists > 1), "\n\n")
cat("Distribution by number of lists:\n")
print(table(producer_provenance$n_lists))
cat("\n")
flush.console()

# STEP 5: Unique account IDs to query ---------------------------------------
cat("STEP 5: Preparing unique surface IDs for querying...\n")
flush.console()

unique_surface_ids <- unique(producer_provenance$surface_id)
total_unique_producers <- length(unique_surface_ids)

cat("Unique surface IDs to query:", total_unique_producers, "\n\n")
flush.console()

# STEP 6: Build result-size-aware batches -----------------------------------
cat("STEP 6: Creating smart batches based on estimated results...\n")
flush.console()

smart_batches <- mcl_create_smart_batches(
  surface_ids = unique_surface_ids,
  start_date = START_DATE,
  end_date = END_DATE,
  max_results = MAX_RESULTS_PER_BATCH
)

total_estimated_usage <- sum(sapply(smart_batches, function(b) b$estimated_results))
total_batched_surfaces <- sum(sapply(smart_batches, function(b) length(b$surface_ids)))

cat("\nBATCH SUMMARY:\n")
cat("Total batches:", length(smart_batches), "\n")
cat("Total estimated results:", format(total_estimated_usage, big.mark = ","), "\n")
cat("Surfaces batched:", total_batched_surfaces, "/", total_unique_producers, "\n\n")
flush.console()

if (ENABLE_QUOTA_CHECKS && total_estimated_usage > initial_quota$available) {
  cat("WARNING: Estimated usage exceeds available quota!\n")
  cat("  Estimated:", format(total_estimated_usage, big.mark = ","), "\n")
  cat("  Available:", format(initial_quota$available, big.mark = ","), "\n")
  cat("  Consider reducing date range or number of producers.\n\n")
  flush.console()
}

# STEP 7: Run one async job per batch ---------------------------------------
cat("STEP 7: Executing batch queries...\n\n")
flush.console()

all_results <- list()
successful_batches <- 0L
failed_batches <- 0L
actual_quota_used <- 0L

for (i in seq_along(smart_batches)) {
  batch <- smart_batches[[i]]

  cat("\n", strrep("─", 70), "\n", sep = "")
  cat("BATCH", i, "/", length(smart_batches), "\n")
  cat("Surfaces:", length(batch$surface_ids), "\n")
  cat("Estimated results:", format(batch$estimated_results, big.mark = ","), "\n")
  flush.console()

  result <- mcl_execute_async_search(
    surface_ids = batch$surface_ids,
    start_date = START_DATE,
    end_date = END_DATE,
    fields = API_FIELDS_STRING,
    name = paste0(PROJECT_NAME, " - Batch ", i, " of ", length(smart_batches)),
    description = paste0(
      "Batch ", i, ": ", length(batch$surface_ids), " surfaces, ",
      "estimated ", format(batch$estimated_results, big.mark = ","), " results"
    ),
    collection_id = collection_id,
    output_dir = OUTPUT_DIR
  )

  if (!is.null(result$data) && nrow(result$data) > 0) {
    all_results[[i]] <- result$data
    successful_batches <- successful_batches + 1L
    actual_quota_used <- actual_quota_used + nrow(result$data)
    cat("\nBatch", i, "completed successfully:",
        format(nrow(result$data), big.mark = ","), "posts\n")
  } else {
    failed_batches <- failed_batches + 1L
    cat("\nBatch", i, "failed or returned no data\n")
  }
  flush.console()

  # Short pause between jobs (rate limiting)
  if (i < length(smart_batches)) {
    cat("\nWaiting 5 seconds before next batch...\n")
    flush.console()
    Sys.sleep(5)
  }
}

cat("\nBATCH EXECUTION COMPLETE\n")
cat("Successful batches:", successful_batches, "/", length(smart_batches), "\n")
cat("Failed batches:", failed_batches, "\n")
cat("Total posts collected:", format(actual_quota_used, big.mark = ","), "\n\n")
flush.console()

# STEP 8: Combine batches and attach provenance to each post ----------------
cat("STEP 8: Combining results and adding provenance information...\n")
flush.console()

if (length(all_results) > 0) {
  all_posts_df <- bind_rows(all_results)
  cat("Combined", format(nrow(all_posts_df), big.mark = ","), "posts from all batches\n")
  flush.console()
} else {
  cat("No data collected from any batch!\n")
  flush.console()
  stop("No data collected - cannot proceed.")
}

# The account ID column name depends on how the JSON was flattened
surface_id_col <- NULL
possible_cols <- c("surface.id", "surface_id", "surfaceId")
for (col in possible_cols) {
  if (col %in% names(all_posts_df)) {
    surface_id_col <- col
    break
  }
}

if (!is.null(surface_id_col)) {
  cat("Using surface ID column:", surface_id_col, "\n")
  flush.console()

  posts_with_provenance <- all_posts_df %>%
    left_join(
      producer_provenance %>% select(surface_id, n_lists, list_names, list_overlap),
      by = setNames("surface_id", surface_id_col)
    )

  cat("Added provenance information to", nrow(posts_with_provenance), "posts\n")
  flush.console()
} else {
  posts_with_provenance <- all_posts_df
  cat("Could not add provenance - surface ID column not found\n")
  flush.console()
}

# STEP 9: Save posts and provenance -----------------------------------------
cat("\nSTEP 9: Saving results...\n")
flush.console()

posts_output_path <- file.path(OUTPUT_DIR, paste0("posts_with_provenance_", Sys.Date(), ".rds"))
saveRDS(posts_with_provenance, posts_output_path)
cat("Saved posts with provenance to:", posts_output_path, "\n")

posts_csv_path <- file.path(OUTPUT_DIR, paste0("posts_with_provenance_", Sys.Date(), ".csv"))
write.csv(posts_with_provenance, posts_csv_path, row.names = FALSE)
cat("Saved CSV to:", posts_csv_path, "\n")

provenance_path <- file.path(OUTPUT_DIR, paste0("producer_provenance_mapping_", Sys.Date(), ".csv"))
write.csv(producer_provenance, provenance_path, row.names = FALSE)
cat("Saved provenance mapping to:", provenance_path, "\n")
flush.console()

# STEP 10: Daily post counts per list (quick visual check) ------------------
cat("\nSTEP 10: Generating visualizations...\n")
flush.console()

if (!is.null(surface_id_col) && "creation_time" %in% names(posts_with_provenance)) {
  ts_by_list <- posts_with_provenance %>%
    filter(!is.na(list_names)) %>%
    mutate(date = as.Date(creation_time)) %>%
    separate_rows(list_names, sep = "; ") %>%
    group_by(date, list_names) %>%
    summarise(n_posts = n(), .groups = "drop")

  if (nrow(ts_by_list) > 0) {
    p1 <- ggplot(ts_by_list, aes(x = date, y = n_posts, color = list_names)) +
      geom_line(linewidth = 1) +
      theme_minimal(base_size = 12) +
      labs(
        title = "Posts Over Time by Producer List",
        subtitle = paste("Data from", START_DATE, "to", END_DATE),
        x = "Date",
        y = "Number of Posts",
        color = "Producer List"
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14)
      )

    plot1_path <- file.path(OUTPUT_DIR, "posts_by_list_over_time.png")
    ggsave(plot1_path, plot = p1, width = 12, height = 6, dpi = 300, bg = "white")
    cat("Saved plot to:", plot1_path, "\n")
    flush.console()
  }
}

# STEP 11: Final summary -----------------------------------------------------
cat("\n", strrep("═", 70), "\n", sep = "")
cat("ANALYSIS SUMMARY\n")
cat(strrep("═", 70), "\n\n", sep = "")

cat("DATA COLLECTION METRICS:\n")
cat("  Producer Lists Queried:", length(PRODUCER_LISTS), "\n")
cat("  Unique Producers:", total_unique_producers, "\n")
cat("  Surfaces Batched:", total_batched_surfaces, "/", total_unique_producers, "\n")
cat("  Posts Collected:", format(nrow(posts_with_provenance), big.mark = ","), "\n")
cat("  Quota Used:", format(actual_quota_used, big.mark = ","), "/",
    format(total_estimated_usage, big.mark = ","), "estimated\n")
cat("  Collection ID:", ifelse(is.null(collection_id), "None", collection_id), "\n\n")

cat("BATCHING METRICS:\n")
cat("  Total batches:", length(smart_batches), "\n")
cat("  Successful:", successful_batches, "\n")
cat("  Failed:", failed_batches, "\n")
cat("  Success rate:", round(100 * successful_batches / length(smart_batches), 1), "%\n\n")
flush.console()

if (ENABLE_QUOTA_CHECKS) {
  cat("FINAL QUOTA STATUS:\n")
  flush.console()
  final_quota <- mcl_check_quota()
}

cat("\nOUTPUT FILES:\n")
cat("  -", posts_output_path, "\n")
cat("  -", posts_csv_path, "\n")
cat("  -", provenance_path, "\n")
if (exists("plot1_path")) {
  cat("  -", plot1_path, "\n")
}
cat("\nDOWNLOAD COMPLETED\n")
flush.console()
