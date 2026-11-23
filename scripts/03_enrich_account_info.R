# =============================================================================
# 03 - Enrich account metadata from the MCL API (v5.0)
# Case Study 1 (thesis section 5.1): Brazilian political actors on Facebook
# =============================================================================
#
# For each account ID produced by script 02, queries the MCL "preview"
# endpoints to retrieve account-level information. IDs are first queried as
# Facebook Pages in batches; IDs not returned as Pages are then tried one by
# one as Groups, Profiles and Events (and once more as a Page).
#
# Runs only inside Meta's Secure Research Environment (SRE); the API client
# authenticates through the SRE session, so no API key is needed.
#
# Input : cleaned_data/surface_ids_for_api_*.csv  (most recent)
#         cleaned_data/surface_info_*.rds         (most recent, optional)
# Output: cleaned_data/surface_info_enriched_<timestamp>.rds / .csv
#         cleaned_data/entity_type_log_<timestamp>.csv
#         cleaned_data/truly_invalid_ids_<timestamp>.csv (if any)
# =============================================================================

library(reticulate)
library(jsonlite)
library(dplyr)
library(tidyr)

rule <- function(n = 70, ch = "─") strrep(ch, n)

cat("\n", rule(70, "═"), "\n", sep = "")
cat("     SURFACE INFO ENRICHMENT SCRIPT v5.0\n")
cat(rule(70, "═"), "\n\n", sep = "")
flush.console()

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
data_dir <- "cleaned_data"
batch_size <- 25L              # IDs per Page query
pause_between_batches <- 2L    # seconds

# =============================================================================
# STEP 1: Load account IDs (and existing account info, if available)
# =============================================================================

cat("STEP 1: Loading input files...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

surface_id_files <- list.files(data_dir,
                               pattern = "surface_ids_for_api_.*\\.csv$",
                               full.names = TRUE)
if (length(surface_id_files) == 0) {
  stop("No surface_ids_for_api_*.csv found in ", data_dir)
}

surface_id_file <- surface_id_files[which.max(file.mtime(surface_id_files))]
cat("Using:", basename(surface_id_file), "\n")
surface_ids_df <- read.csv(surface_id_file, stringsAsFactors = FALSE)
cat("  Loaded", nrow(surface_ids_df), "surface IDs\n\n")
flush.console()

surface_info_files <- list.files(data_dir, pattern = "surface_info_.*\\.rds$", full.names = TRUE)
if (length(surface_info_files) > 0) {
  surface_info_file <- surface_info_files[which.max(file.mtime(surface_info_files))]
  cat("Found surface_info:", basename(surface_info_file), "\n")
  surface_info <- readRDS(surface_info_file)
  cat("  Loaded", nrow(surface_info), "records\n\n")
  flush.console()
} else {
  surface_info <- NULL
}

target_ids <- unique(as.character(surface_ids_df$surface.id))
cat("Total unique IDs:", length(target_ids), "\n\n")
flush.console()

# =============================================================================
# STEP 2: Initialise the MCL API client
# =============================================================================

cat("STEP 2: Initializing API client...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

client <- import("metacontentlibraryapi")$MetaContentLibraryAPIClient
client$set_default_version(client$LATEST_VERSION)
cat("API client initialized\n\n")
flush.console()

# =============================================================================
# STEP 3: Report available quota
# =============================================================================

cat("STEP 3: Checking quota...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

tryCatch({
  response <- client$get(path = "budgets")
  budgets <- fromJSON(response$text, flatten = TRUE)
  cat(sprintf("  Available: %s records\n\n",
              format(budgets$queries$max_usage_limit - budgets$queries$total_usage, big.mark = ",")))
  flush.console()
}, error = function(e) {
  cat("Could not check quota\n\n")
  flush.console()
})

# =============================================================================
# Helper: query one entity type for a vector of IDs
# =============================================================================
# Returns list(success, data, error). `success = TRUE, data = NULL` means the
# call worked but returned no records.
query_entities <- function(ids, entity_type) {

  config <- list(
    page    = list(path = "facebook/pages/preview",    param = "page_ids"),
    group   = list(path = "facebook/groups/preview",   param = "group_ids"),
    profile = list(path = "facebook/profiles/preview", param = "profile_ids"),
    event   = list(path = "facebook/events/preview",   param = "event_ids")
  )[[entity_type]]

  if (is.null(config)) {
    return(list(success = FALSE, data = NULL, error = "Unknown entity type"))
  }

  params <- list()
  params[[config$param]] <- as.list(as.character(ids))

  tryCatch({
    response <- client$get(path = config$path, params = params)
    result <- fromJSON(response$text, flatten = TRUE)

    # Records are normally in result$data (a data frame); fall back to a
    # list-of-records or to the result itself
    data_df <- NULL
    if (!is.null(result$data)) {
      if (is.data.frame(result$data)) {
        data_df <- result$data
      } else if (is.list(result$data) && length(result$data) > 0) {
        data_df <- tryCatch(
          bind_rows(result$data),
          error = function(e) as.data.frame(result$data, stringsAsFactors = FALSE)
        )
      }
    }
    if (is.null(data_df) && is.data.frame(result) && nrow(result) > 0) {
      data_df <- result
    }

    if (!is.null(data_df) && is.data.frame(data_df) && nrow(data_df) > 0) {
      data_df$entity_type <- toupper(entity_type)
      return(list(success = TRUE, data = data_df, error = NULL))
    } else {
      return(list(success = TRUE, data = NULL, error = NULL))
    }
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, error = e$message))
  })
}

# =============================================================================
# STEP 4: Query all IDs as Facebook Pages (batched)
# =============================================================================

cat("STEP 4: Querying as Facebook Pages...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

batches <- split(target_ids, ceiling(seq_along(target_ids) / batch_size))
total_batches <- length(batches)

cat("Total IDs:", length(target_ids), "\n")
cat("Batches:", total_batches, "\n\n")
flush.console()

all_page_data <- list()
ids_not_found_as_pages <- character(0)

for (i in seq_along(batches)) {
  batch_ids <- batches[[i]]

  cat("Batch", i, "/", total_batches, "...")
  flush.console()

  result <- query_entities(batch_ids, "page")

  if (!result$success) {
    # API error on the batch: retry each ID on its own
    cat(" error, retrying individually\n")
    flush.console()

    for (id in batch_ids) {
      single_result <- query_entities(id, "page")
      if (single_result$success && !is.null(single_result$data) && nrow(single_result$data) > 0) {
        all_page_data[[length(all_page_data) + 1]] <- single_result$data
      } else {
        ids_not_found_as_pages <- c(ids_not_found_as_pages, id)
      }
      Sys.sleep(0.3)
    }

  } else if (!is.null(result$data) && nrow(result$data) > 0) {
    cat(" found", nrow(result$data), "\n")
    flush.console()

    all_page_data[[length(all_page_data) + 1]] <- result$data

    # IDs requested but not returned go to the next step
    returned_ids <- as.character(result$data$id)
    missing <- setdiff(batch_ids, returned_ids)
    if (length(missing) > 0) {
      ids_not_found_as_pages <- c(ids_not_found_as_pages, missing)
    }

  } else {
    cat(" no data\n")
    flush.console()
    ids_not_found_as_pages <- c(ids_not_found_as_pages, batch_ids)
  }

  # Rate limiting: short pause after each batch, longer every 30 batches
  if (i < total_batches) Sys.sleep(pause_between_batches)
  if (i %% 30 == 0 && i < total_batches) {
    cat("\nPausing 15 seconds...\n\n")
    flush.console()
    Sys.sleep(15)
  }
}

# Combine Page results (fall back to common columns if bind_rows fails)
page_data <- if (length(all_page_data) > 0) {
  tryCatch(bind_rows(all_page_data), error = function(e) {
    cat("Note: Manual combination needed\n")
    flush.console()
    do.call(rbind, lapply(all_page_data, function(df) {
      df[, intersect(names(df), names(all_page_data[[1]]))]
    }))
  })
} else {
  data.frame()
}

ids_not_found_as_pages <- unique(ids_not_found_as_pages)

cat("\n", rule(), "\n", sep = "")
cat("Pages query complete:\n")
cat("  Found as Pages:", nrow(page_data), "\n")
cat("  Need further checking:", length(ids_not_found_as_pages), "\n")
cat(rule(), "\n\n", sep = "")
flush.console()

# =============================================================================
# STEP 5: Try remaining IDs as Group, Profile, Event, then Page again
# =============================================================================

group_data <- data.frame()
profile_data <- data.frame()
event_data <- data.frame()
truly_invalid_ids <- character(0)
entity_type_log <- data.frame()

if (length(ids_not_found_as_pages) > 0) {

  cat("STEP 5: Checking", length(ids_not_found_as_pages), "IDs for Groups/Profiles/Events...\n")
  cat(rule(), "\n\n", sep = "")
  flush.console()

  group_list <- list()
  profile_list <- list()
  event_list <- list()

  for (i in seq_along(ids_not_found_as_pages)) {
    id <- ids_not_found_as_pages[i]

    if (i %% 25 == 1 || i == length(ids_not_found_as_pages)) {
      cat(sprintf("Checking %d/%d (%.0f%%)...\n",
                  i, length(ids_not_found_as_pages),
                  (i / length(ids_not_found_as_pages)) * 100))
      flush.console()
    }

    found <- FALSE

    # Group
    result <- query_entities(id, "group")
    if (result$success && !is.null(result$data) && nrow(result$data) > 0) {
      group_list[[length(group_list) + 1]] <- result$data
      entity_type_log <- rbind(entity_type_log, data.frame(
        surface_id = id, entity_type = "GROUP",
        name = if ("name" %in% names(result$data)) result$data$name[1] else NA,
        stringsAsFactors = FALSE))
      cat("  GROUP:", id, "\n")
      flush.console()
      found <- TRUE
    }
    Sys.sleep(0.2)

    # Profile
    if (!found) {
      result <- query_entities(id, "profile")
      if (result$success && !is.null(result$data) && nrow(result$data) > 0) {
        profile_list[[length(profile_list) + 1]] <- result$data
        entity_type_log <- rbind(entity_type_log, data.frame(
          surface_id = id, entity_type = "PROFILE",
          name = if ("name" %in% names(result$data)) result$data$name[1] else NA,
          stringsAsFactors = FALSE))
        cat("  PROFILE:", id, "\n")
        flush.console()
        found <- TRUE
      }
      Sys.sleep(0.2)
    }

    # Event
    if (!found) {
      result <- query_entities(id, "event")
      if (result$success && !is.null(result$data) && nrow(result$data) > 0) {
        event_list[[length(event_list) + 1]] <- result$data
        entity_type_log <- rbind(entity_type_log, data.frame(
          surface_id = id, entity_type = "EVENT",
          name = if ("name" %in% names(result$data)) result$data$name[1] else NA,
          stringsAsFactors = FALSE))
        cat("  EVENT:", id, "\n")
        flush.console()
        found <- TRUE
      }
      Sys.sleep(0.2)
    }

    # Page again, individually (in case the batch query missed it)
    if (!found) {
      result <- query_entities(id, "page")
      if (result$success && !is.null(result$data) && nrow(result$data) > 0) {
        all_page_data[[length(all_page_data) + 1]] <- result$data
        entity_type_log <- rbind(entity_type_log, data.frame(
          surface_id = id, entity_type = "PAGE",
          name = if ("name" %in% names(result$data)) result$data$name[1] else NA,
          stringsAsFactors = FALSE))
        cat("  PAGE (retry):", id, "\n")
        flush.console()
        found <- TRUE
      }
      Sys.sleep(0.2)
    }

    if (!found) {
      truly_invalid_ids <- c(truly_invalid_ids, id)
      entity_type_log <- rbind(entity_type_log, data.frame(
        surface_id = id, entity_type = "UNKNOWN", name = NA, stringsAsFactors = FALSE))
    }

    # Rate limiting: 10-second pause every 50 IDs
    if (i %% 50 == 0 && i < length(ids_not_found_as_pages)) {
      cat("\nPausing 10 seconds...\n\n")
      flush.console()
      Sys.sleep(10)
    }
  }

  if (length(group_list) > 0) group_data <- tryCatch(bind_rows(group_list), error = function(e) data.frame())
  if (length(profile_list) > 0) profile_data <- tryCatch(bind_rows(profile_list), error = function(e) data.frame())
  if (length(event_list) > 0) event_data <- tryCatch(bind_rows(event_list), error = function(e) data.frame())

  # Rebuild page_data if the individual retries found more Pages.
  # (Compares number of list elements with number of page_data columns.)
  if (length(all_page_data) > length(page_data)) {
    page_data <- tryCatch(bind_rows(all_page_data), error = function(e) page_data)
  }

  cat("\n", rule(), "\n", sep = "")
  cat("Entity detection complete:\n")
  cat("  Groups:", nrow(group_data), "\n")
  cat("  Profiles:", nrow(profile_data), "\n")
  cat("  Events:", nrow(event_data), "\n")
  cat("  Truly invalid:", length(truly_invalid_ids), "\n")
  cat(rule(), "\n\n", sep = "")
  flush.console()

} else {
  cat("STEP 5: Skipped - all IDs found as Pages\n\n")
  flush.console()
}

# =============================================================================
# STEP 6: Combine all entity types
# =============================================================================

cat("STEP 6: Combining all data...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

if (nrow(page_data) > 0 && !"entity_type" %in% names(page_data)) page_data$entity_type <- "PAGE"
if (nrow(group_data) > 0 && !"entity_type" %in% names(group_data)) group_data$entity_type <- "GROUP"
if (nrow(profile_data) > 0 && !"entity_type" %in% names(profile_data)) profile_data$entity_type <- "PROFILE"
if (nrow(event_data) > 0 && !"entity_type" %in% names(event_data)) event_data$entity_type <- "EVENT"

# Copy the API `id` into a `surface.id` join key
add_surface_id <- function(df) {
  if (nrow(df) > 0 && "id" %in% names(df)) {
    df$surface.id <- as.character(df$id)
  }
  df
}

page_data <- add_surface_id(page_data)
group_data <- add_surface_id(group_data)
profile_data <- add_surface_id(profile_data)
event_data <- add_surface_id(event_data)

# Stack all types (fall back to common columns if bind_rows fails)
all_api_data <- tryCatch({
  bind_rows(page_data, group_data, profile_data, event_data)
}, error = function(e) {
  cat("Manual combination needed\n")
  flush.console()
  all_dfs <- list(page_data, group_data, profile_data, event_data)
  all_dfs <- all_dfs[sapply(all_dfs, function(x) nrow(x) > 0)]
  if (length(all_dfs) == 0) return(data.frame())
  common_cols <- Reduce(intersect, lapply(all_dfs, names))
  do.call(rbind, lapply(all_dfs, function(df) df[, common_cols, drop = FALSE]))
})

cat("Total API records:", nrow(all_api_data), "\n\n")
if (nrow(all_api_data) > 0) {
  cat("Entity type breakdown:\n")
  print(table(all_api_data$entity_type))
  cat("\n")
}
flush.console()

# =============================================================================
# STEP 7: Prefix API columns with "api_" (except the join key and type)
# =============================================================================

cat("STEP 7: Preparing for merge...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

if (nrow(all_api_data) > 0) {
  keep_cols <- c("surface.id", "entity_type")
  rename_cols <- setdiff(names(all_api_data), keep_cols)
  for (col in rename_cols) {
    names(all_api_data)[names(all_api_data) == col] <- paste0("api_", col)
  }
  cat("Columns prepared:", ncol(all_api_data), "\n\n")
  flush.console()
}

# =============================================================================
# STEP 8: Join API data to the existing account info
# =============================================================================

cat("STEP 8: Merging with surface_info...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

if (nrow(all_api_data) > 0) {
  if (!is.null(surface_info)) {
    surface_info$surface.id <- as.character(surface_info$surface.id)
    all_api_data$surface.id <- as.character(all_api_data$surface.id)

    surface_info_enriched <- surface_info %>%
      left_join(all_api_data, by = "surface.id")

    cat("Merged successfully\n")
    cat("  Rows:", nrow(surface_info_enriched), "\n")
    cat("  Columns:", ncol(surface_info_enriched), "\n\n")
  } else {
    surface_ids_df$surface.id <- as.character(surface_ids_df$surface.id)
    surface_info_enriched <- surface_ids_df %>%
      left_join(all_api_data, by = "surface.id")
    cat("Created enriched dataset\n\n")
  }
} else {
  surface_info_enriched <- surface_info
  cat("No API data to merge\n\n")
}
flush.console()

# =============================================================================
# STEP 9: Save
# =============================================================================

cat("STEP 9: Saving outputs...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Write a CSV after converting list columns to JSON strings
safe_write_csv <- function(df, filepath) {
  df_copy <- df
  for (col in names(df_copy)) {
    if (is.list(df_copy[[col]])) {
      df_copy[[col]] <- sapply(df_copy[[col]], function(x) {
        if (is.null(x) || length(x) == 0) NA_character_
        else tryCatch(as.character(toJSON(x, auto_unbox = TRUE)),
                      error = function(e) paste(x, collapse = "; "))
      })
    }
  }
  write.csv(df_copy, filepath, row.names = FALSE, fileEncoding = "UTF-8")
}

if (!is.null(surface_info_enriched)) {
  rds_file <- file.path(data_dir, paste0("surface_info_enriched_", timestamp, ".rds"))
  saveRDS(surface_info_enriched, rds_file)
  cat("RDS:", basename(rds_file), "\n")

  csv_file <- file.path(data_dir, paste0("surface_info_enriched_", timestamp, ".csv"))
  tryCatch({
    safe_write_csv(surface_info_enriched, csv_file)
    cat("CSV:", basename(csv_file), "\n")
  }, error = function(e) {
    cat("CSV failed:", e$message, "\n")
  })
  flush.console()
}

if (nrow(entity_type_log) > 0) {
  log_file <- file.path(data_dir, paste0("entity_type_log_", timestamp, ".csv"))
  write.csv(entity_type_log, log_file, row.names = FALSE)
  cat("Entity log:", basename(log_file), "\n")
}

if (length(truly_invalid_ids) > 0) {
  invalid_file <- file.path(data_dir, paste0("truly_invalid_ids_", timestamp, ".csv"))
  write.csv(data.frame(surface_id = truly_invalid_ids), invalid_file, row.names = FALSE)
  cat("Invalid IDs:", basename(invalid_file), "\n")
}
flush.console()

# =============================================================================
# Summary
# =============================================================================

cat("\n", rule(70, "═"), "\n", sep = "")
cat("SUMMARY\n")
cat(rule(70, "═"), "\n\n", sep = "")

cat("Input: ", length(target_ids), " IDs\n\n", sep = "")
cat("Results by Entity Type:\n")
cat("  Pages:    ", nrow(page_data), "\n", sep = "")
cat("  Groups:   ", nrow(group_data), "\n", sep = "")
cat("  Profiles: ", nrow(profile_data), "\n", sep = "")
cat("  Events:   ", nrow(event_data), "\n", sep = "")
cat("  Invalid:  ", length(truly_invalid_ids), "\n\n", sep = "")

total_found <- nrow(page_data) + nrow(group_data) + nrow(profile_data) + nrow(event_data)
cat("Total found: ", total_found, " / ", length(target_ids),
    " (", sprintf("%.1f", (total_found / length(target_ids)) * 100), "%)\n\n", sep = "")

cat("Load enriched data with:\n")
cat("  data <- readRDS('", rds_file, "')\n\n", sep = "")
flush.console()
