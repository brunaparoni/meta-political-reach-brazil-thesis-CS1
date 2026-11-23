# =============================================================================
# 04 - Map account IDs back to the MCL producer lists
# Case Study 1 (thesis section 5.1): Brazilian political actors on Facebook
# =============================================================================
#
# Checks that every account in the enriched dataset (script 03) is present in
# at least one of the original MCL producer lists, and records which list(s)
# each account belongs to.
#
# Runs only inside Meta's Secure Research Environment (SRE); the API client
# authenticates through the SRE session, so no API key is needed.
#
# Input : cleaned_data/surface_info_enriched_*.rds (most recent)
#         the producer lists below, retrieved from the MCL API
# Output (cleaned_data/, suffixed with a timestamp):
#   surface_id_producer_list_mapping_BR_*.csv
#   surface_ids_not_in_producer_lists_BR_*.csv (if any)
#   surface_info_with_list_mapping_BR_*.rds / .csv
#   producer_list_summary_BR_*.csv
# =============================================================================

library(reticulate)
library(jsonlite)
library(dplyr)
library(tidyr)

rule <- function(n = 70, ch = "─") strrep(ch, n)

cat("\n", rule(70, "═"), "\n", sep = "")
cat("     SURFACE ID TO PRODUCER LIST MAPPING - BRAZILIAN CONTENT\n")
cat(rule(70, "═"), "\n\n", sep = "")
flush.console()

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
data_dir <- "./cleaned_data"
output_dir <- "./cleaned_data"

# MCL producer lists (list ID -> name, description). Replace the placeholders
# with the IDs of your own MCL lists. The IDs of the lists used in the thesis
# are available from the author on request (MCL lists cannot be exported).
producer_lists_to_check <- list(
  "<PRODUCER_LIST_ID_REELECTED>" = list(
    name = "BR_MP_FB_active2021_reelected2022",
    description = "Brazilian MPs - Active in 2021 and Re-elected in 2022"
  ),
  "<PRODUCER_LIST_ID_ELECTED_2022>" = list(
    name = "BR_MP_FB_elected2022",
    description = "Brazilian MPs - Elected in 2022"
  ),
  "<PRODUCER_LIST_ID_PROMINENT>" = list(
    name = "BR_prominent_politicians",
    description = "Brazilian Prominent Politicians"
  ),
  "<PRODUCER_LIST_ID_PROMINENT_EXTRA>" = list(
    name = "BR_FB_prominent_politicians1",
    description = "Prominent Politicians (non-MPs) extra list"
  )
)

# =============================================================================
# STEP 1: Load the enriched account info
# =============================================================================

cat("STEP 1: Loading enriched surface info...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

enriched_files <- list.files(data_dir,
                             pattern = "surface_info_enriched_.*\\.rds$",
                             full.names = TRUE)
if (length(enriched_files) == 0) {
  stop("No surface_info_enriched_*.rds found in ", data_dir, "\n",
       "Please run 03_enrich_account_info.R first.")
}

enriched_file <- enriched_files[which.max(file.mtime(enriched_files))]
cat("Using:", basename(enriched_file), "\n")
surface_info_enriched <- readRDS(enriched_file)
cat("  Loaded", nrow(surface_info_enriched), "records\n\n")
flush.console()

# Account ID column: surface.id, else api_id, else first column ending in "id"
if ("surface.id" %in% names(surface_info_enriched)) {
  enriched_surface_ids <- unique(as.character(surface_info_enriched$surface.id))
} else if ("api_id" %in% names(surface_info_enriched)) {
  enriched_surface_ids <- unique(as.character(surface_info_enriched$api_id))
} else {
  id_cols <- names(surface_info_enriched)[grepl("id$|^id$", names(surface_info_enriched), ignore.case = TRUE)]
  if (length(id_cols) > 0) {
    cat("Using ID column:", id_cols[1], "\n")
    enriched_surface_ids <- unique(as.character(surface_info_enriched[[id_cols[1]]]))
  } else {
    stop("Cannot find surface ID column in enriched data")
  }
}

cat("Unique surface IDs in enriched data:", length(enriched_surface_ids), "\n\n")
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
# STEP 3: Print the lists to be checked
# =============================================================================

cat("STEP 3: Brazilian producer lists to check against...\n")
cat(rule(), "\n\n", sep = "")
cat("Will check membership in", length(producer_lists_to_check), "producer lists:\n\n")

i <- 1
for (list_id in names(producer_lists_to_check)) {
  list_info <- producer_lists_to_check[[list_id]]
  cat("  ", i, ". ", list_info$name, "\n", sep = "")
  cat("     ID: ", list_id, "\n", sep = "")
  cat("     Description: ", list_info$description, "\n\n", sep = "")
  i <- i + 1
}
flush.console()

# =============================================================================
# STEP 4: Retrieve the account IDs in each producer list
# =============================================================================

cat("STEP 4: Retrieving surface IDs from each producer list...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

producer_list_contents <- list()

for (list_id in names(producer_lists_to_check)) {
  list_info <- producer_lists_to_check[[list_id]]
  list_name <- list_info$name

  cat("Retrieving:", list_name, "(", list_id, ")...\n")
  flush.console()

  # Note: assignments made inside the error handler below are local to that
  # function, so a failed list is simply absent from producer_list_contents.
  tryCatch({
    response <- client$get(path = paste0("lists/producers/", list_id))
    list_data <- fromJSON(response$text, flatten = TRUE)

    # The producer records may sit at different depths of the response
    if (!is.null(list_data$producers$data)) {
      producer_data <- list_data$producers$data
    } else if (!is.null(list_data$producers)) {
      producer_data <- list_data$producers
    } else if (!is.null(list_data$data)) {
      producer_data <- list_data$data
    } else {
      producer_data <- NULL
    }

    if (!is.null(producer_data) && nrow(producer_data) > 0) {
      surface_ids <- as.character(producer_data$id)
      surface_ids <- surface_ids[!is.na(surface_ids) & nchar(surface_ids) > 0]

      producer_list_contents[[list_id]] <- list(
        id = list_id,
        name = list_name,
        description = list_info$description,
        surface_ids = surface_ids,
        count = length(surface_ids)
      )
      cat("  Retrieved", length(surface_ids), "surface IDs\n")
    } else {
      cat("  No surface IDs found\n")
      producer_list_contents[[list_id]] <- list(
        id = list_id,
        name = list_name,
        description = list_info$description,
        surface_ids = character(0),
        count = 0
      )
    }
    flush.console()

  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    flush.console()
    producer_list_contents[[list_id]] <- list(
      id = list_id,
      name = list_name,
      description = list_info$description,
      surface_ids = character(0),
      count = 0,
      error = e$message
    )
  })

  Sys.sleep(0.5)
}
cat("\n")
flush.console()

# =============================================================================
# STEP 5: Mapping table (one row per account)
# =============================================================================

cat("STEP 5: Creating surface ID to producer list mapping...\n")
cat(rule(), "\n\n", sep = "")
flush.console()

mapping_rows <- list()

for (surface_id in enriched_surface_ids) {
  member_of_lists <- character(0)
  member_of_list_names <- character(0)

  for (list_id in names(producer_list_contents)) {
    list_info <- producer_list_contents[[list_id]]
    if (surface_id %in% list_info$surface_ids) {
      member_of_lists <- c(member_of_lists, list_id)
      member_of_list_names <- c(member_of_list_names, list_info$name)
    }
  }

  mapping_rows[[length(mapping_rows) + 1]] <- data.frame(
    surface_id = surface_id,
    in_producer_list = length(member_of_lists) > 0,
    num_lists = length(member_of_lists),
    producer_list_ids = paste(member_of_lists, collapse = "; "),
    producer_list_names = paste(member_of_list_names, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

mapping_table <- bind_rows(mapping_rows)

cat("Mapping table created:\n")
cat("  Total surface IDs:", nrow(mapping_table), "\n")
cat("  In at least one list:", sum(mapping_table$in_producer_list), "\n")
cat("  NOT in any list:", sum(!mapping_table$in_producer_list), "\n\n")
flush.console()

# =============================================================================
# STEP 6: Summary statistics
# =============================================================================

cat("STEP 6: Summary statistics...\n")
cat(rule(), "\n\n", sep = "")

ids_in_lists <- mapping_table$surface_id[mapping_table$in_producer_list]
ids_not_in_lists <- mapping_table$surface_id[!mapping_table$in_producer_list]

cat("MEMBERSHIP SUMMARY:\n")
cat("  Surface IDs in enriched data:    ", length(enriched_surface_ids), "\n")
cat("  Found in producer list(s):       ", length(ids_in_lists),
    sprintf(" (%.1f%%)\n", length(ids_in_lists) / length(enriched_surface_ids) * 100))
cat("  NOT found in any producer list:  ", length(ids_not_in_lists),
    sprintf(" (%.1f%%)\n", length(ids_not_in_lists) / length(enriched_surface_ids) * 100))
cat("\n")

cat("DISTRIBUTION BY NUMBER OF PRODUCER LISTS:\n")
list_distribution <- table(mapping_table$num_lists)
for (num in names(list_distribution)) {
  cat("  In", num, "list(s):", list_distribution[num], "\n")
}
cat("\n")

cat("MEMBERSHIP BY PRODUCER LIST:\n")
for (list_id in names(producer_list_contents)) {
  list_info <- producer_list_contents[[list_id]]
  enriched_in_list <- sum(enriched_surface_ids %in% list_info$surface_ids)

  cat("  ", list_info$name, " (", list_info$description, "):\n", sep = "")
  cat("    List ID: ", list_id, "\n", sep = "")
  cat("    Total in list: ", list_info$count, "\n", sep = "")
  cat("    Enriched IDs in list: ", enriched_in_list, "\n", sep = "")
  cat("    Coverage: ", sprintf("%.1f%%", enriched_in_list / length(enriched_surface_ids) * 100), "\n\n", sep = "")
}
flush.console()

# =============================================================================
# STEP 7: Join the mapping to the enriched account info
# =============================================================================

cat("STEP 7: Merging mapping with enriched data...\n")
cat(rule(), "\n\n", sep = "")

if ("surface.id" %in% names(surface_info_enriched)) {
  surface_info_enriched$surface.id <- as.character(surface_info_enriched$surface.id)

  surface_info_with_mapping <- surface_info_enriched %>%
    left_join(mapping_table, by = c("surface.id" = "surface_id"))

  cat("Merged mapping columns with enriched data\n")
  cat("  Added columns: in_producer_list, num_lists, producer_list_ids, producer_list_names\n\n")
} else {
  surface_info_with_mapping <- NULL
  cat("Could not merge - surface.id column not found\n\n")
}
flush.console()

# =============================================================================
# STEP 8: Save
# =============================================================================

cat("STEP 8: Saving outputs...\n")
cat(rule(), "\n\n", sep = "")

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

mapping_file <- file.path(output_dir, paste0("surface_id_producer_list_mapping_BR_", timestamp, ".csv"))
write.csv(mapping_table, mapping_file, row.names = FALSE)
cat("Mapping table:", basename(mapping_file), "\n")

if (length(ids_not_in_lists) > 0) {
  not_found_df <- data.frame(
    surface_id = ids_not_in_lists,
    note = "Not found in any Brazilian producer list",
    stringsAsFactors = FALSE
  )
  not_found_file <- file.path(output_dir, paste0("surface_ids_not_in_producer_lists_BR_", timestamp, ".csv"))
  write.csv(not_found_df, not_found_file, row.names = FALSE)
  cat("IDs not in lists:", basename(not_found_file), "\n")
}

if (!is.null(surface_info_with_mapping)) {
  merged_rds <- file.path(output_dir, paste0("surface_info_with_list_mapping_BR_", timestamp, ".rds"))
  saveRDS(surface_info_with_mapping, merged_rds)
  cat("Enriched data with mapping (RDS):", basename(merged_rds), "\n")

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

  merged_csv <- file.path(output_dir, paste0("surface_info_with_list_mapping_BR_", timestamp, ".csv"))
  tryCatch({
    safe_write_csv(surface_info_with_mapping, merged_csv)
    cat("Enriched data with mapping (CSV):", basename(merged_csv), "\n")
  }, error = function(e) {
    cat("CSV export failed:", e$message, "\n")
  })
}

# One row per producer list: size and coverage of the enriched IDs
summary_rows <- lapply(producer_list_contents, function(x) {
  data.frame(
    list_id = if (!is.null(x$id)) x$id else NA_character_,
    list_name = if (!is.null(x$name)) x$name else NA_character_,
    description = if (!is.null(x$description)) x$description else NA_character_,
    total_surface_ids = if (!is.null(x$count)) x$count else 0,
    enriched_ids_in_list = sum(enriched_surface_ids %in% x$surface_ids),
    stringsAsFactors = FALSE
  )
})

producer_list_summary <- bind_rows(summary_rows)
producer_list_summary$coverage_pct <- round(
  producer_list_summary$enriched_ids_in_list / length(enriched_surface_ids) * 100, 1
)

summary_file <- file.path(output_dir, paste0("producer_list_summary_BR_", timestamp, ".csv"))
write.csv(producer_list_summary, summary_file, row.names = FALSE)
cat("Producer list summary:", basename(summary_file), "\n")
flush.console()

# =============================================================================
# Final summary
# =============================================================================

cat("\n", rule(70, "═"), "\n", sep = "")
cat("FINAL SUMMARY\n")
cat(rule(70, "═"), "\n\n", sep = "")

cat("VERIFICATION RESULTS:\n")
cat("  Total surface IDs checked:       ", length(enriched_surface_ids), "\n")
cat("  Found in producer list(s):       ", length(ids_in_lists),
    sprintf(" (%.1f%%)\n", length(ids_in_lists) / length(enriched_surface_ids) * 100))
cat("  NOT in any producer list:        ", length(ids_not_in_lists),
    sprintf(" (%.1f%%)\n", length(ids_not_in_lists) / length(enriched_surface_ids) * 100))
cat("\n")

if (length(ids_not_in_lists) == 0) {
  cat("SUCCESS: All surface IDs are in at least one producer list!\n\n")
} else {
  cat("WARNING:", length(ids_not_in_lists), "surface IDs not found in any producer list\n")
  cat("   See surface_ids_not_in_producer_lists_BR_*.csv for details\n\n")
  cat("First few IDs not in lists:\n")
  for (id in head(ids_not_in_lists, 5)) {
    cat("  -", id, "\n")
  }
  if (length(ids_not_in_lists) > 5) {
    cat("  ... and", length(ids_not_in_lists) - 5, "more\n")
  }
  cat("\n")
}

cat("BRAZILIAN PRODUCER LIST COVERAGE:\n")
print(producer_list_summary)
cat("\n")

cat("MAPPING TABLE PREVIEW:\n")
print(head(mapping_table, 10))
cat("\n")
flush.console()
