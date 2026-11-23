# =============================================================================
# 01 - Build the combined dataset
# Case Study 1 (thesis section 5.1): Brazilian political actors on Facebook
# =============================================================================
#
# Merges the separate MCL downloads (split by date range and producer list),
# removes duplicate posts, fills in the account ID for Stories, maps producer
# list names to the analysis groups (main_list / sub_list) and adds date
# variables.
#
# Input : the RDS files listed in INPUT_FILES (working directory). Each file
#         carries a `list_names` provenance column (see script 00).
# Output: combined_datasets/brazilian_political_posts_<timestamp>.rds
# =============================================================================

library(tidyverse)
library(lubridate)

cat("\n", strrep("=", 72), "\n", sep = "")
cat("BUILD DATASET - BRAZILIAN POLITICAL CONTENT\n")
cat(strrep("=", 72), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

# Raw downloads (rename your local files to match, or edit the names here).
# The two MP lists were downloaded in four date slices; the two Prominent
# Politicians lists were downloaded separately. The suffix is the download date.
INPUT_FILES <- list(
  mps_slice1         = "mps_2021-01-01_to_2021-10-31_dl2026-02-25.rds",
  mps_slice2         = "mps_2021-11-01_to_2022-08-31_dl2026-02-25.rds",
  mps_slice3         = "mps_2022-09-01_to_2024-04-30_dl2026-03-05.rds",
  mps_slice4         = "mps_2024-05-01_to_2025-12-31_dl2026-02-25.rds",
  prominent          = "prominent_2021-01-01_to_2025-12-31_dl2026-02-25.rds",
  prominent_extra    = "prominent_extra_2021-01-01_to_2025-12-31_dl2026-03-14.rds"  # output of script 00
)

OUTPUT_DIR <- "combined_datasets"

# MCL producer list name -> analysis group.
# Both Prominent Politicians lists map to the same group.
LIST_MAPPINGS <- list(
  "BR_MP_FB_elected2022" = list(
    main_list = "Brazilian_MPs",
    sub_list = "MPs_Elected_2022"
  ),
  "BR_MP_FB_active2021_reelected2022" = list(
    main_list = "Brazilian_MPs",
    sub_list = "MPs_Active2021_Reelected2022"
  ),
  "BR_prominent_politicians" = list(
    main_list = "Prominent_Politicians",
    sub_list = "Prominent_Politicians"
  ),
  "BR_FB_prominent_politicians1" = list(
    main_list = "Prominent_Politicians",
    sub_list = "Prominent_Politicians"
  )
)

cat("Configuration:\n")
cat("  Input files:", length(INPUT_FILES), "\n")
cat("  Output directory:", OUTPUT_DIR, "\n")
cat("  Producer lists to merge:", length(LIST_MAPPINGS), "\n\n")

# =============================================================================
# STEP 0: Load and merge all downloads
# =============================================================================

cat("STEP 0: Merging individual user downloads...\n")
cat(strrep("-", 72), "\n\n", sep = "")

# Load every file that exists; missing files are reported and skipped
cat("Loading individual files:\n")
all_data <- list()

for (name in names(INPUT_FILES)) {
  filepath <- INPUT_FILES[[name]]

  if (!file.exists(filepath)) {
    cat("  File not found:", filepath, "\n")
    next
  }

  cat("  Loading", name, "...", "")
  data <- readRDS(filepath)
  all_data[[name]] <- data
  cat(format(nrow(data), big.mark = ","), "posts\n")
}
cat("\n")

if (length(all_data) == 0) {
  stop("No files were loaded. Please check file paths.")
}

# Report which producer lists each file contains
cat("List membership per file:\n")
for (name in names(all_data)) {
  lists <- unique(all_data[[name]]$list_names)
  cat("  ", name, ":", paste(lists, collapse = ", "), "\n")
}
cat("\n")

# Stack the files (bind_rows tolerates differing columns), parse timestamps,
# keep the first copy of each post ID, and drop posts before 2021-01-01 or
# after the current date
cat("Merging all datasets...\n")
posts_data <- bind_rows(all_data) %>%
  mutate(
    creation_time = as_datetime(creation_time),
    date = as.Date(creation_time)
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(
    creation_time >= as_datetime("2021-01-01"),
    creation_time <= Sys.Date()
  )

cat("\nMerged all datasets\n")
cat("  Total posts:", format(nrow(posts_data), big.mark = ","), "\n")
cat("  Date range:", min(posts_data$date, na.rm = TRUE), "to",
    max(posts_data$date, na.rm = TRUE), "\n")
cat("  Columns:", ncol(posts_data), "\n\n")

cat("Combined list membership:\n")
print(table(posts_data$list_names, useNA = "ifany"))
cat("\n")

# =============================================================================
# STEP 1: Account ID fallback for Stories
# =============================================================================
# Stories can come back without a surface ID. For Stories the posting account
# is the surface, so the post_owner fields are used to fill the gaps.

cat("STEP 1: Applying surface ID fallback for Stories...\n")
cat(strrep("-", 72), "\n\n", sep = "")

# The account ID column name depends on how the JSON was flattened
surface_id_col <- NULL
possible_cols <- c("surface.id", "surface_id", "surfaceId")
for (col in possible_cols) {
  if (col %in% names(posts_data)) {
    surface_id_col <- col
    break
  }
}

if (is.null(surface_id_col)) {
  cat("Warning: No surface ID column found. Using 'surface.id'...\n")
  surface_id_col <- "surface.id"
}

cat("Using surface ID column:", surface_id_col, "\n")

if ("content_type" %in% names(posts_data)) {

  stories_with_na <- sum(
    is.na(posts_data[[surface_id_col]]) &
      posts_data$content_type == "stories",
    na.rm = TRUE
  )

  if (stories_with_na > 0) {
    cat("Found", stories_with_na, "stories with NA surface.id\n")
    cat("Applying fallback: For stories, post_owner = surface\n")

    # Fill surface ID from post_owner.id
    posts_data <- posts_data %>%
      mutate(
        !!surface_id_col := if_else(
          is.na(.data[[surface_id_col]]) & content_type == "stories" & !is.na(post_owner.id),
          post_owner.id,
          .data[[surface_id_col]]
        )
      )

    # Fill surface name from post_owner.name
    if ("surface.name" %in% names(posts_data) && "post_owner.name" %in% names(posts_data)) {
      posts_data <- posts_data %>%
        mutate(
          surface.name = if_else(
            is.na(surface.name) & content_type == "stories" & !is.na(post_owner.name),
            post_owner.name,
            surface.name
          )
        )
    }

    # Fill surface username from post_owner.username
    if ("surface.username" %in% names(posts_data) && "post_owner.username" %in% names(posts_data)) {
      posts_data <- posts_data %>%
        mutate(
          surface.username = if_else(
            is.na(surface.username) & content_type == "stories" & !is.na(post_owner.username),
            post_owner.username,
            surface.username
          )
        )
    }

    cat("Applied fallback to", stories_with_na, "stories\n\n")
  } else {
    cat("No stories with NA surface.id found\n\n")
  }
} else {
  cat("content_type column not found - skipping stories fallback\n\n")
}

remaining_na <- sum(is.na(posts_data[[surface_id_col]]))
cat("Remaining posts with NA surface ID:", remaining_na, "\n\n")

# =============================================================================
# STEP 2: Map producer lists to analysis groups
# =============================================================================
# If an account belongs to several lists ("A; B"), the FIRST list is used
# here. Remaining overlaps are resolved in script 02.

cat("STEP 2: Standardizing list classifications...\n")
cat(strrep("-", 72), "\n\n", sep = "")

if ("list_names" %in% names(posts_data)) {

  cat("Mapping producer list names to standard categories...\n\n")

  get_main_list <- function(list_name) {
    if (is.na(list_name)) return(NA_character_)
    first_list <- strsplit(list_name, "; ")[[1]][1]
    if (first_list %in% names(LIST_MAPPINGS)) {
      return(LIST_MAPPINGS[[first_list]]$main_list)
    } else {
      # Unknown list: keep its name, with non-alphanumeric characters replaced
      return(gsub("[^[:alnum:]_]", "_", first_list))
    }
  }

  get_sub_list <- function(list_name) {
    if (is.na(list_name)) return(NA_character_)
    first_list <- strsplit(list_name, "; ")[[1]][1]
    if (first_list %in% names(LIST_MAPPINGS)) {
      return(LIST_MAPPINGS[[first_list]]$sub_list)
    } else {
      return(gsub("[^[:alnum:]_]", "_", first_list))
    }
  }

  posts_data <- posts_data %>%
    mutate(
      main_list = sapply(list_names, get_main_list),
      sub_list = sapply(list_names, get_sub_list)
    )

  cat("Applied list mappings\n\n")

  cat("Posts by main_list:\n")
  print(table(posts_data$main_list, useNA = "ifany"))
  cat("\n")

  cat("Posts by sub_list:\n")
  print(table(posts_data$sub_list, useNA = "ifany"))
  cat("\n")

} else {
  # No provenance column: assign a single default group
  cat("'list_names' column not found\n")
  cat("Creating default classification...\n\n")

  posts_data <- posts_data %>%
    mutate(
      main_list = "Brazilian_Political_Content",
      sub_list = "All_Content_2021-2025"
    )

  cat("Set default classification\n\n")
}

# =============================================================================
# STEP 3: Date variables
# =============================================================================
# `date` already exists from STEP 0, so only year/month/week/quarter/year_month
# are added here. `week` uses lubridate's default (weeks start on Sunday).

cat("STEP 3: Adding standardized date/time variables...\n")
cat(strrep("-", 72), "\n\n", sep = "")

if ("date" %in% names(posts_data)) {
  cat("Date variable already exists from merging step\n")

  if (!"year" %in% names(posts_data)) {
    posts_data <- posts_data %>%
      mutate(
        year = year(date),
        month = month(date),
        week = floor_date(date, "week"),
        quarter = quarter(date),
        year_month = format(date, "%Y-%m")
      )
    cat("Added year, month, week, quarter, year_month variables\n")
  } else {
    cat("All date variables already present\n")
  }

} else {
  time_col <- intersect(names(posts_data), c("creation_time", "created_time", "timestamp"))[1]

  if (!is.na(time_col)) {
    cat("Using time column:", time_col, "\n")

    posts_data <- posts_data %>%
      mutate(
        date = as.Date(.data[[time_col]]),
        year = year(.data[[time_col]]),
        month = month(.data[[time_col]]),
        week = floor_date(date, "week"),
        quarter = quarter(date),
        year_month = format(date, "%Y-%m")
      )

    cat("Added date, year, month, week, quarter, year_month variables\n")
  } else {
    cat("Warning: No timestamp column found\n")
  }
}

cat("  Date range:",
    format(min(posts_data$date, na.rm = TRUE), "%Y-%m-%d"), "to",
    format(max(posts_data$date, na.rm = TRUE), "%Y-%m-%d"), "\n\n")

# =============================================================================
# STEP 4: Save the combined dataset
# =============================================================================

cat("STEP 4: Saving combined dataset...\n")
cat(strrep("-", 72), "\n\n", sep = "")

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(OUTPUT_DIR, paste0("brazilian_political_posts_", timestamp, ".rds"))

saveRDS(posts_data, output_file)

cat("Saved combined dataset\n")
cat("  File:", output_file, "\n")
cat("  Total posts:", format(nrow(posts_data), big.mark = ","), "\n")
cat("  Total unique surfaces:",
    format(n_distinct(posts_data[[surface_id_col]], na.rm = TRUE), big.mark = ","), "\n\n")

# =============================================================================
# Summary report
# =============================================================================

cat(strrep("=", 72), "\n", sep = "")
cat("BUILD DATASET COMPLETE - BRAZILIAN POLITICAL CONTENT\n")
cat(strrep("=", 72), "\n\n", sep = "")

# Posts, accounts and date range by main_list
summary_by_list <- posts_data %>%
  group_by(main_list) %>%
  summarise(
    n_posts = n(),
    n_accounts = n_distinct(.data[[surface_id_col]], na.rm = TRUE),
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_posts))

cat("By main_list:\n")
print(summary_by_list)
cat("\n")

# Posts and accounts by sub_list
summary_by_sublist <- posts_data %>%
  group_by(sub_list) %>%
  summarise(
    n_posts = n(),
    n_accounts = n_distinct(.data[[surface_id_col]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_posts))

cat("By sub_list:\n")
print(summary_by_sublist)
cat("\n")

# Posts and accounts by year and main_list
if ("year" %in% names(posts_data)) {
  summary_by_year <- posts_data %>%
    group_by(year, main_list) %>%
    summarise(
      n_posts = n(),
      n_accounts = n_distinct(.data[[surface_id_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year, desc(n_posts))

  cat("By year and main_list:\n")
  print(summary_by_year)
  cat("\n")
}

# Check that the columns needed downstream are present
cat("Key columns present:\n")
key_cols <- c(surface_id_col, "main_list", "sub_list", "date", "creation_time",
              "statistics.views", "statistics.reaction_count",
              "statistics.share_count", "statistics.comment_count")
for (col in key_cols) {
  present <- col %in% names(posts_data)
  cat("  ", col, ":", ifelse(present, "yes", "MISSING"), "\n")
}
cat("\n")

cat("Output file:", output_file, "\n")
cat("Next step: run 02_clean_and_impute.R\n")
