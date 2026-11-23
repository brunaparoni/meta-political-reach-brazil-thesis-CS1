# =============================================================================
# 02 - Validation, cleaning and imputation of censored views (v3.2)
# Case Study 1 (thesis section 5.1): Brazilian political actors on Facebook
# =============================================================================
#
# Takes the combined dataset from script 01 and:
#   1. checks merge integrity (one group per account, no duplicate posts,
#      consistent account metadata) and resolves conflicts;
#   2. keeps posts inside the study window (2021-01-01 to 2025-12-31);
#   3. imputes censored view counts (MCL returns NA for posts with <= 100
#      views) with a group-specific power-law method (thesis 5.1.2.3);
#   4. removes posts with missing reactions, shares or comments;
#   5. builds post-, account-, week- and month-level datasets.
#
# Weekly aggregation keeps only complete Sunday-to-Saturday weeks
# (3 January 2021 - 27 December 2025, 260 weeks).
#
# Input : combined_datasets/brazilian_political_posts_*.rds (most recent)
# Output (cleaned_data/, all suffixed with a timestamp):
#   cleaned_posts, accounts_summary, accounts_both_periods, posts_both_periods,
#   weekly_aggregation, monthly_aggregation, account_period, surface_info
#   (.rds/.csv), surface_ids_* (account ID lists for script 03), metadata
#
# NA handling:
#   views (NA)              -> imputed (group-specific power law, see STEP 7a)
#   reactions/shares/comments (NA) -> post removed
#   content_type (NA)       -> "mcl_unsupported_attachment"
# =============================================================================

library(tidyverse)
library(lubridate)

# =============================================================================
# Configuration
# =============================================================================

# Study window: posts outside it are dropped
study_start_date <- as.Date("2021-01-01")
study_end_date   <- as.Date("2025-12-31")

# Imputation settings (thesis 5.1.2.3, Table 5.3)
IMPUTATION_BIN_WIDTH <- 50      # bin width for the power-law fit
IMPUTATION_MAX_VIEW <- 500      # fit range is (100, 500]
IMPUTATION_MIN_BINS <- 3        # minimum non-empty bins for a valid fit
IMPUTATION_MIN_POSTS <- 100     # minimum near-threshold posts for a valid fit
POOLED_FALLBACK_RATIO <- 1.0    # ratio used if a fit is rejected (uniform)

# =============================================================================
# Helper functions: complete weeks (weeks start on Sunday, lubridate default)
# =============================================================================

# First Sunday on or after start_date
get_first_complete_week_start <- function(start_date) {
  week_start <- floor_date(start_date, "week")
  if (week_start == start_date) {
    return(start_date)
  } else {
    return(week_start + days(7))
  }
}

# Last Saturday on or before end_date
get_last_complete_week_end <- function(end_date) {
  week_start <- floor_date(end_date, "week")
  week_end <- week_start + days(6)
  if (end_date >= week_end) {
    return(week_end)
  } else {
    return(week_end - days(7))
  }
}

# TRUE if the week starting on week_start lies entirely inside the window
is_complete_week <- function(week_start, study_start, study_end) {
  week_end <- week_start + days(6)
  return(week_start >= study_start & week_end <= study_end)
}

# =============================================================================
# Helper functions: group-specific imputation
# =============================================================================

# Fit a power law to the binned counts of observed views in (100, max_view]
# and return the ratio  integral_1^50 v^alpha dv / integral_50^100 v^alpha dv,
# i.e. the expected share of censored posts in 1-50 relative to 51-100.
# Falls back to `fallback_ratio` if there are too few posts or bins, or if the
# ratio is outside (0, 10].
derive_imputation_ratio <- function(views,
                                    bin_width = IMPUTATION_BIN_WIDTH,
                                    max_view = IMPUTATION_MAX_VIEW,
                                    min_bins = IMPUTATION_MIN_BINS,
                                    min_posts = IMPUTATION_MIN_POSTS,
                                    fallback_ratio = POOLED_FALLBACK_RATIO) {

  near_threshold <- views[views > 100 & views <= max_view]
  n_near <- length(near_threshold)

  if (n_near < min_posts) {
    return(list(
      ratio = fallback_ratio, alpha = NA, r_squared = NA,
      n_near_threshold = n_near, reliable = FALSE,
      reason = paste("Insufficient posts near threshold:", n_near, "<", min_posts)
    ))
  }

  # Eight bins of width 50: (100,150], ..., (450,500]
  breaks <- seq(100, max_view, by = bin_width)
  bins <- cut(near_threshold, breaks = breaks, include.lowest = TRUE)
  bin_counts <- table(bins)
  bin_mids <- breaks[-length(breaks)] + bin_width / 2

  valid <- bin_counts > 0
  n_valid_bins <- sum(valid)

  if (n_valid_bins < min_bins) {
    return(list(
      ratio = fallback_ratio, alpha = NA, r_squared = NA,
      n_near_threshold = n_near, reliable = FALSE,
      reason = paste("Insufficient non-empty bins:", n_valid_bins, "<", min_bins)
    ))
  }

  # OLS on log(count) ~ log(bin midpoint); the slope is alpha
  fit <- lm(log(as.numeric(bin_counts[valid])) ~ log(bin_mids[valid]))
  alpha <- coef(fit)[2]
  r_squared <- summary(fit)$r.squared

  # Closed-form integrals of v^alpha (log case when alpha = -1)
  a <- alpha + 1
  if (abs(a) > 1e-10) {
    integral_1_50 <- (50^a - 1^a) / a
    integral_50_100 <- (100^a - 50^a) / a
  } else {
    integral_1_50 <- log(50) - log(1)
    integral_50_100 <- log(100) - log(50)
  }

  ratio <- integral_1_50 / integral_50_100

  if (is.na(ratio) || ratio <= 0 || ratio > 10) {
    return(list(
      ratio = fallback_ratio, alpha = alpha, r_squared = r_squared,
      n_near_threshold = n_near, reliable = FALSE,
      reason = paste("Derived ratio out of range:", round(ratio, 3))
    ))
  }

  return(list(
    ratio = ratio, alpha = alpha, r_squared = r_squared,
    n_near_threshold = n_near, reliable = TRUE,
    reason = "Successfully derived from data"
  ))
}

# Draw n integer view counts in [1, 100]: first choose the lower half (1-50)
# with probability ratio / (ratio + 1), then draw uniformly within the half.
generate_imputed_values <- function(n, ratio, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (n == 0) return(integer(0))

  p_low <- ratio / (ratio + 1)
  in_low_half <- runif(n) < p_low

  values <- integer(n)
  n_low <- sum(in_low_half)
  n_high <- n - n_low

  if (n_low > 0) {
    values[in_low_half] <- sample(1:50, n_low, replace = TRUE)
  }
  if (n_high > 0) {
    values[!in_low_half] <- sample(51:100, n_high, replace = TRUE)
  }

  return(values)
}

# Expected mean/median of imputed values for a given ratio, estimated by
# simulation (100,000 draws). NOTE: this function calls set.seed(12345), which
# resets the global random number generator (see STEP 7a).
compute_imputation_stats <- function(ratio, n_samples = 100000) {
  set.seed(12345)
  values <- generate_imputed_values(n_samples, ratio)

  list(
    mean = round(mean(values), 1),
    median = median(values),
    sd = round(sd(values), 1),
    pct_under_50 = round(100 * mean(values <= 50), 1),
    p_low = round(100 * ratio / (ratio + 1), 1)
  )
}

# Small helper for console rules
rule <- function(n = 80, ch = "=") strrep(ch, n)

cat("\n", rule(), "\n", sep = "")
cat("DATA VALIDATION AND CLEANING (v3.2 - GROUP-SPECIFIC IMPUTATION)\n")
cat("WITH COMPLETE WEEKS FILTERING\n")
cat(rule(), "\n\n", sep = "")

# =============================================================================
# STEP 1: Load the most recent combined dataset
# =============================================================================

cat("STEP 1: Loading combined dataset...\n")
cat(rule(80, "-"), "\n\n", sep = "")

combined_files <- list.files("combined_datasets",
                             pattern = "brazilian_political_posts_.*\\.rds$",
                             full.names = TRUE)

if (length(combined_files) == 0) {
  stop("No combined dataset found in combined_datasets/ directory.\n",
       "Please run 01_build_dataset.R first.")
}

data_file <- combined_files[which.max(file.mtime(combined_files))]
cat("Loading:", basename(data_file), "\n")
cat("File date:", format(file.mtime(data_file), "%Y-%m-%d %H:%M:%S"), "\n\n")

data <- readRDS(data_file)

cat("Loaded data\n")
cat("  Total posts:", nrow(data), "\n")
cat("  Columns:", ncol(data), "\n")
cat("  Unique surface.id:", n_distinct(data$surface.id), "\n")
cat("  Unique post id:", n_distinct(data$id), "\n\n")

# =============================================================================
# STEP 2: Merge integrity checks
# =============================================================================

cat("STEP 2: Validating merge integrity (CRITICAL CHECKS)...\n")
cat(rule(80, "-"), "\n\n", sep = "")

integrity_errors <- list()
integrity_warnings <- list()

# --- CHECK 2.1: each account must belong to exactly one sub_list -----------

cat("CHECK 2.1: Verifying each account (surface.id) has exactly ONE sub_list...\n\n")

surface_list_mapping <- data %>%
  group_by(surface.id) %>%
  summarise(
    n_lists = n_distinct(sub_list),
    lists = paste(sort(unique(sub_list)), collapse = ", "),
    n_posts = n(),
    surface_name = first(surface.name),
    surface_username = first(surface.username),
    .groups = "drop"
  )

multi_list_accounts <- surface_list_mapping %>%
  filter(n_lists > 1) %>%
  arrange(desc(n_lists), desc(n_posts))

if (nrow(multi_list_accounts) > 0) {
  cat("CRITICAL ERROR: Found", nrow(multi_list_accounts),
      "accounts assigned to MULTIPLE lists!\n\n")
  cat("Accounts with multiple list assignments:\n")
  cat(rule(60, "-"), "\n", sep = "")

  # Print up to 50 affected accounts with their post distribution by list
  for (i in 1:min(nrow(multi_list_accounts), 50)) {
    acc <- multi_list_accounts[i, ]
    cat(sprintf("\n  Account %d:\n", i))
    cat(sprintf("    surface.id: %s\n", acc$surface.id))
    cat(sprintf("    name: %s\n", acc$surface_name))
    cat(sprintf("    username: %s\n", acc$surface_username))
    cat(sprintf("    assigned to: %s\n", acc$lists))
    cat(sprintf("    total posts: %d\n", acc$n_posts))

    post_dist <- data %>%
      filter(surface.id == acc$surface.id) %>%
      count(sub_list) %>%
      mutate(info = sprintf("%s: %d posts", sub_list, n))

    cat(sprintf("    distribution: %s\n", paste(post_dist$info, collapse = ", ")))
  }

  if (nrow(multi_list_accounts) > 50) {
    cat("\n  ... and", nrow(multi_list_accounts) - 50, "more accounts\n")
  }
  cat("\n")

  integrity_errors$multi_list_accounts <- multi_list_accounts

  affected_posts <- data %>%
    filter(surface.id %in% multi_list_accounts$surface.id) %>%
    nrow()
  cat("Total posts affected:", affected_posts, "\n\n")

} else {
  cat("PASSED: All", nrow(surface_list_mapping),
      "accounts are assigned to exactly ONE list\n\n")
}

# --- CHECK 2.2: duplicate post IDs ----------------------------------------

cat("CHECK 2.2: Checking for duplicate post IDs...\n\n")

duplicate_posts <- data %>%
  group_by(id) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(id)

if (nrow(duplicate_posts) > 0) {
  n_dup_ids <- n_distinct(duplicate_posts$id)
  cat("ERROR: Found", nrow(duplicate_posts), "rows with", n_dup_ids, "duplicate post IDs!\n\n")

  cat("Examples of duplicate posts:\n")
  dup_examples <- duplicate_posts %>%
    group_by(id) %>%
    slice_head(n = 2) %>%
    ungroup() %>%
    head(20) %>%
    dplyr::select(id, surface.id, surface.name, sub_list, date, statistics.views)
  print(dup_examples)
  cat("\n")

  integrity_errors$duplicate_posts <- duplicate_posts
} else {
  cat("PASSED: All", n_distinct(data$id), "post IDs are unique\n\n")
}

# --- CHECK 2.3: account name/username consistent across posts ---------------
# Differences usually reflect renamed accounts; reported as warnings only.

cat("CHECK 2.3: Checking consistency of account metadata across posts...\n\n")

metadata_consistency <- data %>%
  group_by(surface.id) %>%
  summarise(
    n_names = n_distinct(surface.name, na.rm = TRUE),
    n_usernames = n_distinct(surface.username, na.rm = TRUE),
    names = paste(unique(na.omit(surface.name)), collapse = " | "),
    usernames = paste(unique(na.omit(surface.username)), collapse = " | "),
    .groups = "drop"
  )

inconsistent_names <- metadata_consistency %>% filter(n_names > 1)
inconsistent_usernames <- metadata_consistency %>% filter(n_usernames > 1)

if (nrow(inconsistent_names) > 0) {
  cat("WARNING: Found", nrow(inconsistent_names),
      "accounts with inconsistent surface.name values\n")
  cat("  (This may be due to name changes over time - usually not critical)\n")
  if (nrow(inconsistent_names) <= 10) {
    print(inconsistent_names %>% dplyr::select(surface.id, n_names, names))
  } else {
    print(head(inconsistent_names %>% dplyr::select(surface.id, n_names, names), 10))
    cat("  ... and", nrow(inconsistent_names) - 10, "more\n")
  }
  cat("\n")
  integrity_warnings$inconsistent_names <- inconsistent_names
} else {
  cat("PASSED: surface.name is consistent for all accounts\n")
}

if (nrow(inconsistent_usernames) > 0) {
  cat("WARNING: Found", nrow(inconsistent_usernames),
      "accounts with inconsistent surface.username values\n")
  if (nrow(inconsistent_usernames) <= 10) {
    print(inconsistent_usernames %>% dplyr::select(surface.id, n_usernames, usernames))
  } else {
    print(head(inconsistent_usernames %>% dplyr::select(surface.id, n_usernames, usernames), 10))
    cat("  ... and", nrow(inconsistent_usernames) - 10, "more\n")
  }
  cat("\n")
  integrity_warnings$inconsistent_usernames <- inconsistent_usernames
} else {
  cat("PASSED: surface.username is consistent for all accounts\n")
}
cat("\n")

# --- CHECK 2.4: sub_list consistency ---------------------------------------
# Note: because the data are grouped by both surface.id and sub_list,
# n_sublists is always 1 and this check always passes. Kept as in the
# original pipeline.

cat("CHECK 2.4: Checking sub_list consistency within sub_list...\n\n")

if ("sub_list" %in% names(data)) {
  sublist_consistency <- data %>%
    group_by(surface.id, sub_list) %>%
    summarise(
      n_sublists = n_distinct(sub_list, na.rm = TRUE),
      sublists = paste(unique(na.omit(sub_list)), collapse = " | "),
      .groups = "drop"
    ) %>%
    filter(n_sublists > 1)

  if (nrow(sublist_consistency) > 0) {
    cat("WARNING: Found", nrow(sublist_consistency),
        "accounts with inconsistent sub_list within sub_list\n")
    print(head(sublist_consistency, 10))
    cat("\n")
    integrity_warnings$inconsistent_sublists <- sublist_consistency
  } else {
    cat("PASSED: sub_list is consistent within sub_list for all accounts\n\n")
  }
} else {
  cat("sub_list column not found - skipping check\n\n")
}

# --- CHECK 2.5: account counts by group add up -----------------------------

cat("CHECK 2.5: Cross-referencing counts...\n\n")

list_summary <- data %>%
  group_by(sub_list) %>%
  summarise(
    n_posts = n(),
    n_accounts = n_distinct(surface.id),
    posts_per_account = round(n_posts / n_accounts, 1),
    .groups = "drop"
  )

cat("Posts and accounts by sub_list:\n")
print(list_summary)
cat("\n")

total_unique_accounts <- n_distinct(data$surface.id)
sum_accounts_by_list <- sum(list_summary$n_accounts)

if (length(integrity_errors$multi_list_accounts) > 0) {
  cat("Note: Sum of accounts by list (", sum_accounts_by_list,
      ") > unique accounts (", total_unique_accounts, ")\n", sep = "")
  cat("This is expected due to", nrow(integrity_errors$multi_list_accounts),
      "accounts appearing in multiple lists\n\n")
} else {
  if (sum_accounts_by_list == total_unique_accounts) {
    cat("PASSED: Account counts are consistent (sum by list = unique total)\n\n")
  } else {
    cat("Unexpected: Sum by list (", sum_accounts_by_list,
        ") != unique total (", total_unique_accounts, ")\n\n", sep = "")
  }
}

# --- Resolve integrity errors ------------------------------------------------

cat(rule(), "\n", sep = "")
cat("MERGE INTEGRITY CHECK SUMMARY\n")
cat(rule(), "\n\n", sep = "")

has_critical_errors <- length(integrity_errors) > 0

if (has_critical_errors) {
  cat("CRITICAL ERRORS FOUND:\n\n")
  if (!is.null(integrity_errors$multi_list_accounts)) {
    cat("  - ", nrow(integrity_errors$multi_list_accounts),
        " accounts assigned to multiple lists\n", sep = "")
  }
  if (!is.null(integrity_errors$duplicate_posts)) {
    cat("  - ", n_distinct(integrity_errors$duplicate_posts$id),
        " duplicate post IDs\n", sep = "")
  }
  cat("\n")

  # Accounts in more than one list. Available strategies:
  #   STOP     - save diagnostics and stop
  #   REMOVE   - drop all posts of affected accounts
  #   MAJORITY - assign the account to the list with most of its posts
  #   PRIORITY - assign by the fixed priority order below (used)
  if (!is.null(integrity_errors$multi_list_accounts)) {

    multi_list_resolution <- "PRIORITY"
    cat("multi_list_resolution = '", multi_list_resolution, "'\n\n", sep = "")

    if (multi_list_resolution == "STOP") {
      diagnostic_file <- paste0("merge_integrity_errors_",
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      saveRDS(integrity_errors, diagnostic_file)
      cat("Diagnostic information saved to:", diagnostic_file, "\n\n")
      stop("STOPPING: Critical merge integrity errors detected.\n",
           "Please review the errors above and fix the merge script.\n",
           "Alternatively, change 'multi_list_resolution' to handle automatically.")

    } else if (multi_list_resolution == "REMOVE") {
      cat("Removing all posts from accounts with multiple list assignments...\n")
      accounts_to_remove <- integrity_errors$multi_list_accounts$surface.id
      n_posts_before <- nrow(data)
      data <- data %>% filter(!(surface.id %in% accounts_to_remove))
      n_posts_removed <- n_posts_before - nrow(data)
      cat("Removed", n_posts_removed, "posts from",
          length(accounts_to_remove), "accounts\n\n")

    } else if (multi_list_resolution == "MAJORITY") {
      cat("Assigning accounts to the list with most posts...\n")
      majority_assignments <- data %>%
        filter(surface.id %in% integrity_errors$multi_list_accounts$surface.id) %>%
        count(surface.id, sub_list) %>%
        group_by(surface.id) %>%
        slice_max(n, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        dplyr::select(surface.id, assigned_list = sub_list)

      data <- data %>%
        left_join(majority_assignments, by = "surface.id") %>%
        mutate(sub_list = if_else(!is.na(assigned_list), assigned_list, sub_list)) %>%
        dplyr::select(-assigned_list)

      cat("Reassigned", nrow(majority_assignments), "accounts to majority list\n")
      cat("  New distribution:\n")
      print(data %>%
              filter(surface.id %in% integrity_errors$multi_list_accounts$surface.id) %>%
              count(sub_list))
      cat("\n")

    } else if (multi_list_resolution == "PRIORITY") {
      # Lower number = higher priority. Actual order used:
      #   MPs_Active2021_Reelected2022 (1) > Prominent_Politicians (2) > MPs_Elected_2022 (3)
      priority_order <- c("MPs_Active2021_Reelected2022" = 1,
                          "Prominent_Politicians" = 2,
                          "MPs_Elected_2022" = 3)

      cat("Assigning accounts using priority order...\n")
      cat("  Priority: MPs_Active2021_Reelected2022 > Prominent_Politicians > MPs_Elected_2022\n\n")

      priority_assignments <- data %>%
        filter(surface.id %in% integrity_errors$multi_list_accounts$surface.id) %>%
        distinct(surface.id, sub_list) %>%
        mutate(priority = priority_order[sub_list]) %>%
        group_by(surface.id) %>%
        slice_min(priority, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        dplyr::select(surface.id, assigned_list = sub_list)

      # Log every reassignment (account name: original lists -> assigned list)
      cat("Assigning accounts to highest-priority list:\n")
      for (i in 1:nrow(priority_assignments)) {
        acc_id <- priority_assignments$surface.id[i]
        acc_info <- integrity_errors$multi_list_accounts %>% filter(surface.id == acc_id)
        assigned <- priority_assignments$assigned_list[i]
        cat(sprintf("  %s: %s -> %s\n", acc_info$surface_name, acc_info$lists, assigned))
      }
      cat("\n")

      data <- data %>%
        left_join(priority_assignments, by = "surface.id") %>%
        mutate(sub_list = if_else(!is.na(assigned_list), assigned_list, sub_list)) %>%
        dplyr::select(-assigned_list)

      cat("Reassigned", nrow(priority_assignments), "accounts using priority\n")
      cat("  New distribution:\n")
      print(data %>%
              filter(surface.id %in% integrity_errors$multi_list_accounts$surface.id) %>%
              count(sub_list))
      cat("\n")
    }
  }

  # Duplicate posts: keep the first occurrence
  if (!is.null(integrity_errors$duplicate_posts)) {
    cat("Handling duplicate post IDs...\n")
    n_before <- nrow(data)
    data <- data %>% distinct(id, .keep_all = TRUE)
    n_removed <- n_before - nrow(data)
    cat("Removed", n_removed, "duplicate posts (kept first occurrence)\n\n")
  }

} else {
  cat("ALL MERGE INTEGRITY CHECKS PASSED\n\n")
}

if (length(integrity_warnings) > 0) {
  cat("Warnings (non-critical):\n")
  for (warning_name in names(integrity_warnings)) {
    cat("  - ", warning_name, ": ", nrow(integrity_warnings[[warning_name]]), " cases\n", sep = "")
  }
  cat("\n")
}

# Final check: after resolution every account must have one group
cat("FINAL VERIFICATION: Re-checking list assignments...\n")

final_check <- data %>%
  group_by(surface.id) %>%
  summarise(n_lists = n_distinct(sub_list), .groups = "drop") %>%
  filter(n_lists > 1)

if (nrow(final_check) > 0) {
  stop("FATAL: After resolution, ", nrow(final_check),
       " accounts still have multiple list assignments!")
} else {
  cat("VERIFIED: All", n_distinct(data$surface.id),
      "accounts now have exactly ONE list assignment\n\n")
}

# =============================================================================
# STEP 3: Required fields
# =============================================================================

cat("STEP 3: Validating critical fields...\n")
cat(rule(80, "-"), "\n\n", sep = "")

required_fields <- c("id", "surface.id", "sub_list", "date", "creation_time",
                     "statistics.views", "statistics.reaction_count",
                     "statistics.share_count", "statistics.comment_count")

missing_fields <- setdiff(required_fields, names(data))
if (length(missing_fields) > 0) {
  stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
}
cat("All required fields present\n\n")

na_surface_id <- sum(is.na(data$surface.id))
if (na_surface_id > 0) {
  cat("WARNING:", na_surface_id, "posts with NA surface.id\n")
  cat("These will be removed.\n\n")
} else {
  cat("No NA values in surface.id\n\n")
}

na_date <- sum(is.na(data$date))
if (na_date > 0) {
  cat("WARNING:", na_date, "posts with NA date\n")
  cat("These will be removed.\n\n")
} else {
  cat("No NA values in date\n\n")
}

cat("Group distribution (after integrity checks):\n")
sub_list_counts <- data %>% count(sub_list)
print(sub_list_counts)
cat("\n")

# Accepted group labels (the pattern also accepts "Extremists", a label from
# an earlier version of the pipeline that does not occur in this dataset)
valid_sub_lists_pattern <- "^(MPs.*|Prominent_Politicians|Extremists)$"

invalid_lists <- data %>%
  filter(!grepl(valid_sub_lists_pattern, sub_list)) %>%
  distinct(sub_list) %>%
  pull(sub_list)

if (length(invalid_lists) > 0) {
  cat("WARNING: Invalid sub_list values found:\n")
  print(invalid_lists)
  cat("These will be removed.\n\n")
}

# =============================================================================
# STEP 4: Drop invalid records and posts outside the study window
# =============================================================================

cat("STEP 4: Removing invalid records and applying time-frame filter...\n")
cat(rule(80, "-"), "\n\n", sep = "")

cat("Study time-frame:\n")
cat("  Start date:", format(study_start_date, "%Y-%m-%d"), "\n")
cat("  End date:  ", format(study_end_date, "%Y-%m-%d"), "\n\n")

n_before <- nrow(data)

posts_before_start <- sum(data$date < study_start_date, na.rm = TRUE)
posts_after_end <- sum(data$date > study_end_date, na.rm = TRUE)
posts_outside_timeframe <- posts_before_start + posts_after_end

if (posts_outside_timeframe > 0) {
  cat("Posts outside study time-frame:\n")
  cat("  Before", format(study_start_date, "%Y-%m-%d"), ":", posts_before_start, "\n")
  cat("  After", format(study_end_date, "%Y-%m-%d"), " :", posts_after_end, "\n")
  cat("  Total to exclude:", posts_outside_timeframe, "\n\n")

  if (posts_before_start > 0) {
    early_dates <- data %>%
      filter(date < study_start_date) %>%
      summarise(min = min(date), max = max(date))
    cat("  Early posts range:", format(early_dates$min, "%Y-%m-%d"), "to",
        format(early_dates$max, "%Y-%m-%d"), "\n")
  }
  if (posts_after_end > 0) {
    late_dates <- data %>%
      filter(date > study_end_date) %>%
      summarise(min = min(date), max = max(date))
    cat("  Late posts range:", format(late_dates$min, "%Y-%m-%d"), "to",
        format(late_dates$max, "%Y-%m-%d"), "\n")
  }
  cat("\n")
}

data <- data %>%
  filter(
    !is.na(surface.id),
    !is.na(date),
    grepl(valid_sub_lists_pattern, sub_list),
    date >= study_start_date,
    date <= study_end_date
  )

n_removed <- n_before - nrow(data)
n_removed_other <- n_removed - posts_outside_timeframe

cat("Records removed:\n")
cat("  Invalid (NA surface.id, NA date, invalid list):", n_removed_other, "\n")
cat("  Outside study time-frame:", posts_outside_timeframe, "\n")
cat("  Total removed:", n_removed, "\n")
cat("Remaining posts:", nrow(data), "\n\n")

# =============================================================================
# STEP 5: Temporal coverage
# =============================================================================

cat("STEP 5: Validating temporal coverage...\n")
cat(rule(80, "-"), "\n\n", sep = "")

date_range <- data %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    span_days = as.numeric(difftime(max(date), min(date), units = "days"))
  )

cat("Overall date range:\n")
cat("  First post:", format(date_range$min_date, "%Y-%m-%d"), "\n")
cat("  Last post:", format(date_range$max_date, "%Y-%m-%d"), "\n")
cat("  Total span:", date_range$span_days, "days\n\n")

cat("Date range by group:\n")
date_by_list <- data %>%
  group_by(sub_list) %>%
  summarise(
    first_post = min(date),
    last_post = max(date),
    span_days = as.numeric(difftime(last_post, first_post, units = "days")),
    n_posts = n(),
    .groups = "drop"
  )
print(date_by_list)
cat("\n")

# Report any gap longer than 30 days without posts
cat("Checking for temporal gaps...\n")
daily_posts <- data %>% count(date) %>% arrange(date)

gaps <- daily_posts %>%
  mutate(
    next_date = lead(date),
    gap_days = as.numeric(difftime(next_date, date, units = "days"))
  ) %>%
  filter(gap_days > 30) %>%
  dplyr::select(date, next_date, gap_days)

if (nrow(gaps) > 0) {
  cat("WARNING: Found", nrow(gaps), "gaps > 30 days:\n")
  print(gaps)
  cat("\n")
} else {
  cat("No major temporal gaps detected\n\n")
}

# =============================================================================
# STEP 6: Meta's announced policy dates (descriptive only)
# =============================================================================
# These announced dates are used only for the descriptive `policy_period` and
# `policy_phase` variables below. The phases used in the analysis are the
# data-driven breakpoints from script 05.

cat("STEP 6: Defining Meta policy periods...\n")
cat(rule(80, "-"), "\n\n", sep = "")

policy_dates <- tibble(
  event = c(
    "Pre-Policy",
    "Initial Announcement",
    "US Tests Begin",
    "Global Expansion 1",
    "Reduced Engagement Weight Test",
    "Global Implementation",
    "Refinements (Survey Signals)",
    "User Control Setting",
    "Policy Reversal",
    "Updated Approach"
  ),
  date = as.Date(c(
    "2020-01-01",
    "2021-02-10",
    "2021-02-17",
    "2021-10-13",
    "2022-05-24",
    "2022-07-19",
    "2023-04-20",
    "2024-09-04",
    "2025-01-07",
    "2025-05-28"
  ))
)

cat("Key policy dates:\n")
print(policy_dates)
cat("\n")

# Post counts by group in windows around the 2022-07-19 global implementation
cat("Data coverage around key date (2022-07-19 global implementation):\n")
key_date <- as.Date("2022-07-19")

coverage_check <- data %>%
  mutate(
    period = case_when(
      date < key_date - 180 ~ "6mo before",
      date >= key_date - 180 & date < key_date - 90 ~ "3-6mo before",
      date >= key_date - 90 & date < key_date - 30 ~ "1-3mo before",
      date >= key_date - 30 & date < key_date ~ "1mo before",
      date >= key_date & date < key_date + 30 ~ "1mo after",
      date >= key_date + 30 & date < key_date + 90 ~ "1-3mo after",
      date >= key_date + 90 & date < key_date + 180 ~ "3-6mo after",
      date >= key_date + 180 ~ "6mo+ after"
    )
  ) %>%
  count(period, sub_list) %>%
  pivot_wider(names_from = sub_list, values_from = n, values_fill = 0)

print(coverage_check)
cat("\n")

# =============================================================================
# STEP 7: Missing views and engagement
# =============================================================================

cat("STEP 7: Validating and handling engagement metrics...\n")
cat(rule(80, "-"), "\n\n", sep = "")

set.seed(42)
cat("Random seed set to 42\n\n")

engagement_fields <- c("statistics.views", "statistics.reaction_count",
                       "statistics.share_count", "statistics.comment_count")

cat("NA values in engagement metrics (BEFORE any handling):\n")
for (field in engagement_fields) {
  na_count <- sum(is.na(data[[field]]))
  pct <- round(100 * na_count / nrow(data), 2)
  cat(sprintf("  %-30s: %8d (%5.2f%%)\n", field, na_count, pct))
}
cat("\n")

# -----------------------------------------------------------------------------
# STEP 7a: Group-specific imputation of censored views
# -----------------------------------------------------------------------------
# MCL returns NA for posts with 100 or fewer views. For each group, a power
# law fitted to the observed (100, 500] views gives the expected ratio of
# censored posts in 1-50 vs 51-100; imputed values are drawn accordingly.
#
# Reproducibility note: set.seed(42) is called before the group loop, but
# compute_imputation_stats() calls set.seed(12345) once per group. The draws
# that fill in the censored views therefore start from the generator state
# left by the last compute_imputation_stats() call, not from seed 42. The
# result is deterministic and reproducible as long as the group order
# (unique(data$sub_list)) and the data are unchanged.

cat("--- Handling NA Views (GROUP-SPECIFIC METHOD) ---\n\n")
cat("IMPUTATION METHOD: Group-Specific Ratio-Weighted\n")
cat("  Power law fit to bin counts (101-", IMPUTATION_MAX_VIEW, " views)\n\n", sep = "")

cat("NA views by group (BEFORE imputation):\n")
na_views_by_group <- data %>%
  group_by(sub_list) %>%
  summarise(
    total_posts = n(),
    na_views = sum(is.na(statistics.views)),
    pct_na = round(100 * na_views / total_posts, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_na))
print(na_views_by_group)
cat("\n")

# MCL does not report views for posts before 2017 (none expected here)
pre_2017_posts <- sum(data$date < as.Date("2017-01-01"), na.rm = TRUE)
if (pre_2017_posts > 0) {
  cat("NOTE:", pre_2017_posts, "posts are from before 2017-01-01\n")
  cat("  MCL documentation states view counts are not available for posts before this date.\n\n")
}

# Flag why views are missing; only "under_threshold" posts are imputed
data <- data %>%
  mutate(
    views_na_reason = case_when(
      !is.na(statistics.views) ~ "not_na",
      date < as.Date("2017-01-01") ~ "pre_2017",
      TRUE ~ "under_threshold"
    ),
    views_imputed = views_na_reason == "under_threshold",
    views_pre_2017 = views_na_reason == "pre_2017"
  )

cat("NA views breakdown by reason:\n")
na_reason_summary <- data %>%
  count(views_na_reason) %>%
  mutate(pct = round(100 * n / sum(n), 2))
print(na_reason_summary)
cat("\n")

n_to_impute <- sum(data$views_imputed)
n_pre_2017 <- sum(data$views_pre_2017)

if (n_to_impute > 0) {
  cat("IMPUTING", n_to_impute, "posts with NA views (<=100 views threshold)\n\n")

  set.seed(42)
  cat("  Master random seed: 42\n\n")

  groups <- unique(data$sub_list)
  group_imputation_params <- list()

  cat("Deriving imputation parameters by group:\n")
  cat(rule(60, "-"), "\n\n", sep = "")

  # Fit the power law and compute expected statistics for each group
  for (g in groups) {
    group_views <- data %>%
      filter(sub_list == g, !is.na(statistics.views)) %>%
      pull(statistics.views)

    params <- derive_imputation_ratio(
      views = group_views,
      bin_width = IMPUTATION_BIN_WIDTH,
      max_view = IMPUTATION_MAX_VIEW,
      min_bins = IMPUTATION_MIN_BINS,
      min_posts = IMPUTATION_MIN_POSTS,
      fallback_ratio = POOLED_FALLBACK_RATIO
    )

    # Resets the RNG via set.seed(12345) (see note above)
    imp_stats <- compute_imputation_stats(params$ratio)

    group_imputation_params[[g]] <- list(
      group = g,
      n_censored = sum(data$sub_list == g & data$views_imputed),
      ratio = params$ratio,
      alpha = params$alpha,
      r_squared = params$r_squared,
      n_near_threshold = params$n_near_threshold,
      reliable = params$reliable,
      expected_mean = imp_stats$mean,
      expected_median = imp_stats$median,
      expected_pct_under_50 = imp_stats$pct_under_50
    )

    cat(sprintf("%s:\n", g))
    cat(sprintf("  Posts to impute: %d\n", group_imputation_params[[g]]$n_censored))
    cat(sprintf("  Near threshold (101-%d): %d posts\n", IMPUTATION_MAX_VIEW, params$n_near_threshold))
    if (params$reliable) {
      cat(sprintf("  Power law exponent (alpha): %.3f\n", params$alpha))
      cat(sprintf("  R-squared: %.3f\n", params$r_squared))
      cat(sprintf("  Derived ratio (1-50 vs 50-100): %.2f\n", params$ratio))
      cat(sprintf("  -> Expected imputed mean: %.1f, median: %d\n", imp_stats$mean, imp_stats$median))
    } else {
      cat(sprintf("  Using fallback ratio: %.2f (uniform)\n", params$ratio))
    }
    cat("\n")
  }

  # Replace the censored views group by group
  cat("Applying imputation by group:\n")
  cat(rule(40, "-"), "\n", sep = "")

  imputation_log <- list()

  for (g in groups) {
    params <- group_imputation_params[[g]]
    idx <- which(data$sub_list == g & data$views_imputed)
    n_group_impute <- length(idx)

    if (n_group_impute > 0) {
      imputed_values <- generate_imputed_values(n_group_impute, params$ratio, seed = NULL)
      data$statistics.views[idx] <- imputed_values

      imputation_log[[g]] <- list(
        n_imputed = n_group_impute,
        ratio_used = params$ratio,
        actual_mean = round(mean(imputed_values), 1),
        actual_median = median(imputed_values),
        actual_pct_under_50 = round(100 * mean(imputed_values <= 50), 1)
      )

      cat(sprintf("  %s: %d posts (ratio=%.2f, mean=%.1f, median=%d)\n",
                  g, n_group_impute, params$ratio, mean(imputed_values), median(imputed_values)))
    } else {
      imputation_log[[g]] <- list(n_imputed = 0)
    }
  }

  cat("\nImputed", n_to_impute, "view counts using group-specific method\n\n")

  cat("Group-specific imputation summary:\n")
  imp_summary <- tibble(
    Group = sapply(group_imputation_params, function(x) x$group),
    N_Imputed = sapply(group_imputation_params, function(x) x$n_censored),
    Alpha = sapply(group_imputation_params, function(x) round(x$alpha, 3)),
    Ratio = sapply(group_imputation_params, function(x) round(x$ratio, 2)),
    Exp_Mean = sapply(group_imputation_params, function(x) x$expected_mean)
  )
  print(imp_summary)
  cat("\n")

} else {
  data$views_imputed <- FALSE
  group_imputation_params <- list()
  imputation_log <- list()
  cat("No views to impute\n\n")
}

# -----------------------------------------------------------------------------
# STEP 7b: Remove posts with missing reactions, shares or comments
# -----------------------------------------------------------------------------
# MCL documents no censoring threshold for engagement, so these are dropped
# rather than imputed.

cat("--- Handling NA Engagement Metrics (REMOVAL) ---\n\n")

n_before_engagement_removal <- nrow(data)

na_reactions <- sum(is.na(data$statistics.reaction_count))
na_shares <- sum(is.na(data$statistics.share_count))
na_comments <- sum(is.na(data$statistics.comment_count))

cat("Posts with NA engagement metrics (BEFORE removal):\n")
cat("  NA reactions:", na_reactions, sprintf("(%.2f%%)\n", 100 * na_reactions / nrow(data)))
cat("  NA shares:", na_shares, sprintf("(%.2f%%)\n", 100 * na_shares / nrow(data)))
cat("  NA comments:", na_comments, sprintf("(%.2f%%)\n", 100 * na_comments / nrow(data)))
cat("\n")

na_engagement_by_group <- data %>%
  group_by(sub_list) %>%
  summarise(
    total_posts = n(),
    na_reactions = sum(is.na(statistics.reaction_count)),
    na_shares = sum(is.na(statistics.share_count)),
    na_comments = sum(is.na(statistics.comment_count)),
    posts_any_na = sum(is.na(statistics.reaction_count) |
                         is.na(statistics.share_count) |
                         is.na(statistics.comment_count)),
    pct_any_na = round(100 * posts_any_na / total_posts, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_any_na))

cat("NA engagement by group (BEFORE removal):\n")
print(na_engagement_by_group)
cat("\n")

cat("REMOVING posts with NA engagement metrics...\n")
data <- data %>%
  filter(
    !is.na(statistics.reaction_count),
    !is.na(statistics.share_count),
    !is.na(statistics.comment_count)
  )

n_engagement_removed <- n_before_engagement_removal - nrow(data)
cat("Removed", n_engagement_removed, "posts with NA engagement metrics\n")
cat("  Remaining posts:", nrow(data), "\n\n")

n_removed <- n_removed + n_engagement_removed

# -----------------------------------------------------------------------------
# STEP 7c: Final value checks
# -----------------------------------------------------------------------------

cat("--- Final Validation Checks ---\n\n")

# Negative counts are set to 0 (none expected)
negative_checks <- data %>%
  summarise(
    negative_views = sum(statistics.views < 0, na.rm = TRUE),
    negative_reactions = sum(statistics.reaction_count < 0, na.rm = TRUE),
    negative_shares = sum(statistics.share_count < 0, na.rm = TRUE),
    negative_comments = sum(statistics.comment_count < 0, na.rm = TRUE)
  )

if (sum(negative_checks) > 0) {
  cat("WARNING: Found negative values - setting to 0\n")
  data <- data %>%
    mutate(
      statistics.views = pmax(statistics.views, 0, na.rm = TRUE),
      statistics.reaction_count = pmax(statistics.reaction_count, 0),
      statistics.share_count = pmax(statistics.share_count, 0),
      statistics.comment_count = pmax(statistics.comment_count, 0)
    )
} else {
  cat("No negative values found\n")
}

# Missing content_type: MCL does not support this attachment type
na_content_type <- sum(is.na(data$content_type))
if (na_content_type > 0) {
  data <- data %>%
    mutate(content_type = replace_na(content_type, "mcl_unsupported_attachment"))
  cat("Replaced", na_content_type, "NA content_type with 'mcl_unsupported_attachment'\n")
} else {
  cat("No NA values in content_type\n")
}
cat("\n")

cat("NA values in engagement metrics (AFTER handling):\n")
for (field in engagement_fields) {
  na_count <- sum(is.na(data[[field]]))
  cat(sprintf("  %-30s: %d\n", field, na_count))
}
cat("\n")

# =============================================================================
# STEP 8: Analysis variables
# =============================================================================
# policy_period / policy_phase use Meta's announced dates (descriptive only).

cat("STEP 8: Creating analysis-ready variables...\n")
cat(rule(80, "-"), "\n\n", sep = "")

data <- data %>%
  mutate(
    policy_period = case_when(
      date < as.Date("2022-07-19") ~ "Pre-Policy",
      date >= as.Date("2022-07-19") ~ "Post-Policy"
    ),

    days_since_policy = as.numeric(difftime(date, as.Date("2022-07-19"), units = "days")),

    policy_phase = case_when(
      date < as.Date("2021-02-10") ~ "Baseline",
      date >= as.Date("2021-02-10") & date < as.Date("2021-10-13") ~ "Announcement",
      date >= as.Date("2021-10-13") & date < as.Date("2022-07-19") ~ "Testing",
      date >= as.Date("2022-07-19") & date < as.Date("2023-04-20") ~ "Implementation",
      date >= as.Date("2023-04-20") & date < as.Date("2025-01-07") ~ "Refinement",
      date >= as.Date("2025-01-07") ~ "Reversal"
    ),

    year_month = floor_date(date, "month"),
    week = floor_date(date, "week"),            # Sunday-start weeks
    quarter = quarter(date, with_year = TRUE),

    total_engagement = statistics.reaction_count + statistics.share_count + statistics.comment_count,
    engagement_rate = if_else(statistics.views > 0, total_engagement / statistics.views, 0),

    log_views = log1p(statistics.views),
    log_reactions = log1p(statistics.reaction_count),
    log_shares = log1p(statistics.share_count),
    log_comments = log1p(statistics.comment_count),
    log_total_engagement = log1p(total_engagement)
  )

cat("Created all analysis variables\n\n")

# =============================================================================
# STEP 9: Account-level datasets
# =============================================================================

cat("STEP 9: Creating account-level dataset...\n")
cat(rule(80, "-"), "\n\n", sep = "")

# One row per account, with pre/post (2022-07-19) comparison. An account is
# "active" in a period if it has at least 10 posts in it.
accounts <- data %>%
  group_by(surface.id, surface.name, surface.username, sub_list) %>%
  summarise(
    n_posts = n(),
    n_posts_pre = sum(policy_period == "Pre-Policy"),
    n_posts_post = sum(policy_period == "Post-Policy"),
    n_views_imputed = sum(views_imputed),
    pct_views_imputed = round(100 * n_views_imputed / n_posts, 2),
    first_post = min(date),
    last_post = max(date),
    days_active = as.numeric(difftime(last_post, first_post, units = "days")),
    sub_list = first(sub_list),
    total_views = sum(statistics.views, na.rm = TRUE),
    total_reactions = sum(statistics.reaction_count, na.rm = TRUE),
    total_shares = sum(statistics.share_count, na.rm = TRUE),
    total_comments = sum(statistics.comment_count, na.rm = TRUE),
    total_engagement = sum(total_engagement, na.rm = TRUE),
    avg_views = mean(statistics.views, na.rm = TRUE),
    avg_reactions = mean(statistics.reaction_count, na.rm = TRUE),
    avg_shares = mean(statistics.share_count, na.rm = TRUE),
    avg_comments = mean(statistics.comment_count, na.rm = TRUE),
    avg_engagement_rate = mean(engagement_rate, na.rm = TRUE),
    median_views = median(statistics.views, na.rm = TRUE),
    median_reactions = median(statistics.reaction_count, na.rm = TRUE),
    avg_views_pre = mean(statistics.views[policy_period == "Pre-Policy"], na.rm = TRUE),
    avg_views_post = mean(statistics.views[policy_period == "Post-Policy"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    reach_change = avg_views_post - avg_views_pre,
    reach_change_pct = if_else(avg_views_pre > 0,
                               100 * (avg_views_post - avg_views_pre) / avg_views_pre,
                               NA_real_),
    active_pre = n_posts_pre >= 10,
    active_post = n_posts_post >= 10,
    active_both = active_pre & active_post
  )

cat("Created account-level summary\n")
cat("  Unique accounts:", nrow(accounts), "\n\n")

# Accounts with several names/usernames would appear twice; stop if so
account_check <- accounts %>%
  group_by(surface.id) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(account_check) > 0) {
  stop("FATAL: Account-level data has duplicate surface.ids!")
} else {
  cat("VERIFIED: Each account appears exactly once in accounts dataset\n\n")
}

cat("Accounts by group:\n")
print(accounts %>% count(sub_list))
cat("\n")

# --- Surface (account) metadata: most recent non-missing value per field ---

cat("Creating comprehensive surface info dataset...\n\n")

surface_columns <- names(data)[grepl("^surface\\.", names(data))]
cat("  Found", length(surface_columns), "surface-level columns:\n")
cat("   ", paste(surface_columns, collapse = ", "), "\n\n")

surface_info <- data %>%
  group_by(surface.id) %>%
  summarise(
    # Identifiers
    surface.name = last(na.omit(surface.name)),
    surface.username = last(na.omit(surface.username)),
    sub_list = first(sub_list),

    # Any other surface.* fields
    across(
      any_of(setdiff(surface_columns, c("surface.id", "surface.name", "surface.username"))),
      ~last(na.omit(.))
    ),

    # Activity
    n_posts_total = n(),
    n_posts_pre_policy = sum(policy_period == "Pre-Policy"),
    n_posts_post_policy = sum(policy_period == "Post-Policy"),
    first_post_date = min(date),
    last_post_date = max(date),
    days_active = as.numeric(difftime(max(date), min(date), units = "days")),
    spans_policy_change = (min(date) < as.Date("2022-07-19")) &
                          (max(date) >= as.Date("2022-07-19")),

    # Lifetime totals
    total_views = sum(statistics.views, na.rm = TRUE),
    total_reactions = sum(statistics.reaction_count, na.rm = TRUE),
    total_shares = sum(statistics.share_count, na.rm = TRUE),
    total_comments = sum(statistics.comment_count, na.rm = TRUE),
    total_engagement = sum(total_engagement, na.rm = TRUE),

    # Per-post means and medians
    avg_views_per_post = mean(statistics.views, na.rm = TRUE),
    avg_reactions_per_post = mean(statistics.reaction_count, na.rm = TRUE),
    avg_shares_per_post = mean(statistics.share_count, na.rm = TRUE),
    avg_comments_per_post = mean(statistics.comment_count, na.rm = TRUE),
    median_views_per_post = median(statistics.views, na.rm = TRUE),
    median_reactions_per_post = median(statistics.reaction_count, na.rm = TRUE),
    avg_engagement_rate = mean(engagement_rate, na.rm = TRUE),

    # Pre vs post 2022-07-19
    avg_views_pre_policy = mean(statistics.views[policy_period == "Pre-Policy"], na.rm = TRUE),
    avg_views_post_policy = mean(statistics.views[policy_period == "Post-Policy"], na.rm = TRUE),
    avg_reactions_pre_policy = mean(statistics.reaction_count[policy_period == "Pre-Policy"], na.rm = TRUE),
    avg_reactions_post_policy = mean(statistics.reaction_count[policy_period == "Post-Policy"], na.rm = TRUE),

    # Content types
    n_content_types = n_distinct(content_type),
    primary_content_type = names(sort(table(content_type), decreasing = TRUE))[1],

    # Data quality
    n_views_imputed = sum(views_imputed),
    pct_views_imputed = round(100 * sum(views_imputed) / n(), 2),

    .groups = "drop"
  ) %>%
  mutate(
    reach_change = avg_views_post_policy - avg_views_pre_policy,
    reach_change_pct = if_else(
      !is.na(avg_views_pre_policy) & avg_views_pre_policy > 0,
      100 * (avg_views_post_policy - avg_views_pre_policy) / avg_views_pre_policy,
      NA_real_
    ),
    reactions_change = avg_reactions_post_policy - avg_reactions_pre_policy,
    reactions_change_pct = if_else(
      !is.na(avg_reactions_pre_policy) & avg_reactions_pre_policy > 0,
      100 * (avg_reactions_post_policy - avg_reactions_pre_policy) / avg_reactions_pre_policy,
      NA_real_
    )
  ) %>%
  # Column order: identifiers, group, other surface fields, metrics
  dplyr::select(
    surface.id, surface.name, surface.username,
    sub_list,
    starts_with("surface."),
    n_posts_total, n_posts_pre_policy, n_posts_post_policy,
    first_post_date, last_post_date, days_active, spans_policy_change,
    total_views, total_reactions, total_shares, total_comments, total_engagement,
    avg_views_per_post, avg_reactions_per_post, avg_shares_per_post, avg_comments_per_post,
    median_views_per_post, median_reactions_per_post, avg_engagement_rate,
    avg_views_pre_policy, avg_views_post_policy, reach_change, reach_change_pct,
    avg_reactions_pre_policy, avg_reactions_post_policy, reactions_change, reactions_change_pct,
    n_content_types, primary_content_type,
    n_views_imputed, pct_views_imputed,
    everything()
  )

cat("Created surface info dataset\n")
cat("  Total surfaces:", nrow(surface_info), "\n")
cat("  Columns:", ncol(surface_info), "\n\n")

surface_list_check <- surface_info %>%
  group_by(surface.id) %>%
  filter(n() > 1)

if (nrow(surface_list_check) > 0) {
  stop("FATAL: Surface info has duplicate surface.ids!")
} else {
  cat("VERIFIED: Each surface appears exactly once in surface_info\n\n")
}

cat("Surface info summary by group:\n")
surface_summary <- surface_info %>%
  group_by(sub_list) %>%
  summarise(
    n_surfaces = n(),
    n_spanning_policy = sum(spans_policy_change),
    avg_posts = round(mean(n_posts_total), 1),
    avg_total_views = round(mean(total_views), 0),
    avg_reach_change_pct = round(mean(reach_change_pct, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(surface_summary)
cat("\n")

# Account IDs for the enrichment queries in script 03
cat("Creating surface ID list for MCL API queries...\n\n")

surface_ids_for_api <- surface_info %>%
  dplyr::select(surface.id, surface.name, sub_list) %>%
  arrange(sub_list, surface.name)

cat("  Total surface IDs for API query:", nrow(surface_ids_for_api), "\n")
cat("  By group:\n")
print(surface_ids_for_api %>% count(sub_list))
cat("\n")

# Accounts with >= 10 posts both before and after 2022-07-19
accounts_both_periods <- accounts %>% filter(active_both)

cat("Accounts active in BOTH periods (>=10 posts each):\n")
print(accounts_both_periods %>% count(sub_list))
cat("\n")

# =============================================================================
# STEP 10: Final quality checks
# =============================================================================

cat("STEP 10: Final data quality checks...\n")
cat(rule(80, "-"), "\n\n", sep = "")

duplicates <- data %>%
  group_by(id) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicates) > 0) {
  cat("WARNING: Removing", n_distinct(duplicates$id), "duplicate post IDs\n")
  data <- data %>% distinct(id, .keep_all = TRUE)
} else {
  cat("No duplicate posts\n")
}

cat("\nPosts per group in each period:\n")
period_coverage <- data %>%
  count(sub_list, policy_period) %>%
  pivot_wider(names_from = policy_period, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(-sub_list)))
print(period_coverage)
cat("\n")

# =============================================================================
# STEP 11: Analysis-ready datasets
# =============================================================================

cat("STEP 11: Creating analysis-ready datasets...\n")
cat(rule(80, "-"), "\n\n", sep = "")

cleaned_data <- data

data_both_periods <- data %>%
  filter(surface.id %in% accounts_both_periods$surface.id)

# --- Complete-week boundaries ---------------------------------------------

cat("--- Complete Weeks Filtering ---\n\n")

first_complete_week <- get_first_complete_week_start(study_start_date)
last_complete_week_end <- get_last_complete_week_end(study_end_date)
last_complete_week_start <- last_complete_week_end - days(6)

cat("Study time-frame:\n")
cat("  Start date:", format(study_start_date, "%Y-%m-%d"),
    "(", weekdays(study_start_date), ")\n")
cat("  End date:  ", format(study_end_date, "%Y-%m-%d"),
    "(", weekdays(study_end_date), ")\n\n")

cat("Complete weeks boundaries:\n")
cat("  First complete week starts:", format(first_complete_week, "%Y-%m-%d"),
    "(", weekdays(first_complete_week), ")\n")
cat("  Last complete week ends:   ", format(last_complete_week_end, "%Y-%m-%d"),
    "(", weekdays(last_complete_week_end), ")\n\n")

all_weeks <- data %>% distinct(week) %>% arrange(week)

complete_weeks <- all_weeks %>%
  filter(week >= first_complete_week & week <= last_complete_week_start)
incomplete_weeks_start <- all_weeks %>% filter(week < first_complete_week)
incomplete_weeks_end <- all_weeks %>% filter(week > last_complete_week_start)

n_total_weeks <- nrow(all_weeks)
n_complete_weeks <- nrow(complete_weeks)
n_incomplete_start <- nrow(incomplete_weeks_start)
n_incomplete_end <- nrow(incomplete_weeks_end)

cat("Week counts:\n")
cat("  Total unique weeks in data:", n_total_weeks, "\n")
cat("  Complete weeks:            ", n_complete_weeks, "\n")
cat("  Partial weeks at start:    ", n_incomplete_start, "\n")
cat("  Partial weeks at end:      ", n_incomplete_end, "\n\n")

if (n_incomplete_start > 0) {
  cat("Partial weeks at START (excluded from weekly_aggregation):\n")
  for (i in 1:nrow(incomplete_weeks_start)) {
    w <- incomplete_weeks_start$week[i]
    w_end <- w + days(6)
    posts_in_week <- sum(data$week == w)
    cat(sprintf("  %s to %s (%d posts)\n",
                format(w, "%Y-%m-%d"), format(w_end, "%Y-%m-%d"), posts_in_week))
  }
  cat("\n")
}

if (n_incomplete_end > 0) {
  cat("Partial weeks at END (excluded from weekly_aggregation):\n")
  for (i in 1:nrow(incomplete_weeks_end)) {
    w <- incomplete_weeks_end$week[i]
    w_end <- w + days(6)
    posts_in_week <- sum(data$week == w)
    cat(sprintf("  %s to %s (%d posts)\n",
                format(w, "%Y-%m-%d"), format(w_end, "%Y-%m-%d"), posts_in_week))
  }
  cat("\n")
}

posts_in_incomplete_weeks <- data %>%
  filter(week < first_complete_week | week > last_complete_week_start) %>%
  nrow()

cat("Posts in partial weeks:", posts_in_incomplete_weeks,
    sprintf("(%.2f%% of total)\n", 100 * posts_in_incomplete_weeks / nrow(data)))
cat("These posts remain in cleaned_posts but are excluded from weekly_aggregation\n\n")

# --- Weekly series by group (complete weeks only) - input to script 05 ----
weekly_data <- data %>%
  filter(week >= first_complete_week & week <= last_complete_week_start) %>%
  group_by(week, sub_list) %>%
  summarise(
    n_posts = n(),
    n_accounts = n_distinct(surface.id),
    n_imputed = sum(views_imputed),
    pct_imputed = round(100 * n_imputed / n_posts, 2),
    total_views = sum(statistics.views, na.rm = TRUE),
    avg_views = mean(statistics.views, na.rm = TRUE),
    median_views = median(statistics.views, na.rm = TRUE),
    avg_reactions = mean(statistics.reaction_count, na.rm = TRUE),
    avg_shares = mean(statistics.share_count, na.rm = TRUE),
    avg_comments = mean(statistics.comment_count, na.rm = TRUE),
    avg_engagement_rate = mean(engagement_rate, na.rm = TRUE),
    policy_period = first(policy_period),
    policy_phase = first(policy_phase),
    .groups = "drop"
  ) %>%
  mutate(
    week_end = week + days(6),
    is_complete_week = TRUE
  )

cat("Created weekly aggregation with COMPLETE WEEKS ONLY\n")
cat("  Weekly data rows:", nrow(weekly_data), "\n")
cat("  Date range:", format(min(weekly_data$week), "%Y-%m-%d"), "to",
    format(max(weekly_data$week_end), "%Y-%m-%d"), "\n")
cat("  Unique complete weeks:", n_distinct(weekly_data$week), "\n\n")

# --- Monthly series by group (all months are complete in the window) ------
monthly_data <- data %>%
  group_by(year_month, sub_list) %>%
  summarise(
    n_posts = n(),
    n_accounts = n_distinct(surface.id),
    n_imputed = sum(views_imputed),
    pct_imputed = round(100 * n_imputed / n_posts, 2),
    total_views = sum(statistics.views, na.rm = TRUE),
    avg_views = mean(statistics.views, na.rm = TRUE),
    median_views = median(statistics.views, na.rm = TRUE),
    avg_reactions = mean(statistics.reaction_count, na.rm = TRUE),
    avg_shares = mean(statistics.share_count, na.rm = TRUE),
    avg_comments = mean(statistics.comment_count, na.rm = TRUE),
    avg_engagement_rate = mean(engagement_rate, na.rm = TRUE),
    policy_period = first(policy_period),
    policy_phase = first(policy_phase),
    .groups = "drop"
  )

# --- Account x period (pre/post 2022-07-19), wide format ------------------
account_period_data <- data %>%
  group_by(surface.id, surface.name, sub_list, policy_period) %>%
  summarise(
    n_posts = n(),
    n_views_imputed = sum(views_imputed),
    pct_views_imputed = round(100 * n_views_imputed / n_posts, 2),
    total_views = sum(statistics.views, na.rm = TRUE),
    avg_views = mean(statistics.views, na.rm = TRUE),
    median_views = median(statistics.views, na.rm = TRUE),
    avg_reactions = mean(statistics.reaction_count, na.rm = TRUE),
    avg_shares = mean(statistics.share_count, na.rm = TRUE),
    avg_comments = mean(statistics.comment_count, na.rm = TRUE),
    avg_engagement_rate = mean(engagement_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = policy_period,
    values_from = c(n_posts, n_views_imputed, pct_views_imputed, total_views,
                    avg_views, median_views, avg_reactions,
                    avg_shares, avg_comments, avg_engagement_rate),
    names_sep = "_"
  )

cat("Created 6 analysis-ready datasets:\n")
cat("  1. cleaned_data:", nrow(cleaned_data), "posts\n")
cat("  2. data_both_periods:", nrow(data_both_periods), "posts\n")
cat("  3. weekly_data:", nrow(weekly_data), "rows (COMPLETE WEEKS ONLY)\n")
cat("  4. monthly_data:", nrow(monthly_data), "rows\n")
cat("  5. account_period_data:", nrow(account_period_data), "accounts\n")
cat("  6. surface_info:", nrow(surface_info), "surfaces\n\n")

# =============================================================================
# STEP 12: Save
# =============================================================================

cat("STEP 12: Saving cleaned datasets...\n")
cat(rule(80, "-"), "\n\n", sep = "")

if (!dir.exists("cleaned_data")) {
  dir.create("cleaned_data")
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

saveRDS(cleaned_data, paste0("cleaned_data/cleaned_posts_", timestamp, ".rds"))
saveRDS(accounts, paste0("cleaned_data/accounts_summary_", timestamp, ".rds"))
saveRDS(accounts_both_periods, paste0("cleaned_data/accounts_both_periods_", timestamp, ".rds"))
saveRDS(data_both_periods, paste0("cleaned_data/posts_both_periods_", timestamp, ".rds"))
saveRDS(weekly_data, paste0("cleaned_data/weekly_aggregation_", timestamp, ".rds"))
saveRDS(monthly_data, paste0("cleaned_data/monthly_aggregation_", timestamp, ".rds"))
saveRDS(account_period_data, paste0("cleaned_data/account_period_", timestamp, ".rds"))

saveRDS(surface_info, paste0("cleaned_data/surface_info_", timestamp, ".rds"))
cat("  surface_info_", timestamp, ".rds\n", sep = "")

write.csv(surface_info,
          paste0("cleaned_data/surface_info_", timestamp, ".csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("  surface_info_", timestamp, ".csv\n", sep = "")

# Account IDs for script 03, in several formats
writeLines(surface_ids_for_api$surface.id,
           paste0("cleaned_data/surface_ids_for_api_", timestamp, ".txt"))
cat("  surface_ids_for_api_", timestamp, ".txt (one ID per line)\n", sep = "")

write.csv(surface_ids_for_api,
          paste0("cleaned_data/surface_ids_for_api_", timestamp, ".csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("  surface_ids_for_api_", timestamp, ".csv\n", sep = "")

r_vector_file <- paste0("cleaned_data/surface_ids_r_vector_", timestamp, ".R")
r_vector_content <- paste0(
  "# Surface IDs for MCL API script\n",
  "# Generated: ", Sys.time(), "\n",
  "# Total IDs: ", nrow(surface_ids_for_api), "\n",
  "# Copy this vector to the 'account_ids' parameter in the MCL API script\n\n",
  "account_ids <- c(\n",
  paste0('  "', surface_ids_for_api$surface.id, '"', collapse = ",\n"),
  "\n)\n"
)
writeLines(r_vector_content, r_vector_file)
cat("  surface_ids_r_vector_", timestamp, ".R\n", sep = "")

for (list_name in unique(surface_ids_for_api$sub_list)) {
  list_ids <- surface_ids_for_api %>%
    filter(sub_list == list_name) %>%
    pull(surface.id)

  safe_list_name <- gsub("[^a-zA-Z0-9_]", "_", list_name)
  list_file <- paste0("cleaned_data/surface_ids_", safe_list_name, "_", timestamp, ".txt")
  writeLines(list_ids, list_file)
  cat("  surface_ids_", safe_list_name, "_", timestamp, ".txt (",
      length(list_ids), " IDs)\n", sep = "")
}

# Run metadata (settings, counts, imputation parameters)
mp_groups <- unique(data$sub_list[grepl("^MPs", data$sub_list)])
total_views_imputed <- sum(data$views_imputed)
pct_views_imputed <- round(100 * total_views_imputed / nrow(data), 2)
imputed_values_in_data <- data$statistics.views[data$views_imputed]

metadata <- list(
  timestamp = Sys.time(),
  source_file = basename(data_file),
  script_version = "v3.2_group_specific_imputation_complete_weeks",
  dataset_version = if (length(mp_groups) > 1) "v3.2_mp_split" else "v2_or_v3.1_single_mp",
  mp_groups = mp_groups,
  all_sub_lists = unique(data$sub_list),
  n_posts_original = n_before,
  n_posts_cleaned = nrow(cleaned_data),
  n_removed = n_removed,
  n_accounts = nrow(accounts),
  n_accounts_both_periods = nrow(accounts_both_periods),
  n_surfaces = nrow(surface_info),
  date_range = date_range,
  policy_dates = policy_dates,

  study_timeframe = list(
    start_date = study_start_date,
    end_date = study_end_date,
    description = "Posts outside this range were excluded from analysis"
  ),

  complete_weeks = list(
    enabled = TRUE,
    first_complete_week_start = first_complete_week,
    last_complete_week_end = last_complete_week_end,
    n_total_weeks = n_total_weeks,
    n_complete_weeks = n_complete_weeks,
    n_partial_weeks_start = n_incomplete_start,
    n_partial_weeks_end = n_incomplete_end,
    posts_in_partial_weeks = posts_in_incomplete_weeks,
    description = "Weekly aggregation only includes complete weeks (Sunday to Saturday)"
  ),

  integrity_checks = list(
    multi_list_accounts_found = !is.null(integrity_errors$multi_list_accounts),
    duplicate_posts_found = !is.null(integrity_errors$duplicate_posts),
    resolution_applied = if (has_critical_errors) multi_list_resolution else "none_needed",
    warnings = names(integrity_warnings)
  ),

  na_handling = list(
    views = list(
      method = "Group-specific ratio-weighted",
      description = "Power law fit to each group's near-threshold distribution (101-500 views)",
      configuration = list(
        bin_width = IMPUTATION_BIN_WIDTH,
        max_view = IMPUTATION_MAX_VIEW,
        min_bins = IMPUTATION_MIN_BINS,
        min_posts = IMPUTATION_MIN_POSTS,
        fallback_ratio = POOLED_FALLBACK_RATIO
      ),
      seed = 42,
      n_imputed = total_views_imputed,
      pct_imputed = pct_views_imputed,
      group_parameters = group_imputation_params,
      imputation_log = imputation_log,
      imputed_summary = list(
        mean = round(mean(imputed_values_in_data), 1),
        median = median(imputed_values_in_data),
        sd = round(sd(imputed_values_in_data), 1),
        pct_under_50 = round(100 * mean(imputed_values_in_data <= 50), 1)
      ),
      by_group = na_views_by_group
    ),
    engagement = list(
      method = "REMOVAL",
      n_posts_removed = n_engagement_removed,
      by_group = na_engagement_by_group
    )
  ),

  surface_info = list(
    description = "Comprehensive account/surface metadata extracted from posts",
    n_surfaces = nrow(surface_info),
    columns = names(surface_info),
    by_sub_list = as.data.frame(surface_info %>% count(sub_list)),
    api_query_files = c(
      paste0("surface_ids_for_api_", timestamp, ".txt"),
      paste0("surface_ids_for_api_", timestamp, ".csv"),
      paste0("surface_ids_r_vector_", timestamp, ".R")
    )
  ),

  data_flags = c("views_imputed", "views_pre_2017", "views_na_reason")
)

saveRDS(metadata, paste0("cleaned_data/metadata_", timestamp, ".rds"))
cat("Saved all datasets to cleaned_data/\n\n")

# =============================================================================
# STEP 13: Summary report
# =============================================================================

cat(rule(), "\n", sep = "")
cat("DATA VALIDATION AND CLEANING SUMMARY (v3.2)\n")
cat(rule(), "\n\n", sep = "")

cat("STUDY TIME-FRAME:\n")
cat("  Start date:", format(study_start_date, "%Y-%m-%d"), "\n")
cat("  End date:  ", format(study_end_date, "%Y-%m-%d"), "\n\n")

cat("COMPLETE WEEKS FILTERING:\n")
cat("  First complete week:", format(first_complete_week, "%Y-%m-%d"), "to",
    format(first_complete_week + days(6), "%Y-%m-%d"), "\n")
cat("  Last complete week: ", format(last_complete_week_start, "%Y-%m-%d"), "to",
    format(last_complete_week_end, "%Y-%m-%d"), "\n")
cat("  Total complete weeks:", n_complete_weeks, "\n")
cat("  Partial weeks excluded:", n_incomplete_start + n_incomplete_end, "\n")
cat("  Posts in partial weeks:", posts_in_incomplete_weeks,
    sprintf("(%.2f%%)\n\n", 100 * posts_in_incomplete_weeks / nrow(cleaned_data)))

cat("VIEW IMPUTATION (GROUP-SPECIFIC):\n")
for (g in names(group_imputation_params)) {
  p <- group_imputation_params[[g]]
  cat(sprintf("  %s: alpha=%.3f, ratio=%.2f -> mean=%.1f, median=%d\n",
              g, ifelse(is.na(p$alpha), NA, p$alpha), p$ratio,
              p$expected_mean, p$expected_median))
}
cat("  Total imputed:", total_views_imputed, sprintf("(%.1f%%)\n\n", pct_views_imputed))

cat("MERGE INTEGRITY:\n")
if (has_critical_errors) {
  cat("  Issues found and resolved\n")
  if (!is.null(integrity_errors$multi_list_accounts)) {
    cat("    - Multi-list accounts: ", nrow(integrity_errors$multi_list_accounts),
        " (resolution: ", multi_list_resolution, ")\n", sep = "")
  }
  if (!is.null(integrity_errors$duplicate_posts)) {
    cat("    - Duplicate posts: ", n_distinct(integrity_errors$duplicate_posts$id), "\n", sep = "")
  }
} else {
  cat("  All checks passed - no issues found\n")
}
cat("\n")

cat("DATA CLEANING:\n")
cat("  Original posts:", n_before, "\n")
cat("  Posts removed (invalid + NA engagement):", n_removed, "\n")
cat("  Final posts:", nrow(cleaned_data), "\n")
cat("  Unique accounts:", nrow(accounts), "\n")
cat("  Accounts in both periods:", nrow(accounts_both_periods), "\n")
cat("  Surface info records:", nrow(surface_info), "\n\n")

cat(rule(), "\n", sep = "")
cat("DATA VALIDATION AND CLEANING COMPLETE\n")
cat(rule(), "\n\n", sep = "")
