# =============================================================================
# 05 - RQ1: when and to what extent did the policy affect reach?
# Data-driven breakpoint detection with cross-algorithm validation
# Case Study 1 (thesis sections 5.1.2.4 and 5.1.3.1; Appendix B)
# =============================================================================
#
# Design (discovery-validation):
#   1. Detection - Bai-Perron (strucchange, metric ~ 1, h = 0.15, at most 3
#      breaks) and PELT (changepoint::cpt.meanvar, BIC penalty) are run on four
#      weekly series of the discovery group (Re-elected MCs): mean views,
#      reactions, shares and comments per post.
#   2. Clustering - all detected dates are sorted; consecutive dates no more
#      than 30 days apart join the same cluster (single linkage). Each cluster
#      gets a median date (point estimate), a range and a detection count.
#   3. Cross-validation - only clusters detected by BOTH algorithms are kept.
#   4. Mapping - T1 = first retained breakpoint; T3 = first retained
#      breakpoint on/after 2024-09-01 (else the last one); T2 = the retained
#      breakpoint between T1 and T3 with most detections (ties -> earlier).
#   5. Phase comparison - weekly mean views per post are compared across
#      phases within each group (Kruskal-Wallis; Dunn with Bonferroni), using
#      the same phases for the two validation groups.
#
# Election windows (2022 general election, 2024 municipal elections) are
# analysed descriptively as short-term fluctuations, not as breakpoints.
#
# Groups: MPs_Active2021_Reelected2022 (discovery, "Re-elected MCs"),
#         MPs_Elected_2022 ("Newly Elected MCs"),
#         Prominent_Politicians ("Prominent Politicians").
#
# Input : cleaned_data/weekly_aggregation_*.rds (most recent, from script 02)
#         cleaned_data/accounts_summary_*.rds   (for account counts)
# Output: RQ1_*.csv tables, RQ1_Figure1-5 *.png, RQ1_results_summary.rds
#         (written to the working directory)
# =============================================================================

# -----------------------------------------------------------------------------
# Packages (installed through fbrir inside the Meta SRE if missing)
# -----------------------------------------------------------------------------
required_packages <- c(
  "tidyverse", "lubridate", "strucchange", "changepoint", "scales"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    library(fbrir)
    cran <- CRAN$new()
    cran$InstallPackages(pkg)
  }
  library(pkg, character.only = TRUE)
}

rule <- function(n = 80, ch = "=") strrep(ch, n)

cat("\n", rule(), "\n", sep = "")
cat("RQ1 ANALYSIS: META'S POLITICAL CONTENT POLICY EFFECTS IN BRAZIL\n")
cat("Cross-Algorithm Validated Breakpoint Detection\n")
cat("With 2022 Brazilian General Election & 2024 Brazilian Municipal Elections\n")
cat(rule(), "\n\n", sep = "")

# =============================================================================
# STEP 1: Load the weekly series
# =============================================================================

cat("STEP 1: LOADING DATA\n")
cat(rule(40, "-"), "\n\n", sep = "")

weekly_files <- list.files(
  path = "cleaned_data",
  pattern = "weekly_aggregation_.*\\.rds$",
  full.names = TRUE
)
if (length(weekly_files) == 0) {
  stop("No weekly_aggregation files found in cleaned_data/")
}

# Most recent file (timestamps sort lexically)
weekly_file <- sort(weekly_files, decreasing = TRUE)[1]
cat("Loading weekly data:", weekly_file, "\n")
weekly_data <- readRDS(weekly_file)

cat("\nData structure:\n")
cat(" Rows:", nrow(weekly_data), "\n")
cat(" Columns:", ncol(weekly_data), "\n")
cat(" Date range:", as.character(min(weekly_data$week)), "to",
    as.character(max(weekly_data$week)), "\n\n")

# Identify the discovery group and the validation groups
mp_groups <- unique(weekly_data$sub_list[grepl("^MPs", weekly_data$sub_list)])
all_groups <- unique(weekly_data$sub_list)
dataset_version <- if (length(mp_groups) > 1) "v3.2_split" else "v2_v3.1"

discovery_group <- if ("MPs_Active2021_Reelected2022" %in% all_groups) "MPs_Active2021_Reelected2022" else "MPs"
validation_groups <- setdiff(all_groups, discovery_group)

cat("Dataset Configuration:\n")
cat(" Version:", dataset_version, "\n")
cat(" Discovery sample:", discovery_group, "\n")
cat(" Validation groups:", paste(validation_groups, collapse = ", "), "\n\n")

# =============================================================================
# STEP 2: Descriptive statistics (thesis Tables 5.4 and 5.5)
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 2: DESCRIPTIVE STATISTICS\n")
cat(rule(), "\n\n", sep = "")

accounts_files <- list.files(
  path = "cleaned_data",
  pattern = "accounts_summary_.*\\.rds$",
  full.names = TRUE
)

# --- Table 1: weeks, posts, total views and mean weekly views by group ------
cat("TABLE 1: Summary Statistics by Group\n")
cat(rule(60, "-"), "\n\n", sep = "")

table1_from_weekly <- weekly_data %>%
  group_by(sub_list) %>%
  summarise(
    n_weeks = n(),
    total_posts = sum(n_posts, na.rm = TRUE),
    total_views = sum(total_views, na.rm = TRUE),
    avg_views_overall = mean(avg_views, na.rm = TRUE),
    .groups = "drop"
  )

# Account counts: from accounts_summary if available, else the maximum
# weekly n_accounts
if (length(accounts_files) > 0) {
  accounts_summary <- readRDS(sort(accounts_files, decreasing = TRUE)[1])
  account_counts <- accounts_summary %>%
    group_by(sub_list) %>%
    summarise(n_accounts = n(), .groups = "drop")
  table1_from_weekly <- table1_from_weekly %>%
    left_join(account_counts, by = "sub_list")
} else {
  if ("n_accounts" %in% names(weekly_data)) {
    account_counts <- weekly_data %>%
      group_by(sub_list) %>%
      summarise(n_accounts = max(n_accounts, na.rm = TRUE), .groups = "drop")
    table1_from_weekly <- table1_from_weekly %>%
      left_join(account_counts, by = "sub_list")
  } else {
    table1_from_weekly$n_accounts <- NA
  }
}

# Version with Italian group labels (working-paper draft)
table1 <- table1_from_weekly %>%
  mutate(
    gruppo = case_when(
      sub_list == "MPs_Active2021_Reelected2022" ~ "Politici eletti in parlamento nel 2022 e già precedentemente parlamentari",
      sub_list == "MPs_Elected_2022" ~ "Politici eletti in parlamento nel 2022 e non precedentemente parlamentari",
      sub_list == "Prominent_Politicians" ~ "Politici noti all'opinione pubblica",
      TRUE ~ sub_list
    ),
    n_accounts_fmt = ifelse(is.na(n_accounts), "-", format(n_accounts, big.mark = ",")),
    n_posts_fmt = format(total_posts, big.mark = ","),
    avg_views_fmt = format(round(avg_views_overall, 0), big.mark = ",")
  )

table1_display <- data.frame(
  gruppo = table1$gruppo,
  n_accounts = table1$n_accounts_fmt,
  n_posts = table1$n_posts_fmt,
  avg_views = table1$avg_views_fmt,
  stringsAsFactors = FALSE
)

cat("TABLE 1:\n\n")
print(table1_display, row.names = FALSE)
cat("\n")

write.csv(table1_display, "RQ1_Table1_summary.csv", row.names = FALSE)
cat("Saved: RQ1_Table1_summary.csv\n\n")

table1_en <- data.frame(
  Group = table1_from_weekly$sub_list,
  N_Accounts = ifelse(is.na(table1_from_weekly$n_accounts), NA, table1_from_weekly$n_accounts),
  N_Posts = table1_from_weekly$total_posts,
  Avg_Views = round(table1_from_weekly$avg_views_overall, 0),
  stringsAsFactors = FALSE
)

cat("TABLE 1 - English Version:\n\n")
print(table1_en, row.names = FALSE)
cat("\n")

# --- Table C: weekly mean views/reactions/shares/comments per post ---------
# (thesis Table 5.5)
cat("\nTABLE C: Weekly Aggregated Engagement Statistics\n")
cat(rule(60, "-"), "\n\n", sep = "")

tableC_stats <- weekly_data %>%
  group_by(sub_list) %>%
  summarise(
    views_mean = mean(avg_views, na.rm = TRUE),
    views_sd = sd(avg_views, na.rm = TRUE),
    views_median = median(avg_views, na.rm = TRUE),
    views_min = min(avg_views, na.rm = TRUE),
    views_max = max(avg_views, na.rm = TRUE),
    reactions_mean = mean(avg_reactions, na.rm = TRUE),
    reactions_sd = sd(avg_reactions, na.rm = TRUE),
    reactions_median = median(avg_reactions, na.rm = TRUE),
    reactions_min = min(avg_reactions, na.rm = TRUE),
    reactions_max = max(avg_reactions, na.rm = TRUE),
    shares_mean = mean(avg_shares, na.rm = TRUE),
    shares_sd = sd(avg_shares, na.rm = TRUE),
    shares_median = median(avg_shares, na.rm = TRUE),
    shares_min = min(avg_shares, na.rm = TRUE),
    shares_max = max(avg_shares, na.rm = TRUE),
    comments_mean = mean(avg_comments, na.rm = TRUE),
    comments_sd = sd(avg_comments, na.rm = TRUE),
    comments_median = median(avg_comments, na.rm = TRUE),
    comments_min = min(avg_comments, na.rm = TRUE),
    comments_max = max(avg_comments, na.rm = TRUE),
    .groups = "drop"
  )

# Print one block per metric
print_metric_block <- function(prefix, title) {
  cat(title, ":\n", sep = "")
  block <- tableC_stats %>%
    transmute(
      Group = sub_list,
      Mean = round(.data[[paste0(prefix, "_mean")]], 1),
      SD = round(.data[[paste0(prefix, "_sd")]], 1),
      Median = round(.data[[paste0(prefix, "_median")]], 1),
      Min = round(.data[[paste0(prefix, "_min")]], 1),
      Max = round(.data[[paste0(prefix, "_max")]], 1)
    )
  print(as.data.frame(block), row.names = FALSE)
  cat("\n")
  invisible(block)
}

tableC_views <- print_metric_block("views", "Views (Reach)")
tableC_reactions <- print_metric_block("reactions", "Reactions")
tableC_shares <- print_metric_block("shares", "Shares")
tableC_comments <- print_metric_block("comments", "Comments")

# Long format: one row per group x metric
tableC_long <- tableC_stats %>%
  pivot_longer(
    cols = -sub_list,
    names_to = c("metric", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  rename(Group = sub_list, Metric = metric)

write.csv(tableC_long, "RQ1_TableC_engagement_stats.csv", row.names = FALSE)
cat("Saved: RQ1_TableC_engagement_stats.csv\n\n")

# =============================================================================
# STEP 3: Reference dates (Meta announcements and election windows)
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 3: KEY DATES REFERENCE\n")
cat(rule(), "\n\n", sep = "")

# Meta's announced dates (reference only; not used to define phases)
meta_policy_dates <- data.frame(
  date = as.Date(c("2021-02-10", "2022-07-19", "2023-04-20", "2025-01-07")),
  event = c(
    "Initial announcement",
    "Global implementation",
    "Refinements (survey-based signals)",
    "Policy reversal"
  ),
  stringsAsFactors = FALSE
)

# Election days (first and second round). They are used only for the dotted
# markers in the figures and in the reference tables; the election analysis
# in STEP 8 uses the windows below.
# 2022 General Election: 1st round 2 October, 2nd round 30 October 2022
brazilian_election_rounds <- as.Date(c("2022-10-02", "2022-10-30"))
brazilian_election_window_start <- as.Date("2022-08-01")
brazilian_election_window_end <- as.Date("2022-11-30")

# 2024 Municipal Elections: 1st round 6 October, 2nd round 27 October 2024
municipal_election_rounds <- as.Date(c("2024-10-06", "2024-10-27"))
municipal_election_window_start <- as.Date("2024-09-20")
municipal_election_window_end <- as.Date("2024-12-08")

election_events <- data.frame(
  election = c("Brazilian General Election 2022", "Municipal Elections"),
  round_1 = c(brazilian_election_rounds[1], municipal_election_rounds[1]),
  round_2 = c(brazilian_election_rounds[2], municipal_election_rounds[2]),
  window_start = c(brazilian_election_window_start, municipal_election_window_start),
  window_end = c(brazilian_election_window_end, municipal_election_window_end),
  stringsAsFactors = FALSE
)

cat("META POLICY TIMELINE:\n")
for (i in 1:nrow(meta_policy_dates)) {
  cat(" ", as.character(meta_policy_dates$date[i]), " - ",
      meta_policy_dates$event[i], "\n", sep = "")
}
cat("\n")

cat("ELECTORAL EVENTS ANALYZED:\n")
cat(rule(60, "-"), "\n\n", sep = "")
cat("1. BRAZILIAN GENERAL ELECTION 2022:\n")
cat(" Election days (1st, 2nd round):", paste(as.character(brazilian_election_rounds), collapse = ", "), "\n")
cat(" Analysis window:", as.character(brazilian_election_window_start), "to",
    as.character(brazilian_election_window_end), "\n\n")
cat("2. BRAZILIAN MUNICIPAL ELECTIONS 2024:\n")
cat(" Election days (1st, 2nd round):", paste(as.character(municipal_election_rounds), collapse = ", "), "\n")
cat(" Analysis window:", as.character(municipal_election_window_start), "to",
    as.character(municipal_election_window_end), "\n\n")

write.csv(election_events, "RQ1_election_events.csv", row.names = FALSE)
cat("Saved: RQ1_election_events.csv\n\n")

# =============================================================================
# STEP 4: Breakpoint detection on the discovery group
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 4: BREAKPOINT DETECTION (Discovery Sample)\n")
cat("Discovery sample:", discovery_group, "\n")
cat(rule(), "\n\n", sep = "")

discovery_data <- weekly_data %>%
  filter(sub_list == discovery_group) %>%
  arrange(week) %>%
  mutate(time_index = row_number())

cat("Discovery sample size:", nrow(discovery_data), "weeks\n\n")

# --- 4.1 Bai-Perron: shifts in the mean (metric ~ 1) -----------------------

cat("4.1 BAI-PERRON STRUCTURAL BREAK DETECTION\n")
cat(rule(60, "-"), "\n\n", sep = "")

detect_bai_perron <- function(data, metric, max_breaks = 3) {

  clean_data <- data %>%
    filter(!is.na(.data[[metric]])) %>%
    mutate(time_index = row_number())

  bp_result <- tryCatch({
    breakpoints(
      as.formula(paste(metric, "~ 1")),
      data = clean_data,
      h = 0.15,
      breaks = max_breaks
    )
  }, error = function(e) {
    cat(" Error for", metric, ":", e$message, "\n")
    return(NULL)
  })

  if (is.null(bp_result) || is.na(bp_result$breakpoints[1])) {
    return(NULL)
  }

  # Convert break indices to week dates
  break_indices <- bp_result$breakpoints
  break_dates <- clean_data$week[break_indices]

  cat(" ", metric, ": ", length(break_dates), " breakpoints detected\n", sep = "")
  for (i in seq_along(break_dates)) {
    cat(" Break", i, ":", as.character(break_dates[i]), "\n")
  }

  return(list(dates = break_dates, indices = break_indices,
              metric = metric, algorithm = "Bai-Perron"))
}

bp_results <- list()
for (metric in c("avg_views", "avg_reactions", "avg_shares", "avg_comments")) {
  bp_results[[metric]] <- detect_bai_perron(discovery_data, metric)
}
cat("\n")

# --- 4.2 PELT: shifts in mean and variance (BIC penalty) -------------------

cat("4.2 PELT CHANGEPOINT DETECTION\n")
cat(rule(60, "-"), "\n\n", sep = "")

detect_pelt <- function(data, metric) {

  clean_data <- data %>% filter(!is.na(.data[[metric]]))
  values <- clean_data[[metric]]

  pelt_result <- tryCatch({
    cpt.meanvar(values, method = "PELT", penalty = "BIC")
  }, error = function(e) {
    cat(" Error for", metric, ":", e$message, "\n")
    return(NULL)
  })

  if (is.null(pelt_result)) return(NULL)

  changepoints <- cpts(pelt_result)

  if (length(changepoints) == 0) {
    cat(" ", metric, ": No changepoints detected\n", sep = "")
    return(NULL)
  }

  change_dates <- clean_data$week[changepoints]

  cat(" ", metric, ": ", length(change_dates), " changepoints detected\n", sep = "")
  for (i in seq_along(change_dates)) {
    cat(" Changepoint", i, ":", as.character(change_dates[i]), "\n")
  }

  return(list(dates = change_dates, indices = changepoints,
              metric = metric, algorithm = "PELT"))
}

pelt_results <- list()
for (metric in c("avg_views", "avg_reactions", "avg_shares", "avg_comments")) {
  pelt_results[[metric]] <- detect_pelt(discovery_data, metric)
}
cat("\n")

# --- 4.3 Cluster detections into consensus breakpoints ---------------------

cat("4.3 BUILDING CONSENSUS BREAKPOINTS\n")
cat(rule(60, "-"), "\n\n", sep = "")

build_consensus_breakpoints <- function(bp_list, pelt_list, tolerance_days = 30) {

  # One row per (date, metric, algorithm) detection
  all_detections <- data.frame()

  for (metric in names(bp_list)) {
    if (!is.null(bp_list[[metric]])) {
      all_detections <- rbind(all_detections, data.frame(
        date = bp_list[[metric]]$dates, metric = metric,
        algorithm = "Bai-Perron", stringsAsFactors = FALSE
      ))
    }
  }
  for (metric in names(pelt_list)) {
    if (!is.null(pelt_list[[metric]])) {
      all_detections <- rbind(all_detections, data.frame(
        date = pelt_list[[metric]]$dates, metric = metric,
        algorithm = "PELT", stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(all_detections) == 0) {
    cat("No breakpoints detected by any method.\n")
    return(NULL)
  }

  all_detections <- all_detections %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)

  cat("Total detections:", nrow(all_detections), "\n\n")

  # Single-linkage clustering: a new cluster starts whenever the gap to the
  # previous detection exceeds tolerance_days. A cluster can therefore span
  # more than tolerance_days in total.
  all_detections <- all_detections %>%
    mutate(cluster = cumsum(c(1, diff(date) > tolerance_days)))

  consensus <- all_detections %>%
    group_by(cluster) %>%
    summarise(
      consensus_date = median(date),
      date_min = min(date),
      date_max = max(date),
      date_range_days = as.numeric(max(date) - min(date)),
      n_methods = n(),                      # number of detections in the cluster
      n_algorithms = n_distinct(algorithm),
      has_bai_perron = any(algorithm == "Bai-Perron"),
      has_pelt = any(algorithm == "PELT"),
      metrics_bp = paste(unique(metric[algorithm == "Bai-Perron"]), collapse = ", "),
      metrics_pelt = paste(unique(metric[algorithm == "PELT"]), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(
      cross_validated = n_algorithms >= 2,
      strength = case_when(
        cross_validated & n_methods >= 6 ~ "VERY STRONG",
        cross_validated & n_methods >= 4 ~ "STRONG",
        cross_validated ~ "MODERATE",
        n_methods >= 4 ~ "Single-Algo (Many)",
        TRUE ~ "WEAK"
      ),
      date_range = ifelse(
        date_min == date_max,
        as.character(date_min),
        paste(date_min, "to", date_max)
      )
    ) %>%
    arrange(consensus_date)

  cat("CONSENSUS BREAKPOINTS:\n\n")
  cat("Note: 'Consensus date' is the MEDIAN of the detections in a cluster\n")
  cat(" (consecutive detections at most", tolerance_days, "days apart).\n")
  cat(" 'n_methods' counts algorithm x metric detections; with single linkage\n")
  cat(" the same combination can contribute more than once, so it can exceed 8.\n")
  cat(" 'Cross-Validated' = detected by BOTH Bai-Perron AND PELT.\n\n")

  for (i in 1:nrow(consensus)) {
    cat("Cluster", i, ":\n")
    cat(" Consensus date (median):", as.character(consensus$consensus_date[i]), "\n")
    cat(" Detection range:", consensus$date_range[i],
        "(", consensus$date_range_days[i], "days spread)\n")
    cat(" Methods:", consensus$n_methods[i], "| Algorithms:", consensus$n_algorithms[i], "\n")
    cat(" Bai-Perron metrics:", consensus$metrics_bp[i], "\n")
    cat(" PELT metrics:", consensus$metrics_pelt[i], "\n")
    cat(" Cross-Validated:", ifelse(consensus$cross_validated[i], "YES", "NO"), "\n")
    cat(" Strength:", consensus$strength[i], "\n\n")
  }

  return(consensus)
}

consensus_breakpoints <- build_consensus_breakpoints(bp_results, pelt_results)

# =============================================================================
# STEP 5: Select T1, T2, T3
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 5: SELECT FINAL BREAKPOINTS\n")
cat(rule(), "\n\n", sep = "")

# Keep only clusters detected by both algorithms
cross_validated <- consensus_breakpoints %>%
  filter(cross_validated == TRUE) %>%
  arrange(consensus_date)

cat("CROSS-VALIDATED BREAKPOINTS (Detected by BOTH Bai-Perron AND PELT):\n")
cat(rule(60, "-"), "\n\n", sep = "")

# Fallback (not triggered in the thesis data): clusters with >= 4 detections
if (nrow(cross_validated) == 0) {
  cat("WARNING: No cross-validated breakpoints found!\n")
  cat("Falling back to breakpoints with >=4 method detections.\n\n")
  cross_validated <- consensus_breakpoints %>%
    filter(n_methods >= 4) %>%
    arrange(consensus_date)
}

for (i in 1:nrow(cross_validated)) {
  cat(sprintf("%d. %s (%d methods, %s)\n",
              i,
              as.character(cross_validated$consensus_date[i]),
              cross_validated$n_methods[i],
              cross_validated$strength[i]))
}
cat("\n")

# Mapping rules (thesis 5.1.2.4 and Appendix B3):
#   >= 3 breakpoints: T1 = first; T3 = first on/after 2024-09-01 (else last);
#                     T2 = most detections between T1 and T3 (ties -> earlier)
#   2 breakpoints:    T1 and T3, no T2
#   1 breakpoint:     T1 only
assign_final_breakpoints <- function(cv_bp) {

  n_bp <- nrow(cv_bp)
  if (n_bp == 0) {
    stop("No breakpoints available for analysis!")
  }

  cat("\n--- Breakpoint Selection Process ---\n")
  cat("Total cross-validated breakpoints available:", n_bp, "\n\n")

  # Store date, range and evidence for one cluster
  create_bp_struct <- function(row) {
    if (is.null(row) || nrow(row) == 0) return(NULL)
    list(
      date = as.Date(row$consensus_date),
      date_min = as.Date(row$date_min),
      date_max = as.Date(row$date_max),
      date_range_days = row$date_range_days,
      methods = row$n_methods,
      n_algorithms = row$n_algorithms,
      cross_validated = row$cross_validated,
      strength = row$strength,
      date_range = row$date_range
    )
  }

  breakpoints <- list()
  selected_indices <- c()

  # T1: first breakpoint
  if (n_bp >= 1) {
    breakpoints$T1 <- create_bp_struct(cv_bp[1, ])
    selected_indices <- c(selected_indices, 1)
    cat("T1 Selection: First chronological breakpoint\n")
    cat(" -> Selected:", as.character(cv_bp$consensus_date[1]),
        "(", cv_bp$n_methods[1], "methods,", cv_bp$strength[1], ")\n\n")
  }

  if (n_bp >= 3) {

    # T3: first breakpoint on/after 2024-09-01, else the last one
    cat("T3 Selection: First breakpoint after 2024-09-01 (reversal period)\n")
    reversal_candidates <- cv_bp %>%
      mutate(row_idx = row_number()) %>%
      filter(consensus_date >= as.Date("2024-09-01"))

    if (nrow(reversal_candidates) > 0) {
      cat(" Reversal candidates found:", nrow(reversal_candidates), "\n")
      for (i in 1:nrow(reversal_candidates)) {
        cat(" -", as.character(reversal_candidates$consensus_date[i]),
            "(", reversal_candidates$n_methods[i], "methods)\n")
      }
      selected_idx <- reversal_candidates$row_idx[1]
      breakpoints$T3 <- create_bp_struct(cv_bp[selected_idx, ])
      selected_indices <- c(selected_indices, selected_idx)
      cat(" -> Selected:", as.character(cv_bp$consensus_date[selected_idx]), "\n\n")
    } else {
      cat(" No breakpoints after 2024-09-01; using last chronological breakpoint\n")
      breakpoints$T3 <- create_bp_struct(cv_bp[n_bp, ])
      selected_indices <- c(selected_indices, n_bp)
      cat(" -> Selected:", as.character(cv_bp$consensus_date[n_bp]), "\n\n")
    }

    # T2: strongest breakpoint strictly between T1 and T3
    cat("T2 Selection: Strongest intermediate breakpoint between T1 and T3\n")
    middle_candidates <- cv_bp %>%
      mutate(row_idx = row_number()) %>%
      filter(consensus_date > breakpoints$T1$date,
             consensus_date < breakpoints$T3$date) %>%
      arrange(desc(n_methods), consensus_date)

    if (nrow(middle_candidates) > 0) {
      cat(" Intermediate candidates found:", nrow(middle_candidates), "\n")
      for (i in 1:nrow(middle_candidates)) {
        cat(" -", as.character(middle_candidates$consensus_date[i]),
            "(", middle_candidates$n_methods[i], "methods,",
            middle_candidates$strength[i], ")\n")
      }
      selected_idx <- middle_candidates$row_idx[1]
      breakpoints$T2 <- create_bp_struct(cv_bp[selected_idx, ])
      selected_indices <- c(selected_indices, selected_idx)
      cat(" -> Selected:", as.character(cv_bp$consensus_date[selected_idx]),
          "(highest method count)\n\n")

      if (nrow(middle_candidates) > 1) {
        skipped <- middle_candidates[-1, ]
        cat(" Note:", nrow(skipped), "intermediate breakpoint(s) not selected:\n")
        for (i in 1:nrow(skipped)) {
          cat(" - ", as.character(skipped$consensus_date[i]),
              " (", skipped$n_methods[i], " methods)\n", sep = "")
        }
        cat(" These are documented in consensus_breakpoints but not used in phase structure.\n\n")
      }
    } else {
      cat(" No intermediate breakpoints found between T1 and T3\n")
      cat(" -> T2 not assigned (using two-breakpoint model structure)\n\n")
      breakpoints$T2 <- NULL
    }

  } else if (n_bp == 2) {
    cat("Two breakpoints detected -> Two-breakpoint model\n")
    breakpoints$T3 <- create_bp_struct(cv_bp[2, ])
    selected_indices <- c(selected_indices, 2)
    breakpoints$T2 <- NULL
    cat(" T3 (Reversal):", as.character(cv_bp$consensus_date[2]), "\n\n")

  } else {
    cat("Single breakpoint detected -> One-breakpoint model\n\n")
    breakpoints$T2 <- NULL
    breakpoints$T3 <- NULL
  }

  # Report retained breakpoints that were not mapped to T1/T2/T3
  all_indices <- 1:n_bp
  unselected <- setdiff(all_indices, selected_indices)
  if (length(unselected) > 0 && n_bp > 3) {
    cat("ADDITIONAL BREAKPOINTS NOT SELECTED FOR MODEL:\n")
    for (idx in unselected) {
      cat(" -", as.character(cv_bp$consensus_date[idx]),
          "(", cv_bp$n_methods[idx], "methods,", cv_bp$strength[idx], ")\n")
    }
    cat(" These remain in consensus_breakpoints for reference.\n\n")
  }

  cat("--- End Selection Process ---\n\n")
  return(breakpoints)
}

FINAL_BREAKPOINTS <- assign_final_breakpoints(cross_validated)

# Print the selected breakpoints
cat("FINAL BREAKPOINT ASSIGNMENT:\n")
cat(rule(60), "\n\n", sep = "")

print_breakpoint <- function(bp, label, missing_msg = "NOT DETECTED") {
  cat(label, ":\n", sep = "")
  if (!is.null(bp)) {
    cat(" Point estimate:", as.character(bp$date), "\n")
    cat(" Detection range:", bp$date_range,
        "(±", round(bp$date_range_days / 2), "days)\n", sep = "")
    cat(" Methods:", bp$methods, "\n")
    cat(" Cross-Validated:", ifelse(bp$cross_validated, "YES", "NO"), "\n")
    cat(" Strength:", bp$strength, "\n")
  } else {
    cat(" ", missing_msg, "\n")
  }
  cat("\n")
}

print_breakpoint(FINAL_BREAKPOINTS$T1, "T1 (Policy Implementation)")
print_breakpoint(FINAL_BREAKPOINTS$T2, "T2 (Policy Adjustment)",
                 "NOT DETECTED (Using two-breakpoint model)")
print_breakpoint(FINAL_BREAKPOINTS$T3, "T3 (Policy Reversal)")

MODEL_TYPE <- case_when(
  !is.null(FINAL_BREAKPOINTS$T2) ~ "THREE_BREAKPOINT",
  !is.null(FINAL_BREAKPOINTS$T3) ~ "TWO_BREAKPOINT",
  TRUE ~ "ONE_BREAKPOINT"
)
cat("MODEL TYPE:", MODEL_TYPE, "\n\n")

# =============================================================================
# STEP 6: Assign phases and election-window flags to every week
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 6: APPLY PHASES TO ALL DATA\n")
cat(rule(), "\n\n", sep = "")

apply_phases <- function(data, breakpoints, model_type) {

  T1 <- breakpoints$T1$date
  T2 <- if (!is.null(breakpoints$T2)) breakpoints$T2$date else NULL
  T3 <- if (!is.null(breakpoints$T3)) breakpoints$T3$date else NULL

  # A week belongs to the phase in which it starts (week = Sunday start)
  if (model_type == "THREE_BREAKPOINT") {
    data <- data %>%
      mutate(
        phase = case_when(
          week < T1 ~ "0_Pre-Policy",
          week >= T1 & week < T2 ~ "1_Policy-Active",
          week >= T2 & week < T3 ~ "2_Adjusted-Policy",
          week >= T3 ~ "3_Post-Reversal"
        ),
        phase = factor(phase, levels = c(
          "0_Pre-Policy", "1_Policy-Active", "2_Adjusted-Policy", "3_Post-Reversal"
        ))
      )
  } else if (model_type == "TWO_BREAKPOINT") {
    data <- data %>%
      mutate(
        phase = case_when(
          week < T1 ~ "0_Pre-Policy",
          week >= T1 & week < T3 ~ "1_Policy-Active",
          week >= T3 ~ "2_Post-Reversal"
        ),
        phase = factor(phase, levels = c("0_Pre-Policy", "1_Policy-Active", "2_Post-Reversal"))
      )
  } else {
    data <- data %>%
      mutate(
        phase = case_when(
          week < T1 ~ "0_Pre-Policy",
          TRUE ~ "1_Policy-Active"
        ),
        phase = factor(phase, levels = c("0_Pre-Policy", "1_Policy-Active"))
      )
  }

  # Election-window flags
  data <- data %>%
    mutate(
      in_brazilian_election_window = week >= brazilian_election_window_start &
        week <= brazilian_election_window_end,
      in_municipal_election_window = week >= municipal_election_window_start &
        week <= municipal_election_window_end,
      in_any_election_window = in_brazilian_election_window | in_municipal_election_window,
      election_period = case_when(
        in_brazilian_election_window ~ "Brazilian_2022",
        in_municipal_election_window ~ "Municipal_2024",
        TRUE ~ "Non-Election"
      )
    )

  return(data)
}

weekly_data_phased <- apply_phases(weekly_data, FINAL_BREAKPOINTS, MODEL_TYPE)

discovery_data_phased <- weekly_data_phased %>%
  filter(sub_list == discovery_group)

# Phase boundaries and length (thesis Table 5.7)
cat("PHASE STRUCTURE (", MODEL_TYPE, "):\n", sep = "")
cat(rule(60, "-"), "\n\n", sep = "")

phase_structure <- weekly_data_phased %>%
  group_by(phase) %>%
  summarise(
    start_date = min(week),
    end_date = max(week),
    n_weeks = n(),          # group x week rows (all groups)
    .groups = "drop"
  )
print(as.data.frame(phase_structure))
cat("\n")

cat("ELECTION WINDOW COVERAGE:\n")
cat(rule(60, "-"), "\n\n", sep = "")

election_coverage <- weekly_data_phased %>%
  filter(sub_list == discovery_group) %>%
  group_by(election_period) %>%
  summarise(
    n_weeks = n(),
    date_range = paste(min(week), "to", max(week)),
    .groups = "drop"
  )
print(as.data.frame(election_coverage))
cat("\n")

# =============================================================================
# STEP 7: Magnitude of change, discovery group
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 7: MAGNITUDE ASSESSMENT (Discovery Sample)\n")
cat(rule(), "\n\n", sep = "")

discovery_phase_stats <- discovery_data_phased %>%
  group_by(phase) %>%
  summarise(
    n_weeks = n(),
    mean_views = mean(avg_views, na.rm = TRUE),
    median_views = median(avg_views, na.rm = TRUE),
    sd_views = sd(avg_views, na.rm = TRUE),
    mean_reactions = mean(avg_reactions, na.rm = TRUE),
    mean_shares = mean(avg_shares, na.rm = TRUE),
    mean_comments = mean(avg_comments, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(phase)

cat("PHASE STATISTICS FOR", discovery_group, ":\n")
cat(rule(60, "-"), "\n\n", sep = "")
print(as.data.frame(discovery_phase_stats %>%
                      mutate(across(where(is.numeric), ~round(., 1)))))
cat("\n")

# --- Table 2b: engagement by phase, discovery group ------------------------
cat("\nTABLE 2b: Engagement Metrics by Policy Phase (", discovery_group, "):\n", sep = "")
cat(rule(60, "-"), "\n\n", sep = "")

table2b <- discovery_data_phased %>%
  filter(!is.na(phase)) %>%
  group_by(phase) %>%
  summarise(
    Reactions_Mean = mean(avg_reactions, na.rm = TRUE),
    Shares_Mean = mean(avg_shares, na.rm = TRUE),
    Comments_Mean = mean(avg_comments, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(phase)

cat("Engagement Metrics by Phase:\n\n")
print(as.data.frame(table2b %>% mutate(across(where(is.numeric), ~round(., 1)))))
cat("\n")

# Percentage change of each engagement metric relative to T0
if (nrow(table2b) >= 2) {
  baseline_reactions <- table2b$Reactions_Mean[1]
  baseline_shares <- table2b$Shares_Mean[1]
  baseline_comments <- table2b$Comments_Mean[1]

  cat("Engagement Changes from Baseline:\n")
  for (i in 2:nrow(table2b)) {
    phase_name <- gsub("^[0-3]_", "", as.character(table2b$phase[i]))
    reactions_chg <- 100 * (table2b$Reactions_Mean[i] - baseline_reactions) / baseline_reactions
    shares_chg <- 100 * (table2b$Shares_Mean[i] - baseline_shares) / baseline_shares
    comments_chg <- 100 * (table2b$Comments_Mean[i] - baseline_comments) / baseline_comments

    cat(" ", phase_name, ":\n", sep = "")
    cat(" Reactions: ", sprintf("%+.1f%%", reactions_chg), "\n", sep = "")
    cat(" Shares: ", sprintf("%+.1f%%", shares_chg), "\n", sep = "")
    cat(" Comments: ", sprintf("%+.1f%%", comments_chg), "\n", sep = "")
  }
  cat("\n")
}

write.csv(table2b, "RQ1_Table2b_engagement_by_phase.csv", row.names = FALSE)
cat("Saved: RQ1_Table2b_engagement_by_phase.csv\n\n")

# --- Phase-to-phase change in mean views, discovery group -----------------
cat("SEQUENTIAL REACH CHANGES:\n")
cat(rule(40, "-"), "\n\n", sep = "")

phase_means <- discovery_phase_stats$mean_views
phase_names <- as.character(discovery_phase_stats$phase)
observed_directions <- c()

for (i in 2:length(phase_means)) {
  delta <- phase_means[i] - phase_means[i - 1]
  delta_pct <- 100 * delta / phase_means[i - 1]
  direction <- ifelse(delta > 0, "UP", "DOWN")
  observed_directions <- c(observed_directions, direction)

  prev_name <- gsub("^[0-3]_", "", phase_names[i - 1])
  curr_name <- gsub("^[0-3]_", "", phase_names[i])

  cat("Transition", i - 1, ":", prev_name, "->", curr_name, "\n")
  cat(" ", format(round(phase_means[i - 1], 0), big.mark = ","), " -> ",
      format(round(phase_means[i], 0), big.mark = ","), "\n", sep = "")
  cat(" Change:", sprintf("%+.1f%%", delta_pct), direction, "\n\n")
}

# Expected direction: down at the first transition, up at the last
cat("PATTERN EVALUATION:\n")
cat(rule(40, "-"), "\n\n", sep = "")
cat("Observed pattern:", paste(observed_directions, collapse = " "), "\n")

if (MODEL_TYPE == "THREE_BREAKPOINT") {
  T1_correct <- length(observed_directions) >= 1 && observed_directions[1] == "DOWN"
  T3_correct <- length(observed_directions) >= 1 && observed_directions[length(observed_directions)] == "UP"
} else if (MODEL_TYPE == "TWO_BREAKPOINT") {
  T1_correct <- length(observed_directions) >= 1 && observed_directions[1] == "DOWN"
  T3_correct <- length(observed_directions) >= 1 && observed_directions[length(observed_directions)] == "UP"
} else {
  T1_correct <- length(observed_directions) >= 1 && observed_directions[1] == "DOWN"
  T3_correct <- NA
}

cat("T1 (Implementation -> decrease):", ifelse(T1_correct, "CONFIRMED", "UNEXPECTED"), "\n")
if (!is.na(T3_correct)) {
  cat("T3 (Reversal -> increase):", ifelse(T3_correct, "CONFIRMED", "UNEXPECTED"), "\n")
}
cat("\n")

# =============================================================================
# RQ1 headline figures (thesis Table 5.9a, discovery group)
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("RQ1 FINDINGS\n")
cat(rule(), "\n\n", sep = "")

phase_names_clean <- gsub("^[0-3]_", "", as.character(discovery_phase_stats$phase))

# Baseline = T0; trough = lowest mean among the policy phases (T1, T2);
# post-reversal = last phase
baseline_mean <- discovery_phase_stats$mean_views[1]
policy_phases <- discovery_phase_stats %>%
  filter(grepl("Policy", phase) & !grepl("Post-Reversal|Pre-Policy", phase))
trough_mean <- min(policy_phases$mean_views, na.rm = TRUE)
post_reversal_mean <- discovery_phase_stats$mean_views[nrow(discovery_phase_stats)]

decline_from_baseline <- 100 * (trough_mean - baseline_mean) / baseline_mean
recovery_from_trough <- 100 * (post_reversal_mean - trough_mean) / trough_mean
recovery_vs_baseline <- 100 * (post_reversal_mean - baseline_mean) / baseline_mean
recovery_pct_of_baseline <- 100 * post_reversal_mean / baseline_mean

cat("KEY STATISTICS FOR", discovery_group, ":\n")
cat(rule(60, "-"), "\n\n", sep = "")
cat("BASELINE (Pre-Policy):\n")
cat(" Mean reach:", format(round(baseline_mean, 0), big.mark = ","), "views/post\n\n")
cat("TROUGH (During Policy):\n")
cat(" Mean reach:", format(round(trough_mean, 0), big.mark = ","), "views/post\n")
cat(" Decline from baseline:", sprintf("%.1f%%", decline_from_baseline), "\n\n")
cat("POST-REVERSAL:\n")
cat(" Mean reach:", format(round(post_reversal_mean, 0), big.mark = ","), "views/post\n")
cat(" Recovery from trough:", sprintf("%+.1f%%", recovery_from_trough), "\n")
cat(" Recovery vs baseline:", sprintf("%.1f%% of pre-policy level", recovery_pct_of_baseline), "\n\n")

rq1_findings <- list(
  baseline_mean = baseline_mean,
  trough_mean = trough_mean,
  post_reversal_mean = post_reversal_mean,
  decline_pct = decline_from_baseline,
  recovery_from_trough_pct = recovery_from_trough,
  recovery_vs_baseline_pct = recovery_vs_baseline,
  recovery_pct_of_baseline = recovery_pct_of_baseline
)

rq1_findings_df <- data.frame(
  Metric = c("Baseline (Pre-Policy) Mean Views",
             "Trough (During Policy) Mean Views",
             "Post-Reversal Mean Views",
             "Decline from Baseline (%)",
             "Recovery from Trough (%)",
             "Recovery as % of Baseline"),
  Value = c(round(baseline_mean, 0),
            round(trough_mean, 0),
            round(post_reversal_mean, 0),
            round(decline_from_baseline, 1),
            round(recovery_from_trough, 1),
            round(recovery_pct_of_baseline, 1))
)
write.csv(rq1_findings_df, "RQ1_findings_summary.csv", row.names = FALSE)
cat("Saved: RQ1_findings_summary.csv\n\n")

# Breakpoint dates, ranges and evidence (thesis Table 5.6)
breakpoint_row <- function(bp, label) {
  data.frame(
    Breakpoint = label,
    Point_Estimate = as.character(bp$date),
    Range_Start = as.character(bp$date_min),
    Range_End = as.character(bp$date_max),
    Range_Days = bp$date_range_days,
    N_Methods = bp$methods,
    N_Algorithms = bp$n_algorithms,
    Cross_Validated = bp$cross_validated,
    Strength = bp$strength,
    stringsAsFactors = FALSE
  )
}

breakpoint_details <- data.frame(
  Breakpoint = character(), Point_Estimate = character(),
  Range_Start = character(), Range_End = character(),
  Range_Days = numeric(), N_Methods = numeric(), N_Algorithms = numeric(),
  Cross_Validated = logical(), Strength = character(),
  stringsAsFactors = FALSE
)
if (!is.null(FINAL_BREAKPOINTS$T1)) {
  breakpoint_details <- rbind(breakpoint_details, breakpoint_row(FINAL_BREAKPOINTS$T1, "T1 (Implementation)"))
}
if (!is.null(FINAL_BREAKPOINTS$T2)) {
  breakpoint_details <- rbind(breakpoint_details, breakpoint_row(FINAL_BREAKPOINTS$T2, "T2 (Adjustment)"))
}
if (!is.null(FINAL_BREAKPOINTS$T3)) {
  breakpoint_details <- rbind(breakpoint_details, breakpoint_row(FINAL_BREAKPOINTS$T3, "T3 (Reversal)"))
}

write.csv(breakpoint_details, "RQ1_breakpoint_details.csv", row.names = FALSE)
cat("Saved: RQ1_breakpoint_details.csv\n\n")

# =============================================================================
# STEP 8: Election windows (descriptive, discovery group)
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 8: SUPPLEMENTARY ELECTION ANALYSIS\n")
cat("2022 Brazilian General Election & 2024 Brazilian Municipal Elections\n")
cat(rule(), "\n\n", sep = "")

# --- 8.1 Election vs non-election weeks, whole period ----------------------
cat("8.1 OVERALL ELECTION VS NON-ELECTION COMPARISON\n")
cat(rule(60, "-"), "\n\n", sep = "")

election_overall_stats <- discovery_data_phased %>%
  group_by(election_period) %>%
  summarise(
    n_weeks = n(),
    mean_views = mean(avg_views, na.rm = TRUE),
    median_views = median(avg_views, na.rm = TRUE),
    sd_views = sd(avg_views, na.rm = TRUE),
    mean_reactions = mean(avg_reactions, na.rm = TRUE),
    mean_shares = mean(avg_shares, na.rm = TRUE),
    mean_comments = mean(avg_comments, na.rm = TRUE),
    .groups = "drop"
  )

cat("Overall Election Period Statistics (", discovery_group, "):\n\n", sep = "")
print(as.data.frame(election_overall_stats %>% mutate(across(where(is.numeric), ~round(., 0)))))
cat("\n")

non_election_mean <- election_overall_stats$mean_views[election_overall_stats$election_period == "Non-Election"]

cat("Election Bounce Effects (relative to non-election periods):\n\n")
for (i in 1:nrow(election_overall_stats)) {
  if (election_overall_stats$election_period[i] != "Non-Election") {
    bounce <- 100 * (election_overall_stats$mean_views[i] - non_election_mean) / non_election_mean
    cat(" ", election_overall_stats$election_period[i], ": ",
        sprintf("%+.1f%%", bounce), "\n", sep = "")
  }
}
cat("\n")

# --- 8.2 2022 election window vs surrounding weeks (+/- 90 days) -----------
cat("8.2 BRAZILIAN GENERAL ELECTION 2022 ANALYSIS\n")
cat(rule(60, "-"), "\n\n", sep = "")

brazilian_election_phase_data <- discovery_data_phased %>%
  filter(phase %in% c("1_Policy-Active", "2_Adjusted-Policy")) %>%
  filter(week >= (brazilian_election_window_start - 90) &
           week <= (brazilian_election_window_end + 90))

if (nrow(brazilian_election_phase_data) > 0) {

  brazilian_comparison <- brazilian_election_phase_data %>%
    group_by(in_brazilian_election_window) %>%
    summarise(
      n_weeks = n(),
      mean_views = mean(avg_views, na.rm = TRUE),
      median_views = median(avg_views, na.rm = TRUE),
      sd_views = sd(avg_views, na.rm = TRUE),
      .groups = "drop"
    )

  cat("Brazilian Election Window Comparison (within nearby policy period):\n")
  cat("Analysis window:", as.character(brazilian_election_window_start - 90), "to",
      as.character(brazilian_election_window_end + 90), "\n\n")
  print(as.data.frame(brazilian_comparison %>% mutate(across(where(is.numeric), ~round(., 0)))))
  cat("\n")

  non_election_brazilian <- brazilian_comparison$mean_views[brazilian_comparison$in_brazilian_election_window == FALSE]
  election_brazilian <- brazilian_comparison$mean_views[brazilian_comparison$in_brazilian_election_window == TRUE]

  if (length(non_election_brazilian) > 0 && length(election_brazilian) > 0) {
    brazilian_bounce <- 100 * (election_brazilian - non_election_brazilian) / non_election_brazilian
    cat("Brazilian Election 2022 Bounce Effect:", sprintf("%+.1f%%", brazilian_bounce), "\n\n")

    # Wilcoxon rank-sum test, election vs surrounding weeks
    election_data <- brazilian_election_phase_data$avg_views[brazilian_election_phase_data$in_brazilian_election_window == TRUE]
    non_election_data <- brazilian_election_phase_data$avg_views[brazilian_election_phase_data$in_brazilian_election_window == FALSE]

    if (length(election_data) >= 3 && length(non_election_data) >= 3) {
      wilcox_result_brazilian <- wilcox.test(election_data, non_election_data)
      cat("Wilcoxon test p-value:", format.pval(wilcox_result_brazilian$p.value, digits = 3), "\n")
      cat("Significant:", ifelse(wilcox_result_brazilian$p.value < 0.05, "YES", "NO"), "\n\n")
    }
  }
}

# --- 8.3 2024 municipal election window vs surrounding weeks (+/- 90 days) -
cat("8.3 BRAZILIAN MUNICIPAL ELECTIONS 2024 ANALYSIS\n")
cat(rule(60, "-"), "\n\n", sep = "")

municipal_election_phase_data <- discovery_data_phased %>%
  filter(week >= (municipal_election_window_start - 90) &
           week <= (municipal_election_window_end + 90))

if (nrow(municipal_election_phase_data) > 0) {

  municipal_comparison <- municipal_election_phase_data %>%
    group_by(in_municipal_election_window) %>%
    summarise(
      n_weeks = n(),
      mean_views = mean(avg_views, na.rm = TRUE),
      median_views = median(avg_views, na.rm = TRUE),
      sd_views = sd(avg_views, na.rm = TRUE),
      .groups = "drop"
    )

  cat("Municipal Election Window Comparison:\n")
  cat("Analysis window:", as.character(municipal_election_window_start - 90), "to",
      as.character(municipal_election_window_end + 90), "\n\n")
  print(as.data.frame(municipal_comparison %>% mutate(across(where(is.numeric), ~round(., 0)))))
  cat("\n")

  non_election_municipal <- municipal_comparison$mean_views[municipal_comparison$in_municipal_election_window == FALSE]
  election_municipal <- municipal_comparison$mean_views[municipal_comparison$in_municipal_election_window == TRUE]

  if (length(non_election_municipal) > 0 && length(election_municipal) > 0) {
    municipal_bounce <- 100 * (election_municipal - non_election_municipal) / non_election_municipal
    cat("Municipal Elections 2024 Bounce Effect:", sprintf("%+.1f%%", municipal_bounce), "\n\n")

    election_data_municipal <- municipal_election_phase_data$avg_views[municipal_election_phase_data$in_municipal_election_window == TRUE]
    non_election_data_municipal <- municipal_election_phase_data$avg_views[municipal_election_phase_data$in_municipal_election_window == FALSE]

    if (length(election_data_municipal) >= 3 && length(non_election_data_municipal) >= 3) {
      wilcox_result_municipal <- wilcox.test(election_data_municipal, non_election_data_municipal)
      cat("Wilcoxon test p-value:", format.pval(wilcox_result_municipal$p.value, digits = 3), "\n")
      cat("Significant:", ifelse(wilcox_result_municipal$p.value < 0.05, "YES", "NO"), "\n\n")
    }
  }
} else {
  cat("Note: Municipal Elections 2024 period has insufficient data for comparison.\n\n")
}

# --- 8.4 Summary table ----------------------------------------------------
cat("8.4 ELECTION COMPARISON SUMMARY\n")
cat(rule(60, "-"), "\n\n", sep = "")

election_summary <- data.frame(
  Election = c("Brazilian General 2022", "Municipal Elections"),
  Date = c(paste(brazilian_election_rounds, collapse = " / "),
           paste(municipal_election_rounds, collapse = " / ")),
  Window = c(paste(brazilian_election_window_start, "to", brazilian_election_window_end),
             paste(municipal_election_window_start, "to", municipal_election_window_end)),
  Policy_Phase = c(
    if (exists("brazilian_election_phase_data") && nrow(brazilian_election_phase_data) > 0) {
      paste(unique(brazilian_election_phase_data$phase[brazilian_election_phase_data$in_brazilian_election_window]), collapse = ", ")
    } else { "N/A" },
    if (exists("municipal_election_phase_data") && nrow(municipal_election_phase_data) > 0) {
      paste(unique(municipal_election_phase_data$phase[municipal_election_phase_data$in_municipal_election_window]), collapse = ", ")
    } else { "N/A" }
  ),
  Bounce_Effect = c(
    if (exists("brazilian_bounce")) sprintf("%+.1f%%", brazilian_bounce) else "N/A",
    if (exists("municipal_bounce")) sprintf("%+.1f%%", municipal_bounce) else "N/A"
  ),
  Significant = c(
    if (exists("wilcox_result_brazilian")) ifelse(wilcox_result_brazilian$p.value < 0.05, "Yes", "No") else "N/A",
    if (exists("wilcox_result_municipal")) ifelse(wilcox_result_municipal$p.value < 0.05, "Yes", "No") else "N/A"
  ),
  stringsAsFactors = FALSE
)

cat("Summary of Election Effects:\n\n")
print(election_summary, row.names = FALSE)
cat("\n")

write.csv(election_summary, "RQ1_election_analysis_summary.csv", row.names = FALSE)
cat("Saved: RQ1_election_analysis_summary.csv\n\n")

# =============================================================================
# STEP 9: Validation across groups (same phases for every group)
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 9: VALIDATION ACROSS GROUPS\n")
cat(rule(), "\n\n", sep = "")
cat("Using the SAME breakpoints (T1, T2, T3) discovered from", discovery_group, "\n")
cat("across all validation groups.\n\n")

validation_results <- data.frame()

for (group in c(discovery_group, validation_groups)) {

  cat(rule(50, "-"), "\nGroup:", group, "\n", rule(50, "-"), "\n\n", sep = "")

  group_data <- weekly_data_phased %>% filter(sub_list == group, !is.na(phase))
  if (nrow(group_data) == 0) {
    cat(" No data available.\n\n")
    next
  }

  # Mean weekly views per phase
  group_phase_stats <- group_data %>%
    group_by(phase) %>%
    summarise(n_weeks = n(), mean_views = mean(avg_views, na.rm = TRUE), .groups = "drop") %>%
    arrange(phase)

  cat("Phase means:\n")
  print(as.data.frame(group_phase_stats %>% mutate(mean_views = round(mean_views, 0))))
  cat("\n")

  # Direction of each phase-to-phase transition
  group_means <- group_phase_stats$mean_views
  group_directions <- c()
  group_pcts <- c()
  for (i in 2:length(group_means)) {
    delta_pct <- 100 * (group_means[i] - group_means[i - 1]) / group_means[i - 1]
    direction <- ifelse(delta_pct > 0, "UP", "DOWN")
    group_directions <- c(group_directions, direction)
    group_pcts <- c(group_pcts, delta_pct)
  }
  pattern <- paste(group_directions, collapse = " ")

  # Pattern check: first transition down, last transition up
  T1_correct <- length(group_directions) >= 1 && group_directions[1] == "DOWN"
  T3_correct <- length(group_directions) >= 1 && group_directions[length(group_directions)] == "UP"

  cat("Pattern:", pattern, "\n")
  cat("T1 decrease:", ifelse(T1_correct, "yes", "no"), "\n")
  cat("T3 increase:", ifelse(T3_correct, "yes", "no"), "\n\n")

  # Kruskal-Wallis test of weekly mean views across phases
  kw_test <- kruskal.test(avg_views ~ phase, data = group_data)
  cat("Kruskal-Wallis test: chi2 =", round(kw_test$statistic, 2),
      ", p =", format.pval(kw_test$p.value, digits = 3), "\n\n")

  validation_results <- rbind(validation_results, data.frame(
    group = group,
    pattern = pattern,
    T1_correct = T1_correct,
    T3_correct = T3_correct,
    pattern_valid = T1_correct && T3_correct,
    kw_chi2 = kw_test$statistic,
    kw_p = kw_test$p.value,
    stringsAsFactors = FALSE
  ))
}

cat("\nVALIDATION SUMMARY:\n")
cat(rule(60), "\n\n", sep = "")
print(validation_results[, c("group", "pattern", "T1_correct", "T3_correct", "pattern_valid")])
cat("\n")

n_valid <- sum(validation_results$pattern_valid, na.rm = TRUE)
n_total <- nrow(validation_results)
cat("Groups with valid pattern:", n_valid, "/", n_total, "\n")
cat("Groups with significant Kruskal-Wallis (p < 0.05):",
    sum(validation_results$kw_p < 0.05, na.rm = TRUE), "/", n_total, "\n\n")

# --- Table 7: Dunn post-hoc tests, Bonferroni (thesis Table 5.9b) ----------
cat("TABLE 7: Pairwise Phase Comparisons (Dunn's Test)\n")
cat(rule(60, "-"), "\n\n", sep = "")

# Installed from CRAN directly if missing (inside the SRE, use fbrir instead)
if (!require(dunn.test, quietly = TRUE)) {
  install.packages("dunn.test", repos = "https://cloud.r-project.org")
  library(dunn.test)
}

pairwise_results <- list()

for (group in unique(weekly_data_phased$sub_list)) {
  group_data <- weekly_data_phased %>% filter(sub_list == group, !is.na(phase))

  cat("===", group, "===\n")

  dunn_result <- dunn.test(group_data$avg_views, group_data$phase,
                           method = "bonferroni", kw = FALSE,
                           table = FALSE, list = TRUE)

  comparisons <- data.frame(
    comparison = dunn_result$comparisons,
    Z = round(dunn_result$Z, 3),
    p_adj = dunn_result$P.adjusted,
    sig = ifelse(dunn_result$P.adjusted < 0.001, "***",
                 ifelse(dunn_result$P.adjusted < 0.01, "**",
                        ifelse(dunn_result$P.adjusted < 0.05, "*", "n.s.")))
  )
  print(comparisons)
  cat("\n")

  pairwise_results[[group]] <- comparisons
}

# Significance stars for five key comparisons, one column per group
cat("\nSUMMARY TABLE:\n")
cat(rule(60, "-"), "\n\n", sep = "")

key_comparisons <- c("0_Pre-Policy - 1_Policy-Active",
                     "0_Pre-Policy - 2_Adjusted-Policy",
                     "1_Policy-Active - 2_Adjusted-Policy",
                     "2_Adjusted-Policy - 3_Post-Reversal",
                     "0_Pre-Policy - 3_Post-Reversal")

summary_matrix <- matrix(NA, nrow = length(key_comparisons),
                         ncol = length(unique(weekly_data_phased$sub_list)))
colnames(summary_matrix) <- unique(weekly_data_phased$sub_list)
rownames(summary_matrix) <- c("Phase 0 vs Phase 1", "Phase 0 vs Phase 2",
                              "Phase 1 vs Phase 2", "Phase 2 vs Phase 3",
                              "Phase 0 vs Phase 3")

for (i in seq_along(unique(weekly_data_phased$sub_list))) {
  group <- unique(weekly_data_phased$sub_list)[i]
  if (!is.null(pairwise_results[[group]])) {
    for (j in seq_along(key_comparisons)) {
      idx <- which(pairwise_results[[group]]$comparison == key_comparisons[j])
      if (length(idx) > 0) {
        summary_matrix[j, i] <- pairwise_results[[group]]$sig[idx]
      }
    }
  }
}

print(summary_matrix)
cat("\n")

table7_df <- as.data.frame(summary_matrix)
table7_df$Comparison <- rownames(summary_matrix)
table7_df <- table7_df[, c("Comparison", colnames(summary_matrix))]
write.csv(table7_df, "RQ1_Table7_pairwise_comparisons.csv", row.names = FALSE)
cat("Saved: RQ1_Table7_pairwise_comparisons.csv\n\n")

# --- Table 6: phase means and % change by group (thesis Tables 5.8, 5.9a) --
cat("\nTABLE 6: Cross-Group Magnitude Comparison (Views)\n")
cat(rule(60, "-"), "\n\n", sep = "")

table6_data <- data.frame()

for (group in unique(weekly_data_phased$sub_list)) {

  group_stats <- weekly_data_phased %>%
    filter(sub_list == group, !is.na(phase)) %>%
    group_by(phase) %>%
    summarise(mean_views = mean(avg_views, na.rm = TRUE), .groups = "drop") %>%
    arrange(phase)

  phase0 <- group_stats$mean_views[group_stats$phase == "0_Pre-Policy"]
  phase1 <- group_stats$mean_views[group_stats$phase == "1_Policy-Active"]
  phase2 <- group_stats$mean_views[group_stats$phase == "2_Adjusted-Policy"]
  phase3 <- group_stats$mean_views[group_stats$phase == "3_Post-Reversal"]

  if (length(phase0) == 0) phase0 <- NA
  if (length(phase1) == 0) phase1 <- NA
  if (length(phase2) == 0) phase2 <- NA
  if (length(phase3) == 0) phase3 <- NA

  # Trough = lower of T1 and T2 means
  trough <- min(c(phase1, phase2), na.rm = TRUE)
  if (is.infinite(trough)) trough <- phase1

  # Delta_1: baseline -> trough; Delta_2: trough -> post-reversal
  delta1 <- if (!is.na(phase0) && !is.na(trough)) {
    100 * (trough - phase0) / phase0
  } else NA

  delta2 <- if (!is.na(trough) && !is.na(phase3) && !is.infinite(trough)) {
    100 * (phase3 - trough) / trough
  } else NA

  table6_data <- rbind(table6_data, data.frame(
    Group = group,
    Phase_0 = round(phase0, 0),
    Phase_1 = round(phase1, 0),
    Phase_2 = round(phase2, 0),
    Phase_3 = round(phase3, 0),
    Delta_1 = delta1,
    Delta_2 = delta2,
    stringsAsFactors = FALSE
  ))
}

table6_display <- table6_data %>%
  mutate(
    Phase_0 = format(Phase_0, big.mark = ","),
    Phase_1 = format(Phase_1, big.mark = ","),
    Phase_2 = format(Phase_2, big.mark = ","),
    Phase_3 = format(Phase_3, big.mark = ","),
    Delta_1 = sprintf("%.1f%%", Delta_1),
    Delta_2 = sprintf("%+.1f%%", Delta_2)
  )

cat("Views by Phase:\n\n")
print(as.data.frame(table6_display), row.names = FALSE)
cat("\n")
cat("Legend: Phase_0 Pre-Policy | Phase_1 Policy-Active | Phase_2 Adjusted-Policy |",
    "Phase_3 Post-Reversal\n")
cat(" Delta_1: % change from baseline to trough\n")
cat(" Delta_2: % change from trough to post-reversal\n\n")

write.csv(table6_data, "RQ1_Table6_cross_group_comparison.csv", row.names = FALSE)
cat("Saved: RQ1_Table6_cross_group_comparison.csv\n\n")

# =============================================================================
# Robustness check: total weekly views instead of views per post
# =============================================================================
# Per-post means can understate the total visibility of groups that post more
# often; this repeats the phase comparison on total weekly views.

cat("\n", rule(), "\n", sep = "")
cat("ROBUSTNESS CHECK: TOTAL WEEKLY REACH ANALYSIS\n")
cat(rule(), "\n\n", sep = "")

total_reach_weekly <- weekly_data_phased %>%
  filter(!is.na(phase)) %>%
  mutate(
    total_weekly_views = avg_views * n_posts,
    total_weekly_reactions = avg_reactions * n_posts,
    total_weekly_shares = avg_shares * n_posts,
    total_weekly_comments = avg_comments * n_posts
  )

total_reach_by_phase <- total_reach_weekly %>%
  group_by(sub_list, phase) %>%
  summarise(
    n_weeks = n(),
    mean_total_views = mean(total_weekly_views, na.rm = TRUE),
    median_total_views = median(total_weekly_views, na.rm = TRUE),
    sd_total_views = sd(total_weekly_views, na.rm = TRUE),
    mean_n_posts = mean(n_posts, na.rm = TRUE),
    .groups = "drop"
  )

cat("TOTAL WEEKLY REACH BY GROUP AND PHASE:\n")
cat(rule(70, "-"), "\n\n", sep = "")

total_reach_display <- total_reach_by_phase %>%
  mutate(
    mean_total_views = format(round(mean_total_views, 0), big.mark = ","),
    median_total_views = format(round(median_total_views, 0), big.mark = ","),
    sd_total_views = format(round(sd_total_views, 0), big.mark = ","),
    mean_n_posts = round(mean_n_posts, 0)
  ) %>%
  dplyr::select(sub_list, phase, n_weeks, mean_total_views, median_total_views, mean_n_posts)

print(as.data.frame(total_reach_display), row.names = FALSE)
cat("\n")

cat("\nCOMPARISON: PER-POST vs TOTAL REACH PATTERNS\n")
cat(rule(70, "-"), "\n\n", sep = "")

# Baseline (T0) -> T2 and T2 -> T3 changes in total weekly views
total_reach_comparison <- data.frame()

for (group in unique(total_reach_by_phase$sub_list)) {
  group_data <- total_reach_by_phase %>% filter(sub_list == group)

  phase0 <- group_data$mean_total_views[group_data$phase == "0_Pre-Policy"]
  phase1 <- group_data$mean_total_views[group_data$phase == "1_Policy-Active"]
  phase2 <- group_data$mean_total_views[group_data$phase == "2_Adjusted-Policy"]
  phase3 <- group_data$mean_total_views[group_data$phase == "3_Post-Reversal"]

  if (length(phase0) == 0) phase0 <- NA
  if (length(phase1) == 0) phase1 <- NA
  if (length(phase2) == 0) phase2 <- NA
  if (length(phase3) == 0) phase3 <- NA

  delta_baseline_trough <- ifelse(!is.na(phase0) & !is.na(phase2) & phase0 > 0,
                                  (phase2 - phase0) / phase0 * 100, NA)
  delta_trough_reversal <- ifelse(!is.na(phase2) & !is.na(phase3) & phase2 > 0,
                                  (phase3 - phase2) / phase2 * 100, NA)

  total_reach_comparison <- rbind(total_reach_comparison, data.frame(
    Group = group,
    Total_Phase0 = round(phase0, 0),
    Total_Phase2 = round(phase2, 0),
    Total_Phase3 = round(phase3, 0),
    Total_Delta_Baseline_Trough = delta_baseline_trough,
    Total_Delta_Trough_Reversal = delta_trough_reversal,
    stringsAsFactors = FALSE
  ))
}

comparison_table <- merge(
  table6_data %>%
    dplyr::select(Group, Phase_0, Phase_2, Phase_3, Delta_1) %>%
    rename(PerPost_Phase0 = Phase_0,
           PerPost_Phase2 = Phase_2,
           PerPost_Phase3 = Phase_3,
           PerPost_Delta = Delta_1),
  total_reach_comparison,
  by = "Group"
)

cat("Per-Post Reach (avg views/post):\n")
perpost_display <- comparison_table %>%
  dplyr::select(Group, PerPost_Phase0, PerPost_Phase2, PerPost_Delta) %>%
  mutate(
    PerPost_Phase0 = format(PerPost_Phase0, big.mark = ","),
    PerPost_Phase2 = format(PerPost_Phase2, big.mark = ","),
    PerPost_Delta = sprintf("%.1f%%", PerPost_Delta)
  )
print(as.data.frame(perpost_display), row.names = FALSE)

cat("\nTotal Weekly Reach (sum of all post views):\n")
total_display <- comparison_table %>%
  dplyr::select(Group, Total_Phase0, Total_Phase2, Total_Delta_Baseline_Trough) %>%
  mutate(
    Total_Phase0 = format(Total_Phase0, big.mark = ","),
    Total_Phase2 = format(Total_Phase2, big.mark = ","),
    Total_Delta_Baseline_Trough = sprintf("%.1f%%", Total_Delta_Baseline_Trough)
  )
print(as.data.frame(total_display), row.names = FALSE)
cat("\n")

cat("GROUP RANKINGS BY TOTAL WEEKLY REACH:\n")
cat(rule(70, "-"), "\n\n", sep = "")

for (phase_name in c("0_Pre-Policy", "2_Adjusted-Policy", "3_Post-Reversal")) {
  phase_data <- total_reach_by_phase %>%
    filter(phase == phase_name) %>%
    arrange(desc(mean_total_views))

  cat(phase_name, ":\n", sep = "")
  for (i in 1:nrow(phase_data)) {
    cat(" ", i, ". ", phase_data$sub_list[i], ": ",
        format(round(phase_data$mean_total_views[i], 0), big.mark = ","),
        " total weekly views\n", sep = "")
  }
  cat("\n")
}

# Kruskal-Wallis on total weekly views, and direction pattern per group
cat("STATISTICAL VALIDATION:\n")
cat(rule(70, "-"), "\n\n", sep = "")
cat("Kruskal-Wallis tests on TOTAL weekly views by phase:\n\n")

total_reach_validation <- data.frame()

for (group in unique(total_reach_weekly$sub_list)) {
  group_data <- total_reach_weekly %>% filter(sub_list == group, !is.na(phase))

  kw_test <- kruskal.test(total_weekly_views ~ phase, data = group_data)

  phase_means <- group_data %>%
    group_by(phase) %>%
    summarise(mean_views = mean(total_weekly_views, na.rm = TRUE), .groups = "drop") %>%
    arrange(phase)

  m0 <- phase_means$mean_views[phase_means$phase == "0_Pre-Policy"]
  m1 <- phase_means$mean_views[phase_means$phase == "1_Policy-Active"]
  m2 <- phase_means$mean_views[phase_means$phase == "2_Adjusted-Policy"]
  m3 <- phase_means$mean_views[phase_means$phase == "3_Post-Reversal"]

  dir1 <- ifelse(m1 < m0, "DOWN", "UP")
  dir2 <- ifelse(m2 < m1, "DOWN", "UP")
  dir3 <- ifelse(m3 > m2, "UP", "DOWN")
  pattern <- paste(dir1, dir2, dir3, sep = " ")

  cat(" ", group, ": chi2 = ", round(kw_test$statistic, 2),
      ", p = ", format.pval(kw_test$p.value, digits = 3),
      ", Pattern: ", pattern, "\n", sep = "")

  total_reach_validation <- rbind(total_reach_validation, data.frame(
    Group = group,
    chi_squared = kw_test$statistic,
    p_value = kw_test$p.value,
    pattern = pattern,
    stringsAsFactors = FALSE
  ))
}
cat("\n")

# Do per-post and total-views patterns agree?
cat("PATTERN COMPARISON (Per-Post vs Total Reach):\n\n")

pattern_comparison <- merge(
  validation_results %>% dplyr::select(group, pattern) %>% rename(Group = group, PerPost_Pattern = pattern),
  total_reach_validation %>% dplyr::select(Group, pattern) %>% rename(Total_Pattern = pattern),
  by = "Group"
)
pattern_comparison$Pattern_Match <- pattern_comparison$PerPost_Pattern == pattern_comparison$Total_Pattern

print(as.data.frame(pattern_comparison), row.names = FALSE)
cat("\n")

if (all(pattern_comparison$Pattern_Match)) {
  cat("Per-post and total reach patterns are CONSISTENT across all groups.\n\n")
} else {
  cat("NOTE: Some groups show DIFFERENT patterns for per-post vs total reach.\n")
  divergent <- pattern_comparison$Group[!pattern_comparison$Pattern_Match]
  cat("Divergent groups: ", paste(divergent, collapse = ", "), "\n\n")
}

write.csv(total_reach_by_phase, "RQ1_total_reach_by_phase.csv", row.names = FALSE)
write.csv(comparison_table, "RQ1_perpost_vs_total_comparison.csv", row.names = FALSE)
cat("Saved: RQ1_total_reach_by_phase.csv\n")
cat("Saved: RQ1_perpost_vs_total_comparison.csv\n\n")

# =============================================================================
# STEP 10: Figures
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("STEP 10: VISUALIZATIONS\n")
cat(rule(), "\n\n", sep = "")

# --- Figure 1: weekly mean views per post, all groups (thesis Figure 5.3) --
create_main_visualization <- function(data, breakpoints, model_type) {

  T1 <- breakpoints$T1$date
  T2 <- if (!is.null(breakpoints$T2)) breakpoints$T2$date else NULL
  T3 <- if (!is.null(breakpoints$T3)) breakpoints$T3$date else NULL

  # Phase shading: grey (pre), salmon (policy), green (post-reversal)
  p <- ggplot(data, aes(x = week, y = avg_views, color = sub_list)) +
    annotate("rect", xmin = min(data$week), xmax = T1,
             ymin = -Inf, ymax = Inf, fill = "#E8E8E8", alpha = 0.3) +
    annotate("rect", xmin = T1, xmax = if (!is.null(T3)) T3 else max(data$week),
             ymin = -Inf, ymax = Inf, fill = "#FFCCCC", alpha = 0.3)

  if (!is.null(T3)) {
    p <- p + annotate("rect", xmin = T3, xmax = max(data$week),
                      ymin = -Inf, ymax = Inf, fill = "#CCFFCC", alpha = 0.3)
  }

  # Election windows: purple (2022 general), blue (2024 municipal)
  p <- p +
    annotate("rect", xmin = brazilian_election_window_start,
             xmax = brazilian_election_window_end,
             ymin = -Inf, ymax = Inf, fill = "purple", alpha = 0.1) +
    annotate("rect", xmin = municipal_election_window_start,
             xmax = municipal_election_window_end,
             ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1)

  # Weekly series, LOESS trend, breakpoint lines
  p <- p +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    geom_smooth(method = "loess", span = 0.2, se = FALSE,
                linewidth = 1.5, linetype = "solid") +
    geom_vline(xintercept = as.numeric(T1), linetype = "dashed",
               color = "red", linewidth = 1) +
    annotate("text", x = T1, y = Inf, label = "T₁: Policy\nImplementation",
             vjust = 1.5, hjust = -0.1, color = "red", fontface = "bold", size = 3)

  if (!is.null(T2)) {
    p <- p +
      geom_vline(xintercept = as.numeric(T2), linetype = "dashed",
                 color = "orange", linewidth = 1) +
      annotate("text", x = T2, y = Inf, label = "T₂: Adjustment",
               vjust = 1.5, hjust = -0.1, color = "orange", fontface = "bold", size = 3)
  }

  if (!is.null(T3)) {
    p <- p +
      geom_vline(xintercept = as.numeric(T3), linetype = "dashed",
                 color = "darkgreen", linewidth = 1) +
      annotate("text", x = T3, y = Inf, label = "T₃: Policy\nReversal",
               vjust = 1.5, hjust = -0.1, color = "darkgreen", fontface = "bold", size = 3)
  }

  # Election-day markers: one dotted line per round, label at the first round
  p <- p +
    geom_vline(xintercept = as.numeric(brazilian_election_rounds),
               linetype = "dotted", color = "purple", linewidth = 0.8) +
    annotate("text", x = brazilian_election_rounds[1], y = -Inf,
             label = "General\nElection 2022\n(1st/2nd round)", vjust = -0.3, hjust = 1.05,
             color = "purple", size = 2.5) +
    geom_vline(xintercept = as.numeric(municipal_election_rounds),
               linetype = "dotted", color = "blue", linewidth = 0.8) +
    annotate("text", x = municipal_election_rounds[1], y = -Inf,
             label = "Municipal\nElections 2024\n(1st/2nd round)", vjust = -0.3, hjust = 1.05,
             color = "blue", size = 2.5)

  p <- p +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "RQ1: Meta's Political Content Policy Effects on Brazilian Political Actors",
      subtitle = paste0("Breakpoints: T₁ = ", as.character(T1),
                        if (!is.null(T2)) paste0(", T₂ = ", as.character(T2)) else "",
                        if (!is.null(T3)) paste0(", T₃ = ", as.character(T3)) else "",
                        " | Elections: Purple = General 2022, Blue = Municipal 2024"),
      x = "Date",
      y = "Average Views per Post",
      color = "Group"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  return(p)
}

main_plot <- create_main_visualization(weekly_data, FINAL_BREAKPOINTS, MODEL_TYPE)
ggsave("RQ1_Figure1_main_timeseries.png", main_plot, width = 14, height = 8, dpi = 300)
cat("Saved: RQ1_Figure1_main_timeseries.png\n")

# --- Figure 2: discovery group with phase means (thesis Figure 5.1) --------
create_discovery_plot <- function(data, breakpoints, group_name) {

  T1 <- breakpoints$T1$date
  T2 <- if (!is.null(breakpoints$T2)) breakpoints$T2$date else NULL
  T3 <- if (!is.null(breakpoints$T3)) breakpoints$T3$date else NULL

  group_data <- data %>% filter(sub_list == group_name)

  # Phase means, labelled at the median week of each phase
  phase_means <- group_data %>%
    group_by(phase) %>%
    summarise(mean_views = mean(avg_views, na.rm = TRUE), mid_date = median(week), .groups = "drop")

  p <- ggplot(group_data, aes(x = week, y = avg_views)) +
    annotate("rect", xmin = min(group_data$week), xmax = T1,
             ymin = -Inf, ymax = Inf, fill = "#E8E8E8", alpha = 0.4) +
    annotate("rect", xmin = T1, xmax = if (!is.null(T3)) T3 else max(group_data$week),
             ymin = -Inf, ymax = Inf, fill = "#FFCCCC", alpha = 0.4)

  if (!is.null(T3)) {
    p <- p + annotate("rect", xmin = T3, xmax = max(group_data$week),
                      ymin = -Inf, ymax = Inf, fill = "#CCFFCC", alpha = 0.4)
  }

  p <- p +
    annotate("rect", xmin = brazilian_election_window_start,
             xmax = brazilian_election_window_end,
             ymin = -Inf, ymax = Inf, fill = "purple", alpha = 0.15) +
    annotate("rect", xmin = municipal_election_window_start,
             xmax = municipal_election_window_end,
             ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.15)

  p <- p +
    geom_line(color = "steelblue", linewidth = 0.6, alpha = 0.7) +
    geom_smooth(method = "loess", span = 0.15, se = TRUE,
                color = "navy", fill = "lightblue", linewidth = 1.2) +
    geom_vline(xintercept = as.numeric(T1), linetype = "dashed",
               color = "red", linewidth = 1)

  if (!is.null(T2)) {
    p <- p + geom_vline(xintercept = as.numeric(T2), linetype = "dashed",
                        color = "orange", linewidth = 1)
  }
  if (!is.null(T3)) {
    p <- p + geom_vline(xintercept = as.numeric(T3), linetype = "dashed",
                        color = "darkgreen", linewidth = 1)
  }

  p <- p +
    geom_vline(xintercept = as.numeric(brazilian_election_rounds),
               linetype = "dotted", color = "purple", linewidth = 0.8) +
    geom_vline(xintercept = as.numeric(municipal_election_rounds),
               linetype = "dotted", color = "blue", linewidth = 0.8)

  for (i in 1:nrow(phase_means)) {
    p <- p + annotate("text",
                      x = phase_means$mid_date[i],
                      y = max(group_data$avg_views, na.rm = TRUE) * 0.95,
                      label = paste0("Mean: ", format(round(phase_means$mean_views[i], 0), big.mark = ",")),
                      color = "gray30", size = 3, fontface = "italic")
  }

  p <- p +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste0("RQ1: Reach Trends for ", group_name),
      subtitle = paste0("T₁ = ", as.character(T1),
                        if (!is.null(T2)) paste0(" | T₂ = ", as.character(T2)) else "",
                        if (!is.null(T3)) paste0(" | T₃ = ", as.character(T3)) else "",
                        "\nElections: Purple shading = General 2022, Blue shading = Municipal 2024"),
      x = "Date",
      y = "Average Views per Post"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )

  return(p)
}

discovery_plot <- create_discovery_plot(weekly_data_phased, FINAL_BREAKPOINTS, discovery_group)
ggsave("RQ1_Figure2_discovery_sample.png", discovery_plot, width = 12, height = 6, dpi = 300)
cat("Saved: RQ1_Figure2_discovery_sample.png\n")

# --- Figure 3: box plots of weekly mean views by phase and group ----------
boxplot_by_phase <- ggplot(weekly_data_phased %>% filter(!is.na(phase)),
                           aes(x = phase, y = avg_views, fill = phase)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~sub_list, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c(
    "0_Pre-Policy" = "#CCCCCC",
    "1_Policy-Active" = "#FF9999",
    "2_Adjusted-Policy" = "#FFCC99",
    "3_Post-Reversal" = "#99FF99"
  )) +
  labs(
    title = "Distribution of Weekly Average Views by Phase",
    subtitle = paste0("Breakpoints: T₁ = ", as.character(FINAL_BREAKPOINTS$T1$date),
                      if (!is.null(FINAL_BREAKPOINTS$T2)) paste0(" | T₂ = ", as.character(FINAL_BREAKPOINTS$T2$date)) else "",
                      if (!is.null(FINAL_BREAKPOINTS$T3)) paste0(" | T₃ = ", as.character(FINAL_BREAKPOINTS$T3$date)) else ""),
    x = "Phase",
    y = "Average Views per Post"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("RQ1_Figure3_boxplot_by_phase.png", boxplot_by_phase, width = 12, height = 10, dpi = 300)
cat("Saved: RQ1_Figure3_boxplot_by_phase.png\n")

# --- Figure 4: one panel per group with breakpoints and election days -----
faceted_trends <- ggplot(weekly_data_phased, aes(x = week, y = avg_views)) +
  geom_line(color = "steelblue", linewidth = 0.5, alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "navy", linewidth = 1) +
  geom_vline(xintercept = as.numeric(FINAL_BREAKPOINTS$T1$date),
             linetype = "dashed", color = "red", linewidth = 0.8)

if (!is.null(FINAL_BREAKPOINTS$T2)) {
  faceted_trends <- faceted_trends +
    geom_vline(xintercept = as.numeric(FINAL_BREAKPOINTS$T2$date),
               linetype = "dashed", color = "orange", linewidth = 0.8)
}
if (!is.null(FINAL_BREAKPOINTS$T3)) {
  faceted_trends <- faceted_trends +
    geom_vline(xintercept = as.numeric(FINAL_BREAKPOINTS$T3$date),
               linetype = "dashed", color = "darkgreen", linewidth = 0.8)
}

faceted_trends <- faceted_trends +
  geom_vline(xintercept = as.numeric(brazilian_election_rounds),
             linetype = "dotted", color = "purple", linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(municipal_election_rounds),
             linetype = "dotted", color = "blue", linewidth = 0.6) +
  facet_wrap(~sub_list, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "RQ1: Reach Trends by Group (Breakpoint Validation)",
    subtitle = paste0("T₁ = ", as.character(FINAL_BREAKPOINTS$T1$date), " (red)",
                      if (!is.null(FINAL_BREAKPOINTS$T2)) paste0(" | T₂ = ", as.character(FINAL_BREAKPOINTS$T2$date), " (orange)") else "",
                      if (!is.null(FINAL_BREAKPOINTS$T3)) paste0(" | T₃ = ", as.character(FINAL_BREAKPOINTS$T3$date), " (green)") else "",
                      "\nElections: Purple = General 2022, Blue = Municipal 2024"),
    x = "Date",
    y = "Average Views per Post"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave("RQ1_Figure4_faceted_trends.png", faceted_trends, width = 12, height = 10, dpi = 300)
cat("Saved: RQ1_Figure4_faceted_trends.png\n")

# --- Figure 5: discovery group around the two elections (thesis Fig. 5.2) --
cat("Creating Figure 5: Election Period Focus...\n")

# Election window +/- 60 days
election_focus_data <- weekly_data_phased %>%
  filter(sub_list == discovery_group) %>%
  filter(
    (week >= (brazilian_election_window_start - 60) &
       week <= (brazilian_election_window_end + 60)) |
      (week >= (municipal_election_window_start - 60) &
         week <= (municipal_election_window_end + 60))
  ) %>%
  mutate(
    election_context = case_when(
      week >= (brazilian_election_window_start - 60) &
        week <= (brazilian_election_window_end + 60) ~ "General Election 2022 Period",
      week >= (municipal_election_window_start - 60) &
        week <= (municipal_election_window_end + 60) ~ "Municipal Elections 2024 Period"
    )
  )

if (nrow(election_focus_data) > 0) {

  election_focus_plot <- ggplot(election_focus_data, aes(x = week, y = avg_views)) +
    geom_rect(data = data.frame(
      xmin = c(brazilian_election_window_start, municipal_election_window_start),
      xmax = c(brazilian_election_window_end, municipal_election_window_end),
      election_context = c("General Election 2022 Period", "Municipal Elections 2024 Period")
    ), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = c("purple", "blue"), alpha = 0.2, inherit.aes = FALSE) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(color = in_any_election_window), size = 2) +
    # Dashed lines: 1st and 2nd round of each election
    geom_vline(data = data.frame(
      x = c(brazilian_election_rounds, municipal_election_rounds),
      election_context = rep(c("General Election 2022 Period", "Municipal Elections 2024 Period"), each = 2)
    ), aes(xintercept = as.numeric(x)), linetype = "dashed", color = "black", linewidth = 1) +
    facet_wrap(~election_context, scales = "free_x", ncol = 2) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"),
                       labels = c("TRUE" = "Election Window", "FALSE" = "Non-Election"),
                       name = "Period") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste0("Election Period Focus: ", discovery_group),
      subtitle = "Reach around the 2022 general and 2024 municipal elections\nShaded regions = Election windows, Dashed lines = 1st and 2nd round",
      x = "Date",
      y = "Average Views per Post"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )

  ggsave("RQ1_Figure5_election_focus.png", election_focus_plot, width = 14, height = 6, dpi = 300)
  cat("Saved: RQ1_Figure5_election_focus.png\n")
}

cat("\nAll visualizations saved.\n\n")

# =============================================================================
# STEP 11: Final summary and saved results
# =============================================================================

cat("\n", rule(), "\n", sep = "")
cat("FINAL SUMMARY\n")
cat(rule(), "\n\n", sep = "")

cat("METHODOLOGY:\n")
cat(" - Detection: Bai-Perron (4 metrics) + PELT (4 metrics)\n")
cat(" - Validation: Cross-algorithm agreement required\n")
cat(" - Selection: T1=first, T3=post-Sept-2024, T2=strongest intermediate\n")
cat(" - Discovery sample:", discovery_group, "\n")
cat(" - Validation across:", paste(validation_groups, collapse = ", "), "\n")
cat(" - Model type:", MODEL_TYPE, "\n\n")

cat("BREAKPOINTS (medians of clustered detections; ranges show spread):\n")
cat(" T1 (Implementation):", as.character(FINAL_BREAKPOINTS$T1$date),
    "| Range:", FINAL_BREAKPOINTS$T1$date_range,
    "|", FINAL_BREAKPOINTS$T1$methods, "methods,", FINAL_BREAKPOINTS$T1$strength, "\n")
if (!is.null(FINAL_BREAKPOINTS$T2)) {
  cat(" T2 (Adjustment):", as.character(FINAL_BREAKPOINTS$T2$date),
      "| Range:", FINAL_BREAKPOINTS$T2$date_range,
      "|", FINAL_BREAKPOINTS$T2$methods, "methods,", FINAL_BREAKPOINTS$T2$strength, "\n")
}
if (!is.null(FINAL_BREAKPOINTS$T3)) {
  cat(" T3 (Reversal):", as.character(FINAL_BREAKPOINTS$T3$date),
      "| Range:", FINAL_BREAKPOINTS$T3$date_range,
      "|", FINAL_BREAKPOINTS$T3$methods, "methods,", FINAL_BREAKPOINTS$T3$strength, "\n")
}
cat("\n")

cat("ELECTORAL EVENTS:\n")
cat(" 1. General Election 2022: window", as.character(brazilian_election_window_start), "to",
    as.character(brazilian_election_window_end), "\n")
if (exists("brazilian_bounce")) {
  cat("    Bounce effect:", sprintf("%+.1f%%", brazilian_bounce), "\n")
}
cat(" 2. Municipal Elections 2024: window", as.character(municipal_election_window_start), "to",
    as.character(municipal_election_window_end), "\n")
if (exists("municipal_bounce")) {
  cat("    Bounce effect:", sprintf("%+.1f%%", municipal_bounce), "\n")
}
cat("\n")

cat("MAGNITUDE (", discovery_group, "):\n", sep = "")
cat(" - Decline from baseline to trough:", sprintf("%.1f%%", rq1_findings$decline_pct), "\n")
cat(" - Post-reversal reach as % of baseline:",
    sprintf("%.1f%%", rq1_findings$recovery_pct_of_baseline), "\n\n")

cat("VALIDATION:\n")
cat(" - Expected pattern in ", n_valid, "/", n_total, " groups\n", sep = "")
cat(" - Significant Kruskal-Wallis in ", sum(validation_results$kw_p < 0.05, na.rm = TRUE),
    "/", n_total, " groups\n\n", sep = "")

# Everything needed to reproduce the tables, in one object
results_summary <- list(
  analysis_date = Sys.time(),
  data_file = weekly_file,
  model_type = MODEL_TYPE,
  discovery_group = discovery_group,
  validation_groups = validation_groups,

  breakpoints = FINAL_BREAKPOINTS,
  all_consensus = consensus_breakpoints,
  cross_validated_only = cross_validated,

  rq1_findings = rq1_findings,
  phase_stats_discovery = discovery_phase_stats,
  validation_results = validation_results,

  table1 = table1,
  tableC = tableC_stats,
  table2b = table2b,
  table6 = table6_data,
  table7 = if (exists("table7_df")) table7_df else NULL,

  total_reach_by_phase = if (exists("total_reach_by_phase")) total_reach_by_phase else NULL,
  total_reach_comparison = if (exists("comparison_table")) comparison_table else NULL,
  total_reach_validation = if (exists("total_reach_validation")) total_reach_validation else NULL,

  election_events = election_events,
  brazilian_election = list(
    rounds = brazilian_election_rounds,
    window_start = brazilian_election_window_start,
    window_end = brazilian_election_window_end,
    bounce = if (exists("brazilian_bounce")) brazilian_bounce else NA
  ),
  municipal_election = list(
    rounds = municipal_election_rounds,
    window_start = municipal_election_window_start,
    window_end = municipal_election_window_end,
    bounce = if (exists("municipal_bounce")) municipal_bounce else NA
  ),
  election_summary = if (exists("election_summary")) election_summary else NULL,

  meta_policy_dates = meta_policy_dates
)

saveRDS(results_summary, "RQ1_results_summary.rds")
cat("Results saved to: RQ1_results_summary.rds\n")

# Phase statistics for every group (thesis Table 5.8)
all_phase_stats <- weekly_data_phased %>%
  group_by(sub_list, phase) %>%
  summarise(
    n_weeks = n(),
    mean_views = mean(avg_views, na.rm = TRUE),
    median_views = median(avg_views, na.rm = TRUE),
    sd_views = sd(avg_views, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(all_phase_stats, "RQ1_all_phase_stats.csv", row.names = FALSE)
cat("Saved: RQ1_all_phase_stats.csv\n")

cat("\n=== RQ1 Analysis Complete ===\n")
