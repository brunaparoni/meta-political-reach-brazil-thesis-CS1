# =============================================================================
# 08 - Amplification: did the policy change the engagement -> views relation?
# Fixed-effects panel regressions with views as the dependent variable
# Case Study 1 (thesis sections 5.1.2.5 and 5.1.3.2; Table 5.10; Appendix C)
# =============================================================================
#
# Question: during the policy, did the same level of engagement still bring
# the same amount of visibility (views)?
#
# Unit of analysis: account x week (Monday-start weeks), only weeks in which
# the account posted. Each week is assigned to the phase in which its
# midpoint (Thursday) falls, using the breakpoints estimated in script 05.
#
# Models (one per engagement type: reactions, shares, comments), estimated
# with fixest::feols, account and year-month fixed effects, standard errors
# clustered by account:
#
#   Step A: log(views + 1) ~ log(engagement + 1) * phase + log(n_posts + 1)
#   Step B: log(views + 1) ~ log(engagement + 1) * phase * sub_list
#                            + log(n_posts + 1)
#
# Reference categories: T0 (pre-policy) and Re-elected MCs
# (MPs_Active2021_Reelected2022). The slope for a group x phase is the sum of
# the relevant base, phase, group and interaction coefficients; its standard
# error uses the full variance-covariance matrix. Slopes are elasticities.
#
# These are within-account associations conditional on the fixed effects,
# not causal estimates: engagement and views are jointly determined.
#
# Input : cleaned_data/cleaned_posts_*.rds (most recent, from script 02)
# Output (cleaned_data/, suffixed with a timestamp):
#   rq_views_panel_data_*.rds, rq_views_models_step_a_*.rds,
#   rq_views_models_step_b_*.rds, rq_views_coefficients_*.csv,
#   rq_views_step_a_slopes_*.csv, rq_views_step_b_slopes_*.csv
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 1: Packages (installed through fbrir inside the Meta SRE if missing)
# -----------------------------------------------------------------------------
required_packages <- c("tidyverse", "lubridate", "fixest", "broom")

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
cat("RQ-VIEWS: ALGORITHMIC FEEDBACK LOOP\n")
cat("Did Meta's policy alter the relationship engagement -> views?\n")
cat(rule(), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# STEP 2: Load the cleaned posts
# -----------------------------------------------------------------------------
cat("STEP 2: LOADING CLEANED POSTS\n")
cat(rule(40, "-"), "\n\n", sep = "")

posts_files <- list.files(
  path = "cleaned_data",
  pattern = "cleaned_posts_.*\\.rds$",
  full.names = TRUE
)
if (length(posts_files) == 0) {
  stop("No cleaned_posts files found in cleaned_data/")
}

posts_file <- sort(posts_files, decreasing = TRUE)[1]
cat("Loading posts data:", posts_file, "\n")
posts <- readRDS(posts_file)

cat("\nData structure:\n")
cat("  Rows (posts):", format(nrow(posts), big.mark = ","), "\n")
cat("  Unique accounts:", n_distinct(posts$surface.id), "\n")
cat("  Date range:", format(min(posts$date)), "to", format(max(posts$date)), "\n\n")

# -----------------------------------------------------------------------------
# STEP 3: Breakpoints from script 05 and group factor
# -----------------------------------------------------------------------------
cat("STEP 3: DATA-DRIVEN BREAKPOINTS\n")
cat(rule(40, "-"), "\n\n", sep = "")

T1 <- as.Date("2021-09-26")
T2 <- as.Date("2023-04-30")
T3 <- as.Date("2025-03-09")

cat("Data-driven breakpoints (from RQ1, script 05):\n")
cat("  T1 (Implementation):", format(T1), "\n")
cat("  T2 (Adjustment):    ", format(T2), "\n")
cat("  T3 (Reversal):      ", format(T3), "\n\n")

# Reference group first
posts <- posts %>%
  mutate(
    sub_list = factor(sub_list, levels = c("MPs_Active2021_Reelected2022",
                                           "MPs_Elected_2022",
                                           "Prominent_Politicians"))
  )

# -----------------------------------------------------------------------------
# STEP 4: Weekly account-level panel
# -----------------------------------------------------------------------------
cat("STEP 4: BUILDING WEEKLY ACCOUNT-LEVEL PANEL\n")
cat(rule(40, "-"), "\n\n", sep = "")

# Sum views and engagement per account and Monday-start week; assign each week
# to the phase containing its midpoint (week start + 3 days)
panel <- posts %>%
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(surface.id, sub_list, week) %>%
  summarise(
    n_posts = n(),
    total_views = sum(statistics.views, na.rm = TRUE),
    total_reactions = sum(statistics.reaction_count, na.rm = TRUE),
    total_shares = sum(statistics.share_count, na.rm = TRUE),
    total_comments = sum(statistics.comment_count, na.rm = TRUE),
    pct_views_imputed = mean(views_imputed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    week_midpoint = week + days(3),
    phase = case_when(
      week_midpoint < T1 ~ "T0_Pre-Policy",
      week_midpoint >= T1 & week_midpoint < T2 ~ "T1_Implementation",
      week_midpoint >= T2 & week_midpoint < T3 ~ "T2_Adjustment",
      week_midpoint >= T3 ~ "T3_Reversal"
    ),
    phase = factor(phase, levels = c("T0_Pre-Policy", "T1_Implementation",
                                     "T2_Adjustment", "T3_Reversal")),
    sub_list = factor(sub_list, levels = c("MPs_Active2021_Reelected2022",
                                           "MPs_Elected_2022",
                                           "Prominent_Politicians")),
    year_month = floor_date(week, "month")
  )

# Keep account-weeks with at least one post and positive views
panel_clean <- panel %>%
  filter(n_posts >= 1, total_views > 0)

cat("Weekly panel structure:\n")
cat("  Account-week observations:", format(nrow(panel_clean), big.mark = ","), "\n")
cat("  Unique accounts:", n_distinct(panel_clean$surface.id), "\n")
cat("  Unique weeks:", n_distinct(panel_clean$week), "\n\n")

cat("Account-weeks per phase x group:\n")
print(table(panel_clean$phase, panel_clean$sub_list))
cat("\n")

# Sanity check: median views and median engagement/views ratio by phase
for (eng in c("total_reactions", "total_comments", "total_shares")) {
  cat("Median views and ", sub("total_", "", eng), " per phase (sanity check):\n", sep = "")
  panel_clean %>%
    group_by(phase) %>%
    summarise(
      median_views = round(median(total_views), 0),
      median_eng_views_ratio = round(median(.data[[eng]] / total_views), 4),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    as.data.frame() %>%
    print()
  cat("\n")
}

# -----------------------------------------------------------------------------
# STEP 5: Step A - engagement x phase (groups pooled)
# -----------------------------------------------------------------------------
cat("STEP 5: STEP A - ALGORITHMIC FEEDBACK ACROSS PHASES\n")
cat(rule(40, "-"), "\n\n", sep = "")
cat("Specification: log(views+1) ~ log(engagement+1) * phase + log(n_posts+1)\n")
cat("                              | surface.id + year_month\n\n")

step_a_reactions <- feols(
  log(total_views + 1) ~ log(total_reactions + 1) * phase + log(n_posts + 1)
  | surface.id + year_month,
  data = panel_clean,
  cluster = ~surface.id
)

step_a_shares <- feols(
  log(total_views + 1) ~ log(total_shares + 1) * phase + log(n_posts + 1)
  | surface.id + year_month,
  data = panel_clean,
  cluster = ~surface.id
)

step_a_comments <- feols(
  log(total_views + 1) ~ log(total_comments + 1) * phase + log(n_posts + 1)
  | surface.id + year_month,
  data = panel_clean,
  cluster = ~surface.id
)

cat("STEP A: models by engagement type\n")
cat(rule(60, "-"), "\n", sep = "")
etable(step_a_reactions, step_a_shares, step_a_comments,
       headers = c("Reactions", "Shares", "Comments"),
       digits = 4,
       fitstat = c("n", "r2", "war2"))
cat("\n")

# -----------------------------------------------------------------------------
# STEP 6: Step A - slope in each phase
# -----------------------------------------------------------------------------
cat("STEP 6: COMPUTING PHASE-SPECIFIC SLOPES (engagement -> views)\n")
cat(rule(40, "-"), "\n\n", sep = "")
cat("Slope in T0 = main effect; slope in phase X = main effect + interaction.\n\n")

# p-values use a normal approximation (2 * (1 - pnorm(|z|)))
compute_phase_slopes <- function(model, eng_var, model_name) {
  coefs <- coef(model)
  vcov_m <- vcov(model)

  baseline_term <- paste0("log(", eng_var, " + 1)")
  phase_levels <- c("T1_Implementation", "T2_Adjustment", "T3_Reversal")

  results <- list()

  # T0
  est <- coefs[baseline_term]
  se  <- sqrt(vcov_m[baseline_term, baseline_term])
  results[[length(results) + 1]] <- data.frame(
    model = model_name,
    phase = "T0_Pre-Policy",
    slope = unname(est),
    std_error = unname(se),
    p_value = 2 * (1 - pnorm(abs(est / se)))
  )

  # T1-T3: base + base:phase, SE = sqrt(Var(a) + Var(b) + 2 Cov(a, b))
  for (ph in phase_levels) {
    inter_term <- paste0(baseline_term, ":phase", ph)
    if (inter_term %in% names(coefs)) {
      est_p <- coefs[baseline_term] + coefs[inter_term]
      var_a <- vcov_m[baseline_term, baseline_term]
      var_b <- vcov_m[inter_term, inter_term]
      cov_ab <- vcov_m[baseline_term, inter_term]
      se_p <- sqrt(var_a + var_b + 2 * cov_ab)
      p_p  <- 2 * (1 - pnorm(abs(est_p / se_p)))

      results[[length(results) + 1]] <- data.frame(
        model = model_name,
        phase = ph,
        slope = unname(est_p),
        std_error = unname(se_p),
        p_value = unname(p_p)
      )
    }
  }

  do.call(rbind, results)
}

phase_slopes <- bind_rows(
  compute_phase_slopes(step_a_reactions, "total_reactions", "Reactions"),
  compute_phase_slopes(step_a_shares, "total_shares", "Shares"),
  compute_phase_slopes(step_a_comments, "total_comments", "Comments")
) %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE            ~ ""
    ),
    slope_label = sprintf("%.3f %s", slope, sig)
  )

cat("ENGAGEMENT -> VIEWS SLOPES BY PHASE (Step A, no group interaction):\n")
cat(rule(60, "-"), "\n", sep = "")

slopes_wide <- phase_slopes %>%
  select(model, phase, slope_label) %>%
  pivot_wider(names_from = phase, values_from = slope_label)
print(as.data.frame(slopes_wide))
cat("\n")
cat("(Slopes are elasticities: % change in views for a 1% change in engagement.)\n\n")

# -----------------------------------------------------------------------------
# STEP 7: Step B - engagement x phase x group
# -----------------------------------------------------------------------------
cat("STEP 7: STEP B - CROSS-GROUP DIFFERENCES (engagement x phase x group)\n")
cat(rule(40, "-"), "\n\n", sep = "")
cat("Specification: log(views+1) ~ log(engagement+1) * phase * sub_list\n")
cat("                              + log(n_posts+1)\n")
cat("                              | surface.id + year_month\n\n")

step_b_reactions <- feols(
  log(total_views + 1) ~ log(total_reactions + 1) * phase * sub_list
  + log(n_posts + 1)
  | surface.id + year_month,
  data = panel_clean,
  cluster = ~surface.id
)

step_b_shares <- feols(
  log(total_views + 1) ~ log(total_shares + 1) * phase * sub_list
  + log(n_posts + 1)
  | surface.id + year_month,
  data = panel_clean,
  cluster = ~surface.id
)

step_b_comments <- feols(
  log(total_views + 1) ~ log(total_comments + 1) * phase * sub_list
  + log(n_posts + 1)
  | surface.id + year_month,
  data = panel_clean,
  cluster = ~surface.id
)

cat("STEP B: models by engagement type (thesis Appendix C, Table C1)\n")
cat(rule(60, "-"), "\n", sep = "")
etable(step_b_reactions, step_b_shares, step_b_comments,
       headers = c("Reactions", "Shares", "Comments"),
       digits = 4,
       fitstat = c("n", "r2", "war2"))
cat("\n")

# -----------------------------------------------------------------------------
# STEP 8: Step B - slope for each group x phase (thesis Table 5.10)
# -----------------------------------------------------------------------------
cat("STEP 8: COMPUTING GROUP-SPECIFIC SLOPES (Step B)\n")
cat(rule(40, "-"), "\n\n", sep = "")

# Slope = sum of base, base:phase, base:group and base:phase:group terms that
# exist in the model; SE from the sum of the corresponding vcov block.
# p-values use a normal approximation.
compute_group_phase_slopes <- function(model, eng_var, model_name) {
  coefs <- coef(model)
  vcov_m <- vcov(model)

  base <- paste0("log(", eng_var, " + 1)")
  phase_levels <- c("T1_Implementation", "T2_Adjustment", "T3_Reversal")
  group_levels <- c("MPs_Elected_2022", "Prominent_Politicians")
  ref_group <- "MPs_Active2021_Reelected2022"

  results <- list()

  # T0, reference group
  est <- coefs[base]
  se  <- sqrt(vcov_m[base, base])
  results[[length(results) + 1]] <- data.frame(
    model = model_name, phase = "T0_Pre-Policy", group = ref_group,
    slope = unname(est), std_error = unname(se),
    p_value = 2 * (1 - pnorm(abs(est / se)))
  )

  # T0, other groups: base + base:group
  for (g in group_levels) {
    g_term <- paste0(base, ":sub_list", g)
    if (g_term %in% names(coefs)) {
      est_g <- coefs[base] + coefs[g_term]
      var_a <- vcov_m[base, base]
      var_b <- vcov_m[g_term, g_term]
      cov_ab <- vcov_m[base, g_term]
      se_g <- sqrt(var_a + var_b + 2 * cov_ab)

      results[[length(results) + 1]] <- data.frame(
        model = model_name, phase = "T0_Pre-Policy", group = g,
        slope = unname(est_g), std_error = unname(se_g),
        p_value = 2 * (1 - pnorm(abs(est_g / se_g)))
      )
    }
  }

  for (ph in phase_levels) {
    inter <- paste0(base, ":phase", ph)
    if (inter %in% names(coefs)) {
      # T1-T3, reference group: base + base:phase
      est_p <- coefs[base] + coefs[inter]
      var_a <- vcov_m[base, base]
      var_b <- vcov_m[inter, inter]
      cov_ab <- vcov_m[base, inter]
      se_p <- sqrt(var_a + var_b + 2 * cov_ab)

      results[[length(results) + 1]] <- data.frame(
        model = model_name, phase = ph, group = ref_group,
        slope = unname(est_p), std_error = unname(se_p),
        p_value = 2 * (1 - pnorm(abs(est_p / se_p)))
      )

      # T1-T3, other groups: base + base:phase + base:group + base:phase:group
      for (g in group_levels) {
        g_term <- paste0(base, ":sub_list", g)
        triple_term <- paste0(base, ":phase", ph, ":sub_list", g)

        all_terms <- c(base, inter, g_term, triple_term)
        all_terms_present <- all_terms[all_terms %in% names(coefs)]

        if (length(all_terms_present) > 0) {
          est_gp <- sum(coefs[all_terms_present])
          var_sum <- 0
          for (i in seq_along(all_terms_present)) {
            for (j in seq_along(all_terms_present)) {
              var_sum <- var_sum + vcov_m[all_terms_present[i], all_terms_present[j]]
            }
          }
          se_gp <- sqrt(var_sum)

          results[[length(results) + 1]] <- data.frame(
            model = model_name, phase = ph, group = g,
            slope = unname(est_gp), std_error = unname(se_gp),
            p_value = 2 * (1 - pnorm(abs(est_gp / se_gp)))
          )
        }
      }
    }
  }

  do.call(rbind, results)
}

group_phase_slopes <- bind_rows(
  compute_group_phase_slopes(step_b_reactions, "total_reactions", "Reactions"),
  compute_group_phase_slopes(step_b_shares, "total_shares", "Shares"),
  compute_group_phase_slopes(step_b_comments, "total_comments", "Comments")
) %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE            ~ ""
    ),
    slope_label = sprintf("%.3f %s", slope, sig)
  )

cat("GROUP-SPECIFIC SLOPES engagement -> views (Step B):\n")
cat(rule(60, "-"), "\n", sep = "")
print(as.data.frame(group_phase_slopes %>% select(model, phase, group, slope, std_error, p_value, sig)))
cat("\n")

cat("WIDE FORMAT (slope by group x phase):\n")
cat(rule(60, "-"), "\n", sep = "")
for (m in unique(group_phase_slopes$model)) {
  cat("\n--- ", m, " ---\n")
  wide_m <- group_phase_slopes %>%
    filter(model == m) %>%
    select(phase, group, slope_label) %>%
    pivot_wider(names_from = group, values_from = slope_label)
  print(as.data.frame(wide_m))
}
cat("\n")

# -----------------------------------------------------------------------------
# STEP 9: Save
# -----------------------------------------------------------------------------
cat("STEP 9: SAVING OUTPUTS\n")
cat(rule(40, "-"), "\n\n", sep = "")

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

panel_outfile <- file.path("cleaned_data", paste0("rq_views_panel_data_", timestamp, ".rds"))
saveRDS(panel_clean, panel_outfile)
cat("Saved panel:", panel_outfile, "\n")

step_a_models <- list(
  step_a_reactions = step_a_reactions,
  step_a_shares = step_a_shares,
  step_a_comments = step_a_comments
)
saveRDS(step_a_models, file.path("cleaned_data", paste0("rq_views_models_step_a_", timestamp, ".rds")))
cat("Saved Step A models\n")

step_b_models <- list(
  step_b_reactions = step_b_reactions,
  step_b_shares = step_b_shares,
  step_b_comments = step_b_comments
)
saveRDS(step_b_models, file.path("cleaned_data", paste0("rq_views_models_step_b_", timestamp, ".rds")))
cat("Saved Step B models\n")

# All coefficients of all six models in one table
get_coefs <- function(model, model_name, step_label) {
  broom::tidy(model) %>%
    mutate(model = model_name, step = step_label) %>%
    select(step, model, term, estimate, std.error, statistic, p.value)
}

coef_summary <- bind_rows(
  get_coefs(step_a_reactions, "Reactions", "A"),
  get_coefs(step_a_shares, "Shares", "A"),
  get_coefs(step_a_comments, "Comments", "A"),
  get_coefs(step_b_reactions, "Reactions", "B"),
  get_coefs(step_b_shares, "Shares", "B"),
  get_coefs(step_b_comments, "Comments", "B")
)
write.csv(coef_summary,
          file.path("cleaned_data", paste0("rq_views_coefficients_", timestamp, ".csv")),
          row.names = FALSE)
cat("Saved coefficient summary CSV\n")

write.csv(phase_slopes,
          file.path("cleaned_data", paste0("rq_views_step_a_slopes_", timestamp, ".csv")),
          row.names = FALSE)
write.csv(group_phase_slopes,
          file.path("cleaned_data", paste0("rq_views_step_b_slopes_", timestamp, ".csv")),
          row.names = FALSE)
cat("Saved marginal slopes CSVs (Step A and Step B)\n\n")

# -----------------------------------------------------------------------------
# STEP 10: Summary
# -----------------------------------------------------------------------------
cat(rule(), "\n", sep = "")
cat("RQ-VIEWS ANALYSIS COMPLETE\n")
cat(rule(), "\n\n", sep = "")

cat("Step A - slopes engagement -> views by phase (groups pooled):\n")
print(as.data.frame(phase_slopes %>% select(model, phase, slope, std_error, p_value, sig)))
cat("\n")

cat("All outputs saved to cleaned_data/ with timestamp", timestamp, "\n")
cat(rule(), "\n", sep = "")
