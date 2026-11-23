# =============================================================================
# 09 - Activation: did the policy change the views -> engagement relation?
# Fixed-effects panel regressions with engagement as the dependent variable
# Case Study 1 (thesis sections 5.1.2.5 and 5.1.3.3; Table 5.11; Appendix C)
# =============================================================================
#
# Mirror of script 08 with the roles of the two variables reversed:
#   - script 08: views ~ engagement * phase * sub_list   (amplification)
#   - this one : engagement ~ views * phase * sub_list   (activation)
#
# The panel is built exactly as in script 08 (account x Monday-start week,
# phase assigned by the week midpoint, weeks with >= 1 post and views > 0).
#
# Models (one per engagement type: reactions, shares, comments):
#   log(engagement + 1) ~ log(views + 1) * phase * sub_list + log(n_posts + 1)
#                        | surface.id + year_month,  SE clustered by account
#
# Reference categories: T0 (pre-policy) and Re-elected MCs.
#
# Input : cleaned_data/cleaned_posts_*.rds (most recent, from script 02)
# Output (cleaned_data/, suffixed with a timestamp):
#   rq2b_panel_data_*.rds, rq2b_models_*.rds, rq2b_slopes_*.csv
# =============================================================================

# Packages (installed through fbrir inside the Meta SRE if missing)
required_packages <- c("tidyverse", "lubridate", "fixest", "broom")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    library(fbrir); CRAN$new()$InstallPackages(pkg)
  }
  library(pkg, character.only = TRUE)
}

rule <- function(n = 80, ch = "=") strrep(ch, n)

cat("\n", rule(), "\n", sep = "")
cat("RQ2b-EXTENDED: AUDIENCE RESPONSIVENESS (views -> engagement, cross-group)\n")
cat("Symmetric counterpart of script 08\n")
cat(rule(), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# STEP 1: Load the cleaned posts
# -----------------------------------------------------------------------------
posts_files <- list.files("cleaned_data",
                          pattern = "cleaned_posts_.*\\.rds$",
                          full.names = TRUE)
if (length(posts_files) == 0) stop("No cleaned_posts in cleaned_data/")

posts_file <- sort(posts_files, decreasing = TRUE)[1]
cat("Loading:", posts_file, "\n")
posts <- readRDS(posts_file)
cat("Posts:", format(nrow(posts), big.mark = ","), "| Accounts:", n_distinct(posts$surface.id), "\n\n")

# -----------------------------------------------------------------------------
# STEP 2: Breakpoints from script 05 and group factor (reference first)
# -----------------------------------------------------------------------------
T1 <- as.Date("2021-09-26")
T2 <- as.Date("2023-04-30")
T3 <- as.Date("2025-03-09")

posts <- posts %>%
  mutate(
    sub_list = factor(sub_list, levels = c("MPs_Active2021_Reelected2022",
                                           "MPs_Elected_2022",
                                           "Prominent_Politicians"))
  )

# -----------------------------------------------------------------------------
# STEP 3: Weekly account-level panel (same construction as script 08)
# -----------------------------------------------------------------------------
cat("STEP 3: BUILDING WEEKLY PANEL\n")
panel <- posts %>%
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(surface.id, sub_list, week) %>%
  summarise(
    n_posts = n(),
    total_views = sum(statistics.views, na.rm = TRUE),
    total_reactions = sum(statistics.reaction_count, na.rm = TRUE),
    total_shares = sum(statistics.share_count, na.rm = TRUE),
    total_comments = sum(statistics.comment_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    week_mid = week + days(3),
    phase = case_when(
      week_mid < T1 ~ "T0_Pre-Policy",
      week_mid >= T1 & week_mid < T2 ~ "T1_Implementation",
      week_mid >= T2 & week_mid < T3 ~ "T2_Adjustment",
      week_mid >= T3 ~ "T3_Reversal"
    ),
    phase = factor(phase, levels = c("T0_Pre-Policy", "T1_Implementation",
                                     "T2_Adjustment", "T3_Reversal")),
    year_month = floor_date(week, "month")
  ) %>%
  filter(n_posts >= 1, total_views > 0)

cat("Panel:", format(nrow(panel), big.mark = ","), "account-weeks\n")
cat("Accounts per group:\n")
print(panel %>% distinct(surface.id, sub_list) %>% count(sub_list))
cat("\n")

# -----------------------------------------------------------------------------
# STEP 4: Fit one model per engagement type
# -----------------------------------------------------------------------------
cat("STEP 4: FITTING REGRESSIONS\n")
cat("Specification: log(engagement+1) ~ log(views+1) * phase * sub_list\n")
cat("                                  + log(n_posts+1)\n")
cat("                                  | surface.id + year_month\n\n")

m_reactions <- feols(
  log(total_reactions + 1) ~ log(total_views + 1) * phase * sub_list
  + log(n_posts + 1)
  | surface.id + year_month,
  data = panel, cluster = ~surface.id
)

m_shares <- feols(
  log(total_shares + 1) ~ log(total_views + 1) * phase * sub_list
  + log(n_posts + 1)
  | surface.id + year_month,
  data = panel, cluster = ~surface.id
)

m_comments <- feols(
  log(total_comments + 1) ~ log(total_views + 1) * phase * sub_list
  + log(n_posts + 1)
  | surface.id + year_month,
  data = panel, cluster = ~surface.id
)

cat("All 3 models fitted (thesis Appendix C, Table C2).\n")
etable(m_reactions, m_shares, m_comments,
       headers = c("Reactions", "Shares", "Comments"),
       digits = 4, fitstat = c("n", "r2", "war2"))
cat("\n")

# -----------------------------------------------------------------------------
# STEP 5: Slope for each group x phase (thesis Table 5.11, Table C3 panel B)
# -----------------------------------------------------------------------------
cat("STEP 5: COMPUTING SLOPES (views -> engagement) BY GROUP x PHASE\n\n")

# Same logic as compute_group_phase_slopes() in script 08: sum the relevant
# coefficients, take the SE from the matching vcov block, normal-approximation
# p-values.
compute_slopes <- function(model, model_name) {
  coefs <- coef(model)
  vc <- vcov(model)
  base <- "log(total_views + 1)"
  phases <- c("T1_Implementation", "T2_Adjustment", "T3_Reversal")
  groups <- c("MPs_Elected_2022", "Prominent_Politicians")
  ref_g <- "MPs_Active2021_Reelected2022"

  results <- list()

  # T0, reference group
  est <- coefs[base]
  se  <- sqrt(vc[base, base])
  results[[length(results) + 1]] <- data.frame(
    model = model_name, phase = "T0_Pre-Policy", group = ref_g,
    slope = unname(est), std_error = unname(se),
    p_value = 2 * (1 - pnorm(abs(est / se)))
  )

  # T0, other groups: base + base:group
  for (g in groups) {
    g_t <- paste0(base, ":sub_list", g)
    if (g_t %in% names(coefs)) {
      est_g <- coefs[base] + coefs[g_t]
      var_a <- vc[base, base]; var_b <- vc[g_t, g_t]
      cov_ab <- vc[base, g_t]
      se_g <- sqrt(var_a + var_b + 2 * cov_ab)
      results[[length(results) + 1]] <- data.frame(
        model = model_name, phase = "T0_Pre-Policy", group = g,
        slope = unname(est_g), std_error = unname(se_g),
        p_value = 2 * (1 - pnorm(abs(est_g / se_g)))
      )
    }
  }

  for (ph in phases) {
    inter <- paste0(base, ":phase", ph)
    if (inter %in% names(coefs)) {
      # Reference group: base + base:phase
      est_p <- coefs[base] + coefs[inter]
      var_a <- vc[base, base]; var_b <- vc[inter, inter]
      cov_ab <- vc[base, inter]
      se_p <- sqrt(var_a + var_b + 2 * cov_ab)
      results[[length(results) + 1]] <- data.frame(
        model = model_name, phase = ph, group = ref_g,
        slope = unname(est_p), std_error = unname(se_p),
        p_value = 2 * (1 - pnorm(abs(est_p / se_p)))
      )
      # Other groups: base + base:phase + base:group + base:phase:group
      for (g in groups) {
        g_t <- paste0(base, ":sub_list", g)
        triple <- paste0(base, ":phase", ph, ":sub_list", g)
        all_t <- c(base, inter, g_t, triple)
        present <- all_t[all_t %in% names(coefs)]

        if (length(present) > 0) {
          est_gp <- sum(coefs[present])
          var_sum <- 0
          for (i in seq_along(present))
            for (j in seq_along(present))
              var_sum <- var_sum + vc[present[i], present[j]]
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

# Significance stars and normal-approximation 95% CIs
slopes <- bind_rows(
  compute_slopes(m_reactions, "Reactions"),
  compute_slopes(m_shares, "Shares"),
  compute_slopes(m_comments, "Comments")
) %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE            ~ ""
    ),
    ci_lower = slope - 1.96 * std_error,
    ci_upper = slope + 1.96 * std_error,
    slope_fmt = sprintf("%.3f%s", slope, sig)
  )

# Wide table with thesis labels
cat("Views Elasticity Coefficients Toward Engagement, by Group and Phase\n")
cat(rule(60, "-"), "\n\n", sep = "")

group_labels <- c(
  "MPs_Active2021_Reelected2022" = "Re-elected MCs",
  "MPs_Elected_2022" = "Newly Elected MCs",
  "Prominent_Politicians" = "Prominent Politicians"
)
phase_labels <- c(
  "T0_Pre-Policy" = "T0 (Pre)",
  "T1_Implementation" = "T1 (Impl)",
  "T2_Adjustment" = "T2 (Adj)",
  "T3_Reversal" = "T3 (Rev)"
)

slopes_display <- slopes %>%
  mutate(
    group_label = group_labels[group],
    phase_label = phase_labels[phase]
  )

for (m in c("Reactions", "Shares", "Comments")) {
  cat("--- ", m, " ---\n")
  tbl <- slopes_display %>%
    filter(model == m) %>%
    select(group_label, phase_label, slope_fmt) %>%
    pivot_wider(names_from = phase_label, values_from = slope_fmt)
  print(as.data.frame(tbl))
  cat("\n")
}

# -----------------------------------------------------------------------------
# STEP 6: Save
# -----------------------------------------------------------------------------
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create("cleaned_data", showWarnings = FALSE)

saveRDS(panel, file.path("cleaned_data", paste0("rq2b_panel_data_", timestamp, ".rds")))

saveRDS(list(m_reactions = m_reactions, m_shares = m_shares, m_comments = m_comments),
        file.path("cleaned_data", paste0("rq2b_models_", timestamp, ".rds")))

write.csv(slopes,
          file.path("cleaned_data", paste0("rq2b_slopes_", timestamp, ".csv")),
          row.names = FALSE)

cat(rule(), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat("Files saved in cleaned_data/ with timestamp", timestamp, "\n")
cat(rule(), "\n", sep = "")
