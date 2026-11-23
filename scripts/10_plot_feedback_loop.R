# =============================================================================
# 10 - Figures for the two directions of the feedback loop
# Case Study 1 (thesis Figures 5.4 and 5.5)
# =============================================================================
#
# Plots the elasticities estimated in scripts 08 and 09 by policy phase, one
# panel per engagement type, one line per group, with 95% confidence bands
# (slope +/- 1.96 * SE):
#   Figure A (thesis Fig. 5.4) - amplification: engagement -> views (script 08)
#   Figure B (thesis Fig. 5.5) - activation:    views -> engagement (script 09)
# Both figures share the same y-axis range so they can be compared directly.
#
# Input : cleaned_data/rq_views_step_b_slopes_*.csv (most recent, script 08)
#         cleaned_data/rq2b_slopes_*.csv            (most recent, script 09)
# Output: cleaned_data/figure_engagement_to_views.png
#         cleaned_data/figure_views_to_engagement.png
# =============================================================================

# Packages (installed through fbrir inside the Meta SRE if missing)
required_packages <- c("tidyverse", "scales")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    library(fbrir); CRAN$new()$InstallPackages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# -----------------------------------------------------------------------------
# STEP 1: Load the group x phase slopes
# -----------------------------------------------------------------------------
files_08 <- list.files("cleaned_data",
                       pattern = "rq_views_step_b_slopes_.*\\.csv$",
                       full.names = TRUE)
if (length(files_08) == 0) stop("No script 08 slopes found")
file_08 <- sort(files_08, decreasing = TRUE)[1]
cat("Loading script 08 slopes:", file_08, "\n")
slopes_08 <- read.csv(file_08, stringsAsFactors = FALSE)

files_09 <- list.files("cleaned_data",
                       pattern = "rq2b_slopes_.*\\.csv$",
                       full.names = TRUE)
if (length(files_09) == 0) stop("No script 09 slopes found")
file_09 <- sort(files_09, decreasing = TRUE)[1]
cat("Loading script 09 slopes:", file_09, "\n\n")
slopes_09 <- read.csv(file_09, stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# STEP 2: Labels, colours and axis range
# -----------------------------------------------------------------------------
group_labels <- c(
  "MPs_Active2021_Reelected2022" = "Re-elected MCs",
  "MPs_Elected_2022"             = "Newly Elected MCs",
  "Prominent_Politicians"        = "Prominent Politicians"
)

phase_labels <- c(
  "T0_Pre-Policy"     = "T0",
  "T1_Implementation" = "T1",
  "T2_Adjustment"     = "T2",
  "T3_Reversal"       = "T3"
)

group_colors <- c(
  "Re-elected MCs"        = "#1f77b4",
  "Newly Elected MCs"     = "#ff7f0e",
  "Prominent Politicians" = "#d62728"
)

# Common y-axis for both figures
y_range  <- c(0.55, 1.05)
y_breaks <- seq(0.55, 1.05, by = 0.05)

# Add ordered display labels for group, phase and engagement type
prepare_slopes <- function(slopes_df) {
  slopes_df %>%
    mutate(
      group_label = group_labels[group],
      group_label = factor(group_label,
                           levels = c("Re-elected MCs",
                                      "Newly Elected MCs",
                                      "Prominent Politicians")),
      phase_label = phase_labels[phase],
      phase_label = factor(phase_label, levels = c("T0", "T1", "T2", "T3")),
      model_label = factor(model, levels = c("Reactions", "Shares", "Comments"))
    )
}

slopes_08p <- prepare_slopes(slopes_08)
slopes_09p <- prepare_slopes(slopes_09)

# Warn if any confidence band falls outside the fixed y-axis range
out_of_range_08 <- slopes_08p %>%
  filter(slope - 1.96 * std_error < y_range[1] |
           slope + 1.96 * std_error > y_range[2])
out_of_range_09 <- slopes_09p %>%
  filter(slope - 1.96 * std_error < y_range[1] |
           slope + 1.96 * std_error > y_range[2])

if (nrow(out_of_range_08) > 0 || nrow(out_of_range_09) > 0) {
  cat("WARNING: some confidence intervals extend beyond the fixed range:\n")
  if (nrow(out_of_range_08) > 0) {
    cat("  Figure A out-of-range rows:", nrow(out_of_range_08), "\n")
    print(out_of_range_08 %>% select(model, phase, group, slope, std_error))
  }
  if (nrow(out_of_range_09) > 0) {
    cat("  Figure B out-of-range rows:", nrow(out_of_range_09), "\n")
    print(out_of_range_09 %>% select(model, phase, group, slope, std_error))
  }
  cat("These will be visually clipped. If problematic, widen y_range.\n\n")
}

# -----------------------------------------------------------------------------
# STEP 3: Plot function (line + points per group, 95% CI ribbon, one facet
# per engagement type)
# -----------------------------------------------------------------------------
make_plot <- function(df, title, subtitle, y_label) {
  ggplot(df, aes(x = phase_label, y = slope,
                 color = group_label, fill = group_label,
                 group = group_label)) +
    geom_ribbon(aes(ymin = slope - 1.96 * std_error,
                    ymax = slope + 1.96 * std_error),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    facet_wrap(~ model_label, ncol = 3) +
    # oob_squish keeps values slightly outside the range at the axis limit
    scale_y_continuous(limits = y_range, breaks = y_breaks,
                       oob = scales::oob_squish) +
    scale_color_manual(values = group_colors, name = "Political actor group") +
    scale_fill_manual(values = group_colors, name = "Political actor group") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Policy phase",
      y = y_label,
      caption = "Shaded bands: 95% confidence intervals. N = 155,328 account-weeks."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey30", size = 11,
                                   margin = margin(b = 12)),
      plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "grey95", color = NA),
      panel.grid.minor = element_blank(),
      panel.spacing.x = unit(1.3, "lines"),
      panel.spacing.y = unit(1, "lines"),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# -----------------------------------------------------------------------------
# STEP 4: Figure A - amplification (engagement -> views)
# -----------------------------------------------------------------------------
fig_a <- make_plot(
  slopes_08p,
  title = "Figure A — Algorithmic reward to engagement (engagement → views)",
  subtitle = "Elasticity of views to engagement across policy phases, by political-actor group",
  y_label = "Slope: engagement → views (elasticity)"
)

ggsave("cleaned_data/figure_engagement_to_views.png",
       plot = fig_a, width = 20, height = 5.5, dpi = 600, bg = "white")
print(fig_a)
cat("Saved: cleaned_data/figure_engagement_to_views.png\n")

# -----------------------------------------------------------------------------
# STEP 5: Figure B - activation (views -> engagement)
# -----------------------------------------------------------------------------
fig_b <- make_plot(
  slopes_09p,
  title = "Figure B — Audience responsiveness to visibility (views → engagement)",
  subtitle = "Elasticity of engagement to views across policy phases, by political-actor group",
  y_label = "Slope: views → engagement (elasticity)"
)

ggsave("cleaned_data/figure_views_to_engagement.png",
       plot = fig_b, width = 20, height = 5.5, dpi = 600, bg = "white")
print(fig_b)
cat("Saved: cleaned_data/figure_views_to_engagement.png\n\n")

cat("Both figures generated (y-axis 0.55-1.05, ticks every 0.05)\n")
