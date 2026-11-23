# Reach of Brazilian political actors on Facebook under Meta's political content reduction (2021–2025)

R code for **Case Study 1** of the PhD thesis *The Algorithmic Reconfiguration of Visibility in Brazilian Political Communication within Meta's Political Content Reduction Policy* (Bruna Almeida Paroni, Università degli Studi di Urbino Carlo Bo, PhD Programme in Studi Umanistici, cycle 39, academic year 2025/2026), Chapter 5, section 5.1: *Measuring the Reduction of Brazilian Politicians' Reach on Facebook*.

The pipeline does three things:

1. It collects the posts published between 1 January 2021 and 31 December 2025 by three groups of Brazilian political accounts: Re-elected Members of Congress (the discovery group), Newly Elected Members of Congress and Prominent Politicians. The posts come from the Meta Content Library (MCL) API.
2. It infers the timing of Meta's political content reduction from the data. Bai–Perron and PELT breakpoint detection run on weekly views and engagement, and only breaks found by both methods are kept.
3. It estimates how the relationship between views and engagement changed across policy phases, in both directions, with fixed-effects log–log panel regressions.

## Data availability

**No data are included.** MCL data can only be accessed and analysed inside Meta's Secure Research Environment (SRE), and raw records cannot be exported. The account lists were built as producer lists in the MCL interface, which offers no way to export them. Thesis section 5.1.2.2 documents the criteria used to compile them. The MCL list IDs are available from the author on request. To reproduce the analysis you need your own credentialed MCL/SRE access and lists rebuilt with the same criteria.

The scripts that call the API (00, 03, 04) authenticate through the SRE session. No API keys or passwords are used or stored.

## Analysed sample

| Group | Accounts | Posts |
|---|---|---|
| Re-elected MCs (`MPs_Active2021_Reelected2022`) | 357 | 1,099,091 |
| Newly Elected MCs (`MPs_Elected_2022`) | 239 | 504,481 |
| Prominent Politicians (`Prominent_Politicians`) | 180 | 442,233 |
| **Total** | **776** | **2,045,805** |

- **Censored views:** 144,484 posts have censored view counts (100 or fewer) and are imputed (script 02).
- **Weekly series:** 260 complete Sunday-to-Saturday weeks, 3 January 2021 – 27 December 2025.
- **Regression panel:** 155,330 account-weeks from 776 accounts. The fitted models use 155,328 account-weeks from 774 accounts.

## Pipeline

Run the scripts in order from a single working directory inside the SRE. Each script reads the most recent output of the previous step from `combined_datasets/` or `cleaned_data/`.

| Script | What it does | Thesis |
|---|---|---|
| `R/00_download_producer_list.R` | Queries an MCL producer list with batching below the 100K-results limit and records list membership (provenance). | 5.1.2.2 |
| `R/01_build_dataset.R` | Merges the separate downloads, de-duplicates posts and maps producer lists to the three groups. | 5.1.2.2 |
| `R/02_clean_and_impute.R` | Runs integrity checks and resolves overlapping list membership. Applies the study window, imputes censored views group by group and builds the weekly series. | 5.1.2.2–5.1.2.3, Tables 5.3–5.4 |
| `R/03_enrich_account_info.R` | Adds account metadata from the MCL preview endpoints. | — |
| `R/04_producer_list_mapping.R` | Checks every account against the producer lists. | — |
| `R/05_breakpoint_analysis.R` | Detects and maps breakpoints (T1–T3), compares phases (Kruskal–Wallis, Dunn with Bonferroni correction), looks at election windows and runs a total-reach robustness check. | 5.1.2.4, 5.1.3.1, Tables 5.5–5.9b, Figures 5.1–5.3, Appendix B |
| `R/08_amplification_engagement_to_views.R` | Fixed-effects OLS of log(views+1) on log(engagement+1) × phase × group. | 5.1.3.2, Table 5.10, Appendix C (C1, C3-A) |
| `R/09_activation_views_to_engagement.R` | The mirror models: log(engagement+1) on log(views+1) × phase × group. | 5.1.3.3, Table 5.11, Appendix C (C2, C3-B) |
| `R/10_plot_feedback_loop.R` | Plots the elasticities of 08 and 09 by phase and group, with 95% confidence bands. | Figures 5.4–5.5 |

The numbering follows the original analysis notebooks. Numbers 06 and 07 belonged to earlier versions of the regressions and are not part of this release.

**Downloads.** Script 00 is shown configured for the additional Prominent Politicians list. The two MP lists were downloaded in four date slices and the main Prominent Politicians list separately. Script 01 expects the six raw files under the neutral names listed in `INPUT_FILES`; rename your local files to match, or edit the names in the script.

## Key analytic choices

- **Groups and reference category:** Re-elected MCs form the discovery group and the reference category; Newly Elected MCs and Prominent Politicians are the validation groups.
- **Accounts in more than one list** are assigned by the priority Re-elected MCs > Prominent Politicians > Newly Elected MCs (script 02). In the data, 21 accounts were in both the Re-elected and Prominent lists, and 4 in both the Newly Elected and Prominent lists. Three accounts returned by the list queries (1,883 posts) are no longer in the current version of the lists; they are kept in their original group.
- **Censored views:** MCL returns NA for posts with 100 views or fewer. A power law fitted to each group's observed views in (100, 500] gives the expected share of censored posts in 1–50 versus 51–100. Values are then drawn uniformly within the chosen half; if the fit is unreliable, a uniform ratio of 1.0 is used instead. Random seeds are fixed in the code (42, and 12345 inside the routine that simulates the expected statistics, which also sets the generator state for the draws), so the imputation is fully deterministic.
- **Breakpoints used downstream:** T1 = 26 September 2021, T2 = 30 April 2023, T3 = 9 March 2025. These are hard-coded in scripts 08 and 09 from the output of script 05.
- **Weeks:** the breakpoint analysis uses complete Sunday-to-Saturday weeks (script 02). The regression panels in 08 and 09 use Monday-start weeks, including the partial weeks at either end of the window, and assign each week to the phase that contains its midpoint.
- **Regressions:** `fixest::feols`, with account and year-month fixed effects, a log(n_posts + 1) control and standard errors clustered by account. The elasticity for each group × phase is the sum of the relevant coefficients; its standard error comes from the full variance–covariance matrix, and p-values use the normal approximation.
- **Elections:** figures mark the first and second rounds of the 2022 general election (2 and 30 October 2022) and of the 2024 municipal elections (6 and 27 October 2024). The election analysis compares weekly reach inside each election window (1 August – 30 November 2022; 20 September – 8 December 2024) with the surrounding weeks.
- The regressions describe within-account associations conditional on the fixed effects. They do not identify causal effects.

## Software

The analysis ran on R 4.3.3 inside the Meta SRE. Main packages: `tidyverse`, `lubridate`, `reticulate` (MCL Python client), `jsonlite`, `strucchange`, `changepoint`, `dunn.test`, `fixest`, `broom` and `scales`. Inside the SRE, missing packages are installed with `fbrir::CRAN$new()$InstallPackages()`.

## Configuration

- `MCL_OUTPUT_DIR` (optional, script 00): where raw downloads are written. The default is `raw_downloads/`, relative to the working directory.
- The producer list IDs in scripts 00 and 04 are placeholders (`<PRODUCER_LIST_ID_...>`). Replace them with the IDs of your own MCL lists.

## License

The code is released under the MIT License (see `LICENSE`).

## Citation

If you use this code, please cite the thesis: Almeida Paroni, B. *The Algorithmic Reconfiguration of Visibility in Brazilian Political Communication within Meta's Political Content Reduction Policy*. PhD thesis, Università degli Studi di Urbino Carlo Bo. The breakpoint design follows Giglietto (2026a).
