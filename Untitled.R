#Install packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(countrycode)
#Import Datasets
cp_file    <- "/Users/shanerooney/Downloads/ccpcnc_v5/ccpcnc/ccpcnc_v5_small.csv"
colpus_file <- "/Users/shanerooney/Downloads/Master_Coup List_all regimes_basic (1).dta"
mip_file    <- "/Users/shanerooney/Downloads/participants/mic-part-1.0.rds"
ccp    <- readr::read_csv(ccp_file, show_col_types = FALSE)
colpus <- haven::read_dta(colpus_file)         # .dta
mip    <- readRDS(mip_file)                    # .rds

start_year <- 1946
end_year   <- 2014

# ---- 3A) CCP ----
# CCP commonly has uppercase variable names; adjust if yours differ.
ccp2 <- ccp %>%
  transmute(
    cowcode   = as.integer(cowcode),
    year      = as.integer(year),
    c_inforce = as.integer(c_inforce),
    military  = as.integer(military),
    coding_imputed = if ("coding_imputed" %in% names(ccp)) as.integer(coding_imputed) else 0L
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  filter(c_inforce == 1) %>%
  mutate(
    military_bin = case_when(
      military == 1 ~ 1L,   # Yes
      military == 2 ~ 0L,   # No
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(military_bin))

# Baseline per country = first year constitution in force 
ccp_base <- ccp2 %>%
  arrange(cowcode, year, coding_imputed) %>%
  group_by(cowcode) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(cowcode, baseline_year = year, military_bin)

# ---- 3B) Colpus (.dta) ----e.
# First, standardize names to lower-case for easier reference.
colpus_names <- names(colpus)
names(colpus) <- tolower(colpus_names)

# Check if it already has cowcode/ccode + year and a coup indicator.
# You may need to edit the next transmute() to match the exact column names in your file.
# Print to inspect once:
print(names(colpus))

# Common possibilities:
# - ccode or cowcode for country id
# - year
# - coup_attempt / coup / coup1 / coupattempt / attempt
#
# For now, assume it contains: ccode, year, coup_attempt
# If not, replace with the correct column names after checking print(names(colpus)) output.

colpus2 <- colpus %>%
  transmute(
    cowcode = as.integer(if ("cowcode" %in% names(colpus)) cowcode else ccode),
    year = as.integer(year),
    coup = as.integer(coup)   
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  mutate(
    coup = ifelse(is.na(coup), 0L, ifelse(coup >= 1, 1L, 0L))
  ) %>%
  group_by(cowcode, year) %>%
  summarise(coup = max(coup), .groups = "drop")

# ---- 3C) MIP (.rds) ----
# MIP objects often come in as a tibble/data.frame already.
# Standardize key fields. The participant file should have: ccode, styear, endyear.
mip_names <- names(mip)
names(mip) <- tolower(mip_names)

print(names(mip))

mip2 <- mip %>%
  transmute(
    cowcode = as.integer(ccode),
    styear  = as.integer(styear),
    endyear = as.integer(endyear)
  ) %>%
  filter(!is.na(styear), !is.na(endyear)) %>%
  filter(endyear >= start_year, styear <= end_year) %>%
  mutate(
    styear = pmax(styear, start_year),
    endyear = pmin(endyear, end_year)
  )

# Expand participation episodes into country-years, then de-duplicate
mip_years <- mip2 %>%
  rowwise() %>%
  mutate(year = list(seq(styear, endyear))) %>%
  ungroup() %>%
  unnest(year) %>%
  transmute(cowcode, year = as.integer(year), conflict_year = 1L) %>%
  distinct(cowcode, year, .keep_all = TRUE)

# ---- 4) Post-baseline grid (baseline+1..2014) ----
post_grid <- ccp_base %>%
  mutate(start_post = baseline_year + 1L, end_post = end_year) %>%
  filter(start_post <= end_post) %>%
  rowwise() %>%
  mutate(year = list(seq(start_post, end_post))) %>%
  ungroup() %>%
  unnest(year) %>%
  transmute(cowcode, baseline_year, military_bin, year)

# ---- 5) Construct country-level outcomes ----
# H1: ever coup after baseline
h1_country <- post_grid %>%
  left_join(colpus2, by = c("cowcode", "year")) %>%
  mutate(coup = ifelse(is.na(coup), 0L, coup)) %>%
  group_by(cowcode, baseline_year, military_bin) %>%
  summarise(ever_coup_post = max(coup), .groups = "drop")

# H2: conflict-years after baseline
h2_country <- post_grid %>%
  left_join(mip_years, by = c("cowcode", "year")) %>%
  mutate(conflict_year = ifelse(is.na(conflict_year), 0L, conflict_year)) %>%
  group_by(cowcode, baseline_year, military_bin) %>%
  summarise(
    conflict_years_post = sum(conflict_year),
    years_post = n(),
    conflict_share_post = conflict_years_post / years_post,
    .groups = "drop"
  )

analysis <- ccp_base %>%
  left_join(h1_country, by = c("cowcode", "baseline_year", "military_bin")) %>%
  left_join(h2_country, by = c("cowcode", "baseline_year", "military_bin")) %>%
  filter(!is.na(ever_coup_post), !is.na(conflict_years_post)) %>%
  mutate(
    military_group = ifelse(military_bin == 1, "Military mentioned", "No military mention"),
    country_name = countrycode(cowcode, origin = "cown", destination = "country.name")
  )

# ---- 6) Tests ----
# H1: Chi-square
tab_h1 <- table(analysis$military_group, analysis$ever_coup_post)
print(tab_h1)

chi_h1 <- chisq.test(tab_h1, correct = FALSE)
print(chi_h1)

# Fisher's Exact Test on the contingency table (H1)
fisher_h1 <- fisher.test(tab_h1)
print(fisher_h1)


# H2: Welch t-test (count DV)
tt_h2 <- t.test(conflict_years_post ~ military_bin, data = analysis, var.equal = FALSE)
print(tt_h2)

# Wilcoxon rank-sum test (H2): conflict-years by military group
wilcox_h2 <- wilcox.test(conflict_years_post ~ military_bin,
                         data = analysis,
                         exact = FALSE) 
print(wilcox_h2)




#Save Data: 
dir.create("results", showWarnings = FALSE)
dir.create(file.path("results", "tables"), showWarnings = FALSE)
dir.create(file.path("results", "figures"), showWarnings = FALSE)

# ---- 2) TABLES ----

# (A) Save the main analysis dataset (so results are reproducible)
write.csv(analysis,
          file = file.path("results", "tables", "table_00_analysis_country_level.csv"),
          row.names = FALSE)

# (B) Descriptives by group (helps interpret both H1 and H2)
desc_table <- analysis %>%
  group_by(military_group) %>%
  summarise(
    n = n(),
    coup_rate = mean(ever_coup_post),
    mean_conflict_years = mean(conflict_years_post),
    median_conflict_years = median(conflict_years_post),
    iqr_conflict_years = IQR(conflict_years_post),
    .groups = "drop"
  )

write.csv(desc_table,
          file = file.path("results", "tables", "table_01_descriptives_by_group.csv"),
          row.names = FALSE)

# (C) H1 contingency table: observed counts + row proportions
obs_counts <- as.data.frame.matrix(tab_h1)
row_props  <- as.data.frame.matrix(prop.table(tab_h1, margin = 1))  # within group

write.csv(obs_counts,
          file = file.path("results", "tables", "table_02_h1_contingency_observed.csv"),
          row.names = TRUE)

write.csv(row_props,
          file = file.path("results", "tables", "table_03_h1_row_proportions.csv"),
          row.names = TRUE)

# (D) One clean table containing the results of all 4 tests
#     (same columns for each row; NAs where not applicable)
test_results <- data.frame(
  test = c("H1_ChiSquare", "H1_FisherExact", "H2_WelchTTest", "H2_WilcoxonRankSum"),
  statistic = c(
    unname(chi_h1$statistic),
    NA_real_,                         # Fisher doesn't store a single "statistic" like chi/t
    unname(tt_h2$statistic),
    unname(wilcox_h2$statistic)
  ),
  df = c(
    unname(chi_h1$parameter),
    NA_real_,
    unname(tt_h2$parameter),
    NA_real_
  ),
  p_value = c(
    chi_h1$p.value,
    fisher_h1$p.value,
    tt_h2$p.value,
    wilcox_h2$p.value
  ),
  # Extra columns for interpretation (fill with NA if not relevant)
  estimate = c(
    NA_real_,
    if (!is.null(fisher_h1$estimate)) unname(fisher_h1$estimate) else NA_real_, # odds ratio
    NA_real_,
    NA_real_
  ),
  conf_low = c(
    NA_real_,
    if (!is.null(fisher_h1$conf.int)) fisher_h1$conf.int[1] else NA_real_,
    tt_h2$conf.int[1],
    NA_real_
  ),
  conf_high = c(
    NA_real_,
    if (!is.null(fisher_h1$conf.int)) fisher_h1$conf.int[2] else NA_real_,
    tt_h2$conf.int[2],
    NA_real_
  ),
  mean_group0 = c(
    NA_real_,
    NA_real_,
    unname(tt_h2$estimate[1]),        # mean conflict-years (military_bin==0)
    NA_real_
  ),
  mean_group1 = c(
    NA_real_,
    NA_real_,
    unname(tt_h2$estimate[2]),        # mean conflict-years (military_bin==1)
    NA_real_
  ),
  stringsAsFactors = FALSE
)

write.csv(test_results,
          file = file.path("results", "tables", "table_04_test_results_all_four.csv"),
          row.names = FALSE)

# ---- 3) FIGURES ----

# Keep group order consistent (optional but makes plots cleaner)
analysis$military_group <- factor(analysis$military_group,
                                  levels = c("No military mention", "Military mentioned"))

# (Figure 1) H1: Coup incidence by group (ties to Chi-square + Fisher)
coup_plot_df <- analysis %>%
  group_by(military_group) %>%
  summarise(n = n(), coup_rate = mean(ever_coup_post), .groups = "drop")

fig1 <- ggplot(coup_plot_df, aes(x = military_group, y = coup_rate)) +
  geom_col(width = 0.6, fill = "steelblue") +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "H1: Post-baseline coup incidence by constitutional military mention",
    subtitle = paste0("Chi-square p=", signif(chi_h1$p.value, 3),
                      " | Fisher p=", signif(fisher_h1$p.value, 3)),
    x = NULL,
    y = "Share with ≥1 coup attempt"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("results", "figures", "fig_01_h1_coup_rate.png"),
       fig1, width = 7, height = 4, dpi = 300)

# (Figure 2) H2: Conflict-years by group (ties to t-test + Wilcoxon)
fig2 <- ggplot(analysis, aes(x = military_group, y = conflict_years_post)) +
  geom_boxplot(width = 0.6, fill = "tan", outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.6) +
  labs(
    title = "H2: Post-baseline conflict-years by constitutional military mention",
    subtitle = paste0("Welch t-test p=", signif(tt_h2$p.value, 3),
                      " | Wilcoxon p=", signif(wilcox_h2$p.value, 3)),
    x = NULL,
    y = "Conflict-years (baseline+1 to 2014)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path("results", "figures", "fig_02_h2_conflict_years.png"),
       fig2, width = 7, height = 4, dpi = 300)

# (Figure 3, optional) Same as Figure 2 but with mean markers (helps t-test interpretation)
means_df <- analysis %>%
  group_by(military_group) %>%
  summarise(mean_conflict = mean(conflict_years_post), .groups = "drop")

fig3 <- fig2 +
  geom_point(data = means_df, aes(x = military_group, y = mean_conflict),
             color = "red", size = 3) +
  geom_text(data = means_df,
            aes(x = military_group, y = mean_conflict, label = round(mean_conflict, 1)),
            vjust = -1, color = "red", size = 4) +
  labs(subtitle = paste0("Means in red | Welch p=", signif(tt_h2$p.value, 3),
                         " | Wilcoxon p=", signif(wilcox_h2$p.value, 3)))

ggsave(file.path("results", "figures", "fig_03_h2_conflict_years_with_means.png"),
       fig3, width = 7, height = 4, dpi = 300)

cat("\nSaved results to:\n  results/tables/\n  results/figures/\n")







