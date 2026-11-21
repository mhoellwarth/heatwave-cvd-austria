# ==============================================================================
# Heatwaves and cardiovascular in-hospital case fatality in Austria
# Research question 1: analysis script
#
# Master's thesis, Public Health, UMIT TIROL (2026)
# Author: Manuela Höllwarth
# R version 4.5.1
#
# Input:  aggregated district-day dataset of hospital discharges linked with
#         meteorological data (not publicly available)
# Output: tables (Excel) and figures (PNG) in ./output
# ==============================================================================


# 0. Setup ---------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(splines)
library(writexl)

output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)


# 1. Data import and preparation -----------------------------------------------

# Dataset: TSData_v11_BZK_PW_1219_20260902.rds
dat <- readRDS(file.choose())

dat <- dat %>%
  mutate(
    BKZ    = location_county_home,
    Tavg   = tmp_bl_avg,
    cases  = replace_na(cases_main_I, 0),
    deaths = replace_na(cases_intrahospitalmortality_I, 0)
  )

# ICD-10 diagnosis groups (variable suffixes and labels)
icd_cases  <- c("I10.15", "I20.25", "I26.28", "I30.52", "I60.69", "I70.79", "I80.89")
icd_deaths <- c("I10.I15", "I20.I25", "I26.I28", "I30.I52", "I60.I69", "I70.I79", "I80.I89")
icd_labels <- c(
  "I10–I15 Hypertensive diseases",
  "I20–I25 Ischaemic heart diseases",
  "I26–I28 Pulmonary heart diseases",
  "I30–I52 Other heart diseases",
  "I60–I69 Cerebrovascular diseases",
  "I70–I79 Arterial diseases",
  "I80–I89 Venous diseases"
)

# Check: total CVD admissions equal the sum of the seven diagnosis groups
dat %>%
  mutate(sum_groups = rowSums(across(all_of(paste0("cases_main_", icd_cases))), na.rm = TRUE)) %>%
  summarise(
    total_cases        = sum(cases),
    total_cases_groups = sum(sum_groups),
    rows_different     = sum(cases != sum_groups)
  )

dat %>%
  summarise(
    n_districts  = n_distinct(BKZ),
    date_min     = min(date),
    date_max     = max(date),
    missing_Tavg = sum(is.na(Tavg))
  )


# 2. Heatwave and reference week classification --------------------------------

# District-specific thresholds based on year-round daily mean temperature:
# hot day >= T95; reference day between T20 and T80
dat <- dat %>%
  filter(year(date) >= 2002) %>%
  group_by(BKZ) %>%
  mutate(
    T95     = quantile(Tavg, 0.95, na.rm = TRUE),
    T80     = quantile(Tavg, 0.80, na.rm = TRUE),
    T20     = quantile(Tavg, 0.20, na.rm = TRUE),
    hot_day = as.integer(Tavg >= T95),
    ref_day = as.integer(Tavg >= T20 & Tavg <= T80)
  ) %>%
  ungroup()

# Study weeks: consecutive 7-day blocks counted from 1 January (not ISO weeks)
dat <- dat %>%
  mutate(
    date_year       = year(date),
    date_weekofyear = floor(yday(date) / 7) + 1
  )

# Two consecutive hot days; pairs may span week boundaries
dat <- dat %>%
  arrange(BKZ, date) %>%
  group_by(BKZ) %>%
  mutate(
    consecutive_hot = if_else(
      hot_day == 1 & lag(hot_day) == 1 & as.numeric(date - lag(date)) == 1,
      1L, 0L,
      missing = 0L
    )
  ) %>%
  ungroup()

# Heatwave week: at least one pair of consecutive hot days
# Reference week: seven days, all within T20–T80, no heatwave
dat <- dat %>%
  group_by(BKZ, date_year, date_weekofyear) %>%
  mutate(
    heatwave_week  = as.integer(sum(consecutive_hot) >= 1),
    reference_week = as.integer(
      n() == 7 & sum(ref_day, na.rm = TRUE) == 7 & heatwave_week == 0
    )
  ) %>%
  ungroup()

# Check: no duplicate district-days
dat %>% count(BKZ, date) %>% filter(n > 1)


# 3. Analysis dataset ----------------------------------------------------------

# Warm season (May–October), 2002–2021, heatwave and reference weeks
dat_rq1 <- dat %>%
  filter(
    date_year %in% 2002:2021,
    month(date) %in% 5:10,
    heatwave_week == 1 | reference_week == 1
  ) %>%
  mutate(
    week_type = factor(
      if_else(heatwave_week == 1, "Heatwave week", "Reference week"),
      levels = c("Reference week", "Heatwave week")
    ),
    cases_age_00_64 = rowSums(across(c(
      cases_main_I_age0.19, cases_main_I_age20.49, cases_main_I_age50.64
    )), na.rm = TRUE),
    cases_age_65_plus = rowSums(across(c(
      cases_main_I_age65.74, cases_main_I_age75.84, cases_main_I_age85.inf
    )), na.rm = TRUE),
    deaths_age_00_64 = rowSums(across(c(
      intra.hosp.mort_age_00.24, intra.hosp.mort_age_25.64
    )), na.rm = TRUE),
    deaths_acute = rowSums(
      across(all_of(paste0("intra.hosp.mort_a_", icd_deaths))), na.rm = TRUE
    ),
    deaths_planned = rowSums(
      across(all_of(paste0("intra.hosp.mort_p_", icd_deaths))), na.rm = TRUE
    )
  )

# Aggregation to complete district-weeks (7 days)
aggregate_weeks <- function(data, cases_var, deaths_var) {
  data %>%
    group_by(BKZ, date_year, date_weekofyear, week_type) %>%
    summarise(
      cases   = sum(.data[[cases_var]], na.rm = TRUE),
      deaths  = sum(.data[[deaths_var]], na.rm = TRUE),
      n_days  = n(),
      .groups = "drop"
    ) %>%
    filter(n_days == 7)
}

dat_rq1_week_all <- aggregate_weeks(dat_rq1, "cases", "deaths")

# Weeks without CVD admissions are excluded (log offset)
dat_rq1_week <- dat_rq1_week_all %>%
  filter(cases > 0) %>%
  mutate(cf_per_1000 = deaths / cases * 1000)

stopifnot(
  nrow(dat_rq1_week) == 24728,
  sum(dat_rq1_week$cases) == 1541870
)


# 4. Descriptive statistics ----------------------------------------------------

exclusions <- dat_rq1_week_all %>%
  group_by(week_type) %>%
  summarise(
    weeks_total      = n(),
    weeks_zero_cases = sum(cases == 0),
    weeks_included   = sum(cases > 0),
    percent_excluded = round(100 * weeks_zero_cases / weeks_total, 2),
    .groups = "drop"
  )

descriptives <- dat_rq1_week %>%
  group_by(week_type) %>%
  summarise(
    n_district_weeks  = n(),
    total_cases       = sum(cases),
    total_deaths      = sum(deaths),
    crude_cf_per_1000 = total_deaths / total_cases * 1000,
    cases_mean   = mean(cases),
    cases_sd     = sd(cases),
    cases_median = median(cases),
    cases_q1     = quantile(cases, 0.25),
    cases_q3     = quantile(cases, 0.75),
    deaths_mean   = mean(deaths),
    deaths_sd     = sd(deaths),
    deaths_median = median(deaths),
    deaths_q1     = quantile(deaths, 0.25),
    deaths_q3     = quantile(deaths, 0.75),
    cf_mean   = mean(cf_per_1000),
    cf_median = median(cf_per_1000),
    cf_q1     = quantile(cf_per_1000, 0.25),
    cf_q3     = quantile(cf_per_1000, 0.75),
    .groups = "drop"
  )

overall_sample <- dat_rq1_week %>%
  summarise(
    n_district_weeks = n(),
    n_districts      = n_distinct(BKZ),
    total_cases      = sum(cases),
    total_deaths     = sum(deaths),
    crude_cf_percent = total_deaths / total_cases * 100
  )

# Sex and age distribution of included admissions
dat_rq1_included <- dat_rq1 %>%
  semi_join(dat_rq1_week, by = c("BKZ", "date_year", "date_weekofyear"))

demographics <- dat_rq1_included %>%
  summarise(
    Men          = sum(cases_main_I_sexM, na.rm = TRUE),
    Women        = sum(cases_main_I_sexF, na.rm = TRUE),
    `0–64 years` = sum(cases_age_00_64),
    `65+ years`  = sum(cases_age_65_plus)
  ) %>%
  pivot_longer(everything(), names_to = "group", values_to = "n") %>%
  mutate(variable = rep(c("Sex", "Age"), each = 2), .before = 1) %>%
  group_by(variable) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup()

deaths_by_diagnosis <- tibble(
  diagnosis_group = icd_labels,
  deaths = map_dbl(
    paste0("intra.hosp.mort_", icd_deaths),
    ~ sum(dat_rq1[[.x]], na.rm = TRUE)
  )
) %>%
  mutate(
    percent = round(100 * deaths / sum(dat_rq1$intra.hosp.mort_all.I, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(deaths))

# Admission type is recorded from 2007 onwards
admission_coverage <- dat_rq1 %>%
  filter(date_year >= 2007) %>%
  summarise(
    deaths_total     = sum(deaths),
    deaths_acute     = sum(deaths_acute),
    deaths_planned   = sum(deaths_planned),
    coverage_percent = round(100 * (deaths_acute + deaths_planned) / deaths_total, 2)
  )


# 5. District-level heterogeneity ----------------------------------------------

district_summary <- dat_rq1_week %>%
  mutate(period = if_else(week_type == "Heatwave week", "hw", "ref")) %>%
  group_by(BKZ, period) %>%
  summarise(
    n_weeks   = n(),
    cases     = sum(cases),
    deaths    = sum(deaths),
    median_cf = median(cf_per_1000),
    .groups   = "drop"
  ) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(n_weeks, cases, deaths, median_cf)
  ) %>%
  mutate(
    total_cases = cases_hw + cases_ref,
    cf_ratio    = if_else(median_cf_ref > 0, median_cf_hw / median_cf_ref, NA_real_)
  )

# Districts with a median reference-week case fatality of zero are excluded
district_ratios <- district_summary %>%
  filter(is.finite(cf_ratio)) %>%
  arrange(desc(cf_ratio))

district_ratio_summary <- district_ratios %>%
  summarise(
    n_included = n(),
    n_excluded = nrow(district_summary) - n(),
    median     = median(cf_ratio),
    q1         = quantile(cf_ratio, 0.25),
    q3         = quantile(cf_ratio, 0.75),
    min        = min(cf_ratio),
    max        = max(cf_ratio),
    n_above_1  = sum(cf_ratio > 1),
    n_equal_1  = sum(cf_ratio == 1),
    n_below_1  = sum(cf_ratio < 1)
  )

district_ratios <- district_ratios %>%
  mutate(is_vienna = as.character(BKZ) == "900")

fig_district <- ggplot(district_ratios, aes(x = total_cases, y = cf_ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_point(
    data = filter(district_ratios, !is_vienna),
    size = 2, alpha = 0.7, colour = "grey20"
  ) +
  geom_point(
    data = filter(district_ratios, is_vienna),
    size = 3.5, shape = 17, colour = "black"
  ) +
  geom_text(
    data = filter(district_ratios, is_vienna),
    label = "Vienna", vjust = -1.2, size = 3.8
  ) +
  scale_x_log10(
    name   = "Cardiovascular hospitalisations per district (log scale)",
    labels = scales::label_comma()
  ) +
  scale_y_continuous(
    name = "Ratio of median weekly case fatality\n(heatwave vs. reference weeks)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())


# 6. Main analysis: quasi-Poisson regression -----------------------------------

# Q-AIC with the dispersion parameter in the penalty term
fqaic <- function(model) {
  loglik <- sum(dpois(model$y, model$fitted.values, log = TRUE))
  phi    <- summary(model)$dispersion
  k      <- sum(!is.na(coef(model)))
  -2 * loglik + 2 * k * phi
}

extract_rr <- function(model) {
  term <- "week_typeHeatwave week"
  ci   <- exp(confint.default(model)[term, ])
  tibble(
    RR         = exp(coef(model)[[term]]),
    CI_low     = ci[[1]],
    CI_high    = ci[[2]],
    dispersion = summary(model)$dispersion,
    QAIC       = fqaic(model)
  )
}

fit_model <- function(data, df_season = 6, year_term = "factor(date_year)") {
  f <- as.formula(paste0(
    "deaths ~ week_type + factor(BKZ) + ", year_term,
    " + ns(date_weekofyear, df = ", df_season, ") + offset(log(cases))"
  ))
  glm(f, family = quasipoisson(), data = data)
}

fmt_rr <- function(rr, lo, hi, digits = 2) {
  f <- function(x) formatC(x, format = "f", digits = digits)
  paste0(f(rr), " (", f(lo), "–", f(hi), ")")
}

# Overdispersion check (Poisson)
model_poisson <- glm(
  deaths ~ week_type + offset(log(cases)),
  family = poisson(), data = dat_rq1_week
)
sum(residuals(model_poisson, type = "pearson")^2) / model_poisson$df.residual

models_main <- list(
  "Unadjusted quasi-Poisson" = glm(
    deaths ~ week_type + offset(log(cases)),
    family = quasipoisson(), data = dat_rq1_week
  ),
  "Adjusted for district" = glm(
    deaths ~ week_type + factor(BKZ) + offset(log(cases)),
    family = quasipoisson(), data = dat_rq1_week
  ),
  "Adjusted for district and year" = glm(
    deaths ~ week_type + factor(BKZ) + factor(date_year) + offset(log(cases)),
    family = quasipoisson(), data = dat_rq1_week
  )
)

for (d in 3:8) {
  models_main[[paste0("District + year + seasonal spline df", d)]] <-
    fit_model(dat_rq1_week, df_season = d)
}

model_comparison <- bind_rows(
  imap(models_main, ~ extract_rr(.x) %>% mutate(model = .y, .before = 1))
)

model_comparison

# Sensitivity analysis: calendar year as natural spline instead of factor
model_year_spline <- fit_model(
  dat_rq1_week, df_season = 6, year_term = "ns(date_year, df = 3)"
)
year_spline_result <- extract_rr(model_year_spline) %>%
  mutate(model = "District + ns(year, df = 3) + seasonal spline df6", .before = 1)

# Figure: stepwise adjustment (spline df 3–6)
fig_models_levels <- names(models_main)[1:7]

fig_models <- model_comparison %>%
  filter(model %in% fig_models_levels) %>%
  mutate(model = factor(model, levels = rev(fig_models_levels))) %>%
  ggplot(aes(x = RR, y = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), width = 0.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    name   = "Case fatality rate ratio (95% CI)",
    breaks = seq(0.90, 1.15, by = 0.05)
  ) +
  labs(y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())


# 7. Subgroup analyses ---------------------------------------------------------

subgroups <- tribble(
  ~category,         ~label,                              ~cases_var,          ~deaths_var,                   ~first_year,
  "Sex",             "Men",                               "cases_main_I_sexM", "intra.hosp.mort_all.I_sex.m", 2002,
  "Sex",             "Women",                             "cases_main_I_sexF", "intra.hosp.mort_all.I_sex.f", 2002,
  "Age",             "0–64 years",                        "cases_age_00_64",   "deaths_age_00_64",            2002,
  "Age",             "65+ years",                         "cases_age_65_plus", "intra.hosp.mort_age_65.max",  2002,
  "Diagnosis group", "I30–I52 Other heart diseases",      "cases_main_I30.52", "intra.hosp.mort_I30.I52",     2002,
  "Diagnosis group", "I60–I69 Cerebrovascular diseases",  "cases_main_I60.69", "intra.hosp.mort_I60.I69",     2002,
  "Diagnosis group", "I20–I25 Ischaemic heart diseases",  "cases_main_I20.25", "intra.hosp.mort_I20.I25",     2002,
  "Diagnosis group", "I70–I79 Arterial diseases",         "cases_main_I70.79", "intra.hosp.mort_I70.I79",     2002,
  "Diagnosis group", "I26–I28 Pulmonary heart diseases",  "cases_main_I26.28", "intra.hosp.mort_I26.I28",     2002,
  "Diagnosis group", "I10–I15 Hypertensive diseases",     "cases_main_I10.15", "intra.hosp.mort_I10.I15",     2002,
  "Diagnosis group", "I80–I89 Venous diseases",           "cases_main_I80.89", "intra.hosp.mort_I80.I89",     2002,
  "Admission type",  "Acute admissions",                  "cases_main_a_I",    "deaths_acute",                2007,
  "Admission type",  "Planned admissions",                "cases_main_p_I",    "deaths_planned",              2007
)

subgroup_data <- pmap(
  subgroups,
  function(cases_var, deaths_var, first_year, ...) {
    dat_rq1 %>%
      filter(date_year >= first_year) %>%
      aggregate_weeks(cases_var, deaths_var) %>%
      filter(cases > 0)
  }
)
names(subgroup_data) <- subgroups$label

subgroup_descriptives <- subgroups %>%
  select(category, label) %>%
  mutate(
    total_cases      = map_dbl(subgroup_data, ~ sum(.x$cases)),
    total_deaths     = map_dbl(subgroup_data, ~ sum(.x$deaths)),
    crude_cf_percent = round(100 * total_deaths / total_cases, 2)
  )

run_subgroups <- function(df_season) {
  subgroups %>%
    select(category, label) %>%
    mutate(
      df_season = df_season,
      result    = map(subgroup_data, ~ extract_rr(fit_model(.x, df_season)))
    ) %>%
    unnest(result)
}

subgroup_df6 <- run_subgroups(6)

subgroup_df6 %>%
  mutate(`RR (95% CI)` = fmt_rr(RR, CI_low, CI_high))

plot_subgroups <- function(data) {
  data %>%
    mutate(
      label    = factor(label, levels = rev(subgroups$label)),
      category = factor(category, levels = unique(subgroups$category))
    ) %>%
    ggplot(aes(x = RR, y = label)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), width = 0.2) +
    geom_point(size = 2.8) +
    scale_x_continuous(name = "Case fatality rate ratio (95% CI)") +
    labs(y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      axis.text.y       = element_text(size = 10),
      strip.text.y.left = element_text(angle = 0, face = "bold", margin = margin(r = 8)),
      strip.text.x      = element_text(face = "bold"),
      strip.placement   = "outside",
      panel.spacing     = unit(1.4, "lines")
    )
}

fig_subgroups <- plot_subgroups(subgroup_df6) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y", switch = "y")


# 8. Sensitivity analysis: seasonal spline with 3 df ----------------------------

subgroup_df3 <- run_subgroups(3)

subgroup_comparison <- bind_rows(subgroup_df6, subgroup_df3) %>%
  mutate(
    RR_CI       = fmt_rr(RR, CI_low, CI_high),
    significant = CI_low > 1 | CI_high < 1
  ) %>%
  select(category, label, df_season, RR_CI, significant) %>%
  pivot_wider(
    names_from  = df_season,
    values_from = c(RR_CI, significant),
    names_glue  = "{.value}_df{df_season}"
  ) %>%
  transmute(
    category,
    label,
    `RR (95% CI), df = 6` = RR_CI_df6,
    `RR (95% CI), df = 3` = RR_CI_df3,
    conclusion_changed    = if_else(significant_df6 != significant_df3, "Yes", "No")
  )

fig_subgroups_df3 <- bind_rows(subgroup_df6, subgroup_df3) %>%
  mutate(df_label = factor(paste0("df = ", df_season), levels = c("df = 6", "df = 3"))) %>%
  plot_subgroups() +
  facet_grid(category ~ df_label, scales = "free_y", space = "free_y", switch = "y")


# 9. Sensitivity analysis: COVID-19 years --------------------------------------

analysis_data <- list(
  "All CVD admissions" = dat_rq1_week,
  "Acute admissions"   = subgroup_data[["Acute admissions"]],
  "Planned admissions" = subgroup_data[["Planned admissions"]]
)

covid_periods <- tribble(
  ~analysis,            ~period,                         ~first, ~last,
  "All CVD admissions", "2002–2021 (main analysis)",     2002,   2021,
  "All CVD admissions", "2002–2019 (pre-COVID-19)",      2002,   2019,
  "All CVD admissions", "2007–2019 (pre-COVID-19)",      2007,   2019,
  "All CVD admissions", "2020–2021 (COVID-19)",          2020,   2021,
  "Acute admissions",   "2007–2021 (subgroup analysis)", 2007,   2021,
  "Acute admissions",   "2007–2019 (pre-COVID-19)",      2007,   2019,
  "Acute admissions",   "2020–2021 (COVID-19)",          2020,   2021,
  "Planned admissions", "2007–2021 (subgroup analysis)", 2007,   2021,
  "Planned admissions", "2007–2019 (pre-COVID-19)",      2007,   2019,
  "Planned admissions", "2020–2021 (COVID-19)",          2020,   2021
)

run_periods <- function(df_season) {
  covid_periods %>%
    mutate(
      df_season = df_season,
      result = pmap(list(analysis, first, last), function(a, f, l) {
        d <- filter(analysis_data[[a]], between(date_year, f, l))
        bind_cols(
          tibble(
            n_heatwave_weeks  = sum(d$week_type == "Heatwave week"),
            n_reference_weeks = sum(d$week_type == "Reference week")
          ),
          extract_rr(fit_model(d, df_season))
        )
      })
    ) %>%
    unnest(result)
}

covid_results <- bind_rows(run_periods(6), run_periods(3))

covid_table <- covid_results %>%
  mutate(
    RR_CI       = fmt_rr(RR, CI_low, CI_high, digits = 3),
    significant = if_else(CI_low > 1 | CI_high < 1, "Yes", "No")
  ) %>%
  select(analysis, period, df_season, n_heatwave_weeks, n_reference_weeks, RR_CI, significant) %>%
  pivot_wider(
    names_from  = df_season,
    values_from = c(RR_CI, significant),
    names_glue  = "{.value}_df{df_season}"
  ) %>%
  select(
    analysis, period, n_heatwave_weeks, n_reference_weeks,
    `RR (95% CI), df = 6` = RR_CI_df6,
    `Significant, df = 6` = significant_df6,
    `RR (95% CI), df = 3` = RR_CI_df3,
    `Significant, df = 3` = significant_df3
  )


# 10. Export -------------------------------------------------------------------

write_xlsx(
  list(
    "Exclusions"             = exclusions,
    "Descriptives"           = descriptives,
    "Overall sample"         = overall_sample,
    "Demographics"           = demographics,
    "Deaths by diagnosis"    = deaths_by_diagnosis,
    "Admission coverage"     = admission_coverage,
    "District ratios"        = select(district_ratios, -is_vienna),
    "District ratio summary" = district_ratio_summary,
    "Model comparison"       = model_comparison,
    "Year spline"            = year_spline_result,
    "Subgroup descriptives"  = subgroup_descriptives,
    "Subgroups"              = subgroup_df6,
    "Subgroups df6 vs df3"   = subgroup_comparison,
    "COVID-19 periods"       = covid_table
  ),
  path = file.path(output_dir, "rq1_results.xlsx")
)

ggsave(file.path(output_dir, "fig_model_comparison.png"),
       fig_models, width = 8, height = 4.5, dpi = 300)
ggsave(file.path(output_dir, "fig_subgroups.png"),
       fig_subgroups, width = 8, height = 7, dpi = 300)
ggsave(file.path(output_dir, "fig_subgroups_df6_vs_df3.png"),
       fig_subgroups_df3, width = 10, height = 7, dpi = 300)
ggsave(file.path(output_dir, "fig_district_heterogeneity.png"),
       fig_district, width = 7, height = 4.5, dpi = 300)

sessionInfo()
