
#AB HIER STARTET DER NEUE CODE am 06.09.2026#############################################


############################################################
# 0. PREPARATION AND DATA MANAGEMENT
############################################################


# ============================================================
# 0.1 Load packages
# ============================================================
install.packages("writexl")
library(arrow)
library(tidyverse)
library(lubridate)
library(janitor)
library(splines)
library(writexl)


# ============================================================
# 0.2 Import new aggregated dataset
# ============================================================

#diese Datei auswählen TSData_v11_BZK_PW_1219_20260902.rds

dat <- readRDS(file.choose())


# Alte Variablennamen für bestehenden RQ1-Code wiederherstellen
dat <- dat %>%
  mutate(
    BKZ         = location_county_home,
    BKZ_name    = location_home_BZK_name,
    BL_Name     = location_home_BL_name,
    BL_Kennzahl = location_home_BLKZ,
    Tmp_min     = tmp_bl_min,
    Tmp_avg     = tmp_bl_avg
  )


names()



# ============================================================
# 0.4 Define temperature, CVD cases, and in-hospital deaths
# ============================================================

# cases_main_I corresponds to the sum of the seven predefined
# cardiovascular ICD-10 diagnosis groups:
# I10-I15, I20-I25, I26-I28, I30-I52,
# I60-I69, I70-I79, and I80-I89.

dat <- dat %>%
  mutate(
    Tavg = Tmp_avg,
    cases = replace_na(cases_main_I, 0),
    deaths = replace_na(
      cases_intrahospitalmortality_I,
      0
    )
  )


# ============================================================
# 0.5 Verify the CVD case definition
# ============================================================

cvd_variables <- c(
  "cases_main_I10.15",
  "cases_main_I20.25",
  "cases_main_I26.28",
  "cases_main_I30.52",
  "cases_main_I60.69",
  "cases_main_I70.79",
  "cases_main_I80.89"
)

cvd_definition_check <- dat %>%
  mutate(
    sum_7_cvd_groups = rowSums(
      across(all_of(cvd_variables)),
      na.rm = TRUE
    )
  ) %>%
  summarise(
    total_cases_main_I = sum(cases),
    total_cases_7_groups = sum(sum_7_cvd_groups),
    rows_with_difference = sum(
      cases != sum_7_cvd_groups
    )
  )

cvd_definition_check


# ============================================================
# 0.6 Final plausibility check
# ============================================================

dat %>%
  summarise(
    n_rows = n(),
    n_districts = n_distinct(BKZ),
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    total_cases = sum(cases),
    total_deaths = sum(deaths),
    missing_Tavg = sum(is.na(Tavg)),
    missing_cases = sum(is.na(cases)),
    missing_deaths = sum(is.na(deaths))
  )

############################################################
# 0.5 HEATWAVE CLASSIFICATION
############################################################


# ============================================================
# 0.5.2 District-specific temperature thresholds
# ============================================================

# Hot day:
# Tavg > district-specific 95th percentile
#
# Reference day:
# Tavg between the district-specific 20th and 80th percentiles
#
# Thresholds are calculated using all available dates from 2002 onwards.

dat <- dat %>%
  filter(year(date) >= 2002) %>%
  group_by(BKZ) %>%
  mutate(
    hot_day_def_Tavg_by_BZK =
      quantile(Tavg, probs = 0.95, na.rm = TRUE),
    
    hot_day_bin_Tavg_by_BZK =
      if_else(
        Tavg > hot_day_def_Tavg_by_BZK,
        1L,
        0L
      ),
    
    ref_day_def_Tavg_by_BZK_u =
      quantile(Tavg, probs = 0.80, na.rm = TRUE),
    
    ref_day_def_Tavg_by_BZK_l =
      quantile(Tavg, probs = 0.20, na.rm = TRUE),
    
    ref_day_bin_Tavg_by_BZK =
      if_else(
        Tavg >= ref_day_def_Tavg_by_BZK_l &
          Tavg <= ref_day_def_Tavg_by_BZK_u,
        1L,
        0L
      )
  ) %>%
  ungroup()


# Checks

table(
  dat$hot_day_bin_Tavg_by_BZK,
  useNA = "ifany"
)

table(
  dat$ref_day_bin_Tavg_by_BZK,
  useNA = "ifany"
)


# Check: one threshold per district

dat %>%
  group_by(BKZ) %>%
  summarise(
    p20 = first(ref_day_def_Tavg_by_BZK_l),
    p80 = first(ref_day_def_Tavg_by_BZK_u),
    p95 = first(hot_day_def_Tavg_by_BZK),
    .groups = "drop"
  ) %>%
  arrange(BKZ)


# ============================================================
# 0.5.3 Create study-specific weeks
# ============================================================

# These are study-specific weeks based on the day of the year.
# They are not ISO calendar weeks.

dat <- dat %>%
  mutate(
    date_year = year(date),
    date_dayofyear = yday(date),
    date_weekofyear = floor(date_dayofyear / 7) + 1
  )


# Check week assignment

dat %>%
  arrange(BKZ, date) %>%
  select(
    date,
    BKZ,
    date_year,
    date_dayofyear,
    date_weekofyear
  ) %>%
  head(20)


# Number of weeks per year

dat %>%
  group_by(date_year) %>%
  summarise(
    min_week = min(date_weekofyear, na.rm = TRUE),
    max_week = max(date_weekofyear, na.rm = TRUE),
    n_weeks = n_distinct(date_weekofyear),
    .groups = "drop"
  )


# ============================================================
# 0.5.4 Identify consecutive hot days
# ============================================================

# Consecutive hot days are identified across the complete
# chronological sequence within each district.
#
# The calculation is not grouped by week because a hot-day pair
# may cross a week boundary.

dat <- dat %>%
  arrange(BKZ, date) %>%
  group_by(BKZ) %>%
  mutate(
    date_lag1 = lag(date),
    
    hot_lag_day_bin_Tavg_by_BZK =
      lag(hot_day_bin_Tavg_by_BZK),
    
    consecutive_hot_days = if_else(
      hot_day_bin_Tavg_by_BZK == 1 &
        hot_lag_day_bin_Tavg_by_BZK == 1 &
        as.numeric(date - date_lag1) == 1,
      1L,
      0L,
      missing = 0L
    )
  ) %>%
  ungroup()


# Check

table(
  dat$consecutive_hot_days,
  useNA = "ifany"
)


# Optional plausibility check

consecutive_hot_day_check <- dat %>%
  filter(consecutive_hot_days == 1) %>%
  select(
    BKZ,
    date,
    Tavg,
    hot_day_def_Tavg_by_BZK,
    hot_day_bin_Tavg_by_BZK,
    hot_lag_day_bin_Tavg_by_BZK,
    date_lag1,
    consecutive_hot_days
  ) %>%
  arrange(BKZ, date)

head(consecutive_hot_day_check)


# Number of marked consecutive-hot-day events per district

heatwave_days_by_district <- dat %>%
  group_by(BKZ) %>%
  summarise(
    heatwave_days = sum(
      consecutive_hot_days,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(heatwave_days))

heatwave_days_by_district


# ============================================================
# 0.5.5 Classification of weeks
# ============================================================

# Heatwave week:
# At least one occurrence of two consecutive hot days.
#
# Reference week:
# Seven available days and all seven days between p20 and p80.
#
# Other week:
# Neither heatwave week nor reference week.

dat <- dat %>%
  group_by(
    BKZ,
    date_year,
    date_weekofyear
  ) %>%
  mutate(
    n_days_in_week = n(),
    
    n_consecutive_hot_days =
      sum(consecutive_hot_days, na.rm = TRUE),
    
    n_ref_days =
      sum(ref_day_bin_Tavg_by_BZK, na.rm = TRUE),
    
    heatwave_week =
      if_else(
        n_consecutive_hot_days >= 1,
        1L,
        0L
      ),
    
    reference_week =
      if_else(
        n_days_in_week == 7 &
          n_ref_days == 7 &
          heatwave_week == 0,
        1L,
        0L
      ),
    
    other_week =
      if_else(
        heatwave_week == 0 &
          reference_week == 0,
        1L,
        0L
      ),
    
    week_type = case_when(
      heatwave_week == 1 ~ "heatwave_week",
      reference_week == 1 ~ "reference_week",
      TRUE ~ "other_week"
    )
  ) %>%
  ungroup()


# ============================================================
# 0.5.6 Checks of week classification
# ============================================================

table(dat$heatwave_week, useNA = "ifany")
table(dat$reference_week, useNA = "ifany")
table(dat$other_week, useNA = "ifany")
table(dat$week_type, useNA = "ifany")


# Number of district-weeks by classification

dat %>%
  distinct(
    BKZ,
    date_year,
    date_weekofyear,
    heatwave_week,
    reference_week,
    other_week,
    week_type
  ) %>%
  count(week_type)


# Check that each row belongs to exactly one category

dat %>%
  mutate(
    week_flag_sum =
      heatwave_week +
      reference_week +
      other_week
  ) %>%
  count(week_flag_sum)


# Check duplicate district-date combinations

dat %>%
  count(BKZ, date) %>%
  filter(n > 1)


# ============================================================
# 0.5.7 Save prepared dataset
# ============================================================

saveRDS(
  dat,
  "dat_RQ1_newdata_heatwave_ready.rds"
)


# ============================================================
# 0.5.8 Cases and deaths by year and month
# ============================================================

dat %>%
  group_by(date_year) %>%
  summarise(
    sum_cases = sum(cases),
    sum_deaths = sum(deaths),
    .groups = "drop"
  )


dat %>%
  group_by(
    date_month = month(date)
  ) %>%
  summarise(
    sum_cases = sum(cases),
    sum_deaths = sum(deaths),
    .groups = "drop"
  )













# ============================================================
# 6.1 Analyse-Datensatz für RQ1 erstellen
# ============================================================

dat_rq1 <- dat %>%
  filter(
    date_year >= 2002,
    date_year <= 2021,
    month(date) >= 5,
    month(date) <= 10,
    heatwave_week == 1 | reference_week == 1
  ) %>%
  mutate(
    week_type = case_when(
      heatwave_week == 1 ~ "Heatwave week",
      reference_week == 1 ~ "Reference week",
      TRUE ~ NA_character_
    ),
    week_type = factor(
      week_type,
      levels = c("Reference week", "Heatwave week")
    )
  )



# Checks
table(dat_rq1$week_type, dat_rq1$reference_week, useNA = "ifany") #NEU
table(dat_rq1$week_type, dat_rq1$heatwave_week, useNA = "ifany") #NEU

table(dat_rq1$heatwave_week, dat_rq1$reference_week, useNA = "ifany")
table(dat_rq1$week_type, useNA = "ifany")


# ============================================================
# 6.2 Aggregate to district-week level
# One row = district × year × study week
# Only complete 7-day weeks are retained
# ============================================================

dat_rq1_week_all <- dat_rq1 %>%
  group_by(
    BKZ,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(n_days == 7)

# ============================================================
# 6.3 Checks des aggregierten Datensatzes
# ============================================================

dat_rq1_week <- dat_rq1 %>%
  group_by(
    BKZ,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(n_days == 7)








head(dat_rq1_week)

summary(dat_rq1_week$cases)
summary(dat_rq1_week$deaths)

table(dat_rq1_week$week_type, useNA = "ifany")

dat_rq1_week %>%
  group_by(week_type) %>%
  summarise(
    n_weeks = n(),
    total_cases = sum(cases, na.rm = TRUE),
    mean_cases = mean(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    mean_deaths = mean(deaths, na.rm = TRUE), 
    .groups = "drop"
  )

dat_rq1_week %>%
  group_by(BKZ, date_year, date_weekofyear, week_type) %>%
  summarise(
    cases = sum(cases, na.rm=TRUE),
    deaths = sum(deaths, na.rm=TRUE)
  ) %>% ungroup() %>% 
  group_by(week_type) %>%
  summarise(
    n_weeks = n(),
    total_cases = sum(cases, na.rm = TRUE),
    mean_cases = mean(cases, na.rm = TRUE),
    median_cases = median(cases, na.rm = TRUE),
    q25_cases = quantile(cases, 0.25, na.rm = TRUE), #25th quantile
    q75_cases = quantile(cases, 0.75, na.rm = TRUE), #75th quantile
    total_deaths = sum(deaths, na.rm = TRUE),
    mean_deaths = mean(deaths, na.rm = TRUE),
    median_deaths = median(deaths, na.rm = TRUE),
    q25_deaths = quantile(deaths, 0.25, na.rm = TRUE), #25th quantile
    q75_deaths = quantile(deaths, 0.75, na.rm = TRUE), #75th quantile
    .groups = "drop"
  )


# ============================================================
# 6.4 Wochen mit 0 Fällen ausschließen
# Wichtig für spätere Modelle mit log(cases) als Offset
# ============================================================

dat_rq1_week <- dat_rq1_week %>%
  filter(cases > 0)


# ============================================================
# 6.5 Rohe Mortalitätsrate berechnen
# ============================================================

dat_rq1_week <- dat_rq1_week %>%
  mutate(
    mortality_rate = (deaths / cases)*1000 # => Klassisch in der Epidemiologie, würde ich  pro 1000 berichten
  )

summary(dat_rq1_week$mortality_rate)

dat_rq1_week %>%
  group_by(week_type) %>%
  summarise(
    n_weeks = n(),
    total_cases = sum(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    crude_mortality_rate = total_deaths / total_cases,
    mean_weekly_mortality_rate = mean(mortality_rate, na.rm = TRUE),
    .groups = "drop"
  )

########################################################################
########################################################################
# Bezirksweise Deskription der Mortalitäts-Ratios
# Heatwave week vs. Reference week
########################################################################

# Ziel:
# Für jeden Bezirk wird berechnet:
# mediane Mortalitätsrate in Heatwave-Wochen /
# mediane Mortalitätsrate in Reference-Wochen


############################################################
# 6.0 RQ1 – ANALYSIS PREPARATION
############################################################


# ============================================================
# 6.1 Create analysis dataset
# ============================================================

dat_rq1 <- dat %>%
  filter(
    date_year >= 2002,
    date_year <= 2021,
    month(date) %in% 5:10,
    heatwave_week == 1 | reference_week == 1
  ) %>%
  mutate(
    week_type = case_when(
      heatwave_week == 1 ~ "Heatwave week",
      reference_week == 1 ~ "Reference week",
      TRUE ~ NA_character_
    ),
    week_type = factor(
      week_type,
      levels = c(
        "Reference week",
        "Heatwave week"
      )
    )
  )


# ============================================================
# 6.1.1 Checks of week classification
# ============================================================

table(
  dat_rq1$week_type,
  dat_rq1$reference_week,
  useNA = "ifany"
)

table(
  dat_rq1$week_type,
  dat_rq1$heatwave_week,
  useNA = "ifany"
)


# ============================================================
# 6.2 Aggregate to district-week level
# One row = district × year × study week
# ============================================================

dat_rq1_week_all <- dat_rq1 %>%
  group_by(
    BKZ,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(n_days == 7)

# ============================================================
# 6.3 Basic checks of weekly dataset
# ============================================================

head(dat_rq1_week_all)

summary(dat_rq1_week_all$cases)
summary(dat_rq1_week_all$deaths)

table(
  dat_rq1_week_all$week_type,
  useNA = "ifany"
)

table(
  dat_rq1_week_all$n_days,
  useNA = "ifany"
)


# ============================================================
# 6.3.1 Number of weeks before exclusion
# ============================================================

rq1_week_counts_before_exclusion <- dat_rq1_week_all %>%
  count(
    week_type,
    name = "n_weeks_before_exclusion"
  )

rq1_week_counts_before_exclusion


# ============================================================
# 6.3.2 Descriptive statistics before exclusion
# ============================================================

rq1_week_descriptive_all <- dat_rq1_week_all %>%
  group_by(week_type) %>%
  summarise(
    n_weeks = n(),
    
    total_cases = sum(cases),
    mean_cases = mean(cases),
    median_cases = median(cases),
    q25_cases = quantile(cases, 0.25),
    q75_cases = quantile(cases, 0.75),
    
    total_deaths = sum(deaths),
    mean_deaths = mean(deaths),
    median_deaths = median(deaths),
    q25_deaths = quantile(deaths, 0.25),
    q75_deaths = quantile(deaths, 0.75),
    
    .groups = "drop"
  )

rq1_week_descriptive_all


# ============================================================
# 6.3.3 Distribution of number of days per district-week
# ============================================================

rq1_week_length_distribution <- dat_rq1_week_all %>%
  count(
    week_type,
    n_days,
    name = "n_district_weeks"
  ) %>%
  arrange(
    week_type,
    n_days
  )

rq1_week_length_distribution


# ============================================================
# 6.4 Exclude weeks with zero CVD admissions
# Required because log(cases) is used as offset
# ============================================================

dat_rq1_week <- dat_rq1_week_all %>%
  filter(cases > 0)


# ============================================================
# 6.4.1 Document excluded weeks
# ============================================================

rq1_zero_case_exclusion <- dat_rq1_week_all %>%
  group_by(week_type) %>%
  summarise(
    weeks_before_exclusion = n(),
    weeks_with_zero_cases = sum(cases == 0),
    weeks_after_exclusion = sum(cases > 0),
    percent_excluded = round(
      100 * weeks_with_zero_cases /
        weeks_before_exclusion,
      2
    ),
    .groups = "drop"
  )

rq1_zero_case_exclusion


# ============================================================
# 6.5 Calculate crude mortality rates
# ============================================================

dat_rq1_week <- dat_rq1_week %>%
  mutate(
    mortality_rate_per_1000 =
      deaths / cases * 1000
  )


# ============================================================
# 6.5.1 Main descriptive results by week type
# ============================================================

rq1_main_descriptive_results <- dat_rq1_week %>%
  group_by(week_type) %>%
  summarise(
    n_district_weeks = n(),
    
    total_cases = sum(cases),
    total_deaths = sum(deaths),
    
    crude_mortality_rate_per_1000 =
      total_deaths / total_cases * 1000,
    
    mean_weekly_cases =
      mean(cases),
    
    median_weekly_cases =
      median(cases),
    
    q25_weekly_cases =
      quantile(cases, 0.25),
    
    q75_weekly_cases =
      quantile(cases, 0.75),
    
    mean_weekly_deaths =
      mean(deaths),
    
    median_weekly_deaths =
      median(deaths),
    
    q25_weekly_deaths =
      quantile(deaths, 0.25),
    
    q75_weekly_deaths =
      quantile(deaths, 0.75),
    
    mean_weekly_mortality_per_1000 =
      mean(mortality_rate_per_1000),
    
    median_weekly_mortality_per_1000 =
      median(mortality_rate_per_1000),
    
    q25_weekly_mortality_per_1000 =
      quantile(
        mortality_rate_per_1000,
        0.25
      ),
    
    q75_weekly_mortality_per_1000 =
      quantile(
        mortality_rate_per_1000,
        0.75
      ),
    
    .groups = "drop"
  )

rq1_main_descriptive_results


# ============================================================
# 6.5.2 Overall study population
# ============================================================

rq1_overall_sample <- dat_rq1_week %>%
  summarise(
    n_district_weeks = n(),
    n_districts = n_distinct(BKZ),
    min_year = min(date_year),
    max_year = max(date_year),
    total_cases = sum(cases),
    total_deaths = sum(deaths),
    
    crude_mortality_rate_per_1000 =
      total_deaths / total_cases * 1000,
    
    crude_mortality_percent =
      total_deaths / total_cases * 100
  )

rq1_overall_sample


# ============================================================
# 6.6 District-level mortality ratios
# ============================================================

rq1_bzk_summary <- dat_rq1_week %>%
  group_by(
    BKZ,
    week_type
  ) %>%
  summarise(
    n_weeks = n(),
    total_cases = sum(cases),
    total_deaths = sum(deaths),
    
    median_weekly_mortality_per_1000 =
      median(mortality_rate_per_1000),
    
    mean_weekly_mortality_per_1000 =
      mean(mortality_rate_per_1000),
    
    crude_mortality_rate_per_1000 =
      total_deaths / total_cases * 1000,
    
    .groups = "drop"
  )

rq1_bzk_summary


# ============================================================
# 6.6.1 Convert district results to wide format
# ============================================================

rq1_bzk_ratio <- rq1_bzk_summary %>%
  select(
    BKZ,
    week_type,
    n_weeks,
    total_cases,
    total_deaths,
    median_weekly_mortality_per_1000
  ) %>%
  pivot_wider(
    names_from = week_type,
    values_from = c(
      n_weeks,
      total_cases,
      total_deaths,
      median_weekly_mortality_per_1000
    )
  )


# ============================================================
# 6.6.2 Calculate district-specific mortality ratio
# ============================================================

rq1_bzk_ratio <- rq1_bzk_ratio %>%
  mutate(
    mortality_ratio_median = if_else(
      `median_weekly_mortality_per_1000_Reference week` > 0,
      
      `median_weekly_mortality_per_1000_Heatwave week` /
        `median_weekly_mortality_per_1000_Reference week`,
      
      NA_real_
    )
  )

rq1_bzk_ratio


# ============================================================
# 6.6.3 Clean district-level ratios
# ============================================================

rq1_bzk_ratio_clean <- rq1_bzk_ratio %>%
  filter(
    !is.na(mortality_ratio_median),
    !is.infinite(mortality_ratio_median),
    !is.nan(mortality_ratio_median)
  )


# ============================================================
# 6.6.4 Distribution of district-level ratios
# ============================================================

rq1_ratio_distribution <- rq1_bzk_ratio_clean %>%
  summarise(
    n_districts_included = n(),
    
    n_districts_excluded =
      nrow(rq1_bzk_ratio) - n(),
    
    median_ratio =
      median(mortality_ratio_median),
    
    q1_ratio =
      quantile(
        mortality_ratio_median,
        0.25
      ),
    
    q3_ratio =
      quantile(
        mortality_ratio_median,
        0.75
      ),
    
    iqr_ratio =
      IQR(mortality_ratio_median),
    
    minimum_ratio =
      min(mortality_ratio_median),
    
    maximum_ratio =
      max(mortality_ratio_median)
  )

rq1_ratio_distribution


# ============================================================
# 6.6.5 Direction of district-level ratios
# ============================================================

rq1_ratio_groups <- rq1_bzk_ratio_clean %>%
  mutate(
    ratio_group = case_when(
      mortality_ratio_median > 1 ~
        "Higher mortality during heatwave weeks",
      
      mortality_ratio_median == 1 ~
        "No difference",
      
      mortality_ratio_median < 1 ~
        "Lower mortality during heatwave weeks"
    )
  ) %>%
  count(
    ratio_group,
    name = "n_districts"
  ) %>%
  mutate(
    percent_districts =
      round(
        100 * n_districts /
          sum(n_districts),
        1
      )
  )

rq1_ratio_groups


# ============================================================
# 6.6.6 Districts with highest ratios
# ============================================================

rq1_bzk_highest_ratios <- rq1_bzk_ratio_clean %>%
  arrange(
    desc(mortality_ratio_median)
  ) %>%
  slice_head(n = 10)

rq1_bzk_highest_ratios


# ============================================================
# 6.6.7 Districts with lowest ratios
# ============================================================

rq1_bzk_lowest_ratios <- rq1_bzk_ratio_clean %>%
  arrange(
    mortality_ratio_median
  ) %>%
  slice_head(n = 10)

rq1_bzk_lowest_ratios

# ============================================================
# 6.7 Export descriptive RQ1 results to Excel
# ============================================================

library(writexl)

rq1_descriptive_export <- list(
  
  "Overall sample" =
    rq1_overall_sample,
  
  "Main results by week type" =
    rq1_main_descriptive_results,
  
  "Week counts before exclusion" =
    rq1_week_counts_before_exclusion,
  
  "Zero-case exclusions" =
    rq1_zero_case_exclusion,
  
  "Week length distribution" =
    rq1_week_length_distribution,
  
  "Weekly description all" =
    rq1_week_descriptive_all,
  
  "District summary" =
    rq1_bzk_summary,
  
  "District mortality ratios" =
    rq1_bzk_ratio,
  
  "Ratio distribution" =
    rq1_ratio_distribution,
  
  "Ratio direction" =
    rq1_ratio_groups,
  
  "Highest district ratios" =
    rq1_bzk_highest_ratios,
  
  "Lowest district ratios" =
    rq1_bzk_lowest_ratios
)


write_xlsx(
  rq1_descriptive_export,
  path = "RQ1_descriptive_results_new_dataset.xlsx"
)

getwd()

file.exists(
  "RQ1_descriptive_results_new_dataset.xlsx"
)

# ============================================================
# 6.6.8 Figure: District-level ratios by number of hospitalisations
# ============================================================

library(ggplot2)


vienna_code <- 900

plot_bzk <- rq1_bzk_ratio_clean %>%
  mutate(
    total_cases_all = `total_cases_Heatwave week` + `total_cases_Reference week`,
    is_vienna       = as.character(BKZ) == as.character(vienna_code)
  )

# Kontrolle: 
table(plot_bzk$is_vienna)

figure_district <- ggplot(
  plot_bzk,
  aes(x = total_cases_all, y = mortality_ratio_median)
) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_point(
    data = filter(plot_bzk, !is_vienna),
    size = 2, alpha = 0.7, color = "grey20"
  ) +
  geom_point(
    data = filter(plot_bzk, is_vienna),
    size = 3.5, shape = 17, color = "black"
  ) +
  geom_text(
    data = filter(plot_bzk, is_vienna),
    aes(label = "Vienna"),
    vjust = -1.2, size = 3.8
  ) +
  scale_x_log10(
    name = "Cardiovascular hospitalisations per district (log scale)",
    labels = scales::label_comma()
  ) +
  scale_y_continuous(
    name = "Ratio of median weekly case fatality rates\n(heatwave vs. reference weeks)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

figure_district

ggsave(
  filename = "Figure_District_Heterogeneity.png",
  plot = figure_district,
  width = 7, height = 4.5, dpi = 300
)

getwd()


plot_bzk %>% filter(is_vienna) %>% select(total_cases_all, mortality_ratio_median)















############################################################
############################################################
############################################################
############################################################
############################################################
# 7.0 RQ1 – MAIN REGRESSION ANALYSIS
############################################################
############################################################
############################################################
############################################################


# Gedankenstütze:
# Ziel der Modelle ist der Vergleich der intrahospitalen
# CVD-Mortalität zwischen Heatwave- und Reference-Weeks.
#
# Outcome:
# Anzahl intrahospitaler Todesfälle pro Bezirk-Woche.
#
# Offset:
# log(cases) berücksichtigt die Anzahl der hospitalisierten
# CVD-Patient:innen in der jeweiligen Bezirk-Woche.
#
# Der exponentierte Koeffizient für Heatwave week wird als
# Incidence Rate Ratio (IRR) interpretiert.
#
# IRR > 1:
# höhere intrahospitale Mortalitätsrate während Heatwave-Weeks.
#
# IRR < 1:
# niedrigere intrahospitale Mortalitätsrate während Heatwave-Weeks.


# ============================================================
# 7.1 Unadjusted Poisson model
# ============================================================

# Gedankenstütze:
# Dieses einfache Modell enthält nur den Wochentyp.
# Es dient als Ausgangsmodell und zur Prüfung auf Overdispersion.

model1_poisson <- glm(
  deaths ~ week_type,
  offset = log(cases),
  family = poisson(),
  data = dat_rq1_week
)

summary(model1_poisson)


# Heatwave-Effekt als IRR und 95%-Konfidenzintervall

exp(
  coef(model1_poisson)[
    "week_typeHeatwave week"
  ]
)

exp(
  confint.default(model1_poisson)[
    "week_typeHeatwave week",
  ]
)


# ============================================================
# 7.2 Check for overdispersion
# ============================================================

# Gedankenstütze:
# Bei einer Poisson-Verteilung sollten Mittelwert und Varianz
# annähernd gleich sein.
#
# Eine Dispersion deutlich über 1 spricht für Overdispersion.
# In diesem Fall werden die Standardfehler des Poisson-Modells
# unterschätzt und ein Quasi-Poisson-Modell ist geeigneter.

dispersion_model1_poisson <-
  sum(
    residuals(
      model1_poisson,
      type = "pearson"
    )^2
  ) /
  model1_poisson$df.residual

dispersion_model1_poisson


# ============================================================
# 7.3 Unadjusted quasi-Poisson model
# ============================================================

# Gedankenstütze:
# Das Quasi-Poisson-Modell berücksichtigt Overdispersion.
#
# Die Modellgleichung bleibt gleich, aber die Standardfehler
# werden an die beobachtete Dispersion angepasst.

model1_quasi <- glm(
  deaths ~ week_type,
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_week
)

summary(model1_quasi)


dispersion_model1_quasi <-
  sum(
    residuals(
      model1_quasi,
      type = "pearson"
    )^2
  ) /
  model1_quasi$df.residual

dispersion_model1_quasi


# ============================================================
# 7.4 QAIC function
# ============================================================

# Gedankenstütze:
# Für Quasi-Poisson-Modelle wird kein regulärer AIC ausgegeben.
# Deshalb wird für den Vergleich der Modellvarianten ein
# Quasi-Akaike Information Criterion (QAIC) berechnet.
#
# Ein niedrigerer QAIC spricht für ein günstigeres Verhältnis
# zwischen Modellanpassung und Modellkomplexität.


fqaic <- function(model) {
  
  loglik <- sum(
    dpois(
      model$y,
      model$fitted.values,
      log = TRUE
    )
  )
  
  model_summary <- summary(model)
  
  phi <- model_summary$dispersion
  
  number_parameters <-
    sum(
      !is.na(coef(model))
    )
  
  qaic <-
    -2 * loglik +
    2 * number_parameters * phi
  
  return(qaic)
}


fqaic(model1_quasi)


# ============================================================
# 7.5 District-adjusted quasi-Poisson model
# ============================================================

# Gedankenstütze:
# Dieses Modell kontrolliert für zeitlich konstante
# strukturelle Unterschiede zwischen den 94 Bezirken.
#
# Dazu wird BKZ als kategorialer Fixed Effect berücksichtigt.
#
# Wichtig:
# BKZ_name wird im neuen Datensatz nicht verwendet,
# weil diese Variable keine gültigen Bezirksnamen enthält.

model2_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_week
)

summary(model2_quasi)


dispersion_model2_quasi <-
  sum(
    residuals(
      model2_quasi,
      type = "pearson"
    )^2
  ) /
  model2_quasi$df.residual

dispersion_model2_quasi

fqaic(model2_quasi)


# ============================================================
# 7.6 District- and year-adjusted quasi-Poisson model
# ============================================================

# Gedankenstütze:
# Zusätzlich zu regionalen Unterschieden wird für Unterschiede
# zwischen den Kalenderjahren kontrolliert.
#
# factor(date_year) erlaubt für jedes Jahr ein eigenes Niveau
# und setzt keinen linearen zeitlichen Trend voraus.

table(
  dat_rq1_week$date_year
)


model3_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_week
)

summary(model3_quasi)


dispersion_model3_quasi <-
  sum(
    residuals(
      model3_quasi,
      type = "pearson"
    )^2
  ) /
  model3_quasi$df.residual

dispersion_model3_quasi

fqaic(model3_quasi)


# ============================================================
# 7.7 Control for seasonal patterns using natural splines
# ============================================================

# Gedankenstütze:
# Innerhalb der Warm Season können sich Mortalitätsmuster
# auch unabhängig von Hitzewellen verändern.
#
# Beispielsweise können Mai, Juli und Oktober unterschiedliche
# Basisrisiken aufweisen.
#
# Deshalb wird date_weekofyear als Natural Spline modelliert.
# Dadurch wird der saisonale Verlauf flexibel und nicht nur
# als gerade Linie berücksichtigt.
#
# Es werden 3, 4, 5 und 6 Freiheitsgrade verglichen.
# Das Modell mit dem niedrigsten QAIC wird bevorzugt.

library(splines)


model4_quasi_df3 <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, df = 3),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_week
)


model4_quasi_df4 <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, df = 4),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_week
)


model4_quasi_df5 <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, df = 5),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_week
)


model4_quasi_df6 <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, df = 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_week
)

model4_quasi_df7 <- update(model4_quasi_df6,
                           . ~ . - ns(date_weekofyear, df = 6) + ns(date_weekofyear, df = 7))
model4_quasi_df8 <- update(model4_quasi_df6,
                           . ~ . - ns(date_weekofyear, df = 6) + ns(date_weekofyear, df = 8))

sapply(list(df3 = model4_quasi_df3, df4 = model4_quasi_df4,
            df5 = model4_quasi_df5, df6 = model4_quasi_df6,
            df7 = model4_quasi_df7, df8 = model4_quasi_df8), fqaic)






# ============================================================
# 7.8 Function to extract Heatwave effect
# ============================================================

# Gedankenstütze:
# Diese Funktion extrahiert aus jedem Quasi-Poisson-Modell
# den für RQ1 relevanten Heatwave-Effekt.
#
# Ausgegeben werden:
# - IRR
# - 95%-Konfidenzintervall
# - Dispersion
# - QAIC

extract_heatwave_result <- function(
    fitted_model,
    model_name
) {
  
  coefficient_name <- "week_typeHeatwave week"
  
  confidence_interval <-
    confint.default(fitted_model)[
      coefficient_name,
    ]
  
  tibble(
    model_name = model_name,
    
    IRR = exp(
      coef(fitted_model)[
        coefficient_name
      ]
    ),
    
    CI_low = exp(
      confidence_interval[1]
    ),
    
    CI_high = exp(
      confidence_interval[2]
    ),
    
    dispersion =
      sum(
        residuals(
          fitted_model,
          type = "pearson"
        )^2
      ) /
      fitted_model$df.residual,
    
    QAIC = fqaic(fitted_model)
  )
}

extract_heatwave_result(
  model1_quasi,
  "Unadjusted quasi-Poisson"
)

# ============================================================
# 7.9 Main model comparison
# ============================================================

# Gedankenstütze:
# Diese Tabelle zeigt, wie sich der geschätzte Heatwave-Effekt
# verändert, wenn schrittweise für Bezirk, Jahr und saisonale
# Muster kontrolliert wird.

main_model_comparison <- bind_rows(
  
  extract_heatwave_result(
    model1_quasi,
    "Unadjusted quasi-Poisson"
  ),
  
  extract_heatwave_result(
    model2_quasi,
    "Adjusted for district"
  ),
  
  extract_heatwave_result(
    model3_quasi,
    "Adjusted for district and year"
  ),
  
  extract_heatwave_result(
    model4_quasi_df3,
    "District + year + seasonal spline df3"
  ),
  
  extract_heatwave_result(
    model4_quasi_df4,
    "District + year + seasonal spline df4"
  ),
  
  extract_heatwave_result(
    model4_quasi_df5,
    "District + year + seasonal spline df5"
  ),
  
  extract_heatwave_result(
    model4_quasi_df6,
    "District + year + seasonal spline df6"
  )
) %>%
  mutate(
    IRR = round(IRR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    dispersion = round(dispersion, 3),
    QAIC = round(QAIC, 1),
    
    statistically_significant = if_else(
      CI_low > 1 |
        CI_high < 1,
      "Yes",
      "No"
    )
  ) %>%
  arrange(QAIC)

main_model_comparison


# ============================================================
# 7.10 Spline model comparison
# ============================================================

# Gedankenstütze:
# Diese separate Tabelle enthält nur die saisonal adjustierten
# Modelle und erleichtert die Auswahl der Freiheitsgrade.

spline_comparison <- main_model_comparison %>%
  filter(
    str_detect(
      model_name,
      "seasonal spline"
    )
  ) %>%
  arrange(QAIC)

spline_comparison


# ============================================================
# 7.11 Identify model with lowest QAIC
# ============================================================

best_model_summary <- main_model_comparison %>%
  slice_min(
    QAIC,
    n = 1,
    with_ties = FALSE
  )

best_model_summary

main_model_comparison

rq1_key_models <- main_model_comparison %>%
  filter(
    model_name %in% c(
      "Unadjusted quasi-Poisson",
      "Adjusted for district and year",
      "District + year + seasonal spline df6"
    )
  )

rq1_key_models

############################################################
# 7.13 FOREST PLOT - MODEL COMPARISON (Figure 1)
############################################################

# Gedankenstütze:
# Dieser Plot zeigt, wie sich der geschätzte Heatwave-Effekt
# verändert, wenn schrittweise für Bezirk, Jahr und Saisonalität
# adjustiert wird. Die Reihenfolge folgt der logischen
# Modellentwicklung (nicht der QAIC-Sortierung), damit die
# Erzählung "Effekt verschwindet mit Adjustierung" sichtbar wird.

library(ggplot2)


# ============================================================
# 7.13.1 Reihenfolge der Modelle für die Darstellung festlegen
# ============================================================

forest_data <- main_model_comparison %>%
  mutate(
    model_name = factor(
      model_name,
      levels = c(
        "District + year + seasonal spline df6",
        "District + year + seasonal spline df5",
        "District + year + seasonal spline df4",
        "District + year + seasonal spline df3",
        "Adjusted for district and year",
        "Adjusted for district",
        "Unadjusted quasi-Poisson"
      )
    )
  )


# ============================================================
# 7.13.2 Forest Plot erstellen
# ============================================================

figure1_forest_plot <- ggplot(
  forest_data,
  aes(
    x = IRR,
    y = model_name
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_errorbarh(
    aes(
      xmin = CI_low,
      xmax = CI_high
    ),
    height = 0.2,
    color = "black"
  ) +
  geom_point(
    size = 3,
    color = "black"
  ) +
  scale_x_continuous(
    name = "Incidence rate ratio (95% CI)",
    breaks = seq(0.9, 1.15, by = 0.05)
  ) +
  labs(
    y = NULL,
    title = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12)
  )

figure1_forest_plot


# ============================================================
# 7.13.3 Als hochauflösende Datei exportieren
# ============================================================

ggsave(
  filename = "Figure1_ForestPlot_ModelComparison.png",
  plot = figure1_forest_plot,
  width = 8,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = "Figure1_ForestPlot_ModelComparison.tiff",
  plot = figure1_forest_plot,
  width = 8,
  height = 4.5,
  dpi = 300,
  compression = "lzw"
)


############################################################
# 7.13.3 Figure 1 als Datei speichern
############################################################

ggsave(
  filename = "Figure1_ForestPlot_ModelComparison.png",
  plot = figure1_forest_plot,
  width = 8,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = "Figure1_ForestPlot_ModelComparison.tiff",
  plot = figure1_forest_plot,
  width = 8,
  height = 4.5,
  dpi = 300,
  compression = "lzw"
)

# Kontrolle: liegt die Datei im Arbeitsverzeichnis?
getwd()

file.exists("Figure1_ForestPlot_ModelComparison.png")
file.exists("Figure1_ForestPlot_ModelComparison.tiff")














# ============================================================
# 7.12 Export main regression results to Excel
# ============================================================

rq1_main_model_export <- list(
  
  "Main model comparison" =
    main_model_comparison,
  
  "Spline df comparison" =
    spline_comparison,
  
  "Best model (lowest QAIC)" =
    best_model_summary,
  
  "Key models for reporting" =
    rq1_key_models
)

write_xlsx(
  rq1_main_model_export,
  path = "RQ1_main_model_results.xlsx"
)

getwd()

file.exists(
  "RQ1_main_model_results.xlsx"
)






#############################################
#############################################

# ============================================================
# 8.2 Check variables for sex-specific mortality analysis
# ============================================================

# Gedankenstütze:
# Für eine getrennte Analyse nach Geschlecht werden jeweils
# passende Fallzahlen und Todesfallzahlen benötigt.
#
# Bereits bekannte Fallvariablen:
# - cases_main_I_sexM
# - cases_main_I_sexF
#
# Gesucht werden die dazugehörigen Variablen für
# intrahospitale Todesfälle bei Männern und Frauen.

names(dat)[
  grepl(
    "^intra\\.hosp\\.mort",
    names(dat),
    ignore.case = TRUE
  ) &
    grepl(
      "sex|male|female|\\.m|\\.f",
      names(dat),
      ignore.case = TRUE
    )
]

names(dat)[
  grepl(
    "^intra\\.hosp\\.mort",
    names(dat),
    ignore.case = TRUE
  ) &
    grepl(
      "sex\\.f|sex\\.m|female|male",
      names(dat),
      ignore.case = TRUE
    )
]

names(dat)[
  grepl(
    "^intra\\.hosp\\.mort",
    names(dat),
    ignore.case = TRUE
  ) &
    grepl(
      "sex\\.f|sex\\.m",
      names(dat),
      ignore.case = TRUE
    )
]


# ============================================================
# 8.3 Check variables for age-specific mortality analysis
# ============================================================

# Gedankenstütze:
# Für eine altersstratifizierte Analyse müssen Fallzahlen und
# intrahospitale Todesfälle für dieselbe CVD-Definition und
# dieselbe Altersgruppe vorhanden sein.

# CVD-Hospitalisierungen nach Alter
names(dat)[
  grepl(
    "^cases_main_I_age",
    names(dat)
  )
]

# CVD-Mortalitätsvariablen nach Alter
names(dat)[
  grepl(
    "^intra\\.hosp\\.mort.*age",
    names(dat),
    ignore.case = TRUE
  )
]


# ============================================================
# Check district variables
# ============================================================

# Gedankenstütze:
# Prüfen, welche Variable tatsächlich die Bezirkskennzahl
# und welche den Bezirksnamen enthält.

dat %>%
  select(
    location_county_home,
    location_home_GID_2,
    BKZ,
    BKZ_name
  ) %>%
  distinct() %>%
  arrange(BKZ) %>%
  head(30)



# ============================================================
# 8.3 Check matching variables for acute age analysis
# ============================================================

# Gedankenstütze:
# Für eine altersstratifizierte Analyse müssen Hospitalisierungen
# und intrahospitale Todesfälle für dieselbe Altersgruppe vorliegen.
#
# Im neuen Datensatz beginnen die neuen Mortalitätsvariablen mit
# intra.hosp.mort_.
#
# Deshalb werden zunächst die verfügbaren Variablen für akute
# Aufnahmen nach Alter geprüft.

# Akute CVD-Hospitalisierungen nach Alter
names(dat)[
  grepl(
    "^cases_main_a_I_age",
    names(dat),
    ignore.case = TRUE
  )
]

# Intrahospitale Todesfälle bei akuten Aufnahmen nach Alter
names(dat)[
  grepl(
    "^intra\\.hosp\\.mort_a.*age",
    names(dat),
    ignore.case = TRUE
  )
]

###########
###########
# ============================================================
# 8.2 Analyse nach Geschlecht
# ============================================================

# Gedankenstütze:
# Gleiche Modellstruktur wie im finalen Hauptmodell,
# aber getrennt für Männer und Frauen.
# Zusätzlich wird die Kalenderwoche mit ns(date_weekofyear, 6)
# berücksichtigt, um saisonale Trends innerhalb der Warm Season
# zu kontrollieren.
#
# Wie im Hauptmodell und den Alters-/Diagnosemodellen werden nur
# vollständige Bezirks-Wochen (n_days == 7) berücksichtigt,
# um die Analysepopulation über alle Subgruppen konsistent zu halten.


# ------------------------------------------------------------
# Männer
# ------------------------------------------------------------

dat_rq1_week_M <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases_M = sum(
      cases_main_I_sexM,
      na.rm = TRUE
    ),
    deaths_M = sum(
      intra.hosp.mort_all.I_sex.m,
      na.rm = TRUE
    ),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases_M > 0
  )


model_M_quasi <- glm(
  deaths_M ~
    week_type +
    factor(BKZ_name) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases_M),
  family = quasipoisson(),
  data = dat_rq1_week_M
)


dispersion_model_M <-
  sum(
    residuals(
      model_M_quasi,
      type = "pearson"
    )^2
  ) /
  model_M_quasi$df.residual



# ------------------------------------------------------------
# Frauen
# ------------------------------------------------------------

dat_rq1_week_W <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases_W = sum(
      cases_main_I_sexF,
      na.rm = TRUE
    ),
    deaths_W = sum(
      intra.hosp.mort_all.I_sex.f,
      na.rm = TRUE
    ),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases_W > 0
  )


model_W_quasi <- glm(
  deaths_W ~
    week_type +
    factor(BKZ_name) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases_W),
  family = quasipoisson(),
  data = dat_rq1_week_W
)


dispersion_model_W <-
  sum(
    residuals(
      model_W_quasi,
      type = "pearson"
    )^2
  ) /
  model_W_quasi$df.residual



# ------------------------------------------------------------
# Ergebnisse Männer und Frauen zusammenfassen
# ------------------------------------------------------------

sex_results <- tibble(
  
  group = c(
    "Men",
    "Women"
  ),
  
  IRR = c(
    exp(
      coef(model_M_quasi)[
        "week_typeHeatwave week"
      ]
    ),
    exp(
      coef(model_W_quasi)[
        "week_typeHeatwave week"
      ]
    )
  ),
  
  CI_low = c(
    exp(
      confint.default(model_M_quasi)[
        "week_typeHeatwave week",
        "2.5 %"
      ]
    ),
    exp(
      confint.default(model_W_quasi)[
        "week_typeHeatwave week",
        "2.5 %"
      ]
    )
  ),
  
  CI_high = c(
    exp(
      confint.default(model_M_quasi)[
        "week_typeHeatwave week",
        "97.5 %"
      ]
    ),
    exp(
      confint.default(model_W_quasi)[
        "week_typeHeatwave week",
        "97.5 %"
      ]
    )
  ),
  
  QAIC = c(
    fqaic(model_M_quasi),
    fqaic(model_W_quasi)
  ),
  
  dispersion = c(
    dispersion_model_M,
    dispersion_model_W
  )
)


sex_results



######SEX RESULTS ##########

sex_results_export <- sex_results %>%
  transmute(
    Sex = group,
    IRR = round(IRR, 2),
    `95% CI` = paste0(
      round(CI_low, 2),
      "–",
      round(CI_high, 2)
    ),
    Dispersion = round(dispersion, 2)
  )

sex_results_export


write_xlsx(
  list(
    "Sex-specific analysis" = sex_results_export
  ),
  path = "RQ1_sex_specific_results.xlsx"
)







# ============================================================
# 8.3 Analyse nach Altersgruppen 2-Level
# 0–64 Jahre vs. 65+ Jahre
# ============================================================

# Gedankenstütze:
# Die Hospitalisierungs- und Mortalitätsvariablen verwenden
# unterschiedliche interne Alterskategorien.
#
# Für 0–64 Jahre werden daher die jeweiligen Teilgruppen
# innerhalb der Fall- und Mortalitätsvariablen summiert.
#
# Für 65+ Jahre werden die Hospitalisierungen aus
# 65–74, 75–84 und 85+ zusammengesetzt.
# Die Mortalitätsvariable liegt direkt als 65+ vor.


# ------------------------------------------------------------
# 0–64 Jahre
# ------------------------------------------------------------

dat_rq1_week_age_00_64 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases_age_00_64 =
      sum(cases_main_I_age0.19, na.rm = TRUE) +
      sum(cases_main_I_age20.49, na.rm = TRUE) +
      sum(cases_main_I_age50.64, na.rm = TRUE),
    
    deaths_age_00_64 =
      sum(intra.hosp.mort_age_00.24, na.rm = TRUE) +
      sum(intra.hosp.mort_age_25.64, na.rm = TRUE),
    
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases_age_00_64 > 0
  )


model_age_00_64_quasi <- glm(
  deaths_age_00_64 ~
    week_type +
    factor(BKZ_name) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases_age_00_64),
  family = quasipoisson(),
  data = dat_rq1_week_age_00_64
)


dispersion_model_age_00_64 <-
  sum(
    residuals(
      model_age_00_64_quasi,
      type = "pearson"
    )^2
  ) /
  model_age_00_64_quasi$df.residual



# ------------------------------------------------------------
# 65+ Jahre
# ------------------------------------------------------------

dat_rq1_week_age_65_plus <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases_age_65_plus =
      sum(cases_main_I_age65.74, na.rm = TRUE) +
      sum(cases_main_I_age75.84, na.rm = TRUE) +
      sum(cases_main_I_age85.inf, na.rm = TRUE),
    
    deaths_age_65_plus =
      sum(intra.hosp.mort_age_65.max, na.rm = TRUE),
    
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases_age_65_plus > 0
  )


model_age_65_plus_quasi <- glm(
  deaths_age_65_plus ~
    week_type +
    factor(BKZ_name) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases_age_65_plus),
  family = quasipoisson(),
  data = dat_rq1_week_age_65_plus
)


dispersion_model_age_65_plus <-
  sum(
    residuals(
      model_age_65_plus_quasi,
      type = "pearson"
    )^2
  ) /
  model_age_65_plus_quasi$df.residual



# ------------------------------------------------------------
# Ergebnisse zusammenfassen
# ------------------------------------------------------------

age_results <- tibble(
  group = c(
    "0-64 years",
    "65+ years"
  ),
  
  IRR = c(
    exp(
      coef(model_age_00_64_quasi)[
        "week_typeHeatwave week"
      ]
    ),
    exp(
      coef(model_age_65_plus_quasi)[
        "week_typeHeatwave week"
      ]
    )
  ),
  
  CI_low = c(
    exp(
      confint.default(model_age_00_64_quasi)[
        "week_typeHeatwave week",
        "2.5 %"
      ]
    ),
    exp(
      confint.default(model_age_65_plus_quasi)[
        "week_typeHeatwave week",
        "2.5 %"
      ]
    )
  ),
  
  CI_high = c(
    exp(
      confint.default(model_age_00_64_quasi)[
        "week_typeHeatwave week",
        "97.5 %"
      ]
    ),
    exp(
      confint.default(model_age_65_plus_quasi)[
        "week_typeHeatwave week",
        "97.5 %"
      ]
    )
  ),
  
  QAIC = c(
    fqaic(model_age_00_64_quasi),
    fqaic(model_age_65_plus_quasi)
  ),
  
  dispersion = c(
    dispersion_model_age_00_64,
    dispersion_model_age_65_plus
  )
)

age_results
###################################
#######ERGEBNISSE 2 LEVEL AGE#######

###############################


age_results_export <- age_results %>%
  transmute(
    `Age group` = group,
    IRR = round(IRR, 2),
    `95% CI` = paste0(
      round(CI_low, 2),
      "–",
      round(CI_high, 2)
    ),
    Dispersion = round(dispersion, 2)
  )

age_results_export

write_xlsx(
  list(
    "Age-specific analysis" = age_results_export
  ),
  path = "RQ1_age_specific_results.xlsx"
)


# ============================================================
# 8.4 Deskriptive Übersicht der Altersgruppen
# 2 Level: 0–64 Jahre und 65+ Jahre
# ============================================================

# Gedankenstütze:
# Eine feinere Unterteilung in drei Altersgruppen ist mit dem
# neuen aggregierten Datensatz nicht konsistent möglich,
# da Hospitalisierungen und intrahospitale Todesfälle
# unterschiedliche Altersgrenzen verwenden.
#
# Deshalb wird auch die deskriptive Auswertung entsprechend
# der analytischen 2-Level-Stratifizierung durchgeführt:
#
# - 0–64 Jahre
# - 65+ Jahre
#
# Betrachtet werden:
# - absolute Fallzahlen
# - absolute Todesfälle
# - rohe intrahospitale Mortalitätsrate


age_descriptive <- tibble(
  
  group = c(
    "0-64 years",
    "65+ years"
  ),
  
  total_cases = c(
    sum(
      dat_rq1_week_age_00_64$cases_age_00_64,
      na.rm = TRUE
    ),
    sum(
      dat_rq1_week_age_65_plus$cases_age_65_plus,
      na.rm = TRUE
    )
  ),
  
  total_deaths = c(
    sum(
      dat_rq1_week_age_00_64$deaths_age_00_64,
      na.rm = TRUE
    ),
    sum(
      dat_rq1_week_age_65_plus$deaths_age_65_plus,
      na.rm = TRUE
    )
  )
)


# ------------------------------------------------------------
# Rohe Mortalitätsrate berechnen
# ------------------------------------------------------------

age_descriptive <- age_descriptive %>%
  mutate(
    crude_mortality_rate =
      total_deaths / total_cases,
    
    crude_mortality_percent =
      round(
        crude_mortality_rate * 100,
        2
      )
  )


age_descriptive


##########AGE DESKRIPTIVE ERGEBNISSE#######
###########################################


age_descriptive_export <- age_descriptive %>%
  transmute(
    `Age group` = group,
    `Total cases` = total_cases,
    `Total deaths` = total_deaths,
    `Crude mortality (%)` = crude_mortality_percent
  )

age_descriptive_export

write_xlsx(
  list(
    "Age descriptive analysis" = age_descriptive_export
  ),
  path = "RQ1_age_descriptive_results.xlsx"
)

#################
#DIAGNOSE########

##########

# ============================================================
# 8.5.2 Deskriptive Verteilung der Todesfälle
# nach kardiovaskulären Diagnosegruppen
# ============================================================

# Gesamtzahl der intrahospitalen CVD-Todesfälle
# innerhalb des RQ1-Datensatzes

total_deaths_all <- sum(
  dat_rq1$intra.hosp.mort_all.I,
  na.rm = TRUE
)


# Todesfälle nach Diagnosegruppe

icd_deaths <- tibble(
  
  diagnosis_group = c(
    "I10-I15 Hypertensive diseases",
    "I20-I25 Ischemic heart diseases",
    "I26-I28 Pulmonary heart diseases",
    "I30-I52 Other heart diseases",
    "I60-I69 Cerebrovascular diseases",
    "I70-I79 Arterial diseases",
    "I80-I89 Venous diseases"
  ),
  
  total_deaths = c(
    sum(dat_rq1$intra.hosp.mort_I10.I15, na.rm = TRUE),
    sum(dat_rq1$intra.hosp.mort_I20.I25, na.rm = TRUE),
    sum(dat_rq1$intra.hosp.mort_I26.I28, na.rm = TRUE),
    sum(dat_rq1$intra.hosp.mort_I30.I52, na.rm = TRUE),
    sum(dat_rq1$intra.hosp.mort_I60.I69, na.rm = TRUE),
    sum(dat_rq1$intra.hosp.mort_I70.I79, na.rm = TRUE),
    sum(dat_rq1$intra.hosp.mort_I80.I89, na.rm = TRUE)
  )
) %>%
  mutate(
    percent_of_all_deaths =
      round(
        total_deaths / total_deaths_all * 100,
        2
      )
  ) %>%
  arrange(
    desc(total_deaths)
  )


icd_deaths



# ============================================================
# 8.6 Subgruppenanalyse nach Diagnosegruppen
# Quasi-Poisson-Modelle
# ============================================================

# Gedankenstütze:
# Gleiche Modellstruktur wie im finalen Hauptmodell:
#
# deaths ~ week_type +
#          factor(BKZ) +
#          factor(date_year) +
#          ns(date_weekofyear, 6)
#
# Offset = log(cases)


# ============================================================
# 8.6.1 I30-I52 Other heart diseases
# ============================================================

dat_rq1_I30_52 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(
      cases_main_I30.52,
      na.rm = TRUE
    ),
    
    deaths = sum(
      intra.hosp.mort_I30.I52,
      na.rm = TRUE
    ),
    
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases > 0
  )


model_I30_52_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_I30_52
)


dispersion_I30_52 <-
  sum(
    residuals(
      model_I30_52_quasi,
      type = "pearson"
    )^2
  ) /
  model_I30_52_quasi$df.residual


# ============================================================
# 8.6.2 I60-I69 Cerebrovascular diseases
# ============================================================

dat_rq1_I60_69 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(
      cases_main_I60.69,
      na.rm = TRUE
    ),
    
    deaths = sum(
      intra.hosp.mort_I60.I69,
      na.rm = TRUE
    ),
    
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases > 0
  )


model_I60_69_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_I60_69
)


dispersion_I60_69 <-
  sum(
    residuals(
      model_I60_69_quasi,
      type = "pearson"
    )^2
  ) /
  model_I60_69_quasi$df.residual



# ============================================================
# 8.6.3 I20-I25 Ischemic heart diseases
# ============================================================

dat_rq1_I20_25 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(
      cases_main_I20.25,
      na.rm = TRUE
    ),
    
    deaths = sum(
      intra.hosp.mort_I20.I25,
      na.rm = TRUE
    ),
    
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases > 0
  )


model_I20_25_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_I20_25
)


dispersion_I20_25 <-
  sum(
    residuals(
      model_I20_25_quasi,
      type = "pearson"
    )^2
  ) /
  model_I20_25_quasi$df.residual









# ============================================================
# 8.6.4 Ergebnisse der Diagnosegruppen zusammenfassen
# ============================================================

diagnosis_results <- tibble(
  
  diagnosis_group = c(
    "I30-I52 Other heart diseases",
    "I60-I69 Cerebrovascular diseases",
    "I20-I25 Ischemic heart diseases"
  ),
  
  IRR = c(
    exp(
      coef(model_I30_52_quasi)[
        "week_typeHeatwave week"
      ]
    ),
    exp(
      coef(model_I60_69_quasi)[
        "week_typeHeatwave week"
      ]
    ),
    exp(
      coef(model_I20_25_quasi)[
        "week_typeHeatwave week"
      ]
    )
  ),
  
  CI_low = c(
    exp(
      confint.default(model_I30_52_quasi)[
        "week_typeHeatwave week",
        "2.5 %"
      ]
    ),
    exp(
      confint.default(model_I60_69_quasi)[
        "week_typeHeatwave week",
        "2.5 %"
      ]
    ),
    exp(
      confint.default(model_I20_25_quasi)[
        "week_typeHeatwave week",
        "2.5 %"
      ]
    )
  ),
  
  CI_high = c(
    exp(
      confint.default(model_I30_52_quasi)[
        "week_typeHeatwave week",
        "97.5 %"
      ]
    ),
    exp(
      confint.default(model_I60_69_quasi)[
        "week_typeHeatwave week",
        "97.5 %"
      ]
    ),
    exp(
      confint.default(model_I20_25_quasi)[
        "week_typeHeatwave week",
        "97.5 %"
      ]
    )
  ),
  
  dispersion = c(
    dispersion_I30_52,
    dispersion_I60_69,
    dispersion_I20_25
  )
)


diagnosis_results



diagnosis_results_export <- diagnosis_results %>%
  transmute(
    `Diagnosis group` = diagnosis_group,
    IRR = round(IRR, 2),
    `95% CI` = paste0(
      round(CI_low, 2),
      "–",
      round(CI_high, 2)
    ),
    Dispersion = round(dispersion, 2),
    Significant = ifelse(
      CI_low > 1 | CI_high < 1,
      "Yes",
      "No"
    )
  )

diagnosis_results_export

write_xlsx(
  list(
    "Diagnosis-specific analysis" = diagnosis_results_export
  ),
  path = "RQ1_diagnosis_specific_results.xlsx"
)


# ============================================================
# 8.7 Explorative Analyse weiterer Diagnosegruppen
# ============================================================

# Gedankenstütze:
# Die folgenden Diagnosegruppen tragen nur einen kleineren Anteil
# zur gesamten intrahospitalen CVD-Mortalität bei.
#
# Die Analysen werden daher explorativ durchgeführt.
# Die Modellstruktur entspricht weiterhin dem finalen Hauptmodell:
#
# deaths ~ week_type +
#          factor(BKZ) +
#          factor(date_year) +
#          ns(date_weekofyear, 6)
#
# Offset = log(cases)


# ============================================================
# 8.7.1 I70-I79 Arterial diseases
# ============================================================

dat_rq1_I70_79 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(cases_main_I70.79, na.rm = TRUE),
    deaths = sum(intra.hosp.mort_I70.I79, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases > 0
  )

model_I70_79_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_I70_79
)

dispersion_I70_79 <-
  sum(
    residuals(
      model_I70_79_quasi,
      type = "pearson"
    )^2
  ) /
  model_I70_79_quasi$df.residual


# ============================================================
# 8.7.2 I26-I28 Pulmonary heart diseases
# ============================================================

dat_rq1_I26_28 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(cases_main_I26.28, na.rm = TRUE),
    deaths = sum(intra.hosp.mort_I26.I28, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases > 0
  )

model_I26_28_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_I26_28
)

dispersion_I26_28 <-
  sum(
    residuals(
      model_I26_28_quasi,
      type = "pearson"
    )^2
  ) /
  model_I26_28_quasi$df.residual


# ============================================================
# 8.7.3 I10-I15 Hypertensive diseases
# ============================================================

dat_rq1_I10_15 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(cases_main_I10.15, na.rm = TRUE),
    deaths = sum(intra.hosp.mort_I10.I15, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases > 0
  )

model_I10_15_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_I10_15
)

dispersion_I10_15 <-
  sum(
    residuals(
      model_I10_15_quasi,
      type = "pearson"
    )^2
  ) /
  model_I10_15_quasi$df.residual


# ============================================================
# 8.7.4 I80-I89 Venous diseases
# ============================================================

dat_rq1_I80_89 <- dat_rq1 %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases = sum(cases_main_I80.89, na.rm = TRUE),
    deaths = sum(intra.hosp.mort_I80.I89, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases > 0
  )

model_I80_89_quasi <- glm(
  deaths ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases),
  family = quasipoisson(),
  data = dat_rq1_I80_89
)

dispersion_I80_89 <-
  sum(
    residuals(
      model_I80_89_quasi,
      type = "pearson"
    )^2
  ) /
  model_I80_89_quasi$df.residual


# ============================================================
# 8.7.5 Explorative Ergebnisse zusammenfassen
# ============================================================

diagnosis_results_exploratory <- tibble(
  
  diagnosis_group = c(
    "I70-I79 Arterial diseases",
    "I26-I28 Pulmonary heart diseases",
    "I10-I15 Hypertensive diseases",
    "I80-I89 Venous diseases"
  ),
  
  IRR = c(
    exp(coef(model_I70_79_quasi)["week_typeHeatwave week"]),
    exp(coef(model_I26_28_quasi)["week_typeHeatwave week"]),
    exp(coef(model_I10_15_quasi)["week_typeHeatwave week"]),
    exp(coef(model_I80_89_quasi)["week_typeHeatwave week"])
  ),
  
  CI_low = c(
    exp(confint.default(model_I70_79_quasi)["week_typeHeatwave week", "2.5 %"]),
    exp(confint.default(model_I26_28_quasi)["week_typeHeatwave week", "2.5 %"]),
    exp(confint.default(model_I10_15_quasi)["week_typeHeatwave week", "2.5 %"]),
    exp(confint.default(model_I80_89_quasi)["week_typeHeatwave week", "2.5 %"])
  ),
  
  CI_high = c(
    exp(confint.default(model_I70_79_quasi)["week_typeHeatwave week", "97.5 %"]),
    exp(confint.default(model_I26_28_quasi)["week_typeHeatwave week", "97.5 %"]),
    exp(confint.default(model_I10_15_quasi)["week_typeHeatwave week", "97.5 %"]),
    exp(confint.default(model_I80_89_quasi)["week_typeHeatwave week", "97.5 %"])
  ),
  
  dispersion = c(
    dispersion_I70_79,
    dispersion_I26_28,
    dispersion_I10_15,
    dispersion_I80_89
  )
) %>%
  mutate(
    significant = ifelse(
      CI_low > 1 | CI_high < 1,
      "Yes",
      "No"
    )
  )

diagnosis_results_exploratory



# ============================================================
# Diagnosegruppen – alle Ergebnisse für Excel zusammenführen
# ============================================================


diagnosis_results_all <- bind_rows(
  
  diagnosis_results %>%
    mutate(
      analysis_type = "Main diagnosis subgroup analysis"
    ),
  
  diagnosis_results_exploratory %>%
    mutate(
      analysis_type = "Exploratory diagnosis subgroup analysis"
    )
)


# Berichtsfertige Tabelle erstellen
diagnosis_results_export <- diagnosis_results_all %>%
  transmute(
    `Diagnosis group` = diagnosis_group,
    `Analysis type` = analysis_type,
    IRR = round(IRR, 2),
    `95% CI` = paste0(
      round(CI_low, 2),
      "–",
      round(CI_high, 2)
    ),
    Dispersion = round(dispersion, 2),
    Significant = ifelse(
      CI_low > 1 | CI_high < 1,
      "Yes",
      "No"
    )
  )


diagnosis_results_export

write_xlsx(
  list(
    "Diagnosis-specific analysis" = diagnosis_results_export
  ),
  path = "RQ1_diagnosis_specific_all_results.xlsx"
)

###########



##########




###########




############################################################
# 8.8 ADMISSION TYPE (ACUTE VS. PLANNED) - PREPARATION
############################################################

############################################################
# 8.8 ADMISSION TYPE (ACUTE VS. PLANNED) - PREPARATION
############################################################

# Gedankenstütze:
# Für die Mortalität nach Aufnahmeart existiert keine direkte
# "Gesamt-I"-Variable (anders als bei den Fallzahlen, wo
# cases_main_a_I und cases_main_p_I bereits vorliegen).
# Auf der Mortalitätsseite gibt es nur diagnosegruppenspezifische
# Varianten (intra.hosp.mort_a_I10.I15 usw.).
#
# Die Gesamt-CVD-Mortalität nach Aufnahmeart wird daher, analog
# zur Prüfung von cases_main_I in Abschnitt 0.5, durch Summation
# der sieben ICD-10-Gruppen gebildet.

admission_mort_vars_a <- c(
  "intra.hosp.mort_a_I10.I15",
  "intra.hosp.mort_a_I20.I25",
  "intra.hosp.mort_a_I26.I28",
  "intra.hosp.mort_a_I30.I52",
  "intra.hosp.mort_a_I60.I69",
  "intra.hosp.mort_a_I70.I79",
  "intra.hosp.mort_a_I80.I89"
)

admission_mort_vars_p <- c(
  "intra.hosp.mort_p_I10.I15",
  "intra.hosp.mort_p_I20.I25",
  "intra.hosp.mort_p_I26.I28",
  "intra.hosp.mort_p_I30.I52",
  "intra.hosp.mort_p_I60.I69",
  "intra.hosp.mort_p_I70.I79",
  "intra.hosp.mort_p_I80.I89"
)

# Gesamt-CVD-Mortalität nach Aufnahmeart auf dat_rq1 anwenden

dat_rq1 <- dat_rq1 %>%
  mutate(
    deaths_a_I_total = rowSums(
      across(all_of(admission_mort_vars_a)),
      na.rm = TRUE
    ),
    deaths_p_I_total = rowSums(
      across(all_of(admission_mort_vars_p)),
      na.rm = TRUE
    )
  )


# ============================================================
# 8.8.1 Restriction to 2007-2021
# ============================================================

# Gedankenstütze:
# Die Erfassung der Aufnahmeart (akut/geplant) ist erst ab 2007
# vollständig. Für die Jahre 2002-2006 liegt sie praktisch
# vollständig als "unbekannt" vor (98.5-99.9% der Todesfälle,
# siehe Datenprüfung nach Jahr). Diese Subgruppenanalyse wird
# daher auf den Zeitraum 2007-2021 beschränkt.
#
# Das Hauptmodell (RQ1) sowie die Subgruppenanalysen für
# Geschlecht, Alter und Diagnose bleiben hiervon unberührt und
# nutzen weiterhin den vollständigen Zeitraum 2002-2021.

dat_rq1_admission <- dat_rq1 %>%
  filter(date_year >= 2007)


# ============================================================
# 8.8.2 Documentation: coverage after restriction
# ============================================================

# Gedankenstütze:
# Dokumentiert, dass nach der Einschränkung auf 2007-2021 die
# Klassifikation nach Aufnahmeart vollständig ist (100%).
# Dient als Beleg für den Methodentext.

rq1_admission_coverage <- dat_rq1_admission %>%
  summarise(
    total_deaths_all = sum(deaths, na.rm = TRUE),
    total_deaths_acute = sum(deaths_a_I_total, na.rm = TRUE),
    total_deaths_planned = sum(deaths_p_I_total, na.rm = TRUE),
    sum_acute_planned = total_deaths_acute + total_deaths_planned,
    percent_covered = round(
      100 * sum_acute_planned / total_deaths_all,
      2
    )
  )

rq1_admission_coverage





############################################################
# 8.9 ADMISSION TYPE - MAIN ANALYSIS
############################################################

# ------------------------------------------------------------
# Akute Aufnahmen
# ------------------------------------------------------------

dat_rq1_week_acute <- dat_rq1_admission %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases_acute = sum(
      cases_main_a_I,
      na.rm = TRUE
    ),
    deaths_acute = sum(
      deaths_a_I_total,
      na.rm = TRUE
    ),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases_acute > 0
  )


model_acute_quasi <- glm(
  deaths_acute ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases_acute),
  family = quasipoisson(),
  data = dat_rq1_week_acute
)


dispersion_model_acute <-
  sum(
    residuals(
      model_acute_quasi,
      type = "pearson"
    )^2
  ) /
  model_acute_quasi$df.residual



# ------------------------------------------------------------
# Geplante Aufnahmen
# ------------------------------------------------------------

dat_rq1_week_planned <- dat_rq1_admission %>%
  group_by(
    BKZ,
    BKZ_name,
    date_year,
    date_weekofyear,
    week_type
  ) %>%
  summarise(
    cases_planned = sum(
      cases_main_p_I,
      na.rm = TRUE
    ),
    deaths_planned = sum(
      deaths_p_I_total,
      na.rm = TRUE
    ),
    n_days = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_days == 7,
    cases_planned > 0
  )


model_planned_quasi <- glm(
  deaths_planned ~
    week_type +
    factor(BKZ) +
    factor(date_year) +
    ns(date_weekofyear, 6),
  offset = log(cases_planned),
  family = quasipoisson(),
  data = dat_rq1_week_planned
)


dispersion_model_planned <-
  sum(
    residuals(
      model_planned_quasi,
      type = "pearson"
    )^2
  ) /
  model_planned_quasi$df.residual



# ------------------------------------------------------------
# Ergebnisse zusammenfassen
# ------------------------------------------------------------

admission_results <- tibble(
  
  group = c(
    "Acute admissions",
    "Planned admissions"
  ),
  
  IRR = c(
    exp(coef(model_acute_quasi)["week_typeHeatwave week"]),
    exp(coef(model_planned_quasi)["week_typeHeatwave week"])
  ),
  
  CI_low = c(
    exp(confint.default(model_acute_quasi)["week_typeHeatwave week", "2.5 %"]),
    exp(confint.default(model_planned_quasi)["week_typeHeatwave week", "2.5 %"])
  ),
  
  CI_high = c(
    exp(confint.default(model_acute_quasi)["week_typeHeatwave week", "97.5 %"]),
    exp(confint.default(model_planned_quasi)["week_typeHeatwave week", "97.5 %"])
  ),
  
  QAIC = c(
    fqaic(model_acute_quasi),
    fqaic(model_planned_quasi)
  ),
  
  dispersion = c(
    dispersion_model_acute,
    dispersion_model_planned
  )
)

admission_results



######ADMISSION TYPE RESULTS ##########

admission_results_export <- admission_results %>%
  transmute(
    `Admission type` = group,
    IRR = round(IRR, 2),
    `95% CI` = paste0(round(CI_low, 2), "–", round(CI_high, 2)),
    Dispersion = round(dispersion, 2),
    Significant = ifelse(CI_low > 1 | CI_high < 1, "Yes", "No")
  )

admission_results_export


write_xlsx(
  list(
    "Admission type analysis" = admission_results_export
  ),
  path = "RQ1_admission_type_results.xlsx"
)


# ============================================================
# Ungerundete Werte für akute Aufnahmen prüfen
# ============================================================

# IRR ungerundet
exp(
  coef(model_acute_quasi)[
    "week_typeHeatwave week"
  ]
)

# 95%-CI ungerundet
exp(
  confint.default(model_acute_quasi)[
    "week_typeHeatwave week",
  ]
)

# Zur zusätzlichen Einordnung: p-Wert für den Heatwave-Koeffizienten
summary(model_acute_quasi)$coefficients[
  "week_typeHeatwave week",
]


############################################################
# 8.10 FOREST PLOT - SUBGROUP ANALYSES (Figure 2)
############################################################

# Gedankenstütze:
# Dieser Plot fasst alle vier Subgruppenanalysen (Geschlecht,
# Alter, Diagnosegruppen, Aufnahmeart) in einer Abbildung
# zusammen. Eine vertikale Referenzlinie bei IRR = 1 erlaubt
# den direkten Vergleich, welche Subgruppen einen signifikanten
# Effekt zeigen (CI überschneidet die Linie nicht).


# ============================================================
# 8.10.1 Alle Subgruppenergebnisse zu einem Datensatz zusammenführen
# ============================================================

subgroup_forest_data <- bind_rows(
  
  sex_results %>%
    transmute(
      subgroup_category = "Sex",
      label = group,
      IRR, CI_low, CI_high
    ),
  
  age_results %>%
    transmute(
      subgroup_category = "Age",
      label = group,
      IRR, CI_low, CI_high
    ),
  
  diagnosis_results_all %>%
    transmute(
      subgroup_category = "Diagnosis group",
      label = diagnosis_group,
      IRR, CI_low, CI_high
    ),
  
  admission_results %>%
    transmute(
      subgroup_category = "Admission type",
      label = group,
      IRR, CI_low, CI_high
    )
)


# ============================================================
# 8.10.2 Reihenfolge für die Darstellung festlegen
# ============================================================

# Gedankenstütze:
# Reihenfolge von oben nach unten: Geschlecht, Alter, Diagnose-
# gruppen (nach Häufigkeit/Relevanz), Aufnahmeart. Innerhalb der
# Diagnosegruppen werden Haupt- und explorative Analysen nicht
# separat markiert, da dies bereits in Tabelle 3 im Text erfolgt.

subgroup_forest_data <- subgroup_forest_data %>%
  mutate(
    label = factor(
      label,
      levels = rev(c(
        "Men", "Women",
        "0-64 years", "65+ years",
        "I30-I52 Other heart diseases",
        "I60-I69 Cerebrovascular diseases",
        "I20-I25 Ischemic heart diseases",
        "I70-I79 Arterial diseases",
        "I26-I28 Pulmonary heart diseases",
        "I10-I15 Hypertensive diseases",
        "I80-I89 Venous diseases",
        "Acute admissions", "Planned admissions"
      ))
    ),
    subgroup_category = factor(
      subgroup_category,
      levels = c("Sex", "Age", "Diagnosis group", "Admission type")
    )
  )








############################################################
# 8.10 FOREST PLOT - SUBGROUP ANALYSES (Figure 2)
############################################################

# ============================================================
# 8.10.1 Alle Subgruppenergebnisse zu einem Datensatz zusammenführen
# ============================================================

subgroup_forest_data <- bind_rows(
  
  sex_results %>%
    transmute(
      subgroup_category = "Sex",
      label = group,
      IRR, CI_low, CI_high
    ),
  
  age_results %>%
    transmute(
      subgroup_category = "Age",
      label = group,
      IRR, CI_low, CI_high
    ),
  
  diagnosis_results_all %>%
    transmute(
      subgroup_category = "Diagnosis group",
      label = diagnosis_group,
      IRR, CI_low, CI_high
    ),
  
  admission_results %>%
    transmute(
      subgroup_category = "Admission type",
      label = group,
      IRR, CI_low, CI_high
    )
)


# ============================================================
# 8.10.2 Reihenfolge und Analysetyp für die Darstellung festlegen
# ============================================================

subgroup_forest_data <- subgroup_forest_data %>%
  mutate(
    label = factor(
      label,
      levels = rev(c(
        "Men", "Women",
        "0-64 years", "65+ years",
        "I30-I52 Other heart diseases",
        "I60-I69 Cerebrovascular diseases",
        "I20-I25 Ischemic heart diseases",
        "I70-I79 Arterial diseases",
        "I26-I28 Pulmonary heart diseases",
        "I10-I15 Hypertensive diseases",
        "I80-I89 Venous diseases",
        "Acute admissions", "Planned admissions"
      ))
    ),
    subgroup_category = factor(
      subgroup_category,
      levels = c("Sex", "Age", "Diagnosis group", "Admission type")
    ),
    analysis_type = case_when(
      label %in% c(
        "I20-I25 Ischemic heart diseases",
        "I30-I52 Other heart diseases",
        "I60-I69 Cerebrovascular diseases"
      ) ~ "Main diagnosis analysis",
      subgroup_category == "Diagnosis group" ~ "Exploratory diagnosis analysis",
      TRUE ~ "Main diagnosis analysis"
    )
  )


# ============================================================
# 8.10.3 Forest Plot mit Haupt-/Explorativ-Kennzeichnung
# ============================================================

library(ggplot2)

figure2_forest_plot <- ggplot(
  subgroup_forest_data,
  aes(
    x = IRR,
    y = label,
    shape = analysis_type
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high),
    height = 0.2,
    color = "black"
  ) +
  geom_point(
    size = 2.8,
    color = "black",
    fill = "black"
  ) +
  scale_shape_manual(
    values = c(
      "Main diagnosis analysis" = 16,
      "Exploratory diagnosis analysis" = 1
    ),
    name = NULL
  ) +
  facet_grid(
    subgroup_category ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_x_continuous(
    name = "Incidence rate ratio (95% CI)"
  ) +
  labs(y = NULL, title = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    strip.text.y.left = element_text(
      angle = 0,
      face = "bold",
      margin = margin(r = 8)
    ),
    strip.placement = "outside",
    panel.spacing = unit(1.4, "lines"),
    legend.position = "bottom",
    legend.text = element_text(size = 9)
  )

figure2_forest_plot


# ============================================================
# 8.10.4 Als hochauflösende Datei exportieren
# ============================================================

ggsave(
  filename = "Figure2_ForestPlot_Subgroups.png",
  plot = figure2_forest_plot,
  width = 8,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "Figure2_ForestPlot_Subgroups.tiff",
  plot = figure2_forest_plot,
  width = 8,
  height = 7,
  dpi = 300,
  compression = "lzw"
)



#############################################
#############################################
#############################################
#Ergebnsiteil nochmals mit df = 3 laufen lassen 
#############################################
###########################################
##############################################

############################################################
# 9. SENSITIVITY ANALYSIS: SUBGROUPS WITH SEASONAL SPLINE df = 3
############################################################

# Gedankenstuetze:
# Kommentar Prof. Stratil: Subgruppenanalysen zusaetzlich mit
# 3 statt 6 Freiheitsgraden rechnen (Overfitting-Argument).
#
# Dieser Abschnitt veraendert NICHTS an Abschnitt 8.
# - Er verwendet die in Abschnitt 8 erstellten Wochendatensaetze
#   (dat_rq1_week_M, dat_rq1_I30_52, ...), diese werden nur gelesen.
# - Alle neuen Objekte enden auf _sens, alle Dateien enthalten "df3".
# - Modellstruktur identisch zu Abschnitt 8 (inkl. derselben
#   Bezirksvariable), nur die Spline-Freiheitsgrade aendern sich.
#
# VORAUSSETZUNG: Abschnitt 8 wurde in dieser R-Sitzung ausgefuehrt.

library(dplyr)
library(tidyr)
library(splines)
library(ggplot2)
library(writexl)


# ============================================================
# 9.0 Check: Sind alle Datensaetze aus Abschnitt 8 vorhanden?
# ============================================================

needed <- c("dat_rq1_week_M", "dat_rq1_week_W",
            "dat_rq1_week_age_00_64", "dat_rq1_week_age_65_plus",
            "dat_rq1_I30_52", "dat_rq1_I60_69", "dat_rq1_I20_25",
            "dat_rq1_I70_79", "dat_rq1_I26_28", "dat_rq1_I10_15",
            "dat_rq1_I80_89",
            "dat_rq1_week_acute", "dat_rq1_week_planned")

needed[!sapply(needed, exists)]   # muss character(0) ergeben


# ============================================================
# 9.1 Funktion: ein Subgruppenmodell rechnen
# ============================================================

fit_subgroup <- function(data, outcome, cases, district_var,
                         label, category, df_spline) {
  
  f <- as.formula(paste0(
    outcome, " ~ week_type + factor(", district_var, ") + ",
    "factor(date_year) + ns(date_weekofyear, ", df_spline, ") + ",
    "offset(log(", cases, "))"
  ))
  
  m  <- glm(f, family = quasipoisson(), data = data)
  b  <- "week_typeHeatwave week"
  ci <- exp(confint.default(m)[b, ])
  
  tibble(
    subgroup_category = category,
    label      = label,
    df_spline  = df_spline,
    RR         = exp(coef(m)[b]),
    CI_low     = ci[1],
    CI_high    = ci[2],
    dispersion = sum(residuals(m, type = "pearson")^2) / m$df.residual
  )
}


# ============================================================
# 9.2 Alle Subgruppen (wie in Abschnitt 8)
# ============================================================

subgroup_specs <- list(
  # Geschlecht und Alter: in Abschnitt 8 mit factor(BKZ_name)
  list(get("dat_rq1_week_M"),           "deaths_M",           "cases_M",           "BKZ_name", "Men",        "Sex"),
  list(get("dat_rq1_week_W"),           "deaths_W",           "cases_W",           "BKZ_name", "Women",      "Sex"),
  list(get("dat_rq1_week_age_00_64"),   "deaths_age_00_64",   "cases_age_00_64",   "BKZ_name", "0-64 years", "Age"),
  list(get("dat_rq1_week_age_65_plus"), "deaths_age_65_plus", "cases_age_65_plus", "BKZ_name", "65+ years",  "Age"),
  # Diagnosegruppen: in Abschnitt 8 mit factor(BKZ)
  list(get("dat_rq1_I30_52"), "deaths", "cases", "BKZ", "I30-I52 Other heart diseases",     "Diagnosis group"),
  list(get("dat_rq1_I60_69"), "deaths", "cases", "BKZ", "I60-I69 Cerebrovascular diseases", "Diagnosis group"),
  list(get("dat_rq1_I20_25"), "deaths", "cases", "BKZ", "I20-I25 Ischemic heart diseases",  "Diagnosis group"),
  list(get("dat_rq1_I70_79"), "deaths", "cases", "BKZ", "I70-I79 Arterial diseases",        "Diagnosis group"),
  list(get("dat_rq1_I26_28"), "deaths", "cases", "BKZ", "I26-I28 Pulmonary heart diseases", "Diagnosis group"),
  list(get("dat_rq1_I10_15"), "deaths", "cases", "BKZ", "I10-I15 Hypertensive diseases",    "Diagnosis group"),
  list(get("dat_rq1_I80_89"), "deaths", "cases", "BKZ", "I80-I89 Venous diseases",          "Diagnosis group"),
  # Aufnahmeart (2007-2021): in Abschnitt 8 mit factor(BKZ)
  list(get("dat_rq1_week_acute"),   "deaths_acute",   "cases_acute",   "BKZ", "Acute admissions",   "Admission type"),
  list(get("dat_rq1_week_planned"), "deaths_planned", "cases_planned", "BKZ", "Planned admissions", "Admission type")
)

run_all <- function(df_spline) {
  bind_rows(lapply(subgroup_specs, function(s)
    fit_subgroup(s[[1]], s[[2]], s[[3]], s[[4]], s[[5]], s[[6]], df_spline)))
}

# Dauert einige Minuten (26 Modelle)
subgroups_df6_sens <- run_all(6)   # Kontrolle: muss Abschnitt 8 reproduzieren
subgroups_df3_sens <- run_all(3)   # Sensitivitaetsanalyse

subgroups_df6_sens   # mit Figure 3 (Abschnitt 8) vergleichen
subgroups_df3_sens


# ============================================================
# 9.3 Vergleichstabelle df = 6 vs. df = 3
# ============================================================

fmt_rr <- function(rr, lo, hi) {
  paste0(formatC(rr, format = "f", digits = 2), " (",
         formatC(lo, format = "f", digits = 2), "–",
         formatC(hi, format = "f", digits = 2), ")")
}

subgroup_comparison_sens <- bind_rows(subgroups_df6_sens, subgroups_df3_sens) %>%
  mutate(
    RR_CI       = fmt_rr(RR, CI_low, CI_high),
    significant = CI_low > 1 | CI_high < 1
  ) %>%
  select(subgroup_category, label, df_spline, RR_CI, significant) %>%
  pivot_wider(
    names_from  = df_spline,
    values_from = c(RR_CI, significant),
    names_glue  = "{.value}_df{df_spline}"
  ) %>%
  mutate(
    conclusion_changed = if_else(significant_df6 != significant_df3, "Yes", "No")
  ) %>%
  select(subgroup_category, label,
         `RR (95% CI), df = 6` = RR_CI_df6,
         `RR (95% CI), df = 3` = RR_CI_df3,
         conclusion_changed)

View(subgroup_comparison_sens)


# ============================================================
# 9.4 Forest Plot: df = 6 und df = 3 nebeneinander
# ============================================================

label_order <- rev(c(
  "Men", "Women",
  "0-64 years", "65+ years",
  "I30-I52 Other heart diseases",
  "I60-I69 Cerebrovascular diseases",
  "I20-I25 Ischemic heart diseases",
  "I70-I79 Arterial diseases",
  "I26-I28 Pulmonary heart diseases",
  "I10-I15 Hypertensive diseases",
  "I80-I89 Venous diseases",
  "Acute admissions", "Planned admissions"
))

plot_sens <- bind_rows(subgroups_df6_sens, subgroups_df3_sens) %>%
  mutate(
    label = factor(label, levels = label_order),
    subgroup_category = factor(subgroup_category,
                               levels = c("Sex", "Age", "Diagnosis group", "Admission type")),
    df_label = factor(paste0("df = ", df_spline),
                      levels = c("df = 6", "df = 3"))
  )

figure_sens_df3 <- ggplot(plot_sens, aes(x = RR, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 height = 0.2, color = "black") +
  geom_point(size = 2.5, color = "black") +
  facet_grid(subgroup_category ~ df_label,
             scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(name = "Case fatality rate ratio (95% CI)") +
  labs(y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    strip.text.x      = element_text(face = "bold"),
    strip.placement   = "outside",
    panel.spacing     = unit(1.2, "lines")
  )

figure_sens_df3


# ============================================================
# 9.5 Export 
# ============================================================

write_xlsx(
  list(
    "Comparison df6 vs df3" = subgroup_comparison_sens,
    "Raw df6"               = subgroups_df6_sens,
    "Raw df3"               = subgroups_df3_sens
  ),
  path = "RQ1_subgroups_sensitivity_df3.xlsx"
)

ggsave("FigureS_Subgroups_df6_vs_df3.png", plot = figure_sens_df3,
       width = 10, height = 7, dpi = 300)

file.exists("RQ1_subgroups_sensitivity_df3.xlsx")
file.exists("FigureS_Subgroups_df6_vs_df3.png")


getwd()
file.exists("FigureS_Subgroups_df6_vs_df3.png")







# ============================================================
# 6.3.0.1 Geschlechtsverteilung 
# ============================================================

# Gedankenstütze:
# Diese Tabelle ergänzt Tabelle 1 um eine rein deskriptive
# Charakterisierung der hospitalisierten Population (wer wird
# hospitalisiert), getrennt nach Geschlecht und Altersgruppe.
# Es handelt sich NICHT um eine Vergleichsanalyse zwischen
# Heatwave- und Reference-Wochen (das folgt erst in 6.3.4),
# sondern um eine Beschreibung der Gesamtpopulation.


dat_rq1_filtered <- dat_rq1 %>%
  inner_join(
    dat_rq1_week %>%
      distinct(BKZ, date_year, date_weekofyear),
    by = c("BKZ", "date_year", "date_weekofyear")
  )

sex_descriptive <- dat_rq1_filtered %>%
  summarise(
    Men = sum(cases_main_I_sexM, na.rm = TRUE),
    Women = sum(cases_main_I_sexF, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Sex", values_to = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

sex_descriptive


age_descriptive_overall <- dat_rq1_filtered %>%
  summarise(
    `0-64 years` =
      sum(cases_main_I_age0.19, na.rm = TRUE) +
      sum(cases_main_I_age20.49, na.rm = TRUE) +
      sum(cases_main_I_age50.64, na.rm = TRUE),
    
    `65+ years` =
      sum(cases_main_I_age65.74, na.rm = TRUE) +
      sum(cases_main_I_age75.84, na.rm = TRUE) +
      sum(cases_main_I_age85.inf, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Age group", values_to = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

age_descriptive_overall


# Plausibilitätscheck:
sum(sex_descriptive$n)
sum(age_descriptive_overall$n)





# ============================================================
# Sicherstellen, dass dat_rq1_week die gefilterte Version ist
# (n_days == 7, cases > 0) - zur Kontrolle
# ============================================================

nrow(dat_rq1_week)                 # sollte 24,728 sein
sum(dat_rq1_week$cases)            # sollte 1,541,870 sein


# ============================================================
# 6.3.0.1 Geschlechts- und Altersverteilung (korrigiert)
# ============================================================

dat_rq1_filtered <- dat_rq1 %>%
  inner_join(
    dat_rq1_week %>%
      distinct(BKZ, date_year, date_weekofyear),
    by = c("BKZ", "date_year", "date_weekofyear")
  )

# Kontrolle: Fallzahl nach Join sollte jetzt 1,541,870 ergeben
sum(dat_rq1_filtered$cases, na.rm = TRUE)


sex_descriptive <- dat_rq1_filtered %>%
  summarise(
    Men = sum(cases_main_I_sexM, na.rm = TRUE),
    Women = sum(cases_main_I_sexF, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Sex", values_to = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

sex_descriptive


age_descriptive_overall <- dat_rq1_filtered %>%
  summarise(
    `0-64 years` =
      sum(cases_main_I_age0.19, na.rm = TRUE) +
      sum(cases_main_I_age20.49, na.rm = TRUE) +
      sum(cases_main_I_age50.64, na.rm = TRUE),
    
    `65+ years` =
      sum(cases_main_I_age65.74, na.rm = TRUE) +
      sum(cases_main_I_age75.84, na.rm = TRUE) +
      sum(cases_main_I_age85.inf, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Age group", values_to = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

age_descriptive_overall


sum(sex_descriptive$n)              # Ziel: 1,541,870
sum(age_descriptive_overall$n)      # Ziel: 1,541,870


demographic_export <- list(
  "Sex distribution" = sex_descriptive,
  "Age distribution" = age_descriptive_overall
)

write_xlsx(
  demographic_export,
  path = "RQ1_demographic_descriptive.xlsx"
)


######
#checks wegen BEzirken.

names(dat)                      # Variablennamen ansehen

# 1) Wie viele Bezirke?
n_distinct(dat$BKZ)

# 2) Enthält "Fälle gesamt" mehr als die 7 Diagnosegruppen?
#    -> Summe aller Fälle mit Summe der 7 Gruppen vergleichen





######Rechnung für den Methodikteil in der Arbeit###########

dat_rq1_week %>%
  group_by(week_type) %>%
  summarise(
    mean_cases  = mean(cases),
    sd_cases    = sd(cases),
    mean_deaths = mean(deaths),
    sd_deaths   = sd(deaths)
  )


dat_rq1_week %>%
  group_by(week_type) %>%
  summarise(
    median_cases  = median(cases),
    q1_cases      = quantile(cases, 0.25),
    q3_cases      = quantile(cases, 0.75),
    median_deaths = median(deaths),
    q1_deaths     = quantile(deaths, 0.25),
    q3_deaths     = quantile(deaths, 0.75)
  )


library(splines)

fit_df <- function(d) {
  f <- as.formula(paste0("deaths ~ week_type + factor(BKZ) + factor(date_year) + ",
                         "ns(date_weekofyear, df = ", d, ")"))
  m_q <- glm(f, offset = log(cases), family = quasipoisson(), data = dat_rq1_week)
  m_p <- glm(f, offset = log(cases), family = poisson(),      data = dat_rq1_week)
  c_hat <- summary(m_q)$dispersion
  k     <- length(coef(m_p)) + 1
  b     <- grep("week_type", names(coef(m_q)), value = TRUE)
  ci    <- exp(confint.default(m_q)[b, ])
  data.frame(df = d,
             QAIC = round(-2 * as.numeric(logLik(m_p)) / c_hat + 2 * k, 1),
             dispersion = round(c_hat, 3),
             RR = round(exp(coef(m_q)[b]), 3),
             CI_low = round(ci[1], 3), CI_high = round(ci[2], 3))
}

qaic_tab <- do.call(rbind, lapply(3:8, fit_df))
qaic_tab