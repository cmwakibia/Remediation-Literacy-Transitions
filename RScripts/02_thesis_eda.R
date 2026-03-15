## ===== MULTISTATE LITERACY ANALYSIS: EXPLORATORY DATA ANALYSIS =====
## Author: Cyrus Wakibia
## Purpose: Comprehensive EDA with volunteer characteristics and Excel export
## 
## Outputs:
## - Console summary (via sink)
## - Figures (PNG)
## - Excel workbook with all summary tables

library(tidyverse)
library(openxlsx)


# ==============================================================================
# SETUP AND DATA LOADING
# ==============================================================================

## ----- Define labels and colors -----
state_labels <- c("Beginner", "Letter", "Word", "Paragraph", "Story")
state_colors <- c("#d62728", "#ff7f0e", "#bcbd22", "#2ca02c", "#1f77b4")
names(state_colors) <- state_labels

transition_colors <- c(
  "Progression" = "#2ca02c",
  "Regression" = "#d62728", 
  "Stagnation" = "#7f7f7f"
)

## ----- Helper functions -----
format_pct <- function(x, digits = 1) {
  paste0(round(x * 100, digits), "%")
}

format_mean_sd <- function(x, digits = 2) {
  paste0(round(mean(x, na.rm = TRUE), digits), " (", 
         round(sd(x, na.rm = TRUE), digits), ")")
}

# Classify transition type
classify_transition <- function(from, to) {
  case_when(
    to > from ~ "Progression",
    to < from ~ "Regression",
    TRUE ~ "Stagnation"
  )
}

## ----- Load data -----
# camp <- readRDS(file.path(paths$processed, "camp_clean.rds"))
# transitions <- readRDS(file.path(paths$processed, "transitions.rds"))

# ==============================================================================
# INITIALIZE EXCEL WORKBOOK
# ==============================================================================

# Create workbook to store all summary tables
wb <- createWorkbook()

# Define styles for Excel formatting
header_style <- createStyle(
  textDecoration = "bold",
  border = "bottom",
  borderColour = "#000000",
  fgFill = "#DCE6F1"
)

pct_style <- createStyle(numFmt = "0.0%")
num_style <- createStyle(numFmt = "0.00")

# ==============================================================================
# SECTION 1: SAMPLE CHARACTERISTICS
# ==============================================================================

compute_sample_characteristics <- function(camp) {
  
  cat("\n===== SAMPLE CHARACTERISTICS =====\n")
  cat("\nSample size:", nrow(camp), "learners\n")
  cat("Villages:", n_distinct(camp$village), "\n")
  cat("Counties:", n_distinct(camp$county), "\n")
  
  ## ----- County distribution -----
  county_dist <- camp |>
    count(county, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nBy county:\n")
  print(county_dist)
  
  ## ----- Gender distribution -----
  gender_dist <- camp |>
    count(gender, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nGender:\n")
  print(gender_dist)
  
  ## ----- Age distribution -----
  age_summary <- camp |>
    summarise(
      n = n(),
      n_missing = sum(is.na(age_imputed)),
      mean = mean(age_imputed, na.rm = TRUE),
      sd = sd(age_imputed, na.rm = TRUE),
      median = median(age_imputed, na.rm = TRUE),
      min = min(age_imputed, na.rm = TRUE),
      max = max(age_imputed, na.rm = TRUE),
      q25 = quantile(age_imputed, 0.25, na.rm = TRUE),
      q75 = quantile(age_imputed, 0.75, na.rm = TRUE)
    )
  
  cat("\nAge distribution (imputed):\n")
  print(age_summary)
  
  ## ----- Grade distribution -----
  grade_dist <- camp |>
    count(grade, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nGrade distribution:\n")
  print(grade_dist)
  
  ## ----- Camp participation -----
  camp_participation <- camp |>
    count(total_camps, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nCamp participation:\n")
  print(camp_participation)
  
  ## ----- Combined summary table for export -----
  # Create a comprehensive table with all learner characteristics
  learner_summary <- tibble(
    Characteristic = c(
      "Total learners",
      "Villages",
      "Counties",
      "",
      "County",
      "  Bungoma",
      "  Kilifi", 
      "  Kitui",
      "",
      "Gender",
      "  Female",
      "  Male",
      "",
      "Age (years)",
      "  Mean (SD)",
      "  Median [IQR]",
      "  Range",
      "",
      "Grade",
      paste0("  Grade ", sort(unique(camp$grade))),
      "",
      "Camps attended",
      "  0 camps",
      "  1 camp",
      "  2 camps",
      "  3 camps"
    )
  )
  
  # This is a simplified version; the full table construction is below
  
  list(
    county = county_dist,
    gender = gender_dist,
    age = age_summary,
    grade = grade_dist,
    participation = camp_participation
  )
}

# ==============================================================================
# SECTION 2: VOLUNTEER CHARACTERISTICS
# ==============================================================================

compute_volunteer_characteristics <- function(camp) {
  
  cat("\n===== VOLUNTEER CHARACTERISTICS =====\n")
  
  # Get unique volunteers (one per village typically)
  # Note: volunteer data is at village level, so we deduplicate
  volunteers <- camp |>
    distinct(village, .keep_all = TRUE) |>
    dplyr::select(village, county, starts_with("vol_"))
  
  cat("\nTotal volunteers:", nrow(volunteers), "\n")
  
  ## ----- Volunteer gender -----
  vol_gender_dist <- volunteers |>
    dplyr::filter(!is.na(vol_gender)) |>
    count(vol_gender, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nVolunteer gender:\n")
  print(vol_gender_dist)
  
  ## ----- Volunteer age category -----
  vol_age_dist <- volunteers |>
    dplyr::filter(!is.na(vol_age_cat)) |>
    count(vol_age_cat, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nVolunteer age category:\n")
  print(vol_age_dist)
  
  ## ----- Volunteer education level -----
  vol_ed_dist <- volunteers |>
    dplyr::filter(!is.na(vol_edlevel)) |>
    count(vol_edlevel, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nVolunteer education level:\n")
  print(vol_ed_dist)
  
  ## ----- Volunteer characteristics by county -----
  vol_by_county <- volunteers |>
    group_by(county) |>
    summarise(
      n_volunteers = n(),
      pct_female = mean(vol_gender == "Female", na.rm = TRUE),
      pct_secondary = mean(vol_edlevel == "Secondary Level", na.rm = TRUE),
      pct_college = mean(vol_edlevel == "College Level", na.rm = TRUE),
      pct_university = mean(vol_edlevel == "University Level", na.rm = TRUE),
      pct_under25 = mean(vol_age_cat == "Less than 25 years", na.rm = TRUE),
      pct_26_30 = mean(vol_age_cat == "26 - 30 years", na.rm = TRUE),
      pct_31_35 = mean(vol_age_cat == "31 - 35 years", na.rm = TRUE),
      pct_over35 = mean(vol_age_cat == "Greater than 35 years", na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nVolunteer characteristics by county:\n")
  print(vol_by_county)
  
  ## ----- Missing data check -----
  vol_missing <- volunteers |>
    summarise(
      n_total = n(),
      missing_gender = sum(is.na(vol_gender)),
      missing_age = sum(is.na(vol_age_cat)),
      missing_education = sum(is.na(vol_edlevel))
    )
  
  cat("\nVolunteer data missingness:\n")
  print(vol_missing)
  
  list(
    gender = vol_gender_dist,
    age_cat = vol_age_dist,
    education = vol_ed_dist,
    by_county = vol_by_county,
    missing = vol_missing
  )
}

# ==============================================================================
# SECTION 3: BASELINE LITERACY DISTRIBUTION
# ==============================================================================

compute_baseline_literacy <- function(camp) {
  
  cat("\n===== BASELINE LITERACY =====\n")
  
  ## ----- Overall distribution -----
  baseline_dist <- camp |>
    count(baseline_level, name = "n") |>
    mutate(
      level = factor(baseline_level, levels = 1:5, labels = state_labels),
      pct = n / sum(n),
      pct_label = format_pct(pct)
    ) |>
    dplyr::select(level, n, pct, pct_label)
  
  cat("\nBaseline literacy distribution:\n")
  print(baseline_dist)
  
  ## ----- By county -----
  baseline_by_county <- camp |>
    count(county, baseline_level, name = "n") |>
    group_by(county) |>
    mutate(
      pct = n / sum(n),
      level = factor(baseline_level, levels = 1:5, labels = state_labels)
    ) |>
    ungroup() |>
    dplyr::select(county, level, n, pct)
  
  # Wide format for easier reading
  baseline_county_wide <- baseline_by_county |>
    dplyr::select(county, level, pct) |>
    pivot_wider(names_from = level, values_from = pct, values_fill = 0)
  
  cat("\nBaseline literacy by county (proportions):\n")
  print(baseline_county_wide)
  
  ## ----- By gender -----
  baseline_by_gender <- camp |>
    count(gender, baseline_level, name = "n") |>
    group_by(gender) |>
    mutate(
      pct = n / sum(n),
      level = factor(baseline_level, levels = 1:5, labels = state_labels)
    ) |>
    ungroup()
  
  list(
    overall = baseline_dist,
    by_county = baseline_by_county,
    by_county_wide = baseline_county_wide,
    by_gender = baseline_by_gender
  )
}

# ==============================================================================
# SECTION 4: ATTENDANCE PATTERNS
# ==============================================================================

compute_attendance <- function(camp) {
  
  cat("\n===== ATTENDANCE PATTERNS =====\n")
  
  ## ----- Days per camp -----
  attendance_by_camp <- tibble(
    Camp = c("Camp 1", "Camp 2", "Camp 3"),
    n = c(
      sum(!is.na(camp$days_camp1)),
      sum(!is.na(camp$days_camp2)),
      sum(!is.na(camp$days_camp3))
    ),
    mean = c(
      mean(camp$days_camp1, na.rm = TRUE),
      mean(camp$days_camp2, na.rm = TRUE),
      mean(camp$days_camp3, na.rm = TRUE)
    ),
    sd = c(
      sd(camp$days_camp1, na.rm = TRUE),
      sd(camp$days_camp2, na.rm = TRUE),
      sd(camp$days_camp3, na.rm = TRUE)
    ),
    median = c(
      median(camp$days_camp1, na.rm = TRUE),
      median(camp$days_camp2, na.rm = TRUE),
      median(camp$days_camp3, na.rm = TRUE)
    ),
    min = c(
      min(camp$days_camp1, na.rm = TRUE),
      min(camp$days_camp2, na.rm = TRUE),
      min(camp$days_camp3, na.rm = TRUE)
    ),
    max = c(
      max(camp$days_camp1, na.rm = TRUE),
      max(camp$days_camp2, na.rm = TRUE),
      max(camp$days_camp3, na.rm = TRUE)
    )
  )
  
  cat("\nDays attended per camp:\n")
  print(attendance_by_camp)
  
  ## ----- Total days -----
  total_days_summary <- camp |>
    dplyr::filter(!is.na(total_days)) |>
    summarise(
      n = n(),
      mean = mean(total_days),
      sd = sd(total_days),
      median = median(total_days),
      min = min(total_days),
      max = max(total_days),
      q25 = quantile(total_days, 0.25),
      q75 = quantile(total_days, 0.75)
    )
  
  cat("\nTotal days attended:\n")
  print(total_days_summary)
  
  ## ----- Attendance rate -----
  attendance_rate_summary <- camp |>
    dplyr::filter(!is.na(attendance_rate)) |>
    summarise(
      n = n(),
      mean = mean(attendance_rate),
      sd = sd(attendance_rate),
      median = median(attendance_rate),
      min = min(attendance_rate),
      max = max(attendance_rate)
    )
  
  cat("\nAttendance rate:\n")
  print(attendance_rate_summary)
  
  ## ----- Attendance by county -----
  attendance_by_county <- camp |>
    dplyr::filter(!is.na(attendance_rate)) |>
    group_by(county) |>
    summarise(
      n = n(),
      mean_rate = mean(attendance_rate),
      sd_rate = sd(attendance_rate),
      mean_days = mean(total_days, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nAttendance by county:\n")
  print(attendance_by_county)
  
  list(
    by_camp = attendance_by_camp,
    total_days = total_days_summary,
    rate = attendance_rate_summary,
    by_county = attendance_by_county
  )
}

# ==============================================================================
# SECTION 5: TRANSITION PATTERNS
# ==============================================================================

compute_transition_patterns <- function(transitions) {
  
  cat("\n===== TRANSITION PATTERNS =====\n")
  
  ## ----- Overall transition types -----
  trans_type_overall <- transitions |>
    count(transition_type, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nOverall transition types:\n")
  print(trans_type_overall)
  
  ## ----- By interval -----
  trans_type_by_interval <- transitions |>
    count(interval, transition_type, name = "n") |>
    group_by(interval) |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct),
      interval_label = case_when(
        interval == 1 ~ "Baseline → Camp 1",
        interval == 2 ~ "Camp 1 → Camp 2",
        interval == 3 ~ "Camp 2 → Camp 3"
      )
    ) |>
    ungroup()
  
  cat("\nTransition types by interval:\n")
  print(trans_type_by_interval |> 
          dplyr::select(interval_label, transition_type, n, pct_label))
  
  ## ----- Transition matrix (counts) -----
  trans_matrix_counts <- transitions |>
    count(from_state, to_state, name = "n") |>
    pivot_wider(
      names_from = to_state, 
      values_from = n, 
      values_fill = 0,
      names_prefix = "to_"
    ) |>
    rename(from_state = from_state)
  
  cat("\nTransition matrix (counts):\n")
  print(trans_matrix_counts)
  
  ## ----- Transition matrix (probabilities) -----
  trans_matrix_probs <- transitions |>
    count(from_state, to_state, name = "n") |>
    group_by(from_state) |>
    mutate(prob = n / sum(n)) |>
    ungroup() |>
    dplyr::select(from_state, to_state, prob) |>
    pivot_wider(
      names_from = to_state,
      values_from = prob,
      values_fill = 0,
      names_prefix = "to_"
    )
  
  cat("\nTransition matrix (row probabilities):\n")
  print(trans_matrix_probs, digits = 3)
  
  ## ----- Transition matrix by interval -----
  trans_matrix_by_interval <- transitions |>
    count(interval, from_state, to_state, name = "n") |>
    group_by(interval, from_state) |>
    mutate(prob = n / sum(n)) |>
    ungroup()
  
  cat("\nTransition matrices by interval:\n")
  for (i in 1:3) {
    cat("\n--- Interval", i, "---\n")
    trans_matrix_by_interval |>
      dplyr::filter(interval == i) |>
      dplyr::select(from_state, to_state, n) |>
      pivot_wider(names_from = to_state, values_from = n, values_fill = 0) |>
      print()
  }
  
  list(
    type_overall = trans_type_overall,
    type_by_interval = trans_type_by_interval,
    matrix_counts = trans_matrix_counts,
    matrix_probs = trans_matrix_probs,
    matrix_by_interval = trans_matrix_by_interval
  )
}

# ==============================================================================
# SECTION 6: REGRESSION ANALYSIS
# ==============================================================================

compute_regression_patterns <- function(transitions) {
  
  cat("\n===== REGRESSION PATTERNS =====\n")
  
  regressions <- transitions |>
    dplyr::filter(transition_type == "Regression")
  
  total_regressions <- nrow(regressions)
  regression_rate <- total_regressions / nrow(transitions)
  
  cat("\nTotal regressions:", total_regressions, "\n")
  cat("Regression rate:", format_pct(regression_rate), "\n")
  
  ## ----- By origin-destination -----
  regression_by_trans <- regressions |>
    count(from_state, to_state, name = "n") |>
    mutate(
      from_label = state_labels[from_state],
      to_label = state_labels[to_state],
      transition = paste0(from_label, " → ", to_label),
      pct = n / sum(n),
      pct_label = format_pct(pct)
    ) |>
    arrange(desc(n))
  
  cat("\nRegression by origin-destination:\n")
  print(regression_by_trans |> 
          dplyr::select(transition, n, pct_label))
  
  ## ----- Para→Word concentration -----
  para_word <- sum(regressions$from_state == 4 & regressions$to_state == 3)
  para_word_pct <- para_word / total_regressions
  
  cat("\nPara→Word concentration:\n")
  cat("  Count:", para_word, "of", total_regressions, "\n")
  cat("  Percentage:", format_pct(para_word_pct), "\n")
  
  ## ----- By interval -----
  regression_by_interval <- regressions |>
    count(interval, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct),
      interval_label = case_when(
        interval == 1 ~ "Baseline → Camp 1",
        interval == 2 ~ "Camp 1 → Camp 2",
        interval == 3 ~ "Camp 2 → Camp 3"
      )
    )
  
  cat("\nRegression by interval:\n")
  print(regression_by_interval)
  
  ## ----- Para→Word by interval -----
  para_word_by_interval <- regressions |>
    dplyr::filter(from_state == 4, to_state == 3) |>
    count(interval, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    )
  
  cat("\nPara→Word by interval:\n")
  print(para_word_by_interval)
  
  ## ----- Summary table -----
  regression_summary <- tibble(
    Metric = c(
      "Total regressions",
      "Overall regression rate",
      "Para→Word regressions",
      "Para→Word as % of all regressions",
      "Regressions in Interval 1 (Baseline→Camp1)",
      "Regressions in Interval 2 (Camp1→Camp2)",
      "Regressions in Interval 3 (Camp2→Camp3)"
    ),
    Value = c(
      as.character(total_regressions),
      format_pct(regression_rate),
      as.character(para_word),
      format_pct(para_word_pct),
      as.character(regression_by_interval$n[regression_by_interval$interval == 1]),
      as.character(regression_by_interval$n[regression_by_interval$interval == 2]),
      as.character(regression_by_interval$n[regression_by_interval$interval == 3])
    )
  )
  
  list(
    summary = regression_summary,
    by_transition = regression_by_trans,
    by_interval = regression_by_interval,
    para_word_by_interval = para_word_by_interval,
    total = total_regressions,
    rate = regression_rate
  )
}

# ==============================================================================
# SECTION 7: RECOVERY AFTER REGRESSION
# ==============================================================================

compute_recovery_patterns <- function(transitions) {
  
  cat("\n===== RECOVERY AFTER REGRESSION =====\n")
  
  # Identify regressions and their subsequent transitions
  recovery <- transitions |>
    dplyr::filter(transition_type == "Regression") |>
    dplyr::select(child_id, interval, from_state_reg = from_state, to_state_reg = to_state) |>
    left_join(
      transitions |>
        dplyr::select(child_id, interval, next_state = to_state) |>
        mutate(interval = interval - 1),  # Match to previous interval's regression
      by = c("child_id", "interval")
    ) |>
    dplyr::filter(!is.na(next_state)) |>
    mutate(
      recovered = next_state >= from_state_reg,
      outcome = case_when(
        next_state > from_state_reg ~ "Advanced beyond original",
        next_state == from_state_reg ~ "Recovered to original",
        next_state > to_state_reg & next_state < from_state_reg ~ "Partial recovery",
        next_state == to_state_reg ~ "Remained at regressed level",
        next_state < to_state_reg ~ "Further regression"
      )
    )
  
  cat("\nRecovery outcomes after regression (n =", nrow(recovery), "):\n")
  
  recovery_outcomes <- recovery |>
    count(outcome, name = "n") |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    ) |>
    arrange(desc(n))
  
  print(recovery_outcomes)
  
  recovery_rate <- mean(recovery$recovered, na.rm = TRUE)
  cat("\nRecovery rate (reached or exceeded original level):", format_pct(recovery_rate), "\n")
  
  list(
    outcomes = recovery_outcomes,
    recovery_rate = recovery_rate,
    n_tracked = nrow(recovery)
  )
}

# ==============================================================================
# SECTION 8: STATE OCCUPANCY OVER TIME
# ==============================================================================

compute_state_occupancy <- function(camp) {
  
  cat("\n===== STATE OCCUPANCY =====\n")
  
  state_by_camp <- camp |>
    dplyr::select(baseline_level, camp1_level, camp2_level, camp3_level) |>
    pivot_longer(
      everything(),
      names_to = "assessment",
      values_to = "state"
    ) |>
    dplyr::filter(!is.na(state)) |>
    mutate(
      camp = case_when(
        assessment == "baseline_level" ~ "Baseline",
        assessment == "camp1_level" ~ "Camp 1",
        assessment == "camp2_level" ~ "Camp 2",
        assessment == "camp3_level" ~ "Camp 3"
      ),
      camp = factor(camp, levels = c("Baseline", "Camp 1", "Camp 2", "Camp 3")),
      state_label = factor(state, levels = 1:5, labels = state_labels)
    )
  
  occupancy_summary <- state_by_camp |>
    count(camp, state_label, name = "n") |>
    group_by(camp) |>
    mutate(
      pct = n / sum(n),
      pct_label = format_pct(pct)
    ) |>
    ungroup()
  
  cat("\nState occupancy by assessment point:\n")
  occupancy_wide <- occupancy_summary |>
    dplyr::select(camp, state_label, pct) |>
    pivot_wider(names_from = state_label, values_from = pct, values_fill = 0)
  print(occupancy_wide, digits = 3)
  
  list(
    long = occupancy_summary,
    wide = occupancy_wide
  )
}

# ==============================================================================
# SECTION 9: COVARIATE ASSOCIATIONS WITH OUTCOMES
# ==============================================================================

compute_outcome_associations <- function(camp) {
  
  cat("\n===== COVARIATE ASSOCIATIONS WITH OUTCOMES =====\n")
  
  final_outcome <- camp |>
    mutate(
      final_state = coalesce(camp3_level, camp2_level, camp1_level),
      reached_story = final_state == 5,
      net_change = final_state - baseline_level
    )
  
  ## ----- By gender -----
  outcome_by_gender <- final_outcome |>
    group_by(gender) |>
    summarise(
      n = n(),
      pct_story = mean(reached_story, na.rm = TRUE),
      mean_change = mean(net_change, na.rm = TRUE),
      sd_change = sd(net_change, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nOutcomes by gender:\n")
  print(outcome_by_gender)
  
  ## ----- By county -----
  outcome_by_county <- final_outcome |>
    group_by(county) |>
    summarise(
      n = n(),
      pct_story = mean(reached_story, na.rm = TRUE),
      mean_change = mean(net_change, na.rm = TRUE),
      sd_change = sd(net_change, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nOutcomes by county:\n")
  print(outcome_by_county)
  
  ## ----- By baseline level -----
  outcome_by_baseline <- final_outcome |>
    mutate(baseline = factor(baseline_level, levels = 1:5, labels = state_labels)) |>
    group_by(baseline) |>
    summarise(
      n = n(),
      pct_story = mean(reached_story, na.rm = TRUE),
      mean_change = mean(net_change, na.rm = TRUE),
      sd_change = sd(net_change, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nOutcomes by baseline level:\n")
  print(outcome_by_baseline)
  
  list(
    by_gender = outcome_by_gender,
    by_county = outcome_by_county,
    by_baseline = outcome_by_baseline
  )
}

# ==============================================================================
# SECTION 10: VILLAGE-LEVEL VARIATION
# ==============================================================================

compute_village_variation <- function(transitions) {
  
  cat("\n===== VILLAGE-LEVEL VARIATION =====\n")
  
  village_stats <- transitions |>
    group_by(village, county) |>
    summarise(
      n_transitions = n(),
      n_learners = n_distinct(child_id),
      progression_rate = mean(transition_type == "Progression"),
      regression_rate = mean(transition_type == "Regression"),
      stagnation_rate = mean(transition_type == "Stagnation"),
      .groups = "drop"
    )
  
  # Filter to villages with sufficient data
  village_stats_filtered <- village_stats |>
    dplyr::filter(n_transitions >= 10)
  
  cat("\nVillages with >= 10 transitions:", nrow(village_stats_filtered), "\n")
  
  cat("\nProgression rate across villages:\n")
  print(summary(village_stats_filtered$progression_rate))
  
  cat("\nRegression rate across villages:\n")
  print(summary(village_stats_filtered$regression_rate))
  
  ## ----- Village variation by county -----
  village_by_county <- village_stats_filtered |>
    group_by(county) |>
    summarise(
      n_villages = n(),
      mean_progression = mean(progression_rate),
      sd_progression = sd(progression_rate),
      mean_regression = mean(regression_rate),
      sd_regression = sd(regression_rate),
      .groups = "drop"
    )
  
  cat("\nVillage statistics by county:\n")
  print(village_by_county)
  
  list(
    all = village_stats,
    filtered = village_stats_filtered,
    by_county = village_by_county
  )
}

# ==============================================================================
# SECTION 11: TIME DISTRIBUTIONS
# ==============================================================================

compute_time_distributions <- function(transitions) {
  
  cat("\n===== TIME DISTRIBUTIONS =====\n")
  
  time_by_type <- transitions |>
    group_by(transition_type) |>
    summarise(
      n = n(),
      mean_days = mean(to_time),
      sd_days = sd(to_time),
      median_days = median(to_time),
      min_days = min(to_time),
      max_days = max(to_time),
      .groups = "drop"
    )
  
  cat("\nCumulative days by transition type:\n")
  print(time_by_type)
  
  list(by_type = time_by_type)
}

# ==============================================================================
# SECTION 12: EXCEL EXPORT
# ==============================================================================

export_to_excel <- function(wb, results, output_path) {
  
  cat("\n===== EXPORTING TO EXCEL =====\n")
  
  ## ----- Sheet 1: Sample Characteristics -----
  addWorksheet(wb, "Sample_Characteristics")
  
  # Combine learner characteristics into one table
  learner_chars <- bind_rows(
    tibble(Category = "County", Item = as.character(results$sample$county$county), 
           N = results$sample$county$n, Percent = results$sample$county$pct),
    tibble(Category = "Gender", Item = as.character(results$sample$gender$gender),
           N = results$sample$gender$n, Percent = results$sample$gender$pct),
    tibble(Category = "Grade", Item = as.character(results$sample$grade$grade),
           N = results$sample$grade$n, Percent = results$sample$grade$pct),
    tibble(Category = "Camps Attended", Item = as.character(results$sample$participation$total_camps),
           N = results$sample$participation$n, Percent = results$sample$participation$pct)
  )
  
  writeData(wb, "Sample_Characteristics", learner_chars, headerStyle = header_style)
  addStyle(wb, "Sample_Characteristics", pct_style, rows = 2:(nrow(learner_chars)+1), cols = 4)
  setColWidths(wb, "Sample_Characteristics", cols = 1:4, widths = c(20, 25, 10, 12))
  
  ## ----- Sheet 2: Volunteer Characteristics -----
  addWorksheet(wb, "Volunteer_Characteristics")
  
  vol_chars <- bind_rows(
    tibble(Category = "Gender", Item = as.character(results$volunteers$gender$vol_gender),
           N = results$volunteers$gender$n, Percent = results$volunteers$gender$pct),
    tibble(Category = "Age Category", Item = as.character(results$volunteers$age_cat$vol_age_cat),
           N = results$volunteers$age_cat$n, Percent = results$volunteers$age_cat$pct),
    tibble(Category = "Education Level", Item = as.character(results$volunteers$education$vol_edlevel),
           N = results$volunteers$education$n, Percent = results$volunteers$education$pct)
  )
  
  writeData(wb, "Volunteer_Characteristics", vol_chars, headerStyle = header_style)
  addStyle(wb, "Volunteer_Characteristics", pct_style, rows = 2:(nrow(vol_chars)+1), cols = 4)
  setColWidths(wb, "Volunteer_Characteristics", cols = 1:4, widths = c(20, 25, 10, 12))
  
  ## ----- Sheet 3: Volunteers by County -----
  addWorksheet(wb, "Volunteers_by_County")
  writeData(wb, "Volunteers_by_County", results$volunteers$by_county, headerStyle = header_style)
  
  ## ----- Sheet 4: Baseline Literacy -----
  addWorksheet(wb, "Baseline_Literacy")
  writeData(wb, "Baseline_Literacy", results$baseline$overall, headerStyle = header_style)
  writeData(wb, "Baseline_Literacy", results$baseline$by_county_wide, 
            startRow = nrow(results$baseline$overall) + 3, headerStyle = header_style)
  
  ## ----- Sheet 5: Attendance -----
  addWorksheet(wb, "Attendance")
  writeData(wb, "Attendance", results$attendance$by_camp, headerStyle = header_style)
  writeData(wb, "Attendance", results$attendance$by_county, 
            startRow = nrow(results$attendance$by_camp) + 3, headerStyle = header_style)
  
  ## ----- Sheet 6: Transition Types -----
  addWorksheet(wb, "Transition_Types")
  writeData(wb, "Transition_Types", results$transitions$type_overall, headerStyle = header_style)
  
  # By interval (wide format)
  trans_interval_wide <- results$transitions$type_by_interval |>
    dplyr::select(interval_label, transition_type, n, pct) |>
    pivot_wider(names_from = transition_type, values_from = c(n, pct))
  
  writeData(wb, "Transition_Types", trans_interval_wide,
            startRow = nrow(results$transitions$type_overall) + 3, headerStyle = header_style)
  
  ## ----- Sheet 7: Transition Matrix -----
  addWorksheet(wb, "Transition_Matrix")
  
  # Add row labels
  trans_counts_labeled <- results$transitions$matrix_counts |>
    mutate(from_label = state_labels[from_state]) |>
    dplyr::select(from_label, everything(), -from_state)
  
  writeData(wb, "Transition_Matrix", trans_counts_labeled, headerStyle = header_style)
  
  trans_probs_labeled <- results$transitions$matrix_probs |>
    mutate(from_label = state_labels[from_state]) |>
    dplyr::select(from_label, everything(), -from_state)
  
  writeData(wb, "Transition_Matrix", trans_probs_labeled,
            startRow = 7, headerStyle = header_style)
  
  ## ----- Sheet 8: Regression Analysis -----
  addWorksheet(wb, "Regression_Analysis")
  writeData(wb, "Regression_Analysis", results$regression$summary, headerStyle = header_style)
  writeData(wb, "Regression_Analysis", results$regression$by_transition |> 
              dplyr::select(transition, n, pct_label),
            startRow = nrow(results$regression$summary) + 3, headerStyle = header_style)
  
  ## ----- Sheet 9: Recovery After Regression -----
  addWorksheet(wb, "Recovery_Patterns")
  writeData(wb, "Recovery_Patterns", results$recovery$outcomes, headerStyle = header_style)
  
  ## ----- Sheet 10: State Occupancy -----
  addWorksheet(wb, "State_Occupancy")
  writeData(wb, "State_Occupancy", results$occupancy$wide, headerStyle = header_style)
  
  ## ----- Sheet 11: Outcome Associations -----
  addWorksheet(wb, "Outcome_Associations")
  writeData(wb, "Outcome_Associations", results$outcomes$by_gender, headerStyle = header_style)
  writeData(wb, "Outcome_Associations", results$outcomes$by_county,
            startRow = nrow(results$outcomes$by_gender) + 3, headerStyle = header_style)
  writeData(wb, "Outcome_Associations", results$outcomes$by_baseline,
            startRow = nrow(results$outcomes$by_gender) + nrow(results$outcomes$by_county) + 6,
            headerStyle = header_style)
  
  ## ----- Sheet 12: Village Variation -----
  addWorksheet(wb, "Village_Variation")
  writeData(wb, "Village_Variation", results$villages$by_county, headerStyle = header_style)
  writeData(wb, "Village_Variation", results$villages$filtered,
            startRow = nrow(results$villages$by_county) + 3, headerStyle = header_style)
  
  ## ----- Sheet 13: Time Distributions -----
  addWorksheet(wb, "Time_Distributions")
  writeData(wb, "Time_Distributions", results$time$by_type, headerStyle = header_style)
  
  ## ----- Save workbook -----
  saveWorkbook(wb, output_path, overwrite = TRUE)
  cat("Excel workbook saved to:", output_path, "\n")
}

# ==============================================================================
# MAIN EDA EXECUTION
# ==============================================================================

run_eda <- function(camp, transitions, output_dir = ".", excel_filename = "eda_results.xlsx") {
  
  cat("========================================\n")
  cat("MULTISTATE LITERACY TRANSITION EDA\n")
  cat("========================================\n")
  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("Sample:", nrow(camp), "learners,", nrow(transitions), "transitions\n")
  cat("========================================\n")
  
  # Initialize workbook

  wb <- createWorkbook()
  
  # Compute all summaries
  results <- list(
    sample = compute_sample_characteristics(camp),
    volunteers = compute_volunteer_characteristics(camp),
    baseline = compute_baseline_literacy(camp),
    attendance = compute_attendance(camp),
    transitions = compute_transition_patterns(transitions),
    regression = compute_regression_patterns(transitions),
    recovery = compute_recovery_patterns(transitions),
    occupancy = compute_state_occupancy(camp),
    outcomes = compute_outcome_associations(camp),
    villages = compute_village_variation(transitions),
    time = compute_time_distributions(transitions)
  )
  
  # Export to Excel
  excel_path <- file.path(output_dir, excel_filename)
  export_to_excel(wb, results, excel_path)
  
  
  # Return results for further use
  invisible(results)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

source("00_thesis_setup.R")
camp <- readRDS(file.path(paths$processed, "camp_clean.rds"))
transitions <- readRDS(file.path(paths$processed, "transitions.rds"))

# Run EDA with sink for console output
sink("eda_output.txt", split = TRUE)
eda_results <- run_eda(camp, transitions, paths$output, "eda_results.xlsx")
sink()

# Save R results object
saveRDS(eda_results, file.path(paths$processed, "eda_results.rds"))
