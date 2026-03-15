## ===== MULTISTATE LITERACY ANALYSIS: DATA PREPARATION =====
## Author: Cyrus Wakibia
## Purpose: Prepare transition data for multistate survival models
##
## Key steps:
## 1. Load and clean camp-level data
## 2. Exclude learners with zero camp attendance
## 3. Exclude learners with non-consecutive camp attendance
## 4. Create transition dataset (long format)
## 5. Prepare model-ready datasets with centered covariates
## 6. Create analysis subsets (full, unidirectional, household)
##
## Output files:
## - camp_clean.rds: Cleaned learner-level data
## - transitions.rds: Transition-level data for modeling
## - model_data_full.rds: Full bidirectional analysis dataset
## - model_data_uni.rds: Unidirectional subset (no regressions)
## - model_data_hh.rds: Household-matched subset

library(tidyverse)


# ==============================================================================
# SETUP AND CONSTANTS
# ==============================================================================

state_labels <- c("Beginner", "Letter", "Word", "Paragraph", "Story")

classify_transition <- function(from, to) {
  case_when(
    to > from ~ "Progression",
    to < from ~ "Regression",
    TRUE ~ "Stagnation"
  )
}

# ==============================================================================
# SECTION 1: LOAD AND CLEAN CAMP DATA
# ==============================================================================

load_and_clean_camp_data <- function(camp_raw_path) {
  
  cat("\n===== LOADING CAMP DATA =====\n")
  
  # Load raw data
  camp <- readRDS(camp_raw_path)
  
  cat("Initial sample:", nrow(camp), "learners\n")
  
  # ----- EXCLUSION 1: Zero camp attendance -----
  # Per methodology, exclude learners who enrolled but attended zero camps
  
  n_zero_attendance <- sum(camp$total_camps == 0, na.rm = TRUE)
  
  if (n_zero_attendance > 0) {
    cat("\nExclusion 1: Zero camp attendance\n")
    cat("  Excluding", n_zero_attendance, "learners with zero camp attendance\n")
    
    excluded_zero <- camp |> filter(total_camps == 0)
    cat("  IDs excluded:", paste(head(excluded_zero$child_id, 10), collapse = ", "))
    if (n_zero_attendance > 10) cat(" ... and", n_zero_attendance - 10, "more")
    cat("\n")
    
    camp <- camp |> filter(total_camps > 0 | is.na(total_camps))
    cat("  Sample after exclusion:", nrow(camp), "learners\n")
  }
  
  # ----- EXCLUSION 2: Non-consecutive camp attendance -----
  # Exclude learners who skipped a camp (e.g., attended Camp 1 and Camp 3 but not Camp 2)
  # These create ambiguous transitions where from_state would be NA
  
  camp <- camp |>
    mutate(
      # Flag non-consecutive attendance patterns
      skipped_camp2 = !is.na(camp3_level) & is.na(camp2_level),
      skipped_camp1 = !is.na(camp2_level) & is.na(camp1_level)
    )
  
  n_skipped <- sum(camp$skipped_camp2 | camp$skipped_camp1, na.rm = TRUE)
  
  if (n_skipped > 0) {
    cat("\nExclusion 2: Non-consecutive camp attendance\n")
    cat("  Excluding", n_skipped, "learners with non-consecutive attendance\n")
    
    excluded_skipped <- camp |> filter(skipped_camp2 | skipped_camp1)
    cat("  Skipped Camp 2:", sum(camp$skipped_camp2, na.rm = TRUE), "\n")
    cat("  Skipped Camp 1:", sum(camp$skipped_camp1, na.rm = TRUE), "\n")
    cat("  Example IDs:", paste(head(excluded_skipped$child_id, 5), collapse = ", "), "\n")
    
    camp <- camp |> filter(!skipped_camp2 & !skipped_camp1)
    cat("  Sample after exclusion:", nrow(camp), "learners\n")
  }
  
  # Remove helper columns
  camp <- camp |> dplyr::select(-skipped_camp2, -skipped_camp1)
  
  # ----- Verify required variables -----
  required_vars <- c("child_id", "village", "county", "gender", "age",
                     "baseline_level", "camp1_level", "days_camp1")
  
  missing_vars <- setdiff(required_vars, names(camp))
  if (length(missing_vars) > 0) {
    warning("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # ----- Create derived variables -----
  camp <- camp |>
    mutate(
      # Ensure factor levels are correct
      county = factor(county, levels = c("Bungoma", "Kilifi", "Kitui")),
      gender = factor(gender, levels = c("Female", "Male")),
      
      # Total attendance days (cumulative)
      total_days = coalesce(days_camp1, 0L) + 
        coalesce(days_camp2, 0L) + 
        coalesce(days_camp3, 0L),
      
      # Maximum possible days based on camps attended
      max_possible_days = case_when(
        total_camps == 1 ~ 10L,
        total_camps == 2 ~ 20L,
        total_camps >= 3 ~ 30L,
        TRUE ~ NA_integer_
      ),
      
      # Attendance rate
      attendance_rate = total_days / max_possible_days,
      
      # Final literacy level achieved
      final_level = coalesce(camp3_level, camp2_level, camp1_level, baseline_level)
    )
  
  cat("\n===== FINAL SAMPLE =====\n")
  cat("Learners:", nrow(camp), "\n")
  cat("Villages:", n_distinct(camp$village), "\n")
  cat("Counties:", paste(unique(camp$county), collapse = ", "), "\n")
  
  camp
}

# ==============================================================================
# SECTION 2: CREATE TRANSITION DATASET
# ==============================================================================

create_transitions <- function(camp) {
  
  cat("\n===== CREATING TRANSITIONS =====\n")
  
  # Reshape to long format: one row per transition interval
  # Intervals: baseline→camp1, camp1→camp2, camp2→camp3
  
  transitions_list <- list()
  
  # ----- Interval 1: Baseline to Camp 1 -----
  int1 <- camp |>
    filter(!is.na(baseline_level) & !is.na(camp1_level)) |>
    transmute(
      child_id = child_id,
      village = village,
      county = county,
      gender = gender,
      merge = merge,
      age = age,
      grade = grade,
      interval = 1L,
      from_state = baseline_level,
      to_state = camp1_level,
      from_time = 0,
      to_time = coalesce(days_camp1, 10L),
      days_this_camp = days_camp1,
      attendance_rate = attendance_rate,
      total_camps = total_camps,
      # Volunteer variables (if available)
      vol_gender = if ("vol_gender" %in% names(camp)) vol_gender else NA_character_,
      vol_edlevel = if ("vol_edlevel" %in% names(camp)) vol_edlevel else NA_character_,
      vol_age_cat = if ("vol_age_cat" %in% names(camp)) vol_age_cat else NA_character_,
      # Household variables (if available)
      # parent_ed = if ("hh_mother_ed" %in% names(camp)) hh_mother_ed else NA_character_
      parent_ed = parent_ed_collapsed,
      hh_gender = hh_gender
    )
  
  transitions_list[[1]] <- int1
  cat("Interval 1 (Baseline -> Camp 1):", nrow(int1), "transitions\n")
  
  # ----- Interval 2: Camp 1 to Camp 2 -----
  int2 <- camp |>
    filter(!is.na(camp1_level) & !is.na(camp2_level)) |>
    transmute(
      child_id = child_id,
      village = village,
      county = county,
      gender = gender,
      merge = merge,
      age = age,
      grade = grade,
      interval = 2L,
      from_state = camp1_level,
      to_state = camp2_level,
      from_time = coalesce(days_camp1, 10L),
      to_time = coalesce(days_camp1, 10L) + coalesce(days_camp2, 10L),
      days_this_camp = days_camp2,
      attendance_rate = attendance_rate,
      total_camps = total_camps,
      vol_gender = if ("vol_gender" %in% names(camp)) vol_gender else NA_character_,
      vol_edlevel = if ("vol_edlevel" %in% names(camp)) vol_edlevel else NA_character_,
      vol_age_cat = if ("vol_age_cat" %in% names(camp)) vol_age_cat else NA_character_,
      # parent_ed = if ("parent_ed" %in% names(camp)) parent_ed else NA_character_
      parent_ed = parent_ed_collapsed,
      hh_gender = hh_gender
    )
  
  transitions_list[[2]] <- int2
  cat("Interval 2 (Camp 1 -> Camp 2):", nrow(int2), "transitions\n")
  
  # ----- Interval 3: Camp 2 to Camp 3 -----
  int3 <- camp |>
    filter(!is.na(camp2_level) & !is.na(camp3_level)) |>
    transmute(
      child_id = child_id,
      village = village,
      county = county,
      gender = gender,
      merge = merge,
      age = age,
      grade = grade,
      interval = 3L,
      from_state = camp2_level,
      to_state = camp3_level,
      from_time = coalesce(days_camp1, 10L) + coalesce(days_camp2, 10L),
      to_time = coalesce(days_camp1, 10L) + coalesce(days_camp2, 10L) + coalesce(days_camp3, 10L),
      days_this_camp = days_camp3,
      attendance_rate = attendance_rate,
      total_camps = total_camps,
      vol_gender = if ("vol_gender" %in% names(camp)) vol_gender else NA_character_,
      vol_edlevel = if ("vol_edlevel" %in% names(camp)) vol_edlevel else NA_character_,
      vol_age_cat = if ("vol_age_cat" %in% names(camp)) vol_age_cat else NA_character_,
      parent_ed = if ("parent_ed" %in% names(camp)) parent_ed else NA_character_,
      parent_ed = parent_ed_collapsed,
      hh_gender = hh_gender
    )
  
  transitions_list[[3]] <- int3
  cat("Interval 3 (Camp 2 -> Camp 3):", nrow(int3), "transitions\n")
  
  # ----- Combine all intervals -----
  transitions <- bind_rows(transitions_list)
  
  # ----- Verify no NA states -----
  n_na_from <- sum(is.na(transitions$from_state))
  n_na_to <- sum(is.na(transitions$to_state))
  
  if (n_na_from > 0 | n_na_to > 0) {
    warning("Found NA states: from_state=", n_na_from, ", to_state=", n_na_to)
  }
  
  # ----- Add derived variables -----
  transitions <- transitions |>
    mutate(
      # State labels
      from_label = factor(from_state, levels = 1:5, labels = state_labels),
      to_label = factor(to_state, levels = 1:5, labels = state_labels),
      
      # Transition classification
      transition_type = classify_transition(from_state, to_state),
      
      # Transition label
      trans_label = paste0(from_label, " -> ", to_label),
      
      # Sojourn time (time spent in origin state this interval)
      sojourn_time = to_time - from_time
    )
  
  cat("\n===== TRANSITION SUMMARY =====\n")
  cat("Total transitions:", nrow(transitions), "\n")
  cat("From", n_distinct(transitions$child_id), "learners\n")
  cat("\nBy interval:\n")
  print(table(transitions$interval))
  cat("\nBy transition type:\n")
  print(table(transitions$transition_type))
  
  transitions
}

# ==============================================================================
# SECTION 3: PREPARE MODEL DATA
# ==============================================================================

prepare_model_data <- function(transitions) {
  
  cat("\n===== PREPARING MODEL DATA =====\n")
  
  # ----- Compute centering constants on full sample -----
  mean_age <- mean(transitions$age, na.rm = TRUE)
  mean_attendance <- mean(transitions$attendance_rate, na.rm = TRUE)
  
  cat("Centering constants (computed on full sample):\n")
  cat("  Mean age:", round(mean_age, 3), "years\n")
  cat("  Mean attendance:", round(mean_attendance, 3), "\n")
  
  # ----- Create model-ready variables -----
  model_data <- transitions |>
    mutate(
      # Survival structure
      tstart = from_time,
      tstop = to_time,
      event = factor(to_state, levels = 1:5, labels = state_labels),
      from = factor(from_state, levels = 1:5, labels = state_labels),
      
      # Centered covariates
      female = as.integer(gender == "Female"),
      age_centered = age - mean_age,
      attendance_centered = attendance_rate - mean_attendance,
      
      # County factor (Bungoma as reference)
      county_f = factor(county, levels = c("Bungoma", "Kilifi", "Kitui")),
      
      # Sojourn time for semi-Markov
      time_in_state = sojourn_time,
      
      # Volunteer factors
      vol_female = as.integer(vol_gender == "Female"),
      vol_edlevel_f = factor(vol_edlevel),
      vol_age_cat_f = factor(vol_age_cat),
      
      # Household factor
      parent_ed_f = parent_ed
    )
  
  # Store centering constants as attributes
  attr(model_data, "mean_age") <- mean_age
  attr(model_data, "mean_attendance") <- mean_attendance
  
  cat("\nModel data prepared:", nrow(model_data), "transitions\n")
  
  model_data
}

# ==============================================================================
# SECTION 4: CREATE ANALYSIS SUBSETS
# ==============================================================================

create_analysis_subsets <- function(model_data) {
  
  cat("\n===== CREATING ANALYSIS SUBSETS =====\n")
  
  # ----- Full bidirectional dataset -----
  model_data_full <- model_data
  cat("Full (bidirectional):", nrow(model_data_full), "transitions from",
      n_distinct(model_data_full$child_id), "learners\n")
  
  # ----- Unidirectional: exclude regressions -----
  model_data_uni <- model_data |>
    filter(transition_type != "Regression")
  cat("Unidirectional (no regressions):", nrow(model_data_uni), "transitions\n")
  cat("  Regressions excluded:", nrow(model_data_full) - nrow(model_data_uni), "\n")
  
  # ----- Household subset: complete cases for parent_ed -----
  model_data_hh <- model_data |>
    filter(merge == 3)
    # filter(!is.na(parent_ed_f))
  cat("Household matched:", nrow(model_data_hh), "transitions from",
      n_distinct(model_data_hh$child_id), "learners\n")
  
  # ----- Volunteer subset: complete cases for volunteer variables -----
  model_data_vol <- model_data |>
    filter(!is.na(vol_female) & !is.na(vol_edlevel_f) & !is.na(vol_age_cat_f))
  cat("Volunteer complete:", nrow(model_data_vol), "transitions from",
      n_distinct(model_data_vol$child_id), "learners\n")
  
  list(
    full = model_data_full,
    unidirectional = model_data_uni,
    household = model_data_hh,
    volunteer = model_data_vol
  )
}

# ==============================================================================
# SECTION 5: SUMMARY STATISTICS
# ==============================================================================

summarize_model_data <- function(model_data) {
  
  cat("\n===== MODEL DATA SUMMARY =====\n")
  
  cat("\nTransitions by origin state:\n")
  print(table(model_data$from))
  
  cat("\nTransitions by destination state:\n")
  print(table(model_data$event))
  
  cat("\nTransition matrix:\n")
  print(table(model_data$from, model_data$event))
  
  cat("\nTime variable (cumulative instruction days):\n")
  cat("  tstart range:", range(model_data$tstart, na.rm = TRUE), "\n")
  cat("  tstop range:", range(model_data$tstop, na.rm = TRUE), "\n")
  
  cat("\nCovariate summaries:\n")
  cat("  Female:", round(mean(model_data$female, na.rm = TRUE), 3), "\n")
  cat("  Age (centered):", round(mean(model_data$age_centered, na.rm = TRUE), 3), "\n")
  cat("  Attendance (centered):", round(mean(model_data$attendance_centered, na.rm = TRUE), 3), "\n")
  
  cat("\nCounty distribution:\n")
  print(table(model_data$county_f))
  
  cat("\nVolunteer data completeness:\n")
  cat("  vol_female:", sum(!is.na(model_data$vol_female)), "of", nrow(model_data), "\n")
  cat("  vol_edlevel:", sum(!is.na(model_data$vol_edlevel_f)), "of", nrow(model_data), "\n")
  cat("  vol_age_cat:", sum(!is.na(model_data$vol_age_cat_f)), "of", nrow(model_data), "\n")
  
  cat("\nHousehold data completeness:\n")
  cat("  parent_ed:", sum(!is.na(model_data$parent_ed_f)), "of", nrow(model_data), "\n")
}

# ==============================================================================
# SECTION 6: MAIN EXECUTION
# ==============================================================================

run_data_preparation <- function(camp_path, output_dir) {
  
  cat("============================================================\n")
  cat("MULTISTATE LITERACY ANALYSIS: DATA PREPARATION\n")
  cat("============================================================\n")
  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("============================================================\n")
  
  # Step 1: Load and clean camp data
  camp <- load_and_clean_camp_data(camp_path)
  
  # Step 2: Create transitions
  transitions <- create_transitions(camp)
  
  # Step 3: Prepare model data
  model_data <- prepare_model_data(transitions)
  
  # Step 4: Create subsets
  subsets <- create_analysis_subsets(model_data)
  
  # Step 5: Summarize
  summarize_model_data(subsets$full)
  
  # Step 6: Save outputs
  cat("\n===== SAVING OUTPUTS =====\n")
  
  saveRDS(camp, file.path(output_dir, "camp_clean.rds"))
  cat("Saved: camp_clean.rds\n")
  
  saveRDS(transitions, file.path(output_dir, "transitions.rds"))
  cat("Saved: transitions.rds\n")
  
  saveRDS(subsets$full, file.path(output_dir, "model_data_full.rds"))
  cat("Saved: model_data_full.rds\n")
  
  saveRDS(subsets$unidirectional, file.path(output_dir, "model_data_uni.rds"))
  cat("Saved: model_data_uni.rds\n")
  
  saveRDS(subsets$household, file.path(output_dir, "model_data_hh.rds"))
  cat("Saved: model_data_hh.rds\n")
  
  saveRDS(subsets$volunteer, file.path(output_dir, "model_data_vol.rds"))
  cat("Saved: model_data_vol.rds\n")
  
  # Save centering constants for reference
  centering <- list(
    mean_age = attr(subsets$full, "mean_age"),
    mean_attendance = attr(subsets$full, "mean_attendance")
  )
  saveRDS(centering, file.path(output_dir, "centering_constants.rds"))
  cat("Saved: centering_constants.rds\n")
  
  cat("\n============================================================\n")
  cat("DATA PREPARATION COMPLETE\n")
  cat("============================================================\n")
  
  invisible(list(
    camp = camp,
    transitions = transitions,
    subsets = subsets
  ))
}

# ==============================================================================
# EXECUTION
# ==============================================================================

source("00_thesis_setup.R")

results <- run_data_preparation(
  camp_path = file.path(paths$processed, "camp_clean.rds"),
  output_dir = paths$processed
)
