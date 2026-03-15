## ===== MULTISTATE LITERACY ANALYSIS: MODEL FITTING =====
## Author: Cyrus Wakibia
## Purpose: Fit semi-Markov multistate models for literacy transitions
##
## Model Structure (as per Chapter 3 methodology):
## - PRIMARY: Semi-Markov bidirectional model with full covariates
## - COMPARISON 1: Markov model (no time_in_state) 
## - COMPARISON 2: Unidirectional model (no regression transitions)
## - COMPARISON 3: Story-terminal survival model (binary outcome)
##
## Covariate Hierarchy (theoretically specified):
## - Individual: female, age_centered
## - County: county_f (geographic context)
## - Sojourn: time_in_state (semi-Markov extension)
## - Volunteer: vol_female, vol_edlevel_f, vol_age_cat_f
##
## Note: attendance_centered excluded due to collinearity with time variable
##       (time is measured in cumulative attendance days)
##
## Clustering: Robust standard errors via cluster = village
## Note: Frailty models not well-supported for bidirectional multistate



library(survival)
library(tidyverse)
library(broom)
library(openxlsx)


# ==============================================================================
# SETUP
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
# SECTION 1: MODEL FITTING FUNCTIONS
# ==============================================================================

## ----- Primary Model: Semi-Markov with full covariates -----
## This is the FINAL model for inference
library(splines)

fit_primary_semimarkov <- function(model_data) {
  
  cat("\n===== FITTING PRIMARY MODEL: SEMI-MARKOV =====\n")
  cat("Covariates: female + age + county + time_in_state + volunteer\n")
  cat("Note: attendance excluded (collinear with time variable)\n")
  cat("Clustering: Robust SE by village\n\n")
  
  
  
  
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~
      female + age_centered + county_f +
      time_in_state + 
      vol_female + vol_edlevel_f + vol_age_cat_f,
    data = model_data,
    id = child_id,
    cluster = village
  )
  
  cat("Events:", summary(model)$nevent, "\n")
  cat("Coefficients:", length(coef(model)), "\n")
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
  
  model
}

fit_timevarying_volunteers <- function(model_data) {
  
  cat("\n===== FITTING TIME-VARYING VOLUNTEER EFFECTS =====\n")
  
  library(splines)
  
  # Create program phase variable
  model_data <- model_data |>
    mutate(program_phase = interval - 1)  # 0, 1, 2 for intervals 1, 2, 3
  
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~
      female + age_centered + county_f +
      time_in_state, +
      # Time-varying volunteer effects
      vol_female * program_phase +
      vol_edlevel_collapsed_f * program_phase +
      vol_age_cat_f * program_phase +
      hh_gender_f,
    data = model_data,
    id = child_id,
    cluster = village
  )
  
  model
}

## ----- Comparison Model 1: Markov (no sojourn time) -----
## Shows effect of ignoring duration dependence

fit_comparison_markov <- function(model_data) {
  
  cat("\n===== FITTING COMPARISON: MARKOV MODEL =====\n")
  cat("Same covariates, NO time_in_state\n\n")
  
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~
      female + age_centered + county_f +
      vol_female + vol_edlevel_f + vol_age_cat_f,
    data = model_data,
    id = child_id,
    cluster = village
  )
  
  cat("Events:", summary(model)$nevent, "\n")
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
  
  model
}

## ----- Comparison Model 2: Unidirectional -----
## Shows effect of ignoring regressions

fit_comparison_unidirectional <- function(model_data_uni) {
  
  cat("\n===== FITTING COMPARISON: UNIDIRECTIONAL MODEL =====\n")
  cat("Semi-Markov specification, but excluding regression transitions\n\n")
  
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~
      female + age_centered + county_f +
      time_in_state +
      vol_female + vol_edlevel_f + vol_age_cat_f,
    data = model_data_uni,
    id = child_id,
    cluster = village
  )
  
  cat("Events:", summary(model)$nevent, "\n")
  cat("Transitions excluded:", 
      nrow(model_data_uni |> dplyr::filter(transition_type == "Regression")), 
      "regressions\n")
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
  
  model
}

## ----- Baseline models for model building sequence -----

fit_baseline_null <- function(model_data) {
  
  cat("\n===== FITTING M0: NULL MODEL =====\n")
  
  model_data$ones <- 1
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~ ones,
    data = model_data,
    id = child_id
  )
  
  cat("Events:", summary(model)$nevent, "\n")
  cat("Log-likelihood:", round(logLik(model), 1), "\n")
  
  model
}


fit_demographics_only <- function(model_data) {
  
  cat("\n===== FITTING M1: DEMOGRAPHICS ONLY =====\n")
  
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~
      female + age_centered,
    data = model_data,
    id = child_id,
    cluster = village
  )
  
  cat("Events:", summary(model)$nevent, "\n")
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
  
  model
}

fit_demographics_county <- function(model_data) {
  
  cat("\n===== FITTING M2: DEMOGRAPHICS + COUNTY =====\n")
  
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~
      female + age_centered + county_f,
    data = model_data,
    id = child_id,
    cluster = village
  )
  
  cat("Events:", summary(model)$nevent, "\n")
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
  
  model
}

fit_with_attendance <- function(model_data) {
  
  cat("\n===== FITTING M3: + ATTENDANCE =====\n")
  
  model <- coxph(
    Surv(tstart, tstop, event, type = "mstate") ~
      female + age_centered + county_f + attendance_centered,
    data = model_data,
    id = child_id,
    cluster = village
  )
  
  cat("Events:", summary(model)$nevent, "\n")
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
  
  model
}

## ----- Comparison Model 3: Story-Terminal (Binary Survival) -----
## Traditional survival analysis: time to reaching Story level
## This is what a typical impact evaluation would use

create_story_terminal_data <- function(model_data) {
  

  cat("\n===== CREATING STORY-TERMINAL DATASET =====\n")
  
  # Collapse to one row per learner
  # Outcome: reached Story at any point (binary)
  # Time: total instruction days (censored at last observation if no Story)
  
  story_data <- model_data |>
    group_by(child_id) |>
    summarise(
      # Did learner ever reach Story?
      reached_story = any(to_state == 5) | any(from_state == 5),
      
      # Time to Story (or censoring time)
      # If reached Story: time at first Story observation
      # If not: time at last observation (right censored)
      time_to_story = if (any(to_state == 5)) {
        min(tstop[to_state == 5])
      } else {
        max(tstop)
      },
      
      # Baseline state (first observed state)
      baseline_state = first(from_state),
      
      # Covariates (take first observation - these are time-invariant)
      female = first(female),
      age_centered = first(age_centered),
      county_f = first(county_f),
      village = first(village),
      vol_female = first(vol_female),
      vol_edlevel_f = first(vol_edlevel_f),
      vol_age_cat_f = first(vol_age_cat_f),
      
      .groups = "drop"
    ) |>
    mutate(
      # Create factor for baseline level
      baseline_label = factor(baseline_state, levels = 1:5, labels = state_labels),
      
      # Event indicator (1 = reached Story, 0 = censored)
      event = as.integer(reached_story)
    )
  
  cat("Learners:", nrow(story_data), "\n")
  cat("Reached Story:", sum(story_data$reached_story), 
      "(", round(100 * mean(story_data$reached_story), 1), "%)\n")
  cat("Censored:", sum(!story_data$reached_story), "\n")
  
  cat("\nBy baseline level:\n")
  baseline_summary <- story_data |>
    group_by(baseline_label) |>
    summarise(
      n = n(),
      n_story = sum(reached_story),
      pct_story = round(100 * mean(reached_story), 1),
      .groups = "drop"
    )
  print(baseline_summary)
  
  story_data
}

fit_story_terminal <- function(story_data) {
  
  cat("\n===== FITTING STORY-TERMINAL MODEL =====\n")
  cat("Standard survival: time to Story attainment (binary outcome)\n")
  cat("This represents a traditional impact evaluation approach\n\n")
  
  model <- coxph(
    Surv(time_to_story, event) ~
      female + age_centered + county_f +
      vol_female + vol_edlevel_f + vol_age_cat_f,
    data = story_data,
    cluster = village
  )
  
  cat("Events (reached Story):", summary(model)$nevent, "\n")
  cat("Censored:", nrow(story_data) - summary(model)$nevent, "\n")
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
  
  model
}

# ==============================================================================
# SECTION 2: COEFFICIENT EXTRACTION
# ==============================================================================

## ----- Tidy multistate coefficients with transition labels -----

tidy_multistate_coefs <- function(model, conf.int = TRUE, conf.level = 0.95) {
  
  # Extract coefficients from summary
  sm <- summary(model)
  coef_mat <- sm$coefficients
  
  # Build tibble
  coef_tbl <- tibble(
    term = rownames(coef_mat),
    estimate = coef_mat[, "coef"],
    std.error = coef_mat[, "robust se"],  # Use robust SE
    statistic = coef_mat[, "z"],
    p.value = coef_mat[, "Pr(>|z|)"]
  )
  
  # Add confidence intervals
  if (conf.int) {
    z_crit <- qnorm(1 - (1 - conf.level) / 2)
    coef_tbl <- coef_tbl |>
      mutate(
        conf.low = estimate - z_crit * std.error,
        conf.high = estimate + z_crit * std.error
      )
  }
  
  # Add hazard ratios
  coef_tbl <- coef_tbl |>
    mutate(
      hr = exp(estimate),
      hr_lower = exp(conf.low),
      hr_upper = exp(conf.high)
    )
  
  # Parse transition information from term names
  # Format: covariate_fromstate:tostate (e.g., "female_1:2")
  coef_tbl <- coef_tbl |>
    mutate(
      # Extract base covariate name
      base_covariate = str_remove(term, "_\\d+:\\d+$"),
      
      # Extract transition code
      trans_code = str_extract(term, "\\d+:\\d+$"),
      
      # Parse from and to states
      from_state = as.integer(str_extract(trans_code, "^\\d+")),
      to_state = as.integer(str_extract(trans_code, "\\d+$")),
      
      # Create readable labels
      from_label = state_labels[from_state],
      to_label = state_labels[to_state],
      transition = paste0(from_label, " -> ", to_label),
      
      # Classify transition type
      trans_type = classify_transition(from_state, to_state),
      
      # Significance indicators
      sig_stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      significant = p.value < 0.05
    )
  
  coef_tbl
}

## ----- Model fit summary -----

glance_multistate <- function(model) {
  
  sm <- summary(model)
  
  tibble(
    n_events = sm$nevent,
    n_obs = sm$n,
    n_coef = length(coef(model)),
    logLik = as.numeric(logLik(model)),
    AIC = AIC(model),
    concordance = sm$concordance[1],
    concordance_se = sm$concordance[2],
    lr_test_stat = sm$logtest[1],
    lr_test_df = sm$logtest[2],
    lr_test_p = sm$logtest[3],
    wald_test_stat = sm$waldtest[1],
    wald_test_df = sm$waldtest[2],
    wald_test_p = sm$waldtest[3]
  )
}

# ==============================================================================
# SECTION 3: SEMI-MARKOV VS MARKOV COMPARISON
# ==============================================================================

compare_semimarkov_markov <- function(m_semimarkov, m_markov) {
  
  cat("\n===== SEMI-MARKOV VS MARKOV COMPARISON =====\n")
  cat("Purpose: Demonstrate effect of duration dependence\n\n")
  
  # Model fit comparison
  fit_sm <- glance_multistate(m_semimarkov)
  fit_mk <- glance_multistate(m_markov)
  
  fit_comparison <- tibble(
    Model = c("Semi-Markov", "Markov"),
    Events = c(fit_sm$n_events, fit_mk$n_events),
    Coefficients = c(fit_sm$n_coef, fit_mk$n_coef),
    LogLik = c(fit_sm$logLik, fit_mk$logLik),
    AIC = c(fit_sm$AIC, fit_mk$AIC),
    Concordance = c(fit_sm$concordance, fit_mk$concordance)
  )
  
  cat("Model Fit Comparison:\n")
  print(fit_comparison)
  
  # Likelihood ratio test (semi-Markov is nested extension of Markov)
  # Semi-Markov adds time_in_state for each transition
  lr_stat <- 2 * (fit_sm$logLik - fit_mk$logLik)
  df_diff <- fit_sm$n_coef - fit_mk$n_coef
  lr_pval <- pchisq(lr_stat, df_diff, lower.tail = FALSE)
  
  cat("\nLikelihood Ratio Test:\n")
  cat("  Chi-sq =", round(lr_stat, 2), ", df =", df_diff, 
      ", p =", format.pval(lr_pval), "\n")
  
  if (lr_pval < 0.05) {
    cat("  --> Semi-Markov significantly improves fit\n")
    cat("  --> Duration dependence is present in the data\n")
  }
  
  # Extract time_in_state coefficients from semi-Markov model
  coefs_sm <- tidy_multistate_coefs(m_semimarkov)
  time_coefs <- coefs_sm |>
    dplyr::filter(str_detect(base_covariate, "time_in_state")) |>
    dplyr::select(transition, trans_type, hr, 
                  hr_lower, hr_upper, p.value, sig_stars)
  # |>
    # arrange(from_state, to_state)
  
  cat("\nSojourn Time Effects (time_in_state hazard ratios):\n")
  print(time_coefs)
  
  list(
    fit_comparison = fit_comparison,
    lr_test = list(statistic = lr_stat, df = df_diff, p.value = lr_pval),
    time_coefs = time_coefs
  )
}

# ==============================================================================
# SECTION 4: BIDIRECTIONAL VS UNIDIRECTIONAL COMPARISON
# ==============================================================================

compare_bidirectional_unidirectional <- function(m_bidir, m_unidir, 
                                                   model_data_full, model_data_uni) {
  
  cat("\n===== BIDIRECTIONAL VS UNIDIRECTIONAL COMPARISON =====\n")
  cat("Purpose: Quantify information lost by ignoring regressions\n\n")
  
  # Transition coverage
  n_bidir <- nrow(model_data_full)
  n_unidir <- nrow(model_data_uni)
  n_regression <- n_bidir - n_unidir
  pct_regression <- 100 * n_regression / n_bidir
  
  cat("Transition Coverage:\n")
  cat("  Bidirectional:", n_bidir, "transitions\n")
  cat("  Unidirectional:", n_unidir, "transitions\n")
  cat("  Regressions excluded:", n_regression, "(", round(pct_regression, 1), "%)\n")
  
  # Regression breakdown
  regression_detail <- model_data_full |>
    dplyr::filter(transition_type == "Regression") |>
    count(from_label, to_label, name = "n") |>
    mutate(
      transition = paste0(from_label, " -> ", to_label),
      pct = round(100 * n / sum(n), 1)
    ) |>
    arrange(desc(n))
  
  cat("\nRegression Transition Detail:\n")
  print(regression_detail)
  
  # Model fit comparison
  fit_bi <- glance_multistate(m_bidir)
  fit_uni <- glance_multistate(m_unidir)
  
  fit_comparison <- tibble(
    Model = c("Bidirectional", "Unidirectional"),
    Events = c(fit_bi$n_events, fit_uni$n_events),
    Transitions_Modeled = c(
      n_distinct(paste(model_data_full$from_state, model_data_full$to_state)),
      n_distinct(paste(model_data_uni$from_state, model_data_uni$to_state))
    ),
    Concordance = c(fit_bi$concordance, fit_uni$concordance)
  )
  
  cat("\nModel Fit:\n")
  print(fit_comparison)
  
  # Coefficient comparison for shared transitions
  coefs_bi <- tidy_multistate_coefs(m_bidir)
  coefs_uni <- tidy_multistate_coefs(m_unidir)
  
  # Match coefficients that exist in both
  coef_comparison <- coefs_bi |>
    dplyr::select(term, transition, trans_type, base_covariate,
           hr_bi = hr, se_bi = std.error) |>
    inner_join(
      coefs_uni |> dplyr::select(term, hr_uni = hr, se_uni = std.error),
      by = "term"
    ) |>
    mutate(
      hr_ratio = hr_bi / hr_uni,
      pct_change = 100 * (hr_bi - hr_uni) / hr_uni
    ) |>
    dplyr::filter(trans_type != "Regression")  # Only compare forward/stagnation
  
  # Summarize coefficient differences
  coef_diff_summary <- coef_comparison |>
    summarise(
      n_coefs = n(),
      mean_hr_ratio = mean(hr_ratio, na.rm = TRUE),
      median_hr_ratio = median(hr_ratio, na.rm = TRUE),
      mean_abs_pct_change = mean(abs(pct_change), na.rm = TRUE),
      correlation = cor(log(hr_bi), log(hr_uni), use = "complete")
    )
  
  cat("\nCoefficient Comparison (shared forward/stagnation transitions):\n")
  cat("  Coefficients compared:", coef_diff_summary$n_coefs, "\n")
  cat("  Mean HR ratio (bi/uni):", round(coef_diff_summary$mean_hr_ratio, 3), "\n")
  cat("  Correlation of log(HR):", round(coef_diff_summary$correlation, 3), "\n")
  
  # Extract regression-specific coefficients (only in bidirectional)
  regression_coefs <- coefs_bi |>
    dplyr::filter(trans_type == "Regression") |>
    dplyr::select(base_covariate, transition, hr, hr_lower, hr_upper, p.value, sig_stars)
  
  cat("\nRegression-Specific Coefficients (only in bidirectional model):\n")
  print(regression_coefs |> dplyr::filter(base_covariate %in% c("female", "age_centered", "time_in_state")))
  
  list(
    transition_coverage = list(
      bidirectional = n_bidir,
      unidirectional = n_unidir,
      regressions = n_regression,
      pct_regression = pct_regression
    ),
    regression_detail = regression_detail,
    fit_comparison = fit_comparison,
    coef_comparison = coef_comparison,
    coef_diff_summary = coef_diff_summary,
    regression_coefs = regression_coefs
  )
}

# ==============================================================================
# SECTION 4B: MULTISTATE VS STORY-TERMINAL COMPARISON
# ==============================================================================

compare_multistate_story_terminal <- function(m_multistate, m_story_terminal,
                                               coefs_multistate, story_data) {
  
  cat("\n===== MULTISTATE VS STORY-TERMINAL COMPARISON =====\n")
  cat("Purpose: Demonstrate information gained by multistate approach\n")
  cat("         vs traditional binary survival analysis\n\n")
  
  ## ----- Model Structure Comparison -----
  cat("1. MODEL STRUCTURE\n")
  cat("   Multistate: Models all transitions between 5 literacy states\n")
  cat("   Story-Terminal: Single binary outcome (reached Story yes/no)\n\n")
  
  # Count unique transitions modeled
  n_transitions_modeled <- coefs_multistate |>
    distinct(from_state, to_state) |>
    nrow()
  
  cat("   Multistate models", n_transitions_modeled, "distinct transitions\n")
  cat("   Story-Terminal models 1 outcome (absorption into Story)\n\n")
  
  ## ----- Coefficient Comparison -----
  cat("2. COEFFICIENT COMPARISON\n")
  cat("   Story-Terminal gives ONE hazard ratio per covariate\n")
  cat("   Multistate gives TRANSITION-SPECIFIC hazard ratios\n\n")
  
  # Extract Story-terminal coefficients
  sm_story <- summary(m_story_terminal)
  coefs_story <- tibble(
    term = rownames(sm_story$coefficients),
    hr_story = exp(sm_story$coefficients[, "coef"]),
    se_story = sm_story$coefficients[, "robust se"],
    p_story = sm_story$coefficients[, "Pr(>|z|)"]
  )
  
  # Extract multistate coefficients for Story-bound transitions only
  # These are transitions where to_state == 5 (Story)
  coefs_to_story <- coefs_multistate |>
    dplyr::filter(to_state == 5) |>
    dplyr::select(base_covariate, from_label, hr_multistate = hr, 
           se_multistate = std.error, p_multistate = p.value)
  
  # Match base covariates
  key_covariates <- c("female", "age_centered", "county_fKilifi", "county_fKitui")
  
  coef_comparison <- coefs_story |>
    dplyr::filter(term %in% key_covariates) |>
    left_join(
      coefs_to_story |> 
        dplyr::filter(base_covariate %in% key_covariates) |>
        group_by(base_covariate) |>
        summarise(
          hr_range = paste0(round(min(hr_multistate), 2), " - ", round(max(hr_multistate), 2)),
          n_transitions = n(),
          hr_mean = mean(hr_multistate),
          hr_sd = sd(hr_multistate),
          .groups = "drop"
        ),
      by = c("term" = "base_covariate")
    )
  
  cat("   Comparison of key covariates:\n\n")
  print(coef_comparison |> dplyr::select(term, hr_story, hr_range, n_transitions))
  
  ## ----- What Story-Terminal Misses -----
  cat("\n3. WHAT STORY-TERMINAL MISSES\n\n")
  
  # a) Transition-specific effects
  cat("   a) Transition-specific variation:\n")
  
  female_effects <- coefs_multistate |>
    dplyr::filter(base_covariate == "female") |>
    dplyr::select(transition, trans_type, hr, p.value, significant)
  
  n_female_sig <- sum(female_effects$significant)
  female_range <- range(female_effects$hr)
  
  cat("      Female HR ranges from", round(female_range[1], 2), "to", 
      round(female_range[2], 2), "\n")
  cat("      Significant in", n_female_sig, "of", nrow(female_effects), "transitions\n")
  cat("      Story-Terminal gives single HR:", round(coefs_story$hr_story[coefs_story$term == "female"], 2), "\n\n")
  
  # b) Regression insights
  cat("   b) Regression insights (completely invisible to Story-Terminal):\n")
  
  regression_coefs <- coefs_multistate |>
    dplyr::filter(trans_type == "Regression") |>
    dplyr::select(transition, base_covariate, hr, p.value, significant)
  
  n_regression_transitions <- n_distinct(paste(regression_coefs$transition))
  n_regression_sig <- regression_coefs |>
    group_by(transition) |>
    summarise(any_sig = any(significant)) |>
    pull(any_sig) |>
    sum()
  
  cat("      Multistate models", n_regression_transitions, "regression transitions\n")
  cat("      with", n_regression_sig, "having significant covariate effects\n")
  cat("      Story-Terminal: Cannot model regression at all\n\n")
  
  # c) Pathway insights
  cat("   c) Pathway insights:\n")
  cat("      Story-Terminal cannot explain WHY Word-level learners have higher\n")
  cat("      Story attainment than Paragraph-level learners (despite being\n")
  cat("      further from Story). Multistate reveals it's due to Para->Word\n")
  cat("      regression risk.\n\n")
  
  ## ----- Absorption Probability Comparison -----
  cat("4. ABSORPTION PROBABILITY BY BASELINE\n")
  
  absorption_by_baseline <- story_data |>
    group_by(baseline_label) |>
    summarise(
      n = n(),
      n_story = sum(reached_story),
      pct_story_empirical = round(100 * mean(reached_story), 1),
      .groups = "drop"
    )
  
  cat("   Empirical Story attainment by baseline level:\n")
  print(absorption_by_baseline)
  
  cat("\n   Note: Story-Terminal model cannot explain the non-monotonic pattern\n")
  cat("   where Word (baseline 3) outperforms Paragraph (baseline 4)\n")
  
  ## ----- Summary -----
  summary_table <- tibble(
    Aspect = c(
      "Transitions modeled",
      "Coefficients per covariate",
      "Can model regression",
      "Pathway-specific insights",
      "Duration dependence (semi-Markov)"
    ),
    Multistate = c(
      as.character(n_transitions_modeled),
      paste0(n_transitions_modeled, " (transition-specific)"),
      "Yes",
      "Yes",
      "Yes (via time_in_state)"
    ),
    Story_Terminal = c(
      "1 (Story vs not)",
      "1 (pooled across all pathways)",
      "No",
      "No",
      "No"
    )
  )
  
  cat("\n5. SUMMARY COMPARISON\n")
  print(summary_table)
  
  list(
    structure_comparison = summary_table,
    coef_comparison = coef_comparison,
    coefs_story = coefs_story,
    coefs_to_story = coefs_to_story,
    absorption_by_baseline = absorption_by_baseline,
    female_effects = female_effects,
    regression_coefs = regression_coefs
  )
}

# ==============================================================================
# SECTION 5: DIAGNOSTICS
# ==============================================================================

run_model_diagnostics <- function(model, model_data, model_name = "Model") {
  
  cat("\n===== DIAGNOSTICS:", model_name, "=====\n")
  
  ## ----- Proportional Hazards Test -----
  cat("\n1. PROPORTIONAL HAZARDS ASSUMPTION\n")
  
  ph_test <- tryCatch(
    cox.zph(model),
    error = function(e) {
      cat("  PH test failed:", e$message, "\n")
      return(NULL)
    }
  )
  
  if (!is.null(ph_test)) {
    ph_table <- as_tibble(ph_test$table, rownames = "term") |>
      mutate(
        violation = p < 0.05,
        trans_code = str_extract(term, "\\d+:\\d+$"),
        from_state = as.integer(str_extract(trans_code, "^\\d+")),
        to_state = as.integer(str_extract(trans_code, "\\d+$")),
        transition = ifelse(
          !is.na(from_state),
          paste0(state_labels[from_state], " -> ", state_labels[to_state]),
          "GLOBAL"
        )
      )
    
    n_violations <- sum(ph_table$violation, na.rm = TRUE)
    global_row <- ph_table |> dplyr::filter(term == "GLOBAL")
    
    cat("  Violations (p < 0.05):", n_violations - 1, "of", nrow(ph_table) - 1, "\n")
    if (nrow(global_row) > 0) {
      cat("  Global test: Chi-sq =", round(global_row$chisq, 1), 
          ", p =", format.pval(global_row$p), "\n")
    }
    
    if (n_violations > 1) {
      cat("\n  Coefficients with PH violations:\n")
      violations <- ph_table |> 
        dplyr::filter(violation, term != "GLOBAL") |>
        dplyr::select(term, transition, chisq, p)
      print(violations)
    }
  } else {
    ph_table <- NULL
  }
  
  ## ----- Model Fit Statistics -----
  cat("\n2. MODEL FIT STATISTICS\n")
  
  fit_stats <- glance_multistate(model)
  
  cat("  Concordance:", round(fit_stats$concordance, 3), 
      "(SE:", round(fit_stats$concordance_se, 3), ")\n")
  cat("  Log-likelihood:", round(fit_stats$logLik, 1), "\n")
  cat("  AIC:", round(fit_stats$AIC, 1), "\n")
  cat("  LR test: Chi-sq =", round(fit_stats$lr_test_stat, 1),
      ", df =", fit_stats$lr_test_df, 
      ", p =", format.pval(fit_stats$lr_test_p), "\n")
  
  ## ----- Influence Diagnostics -----
  cat("\n3. INFLUENCE DIAGNOSTICS\n")
  
  dfb <- tryCatch(
    residuals(model, type = "dfbeta"),
    error = function(e) {
      cat("  dfbeta residuals not available:", e$message, "\n")
      return(NULL)
    }
  )
  
  if (!is.null(dfb)) {
    n_subjects <- n_distinct(model_data$child_id)
    threshold <- 2 / sqrt(n_subjects)
    
    influential_count <- sum(apply(abs(dfb) > threshold, 1, any), na.rm = TRUE)
    
    cat("  Influence threshold: |dfbeta| >", round(threshold, 4), "\n")
    cat("  Observations exceeding threshold:", influential_count,
        "(", round(100 * influential_count / nrow(dfb), 1), "%)\n")
  } else {
    influential_count <- NA
  }
  
  list(
    ph_test = ph_test,
    ph_table = ph_table,
    fit_stats = fit_stats,
    influential_count = influential_count
  )
}

# ==============================================================================
# SECTION 6: SENSITIVITY ANALYSIS
# ==============================================================================

run_sensitivity_analysis <- function(model_data_full, model_data_matched) {
  
  cat("\n===== SENSITIVITY ANALYSIS =====\n")
  
  ## ----- Sample Comparison -----
  cat("\n1. MATCHED VS UNMATCHED SAMPLE CHARACTERISTICS\n")
  
  # Add matched indicator to full data
  matched_ids <- unique(model_data_matched$child_id)
  
  sample_comparison <- model_data_full |>
    mutate(matched = child_id %in% matched_ids) |>
    group_by(matched) |>
    summarise(
      n_learners = n_distinct(child_id),
      n_transitions = n(),
      pct_female = mean(female, na.rm = TRUE),
      mean_age = mean(age_centered, na.rm = TRUE),
      pct_regression = mean(transition_type == "Regression"),
      pct_bungoma = mean(county_f == "Bungoma", na.rm = TRUE),
      pct_kilifi = mean(county_f == "Kilifi", na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(sample = ifelse(matched, "Matched (HH data)", "Unmatched"))
  
  cat("\nSample Characteristics:\n")
  print(sample_comparison)
  
  
  
  ## ----- Statistical Tests -----
  learner_level <- model_data_full |> 
    distinct(child_id, .keep_all = TRUE) |>
    mutate(matched = child_id %in% matched_ids)
  
  gender_test <- chisq.test(table(learner_level$matched, learner_level$female))
  county_test <- chisq.test(table(learner_level$matched, learner_level$county_f))
  
  # t.test requires both groups to exist; check before running
  if (length(unique(learner_level$matched)) == 2) {
    age_test <- t.test(age_centered ~ matched, data = learner_level)
  } else {
    age_test <- list(statistic = NA, p.value = NA)
    cat("  WARNING: Cannot run age t-test - matched variable has only one level\n")
  }
  

  cat("\nStatistical Tests (matched vs unmatched):\n")
  cat("  Gender: Chi-sq =", round(gender_test$statistic, 2), 
      ", p =", format.pval(gender_test$p.value), "\n")
  cat("  County: Chi-sq =", round(county_test$statistic, 2),
      ", p =", format.pval(county_test$p.value), "\n")
  cat("  Age: t =", round(age_test$statistic, 2),
      ", p =", format.pval(age_test$p.value), "\n")
  
  if (county_test$p.value < 0.05) {
    cat("  --> CAUTION: Matched sample differs by county composition\n")
  }
  
  ## ----- Coefficient Stability -----
  cat("\n2. COEFFICIENT STABILITY\n")
  
  # Fit same model on both samples
  # Using simpler model (no volunteer) for comparison
  ## ----- 2: COEFFICIENT STABILITY -----
  cat("\n2. COEFFICIENT STABILITY\n")
  
  # Check if both samples have sufficient data
  if (nrow(model_data_full) > 0 && nrow(model_data_matched) > 0) {
    
    # Fit same model on both samples
    # Using simpler model (no volunteer) for comparison
    m_full <- coxph(
      Surv(tstart, tstop, event, type = "mstate") ~
        female + age_centered + county_f,
      data = model_data_full,
      id = child_id
    )
    
    m_matched <- coxph(
      Surv(tstart, tstop, event, type = "mstate") ~
        female + age_centered + county_f + parent_ed_f,
      data = model_data_matched,
      id = child_id
    )
    
    # Compare coefficients
    coef_stability <- tibble(
      term = names(coef(m_full)),
      full = coef(m_full),
      matched = coef(m_matched)[names(coef(m_full))]
    ) |>
      mutate(
        diff = matched - full,
        pct_diff = 100 * diff / abs(full)
      )
    
    coef_corr <- cor(coef_stability$full, coef_stability$matched, use = "complete")
    
    cat("  Coefficient correlation (full vs matched):", round(coef_corr, 3), "\n")
    cat("  Mean absolute % difference:", 
        round(mean(abs(coef_stability$pct_diff), na.rm = TRUE), 1), "%\n")
    
    if (coef_corr > 0.8) {
      cat("  --> Good stability: coefficients highly correlated\n")
    } else {
      cat("  --> CAUTION: Substantial coefficient differences between samples\n")
    }
    
  } else {
    cat("  WARNING: Insufficient data for coefficient stability analysis\n")
    cat("  model_data_full:", nrow(model_data_full), "rows\n")
    cat("  model_data_matched:", nrow(model_data_matched), "rows\n")
    coef_stability <- NULL
    coef_corr <- NA
  }
  
  
  list(
    sample_comparison = sample_comparison,
    tests = list(gender = gender_test, county = county_test, age = age_test),
    coef_stability = coef_stability,
    coef_correlation = coef_corr
  )
}

# ==============================================================================
# SECTION 7: EXPORT RESULTS
# ==============================================================================

export_results_to_excel <- function(results, output_path) {
  
  cat("\n===== EXPORTING RESULTS TO EXCEL =====\n")
  
  wb <- createWorkbook()
  
  header_style <- createStyle(
    textDecoration = "bold",
    border = "bottom",
    borderColour = "#000000",
    fgFill = "#DCE6F1"
  )
  
  ## ----- Sheet 1: Model Summary -----
  addWorksheet(wb, "Model_Summary")
  
  model_summary <- bind_rows(
    results$fit_primary |> mutate(Model = "Primary: Semi-Markov"),
    results$fit_markov |> mutate(Model = "Comparison: Markov"),
    results$fit_unidir |> mutate(Model = "Comparison: Unidirectional")
  ) |>
    dplyr::select(Model, everything())
  
  writeData(wb, "Model_Summary", model_summary, headerStyle = header_style)
  setColWidths(wb, "Model_Summary", cols = 1:ncol(model_summary), widths = "auto")
  
  ## ----- Sheet 2: Primary Model Coefficients -----
  addWorksheet(wb, "Primary_Coefficients")
  
  coef_export <- results$coefs_primary |>
    dplyr::select(term, base_covariate, transition, trans_type,
           estimate, std.error, hr, hr_lower, hr_upper, 
           p.value, sig_stars, significant) 
  # |>
    # arrange(base_covariate, from_state, to_state)
  
  writeData(wb, "Primary_Coefficients", coef_export, headerStyle = header_style)
  setColWidths(wb, "Primary_Coefficients", cols = 1:ncol(coef_export), widths = "auto")
  
  ## ----- Sheet 3: Sojourn Time Effects -----
  addWorksheet(wb, "Sojourn_Time_Effects")
  
  if (!is.null(results$sm_vs_markov$time_coefs)) {
    writeData(wb, "Sojourn_Time_Effects", 
              results$sm_vs_markov$time_coefs, 
              headerStyle = header_style)
  }
  
  ## ----- Sheet 4: Semi-Markov vs Markov -----
  addWorksheet(wb, "SemiMarkov_vs_Markov")
  
  writeData(wb, "SemiMarkov_vs_Markov",
            results$sm_vs_markov$fit_comparison,
            headerStyle = header_style)
  
  # Add LR test below
  lr_test_df <- tibble(
    Test = "Likelihood Ratio",
    Statistic = results$sm_vs_markov$lr_test$statistic,
    df = results$sm_vs_markov$lr_test$df,
    p_value = results$sm_vs_markov$lr_test$p.value
  )
  writeData(wb, "SemiMarkov_vs_Markov", lr_test_df,
            startRow = nrow(results$sm_vs_markov$fit_comparison) + 3,
            headerStyle = header_style)
  
  ## ----- Sheet 5: Bidirectional vs Unidirectional -----
  addWorksheet(wb, "BiDir_vs_UniDir")
  
  writeData(wb, "BiDir_vs_UniDir",
            results$bi_vs_uni$fit_comparison,
            headerStyle = header_style)
  
  # Add regression detail
  writeData(wb, "BiDir_vs_UniDir",
            results$bi_vs_uni$regression_detail,
            startRow = nrow(results$bi_vs_uni$fit_comparison) + 3,
            headerStyle = header_style)
  
  ## ----- Sheet 6: Regression Coefficients -----
  addWorksheet(wb, "Regression_Coefficients")
  
  writeData(wb, "Regression_Coefficients",
            results$bi_vs_uni$regression_coefs,
            headerStyle = header_style)
  
  ## ----- Sheet 7: PH Diagnostics -----
  addWorksheet(wb, "PH_Diagnostics")
  
  if (!is.null(results$diagnostics$ph_table)) {
    ph_export <- results$diagnostics$ph_table |>
      dplyr::select(term, transition, chisq, df, p, violation)
    writeData(wb, "PH_Diagnostics", ph_export, headerStyle = header_style)
  }
  
  ## ----- Sheet 8: Sensitivity Analysis -----
  addWorksheet(wb, "Sensitivity_Analysis")
  
  writeData(wb, "Sensitivity_Analysis",
            results$sensitivity$sample_comparison,
            headerStyle = header_style)
  
  writeData(wb, "Sensitivity_Analysis",
            results$sensitivity$coef_stability,
            startRow = nrow(results$sensitivity$sample_comparison) + 3,
            headerStyle = header_style)
  
  ## ----- Sheet 9: Story-Terminal Comparison -----
  addWorksheet(wb, "StoryTerminal_Comparison")
  
  if (!is.null(results$multistate_vs_story)) {
    # Structure comparison
    writeData(wb, "StoryTerminal_Comparison",
              results$multistate_vs_story$structure_comparison,
              headerStyle = header_style)
    
    # Coefficient comparison
    writeData(wb, "StoryTerminal_Comparison",
              results$multistate_vs_story$coef_comparison,
              startRow = nrow(results$multistate_vs_story$structure_comparison) + 3,
              headerStyle = header_style)
    
    # Story-terminal coefficients
    writeData(wb, "StoryTerminal_Comparison",
              results$multistate_vs_story$coefs_story,
              startRow = nrow(results$multistate_vs_story$structure_comparison) + 
                         nrow(results$multistate_vs_story$coef_comparison) + 6,
              headerStyle = header_style)
  }
  
  ## ----- Save -----
  saveWorkbook(wb, output_path, overwrite = TRUE)
  cat("Results exported to:", output_path, "\n")
}

# ==============================================================================
# SECTION 8: MAIN ANALYSIS EXECUTION
# ==============================================================================

run_full_model_analysis <- function(model_data_full, model_data_uni, model_data_hh,
                                     output_dir = ".",
                                     excel_filename = "model_results.xlsx") {
  
  cat("============================================================\n")
  cat("MULTISTATE LITERACY TRANSITION MODEL ANALYSIS\n")
  cat("============================================================\n")
  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("\nModel Structure:\n")
  cat("  PRIMARY: Semi-Markov bidirectional (full covariates)\n")
  cat("  COMPARISON 1: Markov (no time_in_state)\n")
  cat("  COMPARISON 2: Unidirectional (no regressions)\n")
  cat("  COMPARISON 3: Story-terminal (binary survival)\n")
  cat("  CLUSTERING: Robust SE by village\n")
  cat("============================================================\n")
  
  ## ----- Filter to volunteer-complete data -----
  # Primary analysis uses data with volunteer information
  model_data_vol <- model_data_full |>
    dplyr::filter(!is.na(vol_female) & !is.na(vol_edlevel_f) & !is.na(vol_age_cat_f))
  
  model_data_uni_vol <- model_data_uni |>
    dplyr::filter(!is.na(vol_female) & !is.na(vol_edlevel_f) & !is.na(vol_age_cat_f))
  
  cat("\nAnalysis sample (volunteer data complete):\n")
  cat("  Bidirectional:", nrow(model_data_vol), "transitions from",
      n_distinct(model_data_vol$child_id), "learners\n")
  cat("  Unidirectional:", nrow(model_data_uni_vol), "transitions\n")
  
  ## ----- Fit Primary Model -----
  m_primary <- fit_primary_semimarkov(model_data_vol)
  
  ## ----- Fit Comparison Models -----
  m_markov <- fit_comparison_markov(model_data_vol)
  m_unidir <- fit_comparison_unidirectional(model_data_uni_vol)
  
  ## ----- Model Building Sequence (for supplementary) -----
  cat("\n===== MODEL BUILDING SEQUENCE =====\n")
  
  m0 <- fit_baseline_null(model_data_full)
  m1 <- fit_demographics_only(model_data_full)
  m2 <- fit_demographics_county(model_data_full)
  m3 <- fit_with_attendance(model_data_full)
  
  ## ----- Extract Coefficients -----
  coefs_primary <- tidy_multistate_coefs(m_primary)
  coefs_markov <- tidy_multistate_coefs(m_markov)
  coefs_unidir <- tidy_multistate_coefs(m_unidir)
  
  ## ----- Comparisons -----
  sm_vs_markov <- compare_semimarkov_markov(m_primary, m_markov)
  bi_vs_uni <- compare_bidirectional_unidirectional(
    m_primary, m_unidir, model_data_vol, model_data_uni_vol
  )
  
  ## ----- Story-Terminal Comparison -----
  # Create story-terminal dataset and fit traditional survival model
  story_data <- create_story_terminal_data(model_data_vol)
  m_story_terminal <- fit_story_terminal(story_data)
  multistate_vs_story <- compare_multistate_story_terminal(
    m_primary, m_story_terminal, coefs_primary, story_data
  )
  
  ## ----- Diagnostics -----
  diagnostics <- run_model_diagnostics(m_primary, model_data_vol, "Primary Semi-Markov")
  
  ## ----- Sensitivity Analysis -----
  sensitivity <- run_sensitivity_analysis(model_data_full, model_data_hh)
  
  ## ----- Compile Results -----
  results <- list(
    models = list(
      primary = m_primary,
      markov = m_markov,
      unidirectional = m_unidir,
      story_terminal = m_story_terminal,
      m0 = m0, m1 = m1, m2 = m2, m3 = m3
    ),
    coefs_primary = coefs_primary,
    coefs_markov = coefs_markov,
    coefs_unidir = coefs_unidir,
    fit_primary = glance_multistate(m_primary),
    fit_markov = glance_multistate(m_markov),
    fit_unidir = glance_multistate(m_unidir),
    sm_vs_markov = sm_vs_markov,
    bi_vs_uni = bi_vs_uni,
    multistate_vs_story = multistate_vs_story,
    story_data = story_data,
    diagnostics = diagnostics,
    sensitivity = sensitivity
  )
  
  ## ----- Export -----
  excel_path <- file.path(output_dir, excel_filename)
  export_results_to_excel(results, excel_path)
  
  
  invisible(results)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

source("00_thesis_setup.R")

# Load prepared data
model_data_full <- readRDS(file.path(paths$processed, "model_data_full.rds"))
model_data_uni <- readRDS(file.path(paths$processed, "model_data_uni.rds"))
model_data_hh <- readRDS(file.path(paths$processed, "model_data_hh.rds"))

# Run analysis
sink(file.path(paths$output, "model_output.txt"), split = TRUE)
results <- run_full_model_analysis(
  model_data_full = model_data_full,
  model_data_uni = model_data_uni,
  model_data_hh = model_data_hh,
  output_dir = paths$output,
  excel_filename = "model_results.xlsx"
)
sink()

# Save model objects
saveRDS(results$models$primary, file.path(paths$processed, "m_primary_semimarkov.rds"))
saveRDS(results$models$markov, file.path(paths$processed, "m_comparison_markov.rds"))
saveRDS(results$models$unidirectional, file.path(paths$processed, "m_comparison_unidir.rds"))
saveRDS(results, file.path(paths$processed, "analysis_results.rds"))
