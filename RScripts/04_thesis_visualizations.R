## ===== MULTISTATE LITERACY ANALYSIS: VISUALIZATION =====
## Author: Cyrus Wakibia
## Purpose: Generate visualizations following Putter et al. (2007) framework
##
## Key visualizations:
## 1. State occupancy probabilities over time (Putter Fig 3)
## 2. Cumulative incidence for Story attainment
## 3. Transition probability heatmaps
## 4. Hazard ratio forest plots
## 5. Sojourn time distributions
## 6. Absorption probability comparison (Bi vs Uni)

library(survival)
library(tidyverse)
library(patchwork)
library(scales)

# ==============================================================================
# SETUP
# ==============================================================================

state_labels <- c("Beginner", "Letter", "Word", "Paragraph", "Story")

state_colors <- c(
  "Beginner" = "#d62728",
  "Letter" = "#ff7f0e", 
  "Word" = "#bcbd22",
  "Paragraph" = "#2ca02c",
  "Story" = "#1f77b4"
)

transition_colors <- c(
  "Progression" = "#2ca02c",
  "Regression" = "#d62728",
  "Stagnation" = "#7f7f7f"
)

county_colors <- c(
  "Bungoma" = "#1f77b4",
  "Kilifi" = "#ff7f0e",
  "Kitui" = "#2ca02c"
)

theme_thesis <- function() {

  theme_minimal() +
    theme(
      text = element_text(family = "sans", size = 11),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    )
}

save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300, output_dir = ".") {
  filepath <- file.path(output_dir, filename)
  ggsave(filepath, plot, width = width, height = height, dpi = dpi, bg = "white")
  cat("Saved:", filepath, "\n")
}

classify_transition <- function(from, to) {
  case_when(
    to > from ~ "Progression",
    to < from ~ "Regression",
    TRUE ~ "Stagnation"
  )
}

# ==============================================================================
# 1. STATE OCCUPANCY OVER TIME (PUTTER FIG 3 STYLE)
# ==============================================================================

compute_empirical_occupancy <- function(model_data) {
  
  max_time <- max(model_data$tstop, na.rm = TRUE)
  time_grid <- seq(0, max_time, by = 1)
  
  # Build learner trajectories
  learner_traj <- model_data |>
    arrange(child_id, tstart) |>
    group_by(child_id) |>
    summarise(
      times = list(c(0, tstop)),
      states = list(c(as.character(first(from)), as.character(event))),
      .groups = "drop"
    )
  
  # Compute state distribution at each time
  occupancy <- map_dfr(time_grid, function(t) {
    
    state_at_t <- map_chr(seq_len(nrow(learner_traj)), function(i) {
      times_i <- learner_traj$times[[i]]
      states_i <- learner_traj$states[[i]]
      idx <- max(which(times_i <= t))
      if (is.infinite(idx) || idx < 1) return(NA_character_)
      states_i[idx]
    })
    
    tibble(state = state_at_t) |>
      filter(!is.na(state)) |>
      count(state, name = "n") |>
      mutate(time = t, total = sum(n), prob = n / total)
  })
  
  occupancy |>
    complete(time = time_grid, state = state_labels, fill = list(n = 0, prob = 0)) |>
    mutate(state = factor(state, levels = state_labels))
}

plot_state_occupancy_area <- function(occupancy_data, title = "State Occupancy Over Time") {
  
  occupancy_data |>
    ggplot(aes(x = time, y = prob, fill = state)) +
    geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
    scale_fill_manual(values = state_colors) +
    scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, by = 5)) +
    labs(
      title = title,
      subtitle = "Proportion of learners in each literacy state",
      x = "Cumulative Instruction Days",
      y = "Proportion",
      fill = "Literacy Level"
    ) +
    theme_thesis() +
    theme(legend.position = "right")
}

plot_state_occupancy_lines <- function(occupancy_data) {
  
  occupancy_data |>
    ggplot(aes(x = time, y = prob, color = state)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = state_colors) +
    scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    labs(
      title = "State Occupancy Probabilities Over Time",
      x = "Cumulative Instruction Days",
      y = "Probability",
      color = "State"
    ) +
    theme_thesis()
}

# ==============================================================================
# 2. CUMULATIVE INCIDENCE / STORY ATTAINMENT
# ==============================================================================

compute_story_attainment <- function(model_data) {
  
  learner_story <- model_data |>
    group_by(child_id) |>
    summarise(
      start_state = first(from),
      reached_story = any(event == "Story"),
      time_to_story = if_else(
        any(event == "Story"),
        min(tstop[event == "Story"]),
        max(tstop)
      ),
      .groups = "drop"
    )
  
  # Kaplan-Meier by starting state
  km_data <- learner_story |>
    group_by(start_state) |>
    group_modify(~ {
      km <- survfit(Surv(time_to_story, reached_story) ~ 1, data = .x)
      tibble(
        time = c(0, km$time),
        surv = c(1, km$surv),
        cumprob = 1 - surv,
        n_risk = c(km$n.risk[1], km$n.risk),
        lower = c(1, 1 - km$upper),
        upper = c(1, 1 - km$lower)
      )
    }) |>
    ungroup()
  
  km_data
}

plot_story_attainment <- function(km_data) {
  
  km_data |>
    ggplot(aes(x = time, y = cumprob, color = start_state, fill = start_state)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    geom_step(linewidth = 0.9) +
    scale_color_manual(values = state_colors[1:4]) +
    scale_fill_manual(values = state_colors[1:4]) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    labs(
      title = "Cumulative Probability of Reaching Story Level",
      subtitle = "By baseline literacy level (with 95% CI)",
      x = "Cumulative Instruction Days",
      y = "Cumulative Probability",
      color = "Baseline Level",
      fill = "Baseline Level"
    ) +
    theme_thesis() +
    theme(legend.position = c(0.85, 0.3))
}

# ==============================================================================
# 3. TRANSITION PROBABILITY HEATMAP
# ==============================================================================

compute_transition_matrix <- function(model_data, by_interval = FALSE) {
  
  if (by_interval) {
    trans_probs <- model_data |>
      count(interval, from_state, to_state, name = "n") |>
      group_by(interval, from_state) |>
      mutate(prob = n / sum(n)) |>
      ungroup()
  } else {
    trans_probs <- model_data |>
      count(from_state, to_state, name = "n") |>
      group_by(from_state) |>
      mutate(prob = n / sum(n)) |>
      ungroup()
  }
  
  trans_probs |>
    mutate(
      from_label = state_labels[from_state],
      to_label = state_labels[to_state],
      trans_type = classify_transition(from_state, to_state)
    )
}

plot_transition_heatmap <- function(trans_matrix, title = "Transition Probability Matrix") {
  
  trans_matrix |>
    mutate(
      from_label = factor(from_label, levels = rev(state_labels[1:4])),
      to_label = factor(to_label, levels = state_labels)
    ) |>
    ggplot(aes(x = to_label, y = from_label, fill = prob)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", prob * 100)), size = 3.5, fontface = "bold") +
    scale_fill_gradient2(
      low = "white", mid = "#a6bddb", high = "#1f77b4",
      midpoint = 0.3, limits = c(0, 1),
      labels = percent_format()
    ) +
    labs(
      title = title,
      subtitle = "Row probabilities (sum to 100% within each origin state)",
      x = "Destination State",
      y = "Origin State",
      fill = "Probability"
    ) +
    theme_thesis() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0)
    ) +
    coord_fixed()
}

plot_transition_heatmap_by_interval <- function(trans_matrix) {
  
  interval_labels <- c("1" = "Baseline to Camp 1", 
                       "2" = "Camp 1 to Camp 2", 
                       "3" = "Camp 2 to Camp 3")
  
  trans_matrix |>
    mutate(
      from_label = factor(from_label, levels = rev(state_labels[1:4])),
      to_label = factor(to_label, levels = state_labels),
      interval_label = interval_labels[as.character(interval)]
    ) |>
    ggplot(aes(x = to_label, y = from_label, fill = prob)) +
    facet_wrap(~interval_label) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", prob * 100)), size = 2.8) +
    scale_fill_gradient2(
      low = "white", mid = "#a6bddb", high = "#1f77b4",
      midpoint = 0.3, limits = c(0, 1),
      labels = percent_format()
    ) +
    labs(
      title = "Transition Probabilities by Program Phase",
      x = "Destination State",
      y = "Origin State",
      fill = "Probability"
    ) +
    theme_thesis() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# ==============================================================================
# 4. HAZARD RATIO FOREST PLOTS
# ==============================================================================

extract_hr_for_plot <- function(model, covariate_pattern = NULL) {
  
  sm <- summary(model)
  coefs <- as_tibble(sm$coefficients, rownames = "term")
  
  hr_data <- coefs |>
    mutate(
      # Parse structure
      base_covariate = str_remove(term, "_\\d+:\\d+$"),
      trans_code = str_extract(term, "\\d+:\\d+$"),
      from_state = as.integer(str_extract(trans_code, "^\\d+")),
      to_state = as.integer(str_extract(trans_code, "\\d+$")),
      
      # Labels
      from_label = state_labels[from_state],
      to_label = state_labels[to_state],
      transition = paste0(from_label, " -> ", to_label),
      trans_type = classify_transition(from_state, to_state),
      
      # HR and CI
      hr = `exp(coef)`,
      # Use robust SE for CI
      se = `robust se`,
      lower = exp(coef - 1.96 * se),
      upper = exp(coef + 1.96 * se),
      
      sig = `Pr(>|z|)` < 0.05
    )
  
  if (!is.null(covariate_pattern)) {
    hr_data <- hr_data |> filter(str_detect(base_covariate, covariate_pattern))
  }
  
  hr_data
}

plot_hr_forest <- function(hr_data, title = "Hazard Ratios by Transition") {
  
  hr_data |>
    mutate(
      transition = fct_reorder(transition, from_state * 10 + to_state)
    ) |>
    ggplot(aes(x = hr, y = transition)) +
    facet_wrap(~base_covariate, scales = "free_x", ncol = 2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25, color = "grey40") +
    geom_point(aes(color = sig, shape = trans_type), size = 2.5) +
    scale_color_manual(
      values = c("TRUE" = "#1f77b4", "FALSE" = "grey60"),
      labels = c("p >= 0.05", "p < 0.05"),
      name = "Significance"
    ) +
    scale_shape_manual(
      values = c("Progression" = 16, "Regression" = 17, "Stagnation" = 15),
      name = "Transition Type"
    ) +
    labs(
      title = title,
      x = "Hazard Ratio (95% CI)",
      y = NULL
    ) +
    theme_thesis() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )
}

plot_hr_single_covariate <- function(hr_data, covariate, title = NULL) {
  
  plot_data <- hr_data |>
    filter(base_covariate == covariate) |>
    mutate(
      transition = fct_reorder(transition, from_state * 10 + to_state),
      color_var = case_when(
        !sig ~ "Not significant",
        hr > 1 ~ "Increased hazard",
        hr < 1 ~ "Decreased hazard"
      )
    )
  
  if (is.null(title)) {
    title <- paste("Hazard Ratios:", covariate)
  }
  
  plot_data |>
    ggplot(aes(x = hr, y = transition)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, color = "grey50") +
    geom_point(aes(fill = color_var), size = 3, shape = 21, stroke = 0.5) +
    scale_fill_manual(
      values = c(
        "Increased hazard" = "#d62728",
        "Decreased hazard" = "#2ca02c",
        "Not significant" = "grey70"
      ),
      name = NULL
    ) +
    labs(
      title = title,
      x = "Hazard Ratio (95% CI)",
      y = NULL
    ) +
    theme_thesis() +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )
}

# ==============================================================================
# 5. SOJOURN TIME DISTRIBUTIONS
# ==============================================================================

compute_sojourn_times <- function(model_data) {
  
  model_data |>
    mutate(
      sojourn = tstop - tstart,
      from_label = factor(from, levels = state_labels),
      trans_type = classify_transition(from_state, to_state)
    ) |>
    filter(sojourn > 0)
}

plot_sojourn_density <- function(sojourn_data) {
  
  sojourn_data |>
    ggplot(aes(x = sojourn, fill = from_label)) +
    facet_wrap(~from_label, scales = "free_y", ncol = 2) +
    geom_histogram(binwidth = 2, color = "white", alpha = 0.8) +
    scale_fill_manual(values = state_colors, guide = "none") +
    labs(
      title = "Sojourn Time Distributions",
      subtitle = "Time spent in each state before transitioning",
      x = "Days in State",
      y = "Count"
    ) +
    theme_thesis()
}

plot_sojourn_by_transition_type <- function(sojourn_data) {
  
  summary_data <- sojourn_data |>
    group_by(from_label, trans_type) |>
    summarise(
      median = median(sojourn),
      q25 = quantile(sojourn, 0.25),
      q75 = quantile(sojourn, 0.75),
      .groups = "drop"
    )
  
  summary_data |>
    ggplot(aes(x = from_label, y = median, fill = trans_type)) +
    geom_col(position = position_dodge(0.8), width = 0.7, color = "grey30", linewidth = 0.3) +
    geom_errorbar(
      aes(ymin = q25, ymax = q75),
      position = position_dodge(0.8),
      width = 0.2,
      color = "grey30"
    ) +
    scale_fill_manual(values = transition_colors) +
    labs(
      title = "Median Sojourn Time by State and Transition Type",
      subtitle = "Error bars show interquartile range",
      x = "Origin State",
      y = "Days in State",
      fill = "Transition Type"
    ) +
    theme_thesis()
}

# ==============================================================================
# 6. ABSORPTION PROBABILITY COMPARISON (BIDIRECTIONAL VS UNIDIRECTIONAL)
# ==============================================================================

plot_absorption_comparison <- function(absorption_data) {
  
  plot_data <- absorption_data |>
    select(baseline, empirical, bidirectional, unidirectional) |>
    pivot_longer(
      cols = c(empirical, bidirectional, unidirectional),
      names_to = "model",
      values_to = "pct"
    ) |>
    mutate(
      model = factor(model, 
                     levels = c("empirical", "bidirectional", "unidirectional"),
                     labels = c("Empirical", "Bidirectional", "Unidirectional"))
    )
  
  plot_data |>
    ggplot(aes(x = baseline, y = pct, fill = model)) +
    geom_col(position = position_dodge(0.8), width = 0.7, color = "grey30", linewidth = 0.3) +
    scale_fill_manual(
      values = c("Empirical" = "#1f77b4", "Bidirectional" = "#2ca02c", "Unidirectional" = "#d62728")
    ) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    labs(
      title = "Story-Level Attainment: Model Comparison",
      subtitle = "Unidirectional models overestimate attainment by ignoring regressions",
      x = "Baseline Literacy Level",
      y = "Probability of Reaching Story (%)",
      fill = "Estimation Method"
    ) +
    theme_thesis() +
    theme(legend.position = "bottom")
}

plot_absorption_by_county <- function(absorption_county) {
  
  absorption_county |>
    ggplot(aes(x = baseline_label, y = pct_story, fill = county)) +
    geom_col(position = position_dodge(0.8), width = 0.7, color = "grey30", linewidth = 0.3) +
    scale_fill_manual(values = county_colors) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    labs(
      title = "Story-Level Attainment by Baseline and County",
      x = "Baseline Literacy Level",
      y = "% Reaching Story Level",
      fill = "County"
    ) +
    theme_thesis() +
    theme(legend.position = "bottom")
}

# ==============================================================================
# 7. REGRESSION-SPECIFIC VISUALIZATIONS
# ==============================================================================

plot_regression_patterns <- function(model_data) {
  
  regressions <- model_data |>
    filter(transition_type == "Regression") |>
    mutate(
      transition = paste0(from, " -> ", event),
      transition = fct_infreq(transition),
      interval_label = case_when(
        interval == 1 ~ "Baseline to Camp 1",
        interval == 2 ~ "Camp 1 to Camp 2",
        interval == 3 ~ "Camp 2 to Camp 3"
      )
    )
  
  regressions |>
    ggplot(aes(x = transition, fill = interval_label)) +
    geom_bar(color = "grey30", linewidth = 0.3) +
    scale_fill_brewer(palette = "Blues") +
    labs(
      title = "Regression Transitions by Type and Program Phase",
      subtitle = paste("N =", nrow(regressions), "regressions (",
                       round(100 * nrow(regressions) / nrow(model_data), 1), "% of all transitions)"),
      x = "Transition",
      y = "Count",
      fill = "Program Phase"
    ) +
    theme_thesis() +
    theme(axis.text.x = element_text(angle = 0))
}

plot_regression_timing <- function(model_data) {
  
  reg_data <- model_data |>
    filter(transition_type == "Regression")
  
  reg_data |>
    ggplot(aes(x = tstop, fill = from)) +
    geom_histogram(binwidth = 3, color = "white", alpha = 0.8) +
    facet_wrap(~from, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = state_colors, guide = "none") +
    labs(
      title = "Timing of Regression Events",
      subtitle = "When do learners regress to lower levels?",
      x = "Cumulative Instruction Days",
      y = "Count"
    ) +
    theme_thesis()
}

# ==============================================================================
# 8. COUNTY COMPARISON VISUALIZATIONS
# ==============================================================================

plot_county_transition_rates <- function(model_data) {
  
  county_rates <- model_data |>
    group_by(county_f, transition_type) |>
    summarise(n = n(), .groups = "drop_last") |>
    mutate(pct = n / sum(n)) |>
    ungroup()
  
  county_rates |>
    ggplot(aes(x = county_f, y = pct, fill = transition_type)) +
    geom_col(position = "fill", color = "grey30", linewidth = 0.3) +
    scale_fill_manual(values = transition_colors) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Transition Type Distribution by County",
      x = "County",
      y = "Proportion",
      fill = "Transition Type"
    ) +
    theme_thesis()
}

plot_county_progression <- function(model_data) {
  
  # State occupancy by county over time
  county_occupancy <- model_data |>
    group_by(county_f) |>
    group_modify(~ {
      compute_empirical_occupancy(.x)
    }) |>
    ungroup()
  
  county_occupancy |>
    filter(state == "Story") |>
    ggplot(aes(x = time, y = prob, color = county_f)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = county_colors) +
    scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
    labs(
      title = "Story-Level Attainment Over Time by County",
      x = "Cumulative Instruction Days",
      y = "Proportion at Story Level",
      color = "County"
    ) +
    theme_thesis()
}

# ==============================================================================
# 9. MARKOV DIAGRAM DATA FOR TIKZ
# ==============================================================================

export_tikz_diagram <- function(model_data, output_path) {
  
  trans_probs <- model_data |>
    count(from_state, to_state) |>
    group_by(from_state) |>
    mutate(prob = n / sum(n)) |>
    ungroup() |>
    filter(prob >= 0.02)  # Only edges with >= 2% probability
  
  # Generate TikZ code
  tikz_lines <- c(
    "% Multistate Markov diagram for literacy transitions",
    "% Generated from R analysis",
    "\\begin{tikzpicture}[",
    "  state/.style={circle, draw, minimum size=1.5cm, font=\\small},",
    "  forward/.style={->, >=stealth, thick, blue!70},",
    "  backward/.style={->, >=stealth, thick, red!70, dashed},",
    "  stay/.style={->, >=stealth, thick, gray},",
    "]",
    "",
    "% States",
    "\\node[state] (beg) at (0,0) {Beginner};",
    "\\node[state] (let) at (3,0) {Letter};",
    "\\node[state] (wrd) at (6,0) {Word};",
    "\\node[state] (par) at (9,0) {Paragraph};",
    "\\node[state, fill=blue!20] (sto) at (12,0) {Story};",
    "",
    "% Transitions"
  )
  
  state_abbrev <- c("beg", "let", "wrd", "par", "sto")
  
  for (i in seq_len(nrow(trans_probs))) {
    from_abbr <- state_abbrev[trans_probs$from_state[i]]
    to_abbr <- state_abbrev[trans_probs$to_state[i]]
    prob <- trans_probs$prob[i]
    
    style <- if (trans_probs$to_state[i] > trans_probs$from_state[i]) {
      "forward"
    } else if (trans_probs$to_state[i] < trans_probs$from_state[i]) {
      "backward"
    } else {
      "stay"
    }
    
    bend <- if (abs(trans_probs$to_state[i] - trans_probs$from_state[i]) > 1) {
      ", bend left=30"
    } else if (trans_probs$to_state[i] < trans_probs$from_state[i]) {
      ", bend right=20"
    } else {
      ""
    }
    
    label_pos <- if (trans_probs$to_state[i] > trans_probs$from_state[i]) "above" else "below"
    
    line <- sprintf("\\draw[%s%s] (%s) to node[%s, font=\\tiny] {%.0f\\%%} (%s);",
                    style, bend, from_abbr, label_pos, prob * 100, to_abbr)
    tikz_lines <- c(tikz_lines, line)
  }
  
  tikz_lines <- c(tikz_lines, "", "\\end{tikzpicture}")
  
  writeLines(tikz_lines, output_path)
  cat("TikZ diagram saved to:", output_path, "\n")
}

# ==============================================================================
# 10. MAIN VISUALIZATION RUNNER
# ==============================================================================

generate_all_visualizations <- function(model, model_data, camp, 
                                         absorption_comparison = NULL,
                                         output_dir = ".") {
  
  cat("============================================================\n")
  cat("GENERATING VISUALIZATIONS\n")
  cat("============================================================\n\n")
  
  plots <- list()
  
  ## ----- 1. State occupancy -----
  cat("1. State occupancy over time...\n")
  occupancy <- compute_empirical_occupancy(model_data)
  plots$occupancy_area <- plot_state_occupancy_area(occupancy)
  plots$occupancy_lines <- plot_state_occupancy_lines(occupancy)
  save_plot(plots$occupancy_area, "fig_state_occupancy_area.png", 10, 6, output_dir = output_dir)
  save_plot(plots$occupancy_lines, "fig_state_occupancy_lines.png", 10, 6, output_dir = output_dir)
  
  ## ----- 2. Cumulative Story attainment -----
  cat("2. Cumulative Story attainment...\n")
  km_data <- compute_story_attainment(model_data)
  plots$story_attainment <- plot_story_attainment(km_data)
  save_plot(plots$story_attainment, "fig_story_attainment.png", 9, 6, output_dir = output_dir)
  
  ## ----- 3. Transition heatmaps -----
  cat("3. Transition probability heatmaps...\n")
  trans_matrix <- compute_transition_matrix(model_data, by_interval = FALSE)
  trans_matrix_int <- compute_transition_matrix(model_data, by_interval = TRUE)
  plots$trans_heatmap <- plot_transition_heatmap(trans_matrix)
  plots$trans_heatmap_int <- plot_transition_heatmap_by_interval(trans_matrix_int)
  save_plot(plots$trans_heatmap, "fig_transition_matrix.png", 8, 6, output_dir = output_dir)
  save_plot(plots$trans_heatmap_int, "fig_transition_matrix_by_interval.png", 12, 5, output_dir = output_dir)
  
  ## ----- 4. Hazard ratio forest plots -----
  cat("4. Hazard ratio forest plots...\n")
  hr_data <- extract_hr_for_plot(model)
  plots$hr_forest <- plot_hr_forest(hr_data)
  plots$hr_female <- plot_hr_single_covariate(hr_data, "female", "Effect of Female Gender on Transition Hazards")
  plots$hr_age <- plot_hr_single_covariate(hr_data, "age_centered", "Effect of Age on Transition Hazards")
  save_plot(plots$hr_forest, "fig_hr_forest_all.png", 14, 12, output_dir = output_dir)
  save_plot(plots$hr_female, "fig_hr_female.png", 9, 7, output_dir = output_dir)
  save_plot(plots$hr_age, "fig_hr_age.png", 9, 7, output_dir = output_dir)
  
  ## ----- 5. Sojourn times -----
  cat("5. Sojourn time distributions...\n")
  sojourn_data <- compute_sojourn_times(model_data)
  plots$sojourn_density <- plot_sojourn_density(sojourn_data)
  plots$sojourn_by_type <- plot_sojourn_by_transition_type(sojourn_data)
  save_plot(plots$sojourn_density, "fig_sojourn_density.png", 10, 8, output_dir = output_dir)
  save_plot(plots$sojourn_by_type, "fig_sojourn_by_type.png", 10, 6, output_dir = output_dir)
  
  ## ----- 6. Regression patterns -----
  cat("6. Regression patterns...\n")
  plots$regression_patterns <- plot_regression_patterns(model_data)
  plots$regression_timing <- plot_regression_timing(model_data)
  save_plot(plots$regression_patterns, "fig_regression_patterns.png", 9, 6, output_dir = output_dir)
  save_plot(plots$regression_timing, "fig_regression_timing.png", 10, 8, output_dir = output_dir)
  
  ## ----- 7. County comparisons -----
  cat("7. County comparisons...\n")
  plots$county_rates <- plot_county_transition_rates(model_data)
  save_plot(plots$county_rates, "fig_county_transition_rates.png", 8, 6, output_dir = output_dir)
  
  ## ----- 8. Absorption comparison (if provided) -----
  if (!is.null(absorption_comparison)) {
    cat("8. Absorption probability comparison...\n")
    plots$absorption_comp <- plot_absorption_comparison(absorption_comparison)
    save_plot(plots$absorption_comp, "fig_absorption_comparison.png", 10, 6, output_dir = output_dir)
  }
  
  ## ----- 9. TikZ diagram -----
  cat("9. Exporting TikZ Markov diagram...\n")
  export_tikz_diagram(model_data, file.path(output_dir, "tikz_markov_diagram.tex"))
  
  cat("\n============================================================\n")
  cat("VISUALIZATION COMPLETE\n")
  cat("Plots saved to:", output_dir, "\n")
  cat("============================================================\n")
  
  invisible(plots)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

source("00_thesis_setup.R")

sink(file.path(paths$output, "output_viz.txt"), split = TRUE)

model_data <- readRDS(file.path(paths$processed, "model_data_full.rds"))
m_main <- readRDS(file.path(paths$processed, "m_primary_semimarkov.rds"))
camp <- readRDS(file.path(paths$processed, "camp_clean.rds"))
results <- readRDS(file.path(paths$processed, "analysis_results.rds"))

#
plots <- generate_all_visualizations(
  model = m_main,
  model_data = model_data,
  camp = camp,
  absorption_comparison = results$absorption$comparison,
  output_dir = "../Figures_2026"
)

sink
# save_plot(plots$occupancy_area, "fig_state_occupancy_area.png", 10, 6, output_dir = "../Figures_2026")
# save_plot(plots$occupancy_lines, "fig_state_occupancy_lines.png", 10, 6, output_dir = output_dir)