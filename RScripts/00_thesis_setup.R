## ===== MULTISTATE LITERACY ANALYSIS: SETUP =====

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(purrr)
library(forcats)
library(mice)
library(coxme)
library(frailtypack)

## ===== PATHS =====

# paths <- list(
#   raw = "data/raw",
#   processed = "data/processed",
#   figures = "output/figures",
#   tables = "output/tables"
# )

## ===== PATHS =====

paths <- list(
  raw_baseline = "../Data/my-village-English-baselines-data.dta",
  raw_camp     = "../Data/mv-camp-English-2025-wide-data.dta",
  processed    = "../processed/",
  output       = "../output/",
  figures      = "../output/figures/",
  tables      = "../output/tables/"
)


fs::dir_create(
  paths$processed,
  paths$output,
  paths$figures,
  paths$tables
)



## ===== STATE DEFINITIONS =====

state_labels <- c("Beginner", "Letter", "Word", "Paragraph", "Story")
state_codes <- setNames(1:5, state_labels)

## ===== CUSTOM ACADEMIC THEME =====

theme_academic <- function(base_size = 11, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.5),
      axis.ticks = element_line(color = "grey30", linewidth = 0.3),
      axis.text = element_text(color = "grey20"),
      axis.title = element_text(color = "grey10", face = "plain"),
      plot.title = element_text(size = rel(1.1), face = "bold", hjust = 0, 
                                margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.9), hjust = 0, color = "grey30",
                                   margin = margin(b = 10)),
      plot.caption = element_text(size = rel(0.75), hjust = 1, color = "grey50",
                                  margin = margin(t = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9)),
      legend.text = element_text(size = rel(0.85)),
      strip.text = element_text(face = "bold", size = rel(0.95)),
      strip.background = element_rect(fill = "grey95", color = NA)
    )
}

theme_set(theme_academic())

## ===== COLOR PALETTES =====

state_colors <- c(
  "Beginner" = "#d62728",
  "Letter" = "#ff7f0e", 
  "Word" = "#2ca02c",
  "Paragraph" = "#1f77b4",
  "Story" = "#9467bd"
)

county_colors <- c(
  "Bungoma" = "#1b9e77",
  "Kilifi" = "#d95f02",
  "Kitui" = "#7570b3"
)

transition_colors <- c(
  "Progression" = "#2ca02c",
  "Stagnation" = "#7f7f7f",
  "Regression" = "#d62728"
)

## ===== HELPER FUNCTIONS =====

classify_transition <- function(from, to) {
  case_when(
    is.na(from) | is.na(to) ~ NA_character_,
    to > from ~ "Progression",
    to == from ~ "Stagnation",
    to < from ~ "Regression"
  )
}

compute_cumulative_time <- function(days1, days2, days3, camp_num) {
  case_when(
    camp_num == 0 ~ 0,
    camp_num == 1 ~ days1,
    camp_num == 2 ~ days1 + days2,
    camp_num == 3 ~ days1 + days2 + days3
  )
}

format_pct <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

save_plot <- function(plot, filename, width = 7, height = 5) {
  ggsave(
    file.path(paths$figures, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

cat("Setup complete\n")
