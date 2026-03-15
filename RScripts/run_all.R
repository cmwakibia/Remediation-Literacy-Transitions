## ===== MULTISTATE SURVIVAL ANALYSIS: RUN ALL SCRIPTS=====

scripts <- c(
  "00_thesis_setup.R",
  "01_thesis_data_prep.R",
  "02_thesis_eda.R",
  "03_thesis_multistate_models.R",
  "04_thesis_visualizations.R"
)

for (script in scripts) {
  cat("\n--- Running:", script, "---\n")
  tryCatch(
    source(script),
    error = function(e) cat("Error in", script, ":", e$message, "\n")
  )
}

sessionInfo()