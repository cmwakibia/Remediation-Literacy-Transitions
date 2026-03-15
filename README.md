# Multistate Survival Analysis: Literacy Transitions in TARL Camps

> MSc Statistical Science Thesis — Strathmore University

Examining literacy progression patterns among learners in Teaching at the Right Level (TaRL) remediation camps using multistate survival models. The analysis tracks transitions between reading proficiency states as a function of cumulative attendance, allowing for both progression and regression.



---

## State Definitions

| Code | State    | Description                           |
|------|----------|---------------------------------------|
| 1    | Beginner | Cannot recognize letters              |
| 2    | Letter   | Recognizes letters, cannot read words |
| 3    | Word     | Reads words, not sentences            |
| 4    | Para     | Reads paragraphs                      |
| 5    | Story    | Reads and comprehends stories         |

### Transition Structure

The bidirectional model allows:

- **Progressive transitions**: 1→2, 1→3, 2→3, 2→4, 3→4, 3→5, 4→5
- **Adjacent regressions**: 2→1, 3→2, 4→3, 5→4

Time variable: **Cumulative attendance days** (not calendar time)

---

## Project Structure

```
msm_analysis/
├── 00_thesis_setup.R              # Packages, paths, theme, utilities
├── 01_thesis_data_prep.R          # Load and prepare data for msm
├── 02_thesis_eda.R                # Exploratory data analysis
├── 03_thesis_multistate_models.R         # Fit multistate models, perform diagnostics, and model comparisons
├── 04_thesis_visualizations.R         # Visualize transitions, probabilities, and HR
├── run_all.R               # Execute complete pipeline
├── data/
│   ├── raw/                # Place .dta files here
│   └── processed/          # RDS files created by scripts
└── output/
    ├── figures/            # PDF and PNG plots
    └── *.csv               # Result tables
    └── *.xlsx               # Combined result tables
```

## Usage

### Data Setup

Place data files in `Data/raw/`:
- `my-village-English-baselines-data.dta`
- `mv-camp-English-2025-wide-data.dta`

These files are not tracked in version control (see `.gitignore`).

### Running the Analysis

**Full pipeline:**
```r
source("RScripts/run_all.R")
```

**Individual scripts** (run in order):
```r
source("RScripts/00_thesis_setup.R")
source("RScripts/01_thesis_data_prep.R")
# ... etc
```

---

## Key Dependencies

| Package      | Purpose                              |
|--------------|--------------------------------------|
| `survival`   | Core survival analysis functions     |
| `coxme`      | Mixed-effects Cox models             |
| `frailtypack`| Frailty models for clustered data    |
| `mice`       | Multiple imputation                  |
| `tidyverse`  | Data manipulation and visualization  |
| `patchwork`  | Combining ggplot figures             |

<details>
<summary>Full sessionInfo()</summary>

```
R version 4.5.3 (2026-03-11 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

attached base packages:
splines, stats, graphics, grDevices, utils, datasets, methods, base

other attached packages:
scales_1.4.0, patchwork_1.3.2, broom_1.0.12, openxlsx_4.2.8.1,
lubridate_1.9.5, stringr_1.6.0, readr_2.2.0, tibble_3.3.1,
tidyverse_2.0.0, frailtypack_3.8.0, survC1_1.0-3, MASS_7.3-65,
doBy_4.7.1, boot_1.3-32, coxme_2.2-22, bdsmatrix_1.3-7,
mice_3.19.0, forcats_1.0.1, purrr_1.2.1, survival_3.8-6,
ggplot2_4.0.2, tidyr_1.3.2, dplyr_1.2.0, haven_2.5.5
```

</details>

---## License

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

You are free to use, share, and adapt this material for any purpose, provided you give appropriate credit.

**Suggested citation:**

> Wakibia, C. M. (2026). *Multistate Survival Analysis: Literacy Transitions in TaRL Remediation Camps* [MSc Thesis, Strathmore University]. GitHub. https://github.com/cmwakibia/Remediation-Literacy-Transitions

---

## Author

**Cyrus Macharia Wakibia**  
MSc Statistical Science Candidate  
Strathmore University