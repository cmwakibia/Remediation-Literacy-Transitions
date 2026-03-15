# Multistate Survival Analysis: Literacy Transitions in TARL Camps

## Project Structure

```
msm_analysis/
├── 00_thesis_setup.R              # Packages, paths, theme, utilities
├── 01_thesis_data_prep.R          # Load and prepare data for msm
├── 02_thesis_eda.R                # Exploratory data analysis
├── 03_thesis_multistate_models.R         # Fit multistate models
├── 04_diagnostics.R        # Model diagnostics and fit assessment
├── 05_model_comparison.R   # Compare to simpler approaches
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

## Requirements

```r
> sessionInfo()
R version 4.5.3 (2026-03-11 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=English_Kenya.utf8  LC_CTYPE=English_Kenya.utf8   
[3] LC_MONETARY=English_Kenya.utf8 LC_NUMERIC=C                  
[5] LC_TIME=English_Kenya.utf8    

time zone: Africa/Nairobi
tzcode source: internal

attached base packages:
[1] splines   stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] scales_1.4.0      patchwork_1.3.2   broom_1.0.12      openxlsx_4.2.8.1 
 [5] lubridate_1.9.5   stringr_1.6.0     readr_2.2.0       tibble_3.3.1     
 [9] tidyverse_2.0.0   frailtypack_3.8.0 survC1_1.0-3      MASS_7.3-65      
[13] doBy_4.7.1        boot_1.3-32       coxme_2.2-22      bdsmatrix_1.3-7  
[17] mice_3.19.0       forcats_1.0.1     purrr_1.2.1       survival_3.8-6   
[21] ggplot2_4.0.2     tidyr_1.3.2       dplyr_1.2.0       haven_2.5.5      

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1     timeDate_4052.112    rootSolve_1.8.2.4   
 [4] farver_2.1.2         S7_0.2.1             rpart_4.1.24        
 [7] timechange_0.4.0     lifecycle_1.0.5      Deriv_4.2.0         
[10] statmod_1.5.1        magrittr_2.0.4       compiler_4.5.3      
[13] rlang_1.1.7          tools_4.5.3          utf8_1.2.6          
[16] RColorBrewer_1.1-3   withr_3.0.2          nnet_7.3-20         
[19] grid_4.5.3           jomo_2.7-6           colorspace_2.1-2    
[22] iterators_1.0.14     cli_3.6.5            marqLevAlg_2.0.8    
[25] reformulas_0.4.4     generics_0.1.4       rstudioapi_0.18.0   
[28] modelr_0.1.11        tzdb_0.5.0           minqa_1.2.8         
[31] forecast_9.0.1       parallel_4.5.3       urca_1.3-4          
[34] vctrs_0.7.1          glmnet_4.1-10        Matrix_1.7-4        
[37] hms_1.1.4            mitml_0.4-5          foreach_1.5.2       
[40] glue_1.8.0           nloptr_2.2.1         pan_1.9             
[43] codetools_0.2-20     cowplot_1.2.0        stringi_1.8.7       
[46] shape_1.4.6.1        gtable_0.3.6         lme4_2.0-1          
[49] pillar_1.11.1        R6_2.6.1             microbenchmark_1.5.0
[52] matrixcalc_1.0-6     Rdpack_2.6.6         doParallel_1.0.17   
[55] lattice_0.22-9       rbibutils_2.4.1      backports_1.5.0     
[58] fracdiff_1.5-3       Rcpp_1.1.1           zip_2.3.3           
[61] nlme_3.1-168         fs_1.6.7             zoo_1.8-15          
[64] pkgconfig_2.0.3     

```

## Usage

1. Place data files in `data/raw/`:
   - `my-village-English-baselines-data.dta`
   - `mv-camp-English-2025-wide-data.dta`

2. Run complete analysis:
```r
source("run_all.R")
```

Or run individual scripts in order.

## State Definitions

| Code | State    | Description                          |
|------|----------|--------------------------------------|
| 1    | Beginner | Cannot recognize letters             |
| 2    | Letter   | Recognizes letters, cannot read words|
| 3    | Word     | Reads words, not sentences           |
| 4    | Para     | Reads paragraphs                     |
| 5    | Story    | Reads and comprehends stories        |

## Model Specification

The bidirectional model allows:
- Progressive transitions: 1→2, 1→3, 2→3, 2→4, 3→4, 3→5, 4→5
- Adjacent regressions: 2→1, 3→2, 4→3, 5→4
- Story (5) treated as absorbing in some specifications

Time variable: Cumulative attendance days (not calendar time)
