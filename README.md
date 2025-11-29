# LPAmidus
Big Five and Work Outcomes - Latent Profile Analysis using MIDUS datasets
# MIDUS Personality Profiles and Work Outcomes (LPA Project)

This repository hosts a fully reproducible, three-part analysis of Big Five personality profiles and work outcomes using MIDUS 1 (Project 1 Core Survey).

## Project Overview

**Goal:**  
Use latent profile analysis (LPA) of Big Five personality to predict subsequent work outcomes (e.g., job attitudes, stress, leadership roles).

**Data:**  
- MIDUS 1, Project 1 Core Survey (`M1_P1_SURVEY_N7108_20190116.sav`)
- Big Five: Neuroticism, Extraversion, Openness, Conscientiousness, Agreeableness (MIDI adjectives)

## Analysis Plan (3 Steps)

1. **Step 1 – Measurement model (R/01_midus_m1_big5_items_and_scales.R)**  
   - Import MIDUS 1 core survey  
   - Extract Big Five *items* and *pre-scored scales*  
   - Score traits from items  
   - Compare item-based scores to official MIDUS scale variables  
   - Produce cleaned, LPA-ready personality dataset (`data_derived/`)

2. **Step 2 – Latent Profile Analysis (R/02_midus_m1_lpa.R)**  
   - Use Big Five scores as indicators in LPA  
   - Enumerate 1–K class solutions  
   - Select optimal K using BIC, AIC, LMR/BLRT, entropy, and interpretability  
   - Save posterior class probabilities and classification error info

3. **Step 3 – 3-Step Prediction of Work Outcomes (R/03_midus_m1_3step_outcomes.R)**  
   - Implement BCH / 3-step methods to relate profiles to:  
     - Work attitudes (e.g., job satisfaction, stress)  
     - Work roles (e.g., supervisory status, leadership proxies)  
     - Economic outcomes (income, prestige)  
   - Adjust for classification error when estimating profile–outcome relations

## Directory Structure

- `R/` – All analysis scripts  
- `data_raw/` – Original MIDUS files (not tracked in git; see Data Access)  
- `data_derived/` – Cleaned and processed data objects (RDS/CSV)  
- `outputs/` – Tables, figures, model summaries  
- `docs/` – Supporting documentation (variable labels, notes, etc.)  
- `.github/workflows/` – GitHub Actions CI for running analyses  
- `.devcontainer/` – Codespaces / container setup for a reproducible R environment  

## Data Access

The MIDUS data are distributed by ICPSR.  
To reproduce the analyses:

1. Register and download `M1_P1_SURVEY_N7108_20190116.sav` from ICPSR.  
2. Place the file in `data_raw/` (this directory is git-ignored).  
3. Run `R/01_midus_m1_big5_items_and_scales.R`.

## Reproducible Environment

You can run this project:

- Locally (R + RStudio / VS Code)  
- In GitHub Codespaces (via `.devcontainer/devcontainer.json`)  
- Via GitHub Actions CI (`.github/workflows/r-analysis.yml`)

## License

- Code: MIT (or your preferred license)  
- Data: Subject to MIDUS / ICPSR terms of use.
