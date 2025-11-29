###############################################
# 02_midus_m1_lpa.R
# Step 2: Latent Profile Analysis (Big Five)
###############################################

library(here)
library(tidyverse)
# library(tidyLPA) # or mclust / flexmix / MplusAutomation, depending on your preference

# 1. Load LPA input (items + scales)
dat <- readRDS(here("data_derived", "midus_m1_lpa_input_big5_items_and_scales.rds"))

# 2. Choose which indicators to use for LPA (e.g., MIDUS scale scores)
lpa_data <- dat %>%
  select(starts_with("big5_scale_")) %>%
  drop_na()

# 3. Fit models with K = 1..6 classes (placeholder)
# TODO: add your preferred LPA implementation (tidyLPA, mclust, or Mplus)

# 4. Compare models (BIC, AIC, entropy, class sizes)
# TODO: summarise fit indices

# 5. Save selected model, posterior class probabilities, and classification error info
# saveRDS(..., here("data_derived", "midus_m1_lpa_results.rds"))

