###############################################
# 01_midus_m1_big5_items_and_scales.R
# Step 1: Clean, score, and compare Big Five
# Data: MIDUS 1, Project 1 Core Survey
# File expected at: data_raw/M1_P1_SURVEY_N7108_20190116.sav
###############################################

## 0. SETUP ---------------------------------------------------------------

# Load packages (install them first if needed)
suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(haven)
  library(janitor)
  library(skimr)
  library(psych)
  library(Hmisc)
  library(labelled)
})

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

# Ensure project directories exist
dir.create(here("data_raw"),     showWarnings = FALSE)
dir.create(here("data_derived"), showWarnings = FALSE)
dir.create(here("outputs"),      showWarnings = FALSE)
dir.create(here("docs"),         showWarnings = FALSE)

# Path to MIDUS 1 core survey
raw_file <- here("data_raw", "M1_P1_SURVEY_N7108_20190116.sav")


## 1. IMPORT RAW DATA -----------------------------------------------------

message("Reading raw MIDUS 1 SPSS file...")
midus_raw <- haven::read_sav(raw_file)

# Clean names (e.g., A1SNEURO -> a1sneuro)
midus <- midus_raw %>%
  janitor::clean_names()

saveRDS(midus, here("data_derived", "midus_m1_clean_names.rds"))

message("Rows: ", nrow(midus), " | Cols: ", ncol(midus))


## 2. VARIABLE METADATA (SCALES + ITEMS) ----------------------------------

message("Building variable metadata table...")

var_meta <- tibble(
  var_name_raw   = names(midus_raw),
  var_name_clean = names(midus),
  var_label      = sapply(midus_raw, function(x) attr(x, "label"))
)

write_csv(var_meta, here("docs", "midus_m1_variable_labels.csv"))

# Big Five scale variables in MIDUS 1:
# A1SNEURO, A1SEXTRA, A1SOPEN, A1SCONS, A1SAGREE

big5_scales_raw <- c("A1SNEURO", "A1SEXTRA", "A1SOPEN", "A1SCONS", "A1SAGREE")

big5_scales_clean <- var_meta %>%
  filter(var_name_raw %in% big5_scales_raw) %>%
  pull(var_name_clean)

names(big5_scales_clean) <- c(
  "neuroticism",
  "extraversion",
  "openness",
  "conscientiousness",
  "agreeableness"
)

message("Detected Big Five scales (clean names):")
print(big5_scales_clean)

# ID variable: prefer M2ID if available, else fall back to first good candidate
id_candidates <- var_meta %>%
  filter(
    str_detect(var_name_raw, "M2ID") |
      str_detect(tolower(var_label), "respondent id|case id|id number")
  )

if (nrow(id_candidates) == 0) {
  stop("No ID variable automatically detected. Inspect docs/midus_m1_variable_labels.csv and set id_var manually.")
} else {
  id_var <- id_candidates$var_name_clean[1]
  message("Using ", id_var, " as respondent ID.")
}


## 3. LOCATE BIG FIVE ITEM-LEVEL VARIABLES --------------------------------

message("Searching for Big Five adjective items by label text...")

# Keyword patterns for each trait, based on MIDI adjectives
trait_patterns <- list(
  neuroticism = c("moody", "worrying", "nervous", "calm"),
  extraversion = c("outgoing", "friendly", "lively", "active", "talkative"),
  openness = c("creative", "imaginative", "intelligent", "curious",
               "broad-minded", "broad minded", "sophisticated", "adventurous"),
  conscientiousness = c("organized", "responsible", "hardworking", "careless"),
  agreeableness = c("helpful", "warm", "caring", "softhearted", "soft-hearted", "sympathetic")
)

# Helper to find item vars by set of keywords in labels
find_items_by_label <- function(pattern_vec, meta) {
  pattern <- paste(pattern_vec, collapse = "|")
  meta %>%
    filter(str_detect(tolower(var_label), pattern)) %>%
    pull(var_name_clean) %>%
    unique()
}

big5_items <- list(
  neuroticism       = find_items_by_label(trait_patterns$neuroticism, var_meta),
  extraversion      = find_items_by_label(trait_patterns$extraversion, var_meta),
  openness          = find_items_by_label(trait_patterns$openness, var_meta),
  conscientiousness = find_items_by_label(trait_patterns$conscientiousness, var_meta),
  agreeableness     = find_items_by_label(trait_patterns$agreeableness, var_meta)
)

message("Item-level variables detected:")
print(big5_items)

expected_counts <- c(
  neuroticism       = 4,
  extraversion      = 5,
  openness          = 7,
  conscientiousness = 4,
  agreeableness     = 5
)

for (tr in names(big5_items)) {
  n_found <- length(big5_items[[tr]])
  message("  Trait ", tr, ": found ", n_found, " items (expected ~", expected_counts[[tr]], ")")
}

# If counts are off, user can inspect:
# var_meta %>% filter(var_name_clean %in% big5_items$neuroticism)


## 4. EXTRACT PERSONALITY DATA (ITEMS + SCALES + ID) ----------------------

personality_vars <- c(
  id_var,
  unlist(big5_items),
  big5_scales_clean
) %>% unique()

midus_big5 <- midus %>%
  select(all_of(personality_vars))

glimpse(midus_big5)


## 5. INSPECT RAW CODING FOR ADJECTIVE ITEMS ------------------------------

# Pick one neuroticism item as an example to inspect value labels
example_item_name <- big5_items$neuroticism[1]
example_item_raw_name <- var_meta %>%
  filter(var_name_clean == example_item_name) %>%
  pull(var_name_raw)

message("Example item for coding check: ", example_item_name,
        " (raw: ", example_item_raw_name, ")")
message("SPSS value labels for that item:")
print(val_labels(midus_raw[[example_item_raw_name]]))

# We assume coding is 1 = "A lot", 2 = "Some", 3 = "A little", 4 = "Not at all"
# We will reverse 1–4 so higher = more adjective endorsement.


## 6. RECODE ITEMS (HIGHER = MORE ADJECTIVE) ------------------------------

message("Converting item variables to numeric and reverse-coding...")

# Convert all item vars to numeric
midus_big5_num <- midus_big5 %>%
  mutate(across(all_of(unlist(big5_items)), ~ as.numeric(.))) %>%
  mutate(across(all_of(big5_scales_clean), ~ as.numeric(.)))

# Global reverse: 1–4 => 4–1
reverse_1to4 <- function(x) ifelse(is.na(x), NA_real_, 5 - x)

midus_big5_items_rev <- midus_big5_num %>%
  mutate(across(all_of(unlist(big5_items)), reverse_1to4))

# Identify conceptually reversed adjectives: calm (Neuroticism) and careless (Conscientiousness)
reverse_label_patterns <- c("calm", "careless")

reverse_items <- var_meta %>%
  filter(
    var_name_clean %in% unlist(big5_items),
    str_detect(tolower(var_label), paste(reverse_label_patterns, collapse = "|"))
  ) %>%
  pull(var_name_clean)

message("Detected reverse-keyed adjectives needing extra flip:")
print(reverse_items)

midus_big5_items_rev <- midus_big5_items_rev %>%
  mutate(across(all_of(reverse_items), reverse_1to4))


## 7. COMPUTE ITEM-BASED SCALE SCORES -------------------------------------

message("Computing item-based Big Five scale scores...")

compute_scale_mean <- function(df, items, min_frac = 0.5) {
  n_items <- length(items)
  df %>%
    mutate(
      n_nonmiss = rowSums(!is.na(across(all_of(items)))),
      mean_score = ifelse(
        n_nonmiss >= ceiling(n_items * min_frac),
        rowMeans(across(all_of(items)), na.rm = TRUE),
        NA_real_
      )
    ) %>%
    pull(mean_score)
}

midus_big5_scored <- midus_big5_items_rev %>%
  mutate(
    big5_item_n = compute_scale_mean(cur_data(), big5_items$neuroticism),
    big5_item_e = compute_scale_mean(cur_data(), big5_items$extraversion),
    big5_item_o = compute_scale_mean(cur_data(), big5_items$openness),
    big5_item_c = compute_scale_mean(cur_data(), big5_items$conscientiousness),
    big5_item_a = compute_scale_mean(cur_data(), big5_items$agreeableness)
  )

# Attach original MIDUS scale scores with clear names
names_original <- setNames(
  big5_scales_clean,
  nm = paste0("big5_scale_", names(big5_scales_clean))
)

midus_big5_scored <- midus_big5_scored %>%
  rename(!!!names_original)

# Keep ID as well
midus_big5_scored <- midus_big5_scored %>%
  select(all_of(id_var), starts_with("big5_item_"), starts_with("big5_scale_"))

glimpse(midus_big5_scored)


## 8. DESCRIPTIVES: ITEM-BASED VS MIDUS SCORES ----------------------------

message("Computing descriptives for item-based and MIDUS scales...")

# Item-based
desc_item <- midus_big5_scored %>%
  select(starts_with("big5_item_")) %>%
  psych::describe()

# MIDUS-provided scales
desc_scale <- midus_big5_scored %>%
  select(starts_with("big5_scale_")) %>%
  psych::describe()

write.csv(desc_item,
          here("outputs", "midus_m1_big5_descriptives_item_based.csv"),
          row.names = TRUE)

write.csv(desc_scale,
          here("outputs", "midus_m1_big5_descriptives_midus_scales.csv"),
          row.names = TRUE)

# Combined table for quick comparison
desc_item_tbl <- desc_item %>%
  as_tibble(rownames = "var") %>%
  mutate(type = "item_based")

desc_scale_tbl <- desc_scale %>%
  as_tibble(rownames = "var") %>%
  mutate(type = "midus_scale")

desc_combined <- bind_rows(desc_item_tbl, desc_scale_tbl) %>%
  select(type, var, n, mean, sd, min, max)

write.csv(desc_combined,
          here("outputs", "midus_m1_big5_descriptives_combined.csv"),
          row.names = FALSE)

message("Combined descriptives (first few rows):")
print(head(desc_combined))


## 9. CORRELATIONS: ITEM-BASED VS MIDUS SCALES ----------------------------

message("Computing correlations between item-based and MIDUS scales...")

pair_vars <- tibble(
  trait    = c("neuroticism", "extraversion", "openness", "conscientiousness", "agreeableness"),
  item_var = c("big5_item_n", "big5_item_e", "big5_item_o", "big5_item_c", "big5_item_a"),
  scale_var = paste0(
    "big5_scale_",
    c("neuroticism", "extraversion", "openness", "conscientiousness", "agreeableness")
  )
)

cor_results <- map_dfr(
  1:nrow(pair_vars),
  function(i) {
    iv <- pair_vars$item_var[i]
    sv <- pair_vars$scale_var[i]
    dat <- midus_big5_scored %>%
      select(all_of(c(iv, sv))) %>%
      drop_na()
    r <- cor(dat[[iv]], dat[[sv]])
    tibble(
      trait    = pair_vars$trait[i],
      item_var = iv,
      scale_var = sv,
      r = r
    )
  }
)

write.csv(cor_results,
          here("outputs", "midus_m1_big5_item_vs_scale_correlations.csv"),
          row.names = FALSE)

message("Item vs MIDUS scale correlations:")
print(cor_results)

# Full correlation matrix among all 10 variables
corr_mat <- midus_big5_scored %>%
  select(starts_with("big5_item_"), starts_with("big5_scale_")) %>%
  drop_na()

rc <- Hmisc::rcorr(as.matrix(corr_mat), type = "pearson")

sink(here("outputs", "midus_m1_big5_item_and_scale_correlation_matrix.txt"))
cat("Correlation matrix (r):\n")
print(rc$r)
cat("\nP-values:\n")
print(rc$P)
sink()


## 10. SAVE LPA-READY DATASET ---------------------------------------------

message("Saving LPA-ready dataset with both item-based and MIDUS scales...")

midus_m1_lpa_input <- midus_big5_scored

saveRDS(midus_m1_lpa_input,
        here("data_derived", "midus_m1_lpa_input_big5_items_and_scales.rds"))

write_csv(midus_m1_lpa_input,
          here("data_derived", "midus_m1_lpa_input_big5_items_and_scales.csv"),
          na = "")

message("Done. Step 1 complete.")
###############################################
# End of script
###############################################

