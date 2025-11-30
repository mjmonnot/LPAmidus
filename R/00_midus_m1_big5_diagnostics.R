# R/00_midus_m1_big5_diagnostics.R
# Purpose: Inspect MIDUS M1 Big Five adjective items + current scale scores

library(haven)
library(dplyr)
library(janitor)
library(readr)
library(purrr)

# -------------------------------------------------------------------
# 1. Paths & data load
# -------------------------------------------------------------------
data_path <- "data_raw/M1_P1_SURVEY_N7108_20190116.sav"

midus_m1_raw <- read_sav(data_path)

midus_m1 <- midus_m1_raw %>%
  clean_names()

# Convenience metadata
var_meta <- tibble(
  var_name_raw   = names(midus_m1_raw),
  var_name_clean = names(midus_m1),
  label          = sapply(midus_m1_raw, function(x) attr(x, "label")),
  class          = sapply(midus_m1_raw, function(x) paste(class(x), collapse = ","))
)

# -------------------------------------------------------------------
# 2. Explicit item lists based on KLB033* adjective items
#    (used in MIDUS/HRS MIDI personality scales)
# -------------------------------------------------------------------
big5_items_raw <- list(
  neuroticism = c("KLB033C", "KLB033G", "KLB033K", "KLB033P"),  # moody, worrying, nervous, calm (R)
  extraversion = c("KLB033A", "KLB033E", "KLB033I", "KLB033S", "KLB033W"), # outgoing, friendly, lively, active, talkative
  agreeableness = c("KLB033B", "KLB033F", "KLB033J", "KLB033O", "KLB033V"), # helpful, warm, caring, softhearted, sympathetic
  conscientiousness = c("KLB033D", "KLB033H", "KLB033M", "KLB033T", "KLB033Z"), # organized, responsible, hardworking, careless (R), thorough
  openness = c("KLB033L", "KLB033N", "KLB033Q", "KLB033R", "KLB033U", "KLB033X", "KLB033Y") # creative, imaginative, intelligent, curious, broad-minded, sophisticated, adventurous
)

# Translate to cleaned names present in midus_m1
big5_items_clean <- lapply(big5_items_raw, function(raw_names) {
  var_meta %>%
    filter(var_name_raw %in% raw_names) %>%
    arrange(match(var_name_raw, raw_names)) %>%
    pull(var_name_clean)
})

big5_items_clean

# -------------------------------------------------------------------
# 3. Inspect raw value distributions for each item
#    (looking for values outside 1–4)
# -------------------------------------------------------------------

all_item_vars <- unlist(big5_items_clean)

if (length(all_item_vars) == 0) {
  message("No KLB033* MIDI adjective items were found in this dataset ",
          "using the assumed raw names. We now need to discover the actual ",
          "item variable names by searching labels for key adjectives.")

  # Show a sample of variable labels that mention MIDI adjectives
  library(stringr)

  key_words <- c("moody", "worry", "nervous", "calm",
                 "outgoing", "friendly", "lively", "active", "talkative",
                 "helpful", "warm", "caring", "softhearted", "sympathetic",
                 "organized", "responsible", "hardworking", "careless",
                 "creative", "imaginative", "intelligent", "curious",
                 "broad-minded", "sophisticated", "adventurous")

  pattern <- str_c(key_words, collapse = "|")

  candidate_items <- var_meta %>%
    dplyr::filter(!is.na(label),
                  stringr::str_detect(tolower(label), pattern))

  print(candidate_items)

  # Stop here; once we know the correct var_name_raw values,
  # we will update big5_items_raw accordingly.
} else {
  inspect_item <- function(data, var_name) {
    x <- data[[var_name]]
    tibble(
      var_name   = var_name,
      class      = paste(class(x), collapse = ","),
      n          = sum(!is.na(x)),
      min        = suppressWarnings(min(as.numeric(x), na.rm = TRUE)),
      max        = suppressWarnings(max(as.numeric(x), na.rm = TRUE))
    )
  }

  item_summary <- purrr::map_dfr(all_item_vars, ~inspect_item(midus_m1, .x))
  print(item_summary)

  item_freqs <- all_item_vars %>%
    purrr::map(function(v) {
      x <- midus_m1[[v]]
      tibble(
        var_name = v,
        value    = as.numeric(x)
      )
    }) %>%
    bind_rows() %>%
    group_by(var_name, value) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(var_name, value)

  readr::write_csv(item_freqs, "outputs/diagnostics_midus_m1_big5_item_freqs.csv")
}
# Save candidate items to CSV for inspection
readr::write_csv(candidate_items, "outputs/diagnostics_midus_m1_big5_candidate_items.csv")

# -------------------------------------------------------------------
# 4. Inspect *existing* scale scores if they already exist in your data
#    (A1SNEURO, A1SEXTRA, A1SAGREE, A1SCONS*, A1SOPEN)
# -------------------------------------------------------------------
scale_vars_raw <- c("A1SNEURO", "A1SEXTRA", "A1SAGREE", "A1SCONS", "A1SCONS1", "A1SCONS2", "A1SOPEN")

scale_vars_clean <- var_meta %>%
  filter(var_name_raw %in% scale_vars_raw) %>%
  pull(var_name_clean)

scale_summary <- map_dfr(scale_vars_clean, ~inspect_item(midus_m1, .x))

print(scale_summary)

write_csv(scale_summary, "outputs/diagnostics_midus_m1_big5_scale_summary.csv")

# -------------------------------------------------------------------
# 5. (Optional) Inspect *your* newly created item-based scales
#    If in your Step 1 script you named them e.g. big5_item_n, big5_item_e, etc.
# -------------------------------------------------------------------
new_scales <- c("big5_item_n", "big5_item_e", "big5_item_a", "big5_item_c", "big5_item_o")
new_scales <- intersect(new_scales, names(midus_m1))

if (length(new_scales) > 0) {
  new_scale_summary <- map_dfr(new_scales, ~inspect_item(midus_m1, .x))
  print(new_scale_summary)
  write_csv(new_scale_summary, "outputs/diagnostics_midus_m1_new_big5_scale_summary.csv")
} else {
  message("No new item-based scale variables with expected names found in dataset.")
}
