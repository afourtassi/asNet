# Export the Wordbank English 2PL item bank and ability norms from adaptive-cdi
# into this repo, so the anchored DIF analysis doesn't depend on that checkout.
#
# Source model: adaptive-cdi/all_data/eng_ws_wg_mod_2pl.Rds, a unidimensional 2PL
# fit (mirt) to Wordbank English WS production plus WG production (>= 12 months,
# WG items mapped onto WS definitions) plus CDI-III 31-36-month-olds;
# 7,633 children, 680 WS items. See adaptive_cdi_eng_production_12-36mos.Rmd.
#
# Usage: Rscript helper_functions/export_wordbank_params.R [path/to/adaptive-cdi]
suppressPackageStartupMessages({library(tidyverse); library(mirt)})
src <- commandArgs(TRUE)[1]
if (is.na(src)) src <- "../adaptive-cdi"

e <- new.env(); load(file.path(src, "all_data/eng_ws_wg_mod_2pl.Rds"), envir = e)
d <- new.env(); load(file.path(src, "data/wordbank_eng_ws_wg_webcdi31-36mos.Rds"), envir = d)

cf <- coef(e$mod_2pl, simplify = TRUE)$items
items <- tibble(definition = rownames(cf), a = cf[, "a1"], d = cf[, "d"]) %>%
  mutate(b = -d / a)  # difficulty: ability at which P(produce) = .5
write_csv(items, "in_files/wordbank_2pl_items.csv")

# Ability (MAP) by age in the calibration sample
abil <- e$fscores_2pl %>% mutate(data_id = as.numeric(data_id)) %>%
  inner_join(d$d_demo %>% select(data_id, age, production), by = "data_id")
norms <- abil %>% group_by(age) %>%
  summarise(n = n(), mean = mean(ability), sd = sd(ability),
            q10 = quantile(ability, .10), q25 = quantile(ability, .25),
            q50 = median(ability), q75 = quantile(ability, .75),
            q90 = quantile(ability, .90), .groups = "drop")
write_csv(norms, "in_files/wordbank_2pl_ability_norms.csv")
cat("wrote", nrow(items), "items and", nrow(norms), "age rows\n")
