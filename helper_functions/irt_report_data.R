# Collects aggregate results for the IRT/DIF report (no child-level data).
# Run after 01, 03 and 04 (default and exclude_dx_unconfirmed) have been rendered.
# Usage: Rscript helper_functions/irt_report_data.R <out.json>
suppressPackageStartupMessages({library(tidyverse); library(jsonlite)})
out <- commandArgs(TRUE)[1]
load("data/nda_private/cached_data.Rds")
base::load("data/eng_wg_raw_data.Rds")

cats <- bind_rows(d_td_ws %>% distinct(category, definition) %>% mutate(form = "WS"),
                  eng_wg %>% filter(!is.na(category)) %>% distinct(category, definition) %>% mutate(form = "WG"))

load_one <- function(f) { e <- new.env(); load(f, envir = e); base::get(ls(e)[1], envir = e) }
items <- map_dfr(c("WS", "WG"), function(f) map_dfr(c("", "_dxconfirmed"), function(s)
  load_one(sprintf("data/nda_private/dif_items_%s%s.Rds", f, s)) %>%
    transmute(definition, d_TD = d_g1, d_ASD = d_g2, d_diff, d_diff_c, extreme) %>%
    mutate(form = f, version = ifelse(s == "", "main", "dx_confirmed")))) %>%
  left_join(cats, by = c("form", "definition"))

sample <- admins %>% filter(keep_items, one_per_child) %>%
  group_by(form, dx_unconfirmed = asd_dx_unconfirmed) %>%
  summarise(children = n(), collections = n_distinct(collection_id),
            age_min = min(age), age_max = max(age), age_median = median(age), .groups = "drop")

exclusions <- admins %>% count(form, exclusion)
td_n <- list(WS = n_distinct(d_td_ws$data_id), WG = n_distinct(eng_wg$data_id))

write_json(list(items = items, sample = sample, exclusions = exclusions, td_n = td_n,
                profile = profile, trust = trust %>% filter(kept_children > 0)),
           out, auto_unbox = TRUE, digits = 4, na = "null")
cat("wrote", out, "\n")
