# WG gesture and comprehension scores for NDAR and Wordbank children, Wordbank-normed
# z-scores, and the label-free validation used in 08_wg_gestures_gap.Rmd.
#
# Gesture sections (same 63 items, same order, in both sources):
#   fcg     First Communicative Gestures, 12 items (show, give, point, wave, ...);
#           scored 1 if "sometimes" or "often" (NDAR 1/2, Wordbank sometimes/often)
#   games   Games and Routines, 6 items, yes/no
#   later   Actions with Objects (17), Pretending to Be a Parent (13),
#           Imitating Other Adult Actions (15): 45 items, yes/no
#   early   = fcg + games (18; the CDI "Early Gestures" total), total = early + later (63)
# NDAR codes 9 (FCG) and 2 (yes/no sections) are not counted in the NDA's own stored
# totals, so they are treated as missing.

suppressMessages({library(tidyverse); library(splines)})

GEST_PREFIX <- c(fcg = "mcg_cg_", games = "mcg_gr_", ao = "mcg_ao_", pp = "mcg_pp_", ia = "mcg_ia_")
WB_GEST_IDS <- list(fcg = 430:441, games = 442:447, later = 448:492)

# Count of "yes" over a section; NA if more than `max_missing` of its items are missing
section_score <- function(m, max_missing = .2) {
  nmiss <- rowSums(is.na(m)); out <- rowSums(m, na.rm = TRUE)
  # pro-rate the few missing items so a skipped item is not scored as "no"
  out <- out * ncol(m) / pmax(ncol(m) - nmiss, 1)
  ifelse(nmiss / ncol(m) > max_missing, NA, out)
}

ndar_gestures <- function(raw, rows) {
  d <- raw[rows, , drop = FALSE]
  get <- function(p) {
    m <- as.matrix(d[, startsWith(names(d), p), drop = FALSE])
    m <- suppressWarnings(matrix(as.integer(m), nrow(m)))
    if (p == GEST_PREFIX[["fcg"]]) { m[m == 9] <- NA; m <- (m >= 1) * 1 } else { m[m == 2] <- NA }
    m
  }
  fcg_items <- get(GEST_PREFIX[["fcg"]])
  colnames(fcg_items) <- names(d)[startsWith(names(d), GEST_PREFIX[["fcg"]])]
  later <- cbind(get(GEST_PREFIX[["ao"]]), get(GEST_PREFIX[["pp"]]), get(GEST_PREFIX[["ia"]]))
  list(scores = tibble(row = rows, fcg = section_score(fcg_items),
                       games = section_score(get(GEST_PREFIX[["games"]])),
                       later = section_score(later)),
       fcg_items = fcg_items)
}

# NDAR kept WG sample: one administration per child that passed the item-profile check
ndar_wg_scores <- function(cached, raw) {
  keep <- cached$admins %>% filter(form == "WG", one_per_child, item_profile_ok) %>%
    select(row, GUID, collection_id, age, sex, asd_dx_unconfirmed)
  words <- cached$d_asd_wg %>% semi_join(keep, by = "row") %>% group_by(row) %>%
    summarise(prod = sum(value, na.rm = TRUE), comp = sum(understands, na.rm = TRUE),
              n_c1 = sum(code == 1, na.rm = TRUE), n_c2 = sum(code == 2, na.rm = TRUE),
              .groups = "drop")
  g <- ndar_gestures(raw, keep$row)
  out <- keep %>% left_join(words, by = "row") %>% left_join(g$scores, by = "row")
  list(scores = out, fcg_items = g$fcg_items)
}

wordbank_wg_scores <- function(eng_wg) {
  kids <- eng_wg %>% distinct(data_id, age, sex, comp = comprehension, prod = production)
  ges <- eng_wg %>% filter(num_item_id %in% unlist(WB_GEST_IDS)) %>%
    mutate(y = case_when(value %in% c("sometimes", "often", "yes") ~ 1,
                         value %in% c("not yet", "no") ~ 0)) %>%
    select(data_id, num_item_id, y)
  wide <- ges %>% pivot_wider(names_from = num_item_id, values_from = y)
  m <- function(ids) as.matrix(wide[, as.character(ids)])
  scores <- tibble(data_id = wide$data_id, fcg = section_score(m(WB_GEST_IDS$fcg)),
                   games = section_score(m(WB_GEST_IDS$games)),
                   later = section_score(m(WB_GEST_IDS$later)))
  fcg_items <- m(WB_GEST_IDS$fcg); rownames(fcg_items) <- wide$data_id
  list(scores = kids %>% left_join(scores, by = "data_id") %>%
         mutate(sex = case_when(sex == "Male" ~ "M", sex == "Female" ~ "F")),
       fcg_items = fcg_items)
}

# Comprehension is treated as not recorded when every word said is also the only word
# understood (comp == prod > 0): 0.2% of Wordbank children. Before the WG coding fix in
# ndar_qc.R this pattern flagged most of collections 19 and 2027; after it, it is rare.
# Same rule for both sources.
add_derived <- function(d) {
  d %>% mutate(comp_ok = !(comp == prod & prod > 0),
               comp = ifelse(comp_ok, comp, NA),
               early = fcg + games, total = early + later,
               age_n = pmin(pmax(age, 8), 18), in_norm_age = age >= 8 & age <= 18)
}

elogit <- function(x, n) log((x + .5) / (n - x + .5))

SCORES <- c(prod = "Words produced", comp = "Words understood",
            fcg = "First communicative gestures", early = "Early gestures",
            later = "Later gestures", gap = "Comprehension given production",
            fcg_resid = "FCG given words and later gestures",
            later_resid = "Later gestures given words")
N_MAX <- c(prod = 396, comp = 396, fcg = 12, early = 18, later = 45)

# Norm on Wordbank. Level scores: mid-rank percentile among Wordbank children of the same
# (clamped) month -> z. Relative scores: standardised residual from a Wordbank regression
# on age (spline) and language level, all on the empirical-logit scale.
# Every score is oriented so that LOW = the direction expected for autism.
fit_norms <- function(wb) {
  wb <- wb %>% mutate(el_prod = elogit(prod, 396), el_comp = elogit(comp, 396),
                      el_fcg = elogit(fcg, 12), el_later = elogit(later, 45))
  rel <- list(
    gap = lm(el_comp ~ ns(age_n, 3) + ns(el_prod, 3), wb),
    fcg_resid = lm(el_fcg ~ ns(age_n, 3) + ns(el_prod, 3) + ns(el_comp, 3) + ns(el_later, 3), wb),
    later_resid = lm(el_later ~ ns(age_n, 3) + ns(el_prod, 3) + ns(el_comp, 3), wb))
  list(wb = wb, rel = rel, sigma = map_dbl(rel, ~ summary(.x)$sigma))
}

apply_norms <- function(d, norms) {
  wb <- norms$wb
  pct <- function(x, a, v) {
    map2_dbl(x, a, function(xi, ai) {
      ref <- wb[[v]][wb$age_n == ai & !is.na(wb[[v]])]
      if (is.na(xi) || !length(ref)) return(NA)
      p <- (sum(ref < xi) + .5 * sum(ref == xi)) / length(ref)
      qnorm(pmin(pmax(p, .5 / length(ref)), 1 - .5 / length(ref)))
    })
  }
  d <- d %>% mutate(el_prod = elogit(prod, 396), el_comp = elogit(comp, 396),
                    el_fcg = elogit(fcg, 12), el_later = elogit(later, 45))
  for (v in names(N_MAX)) d[[paste0("z_", v)]] <- pct(d[[v]], d$age_n, v)
  for (v in names(norms$rel)) {
    y <- d[[c(gap = "el_comp", fcg_resid = "el_fcg", later_resid = "el_later")[[v]]]]
    d[[paste0("z_", v)]] <- (y - predict(norms$rel[[v]], d)) / norms$sigma[[v]]
  }
  d
}

# ---- label-free validation -------------------------------------------------------------
# AUC that `low` scores pick out the cases (Mann-Whitney)
auc_low <- function(score, is_case) {
  ok <- !is.na(score) & !is.na(is_case); score <- -score[ok]; is_case <- is_case[ok]
  n1 <- sum(is_case); n0 <- sum(!is_case)
  if (n1 == 0 || n0 == 0) return(NA)
  (sum(rank(score)[is_case]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Sex test. If a collection's ASD share is pi and P(male | ASD) = .80, P(male | not) = .51,
# then P(ASD | boy) - P(ASD | girl) = k, and for any score whose ASD-vs-non-ASD AUC is A,
#   AUC(boys score lower than girls) = .5 + k (A - .5)   (+ the ordinary TD sex gap).
# So A is identified from the boys-vs-girls AUC in excess of Wordbank's at the same ages.
P_MALE_ASD <- .80; P_MALE_TD <- .51
sex_k <- function(pi) {
  pm <- pi * P_MALE_ASD; pf <- pi * (1 - P_MALE_ASD)
  pm / (pm + (1 - pi) * P_MALE_TD) - pf / (pf + (1 - pi) * (1 - P_MALE_TD))
}
pi_from_sex <- function(male) pmin(pmax((male - P_MALE_TD) / (P_MALE_ASD - P_MALE_TD), 0), 1)
