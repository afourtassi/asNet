# Quality control for NDAR MacArthur-Bates CDI exports (mci_sentences02 = WS,
# mci_words_gestures01 = WG).
#
# Pipeline:
#   read_ndar()          raw export -> list(data, desc)  (row 1 of export = descriptions)
#   *_dictionary()       NDA item variable -> Wordbank category/definition
#   infer_wg_coding()    which response code means "produces", per dataset_id
#   score_admins()       one row per administration with QC flags
#   dedupe_admins()      collapse duplicate administrations of the same child x age
#   collection_trust()   per-collection trust components, score, and tier
#   to_long()            long item-level data for kept administrations
#
# Design: decisions are made per *administration* (child x age), so a clean,
# non-duplicated child is kept even if their collection has problems. The
# collection score/tier is reported so analyses can be re-run on high-trust
# collections only as a sensitivity check.

suppressPackageStartupMessages(library(tidyverse))

QC <- list(
  min_completeness = 0.90,  # share of vocab items that must be non-missing
  total_tol_abs    = 5,     # stored vs recomputed total may differ by this many words...
  total_tol_rel    = 0.05,  # ...or by this share of the stored total, whichever is larger
  age_ws           = c(12, 48),
  age_wg           = c(8, 36),
  clone_min_words  = 10     # identical vectors across children only suspicious above this
)

read_ndar <- function(path) {
  d <- read.delim(path, header = TRUE, sep = "\t", colClasses = "character",
                  check.names = FALSE, na.strings = NULL)
  list(data = d[-1, ], desc = unlist(d[1, ]))
}

ws_item_vars <- function(d) grep("^mcs_vc[0-9]+_", names(d), value = TRUE)
# mcg_v2_dog is misnamed in the NDA export (missing "c")
wg_item_vars <- function(d) grep("^mcg_vc?[0-9]+_", names(d), value = TRUE)

# ---- item dictionaries -------------------------------------------------------

ws_dictionary <- function(desc, items) {
  tibble(var = items, description = unname(desc[items])) %>%
    separate(description, c("category", "definition"), sep = "\\. ", extra = "merge") %>%
    mutate(category = case_when(category=="Sound Effects and Animal Sounds" ~ "sounds",
                                category=="Animals (Real or Toy)" ~ "animals",
                                category=="Vehicles (Real or Toy)" ~ "vehicles",
                                category=="Food and Drink" ~ "food_drink",
                                category=="Clothinq" ~ "clothing",
                                category=="Body Parts" ~ "body_parts",
                                category=="Small Household Items" ~ "household",
                                category=="Furniture and Rooms" ~ "furniture_rooms",
                                category=="Outside Thlnqs" ~ "outside",
                                category=="Action Words" ~ "action_words",
                                category=="Places to Go" ~ "places",
                                category %in% c("Helping Verbs", "Helpinq Verbs") ~ "helping_verbs",
                                category=="Connectinq Words" ~ "connecting_words",
                                category=="Descriptive Words" ~ "descriptive_words",
                                category=="Words About Time" ~ "time_words",
                                category=="Quantifiers and Articles" ~ "quantifiers",
                                category=="Games and Routines" ~ "games_routines",
                                category=="Question Words" ~ "question_words",
                                category=="Prepositions and Locations" ~ "locations",
                                TRUE ~ category),
           category = tolower(category),
           definition = case_when(definition=="baa" ~ "baa baa",
                                  definition=="cockadoodle" ~ "cockadoodledoo",
                                  definition=="quack" ~ "quack quack",
                                  definition=="uhoh" ~ "uh oh",
                                  definition=="woof" ~ "woof woof",
                                  definition=="yum" ~ "yum yum",
                                  definition=="chicken" & category=="food_drink" ~ "chicken (food)",
                                  definition=="chicken" ~ "chicken (animal)",
                                  definition=="fish" & category=="food_drink" ~ "fish (food)",
                                  definition=="fish" ~ "fish (animal)",
                                  definition=="playdough" ~ "play dough",
                                  definition=="vagina" ~ "vagina*",
                                  definition=="penis" ~ "penis*",
                                  definition=="frenchfries" ~ "french fries",
                                  definition=="greenbeans" ~ "green beans",
                                  definition=="toy" ~ "toy (object)",
                                  definition=="drink" & category=="action_words" ~ "drink (action)",
                                  definition=="drink" ~ "drink (beverage)",
                                  definition=="gasstation" ~ "gas station",
                                  definition=="orange" & category=="food_drink" ~ "orange (food)",
                                  definition=="orange" ~ "orange (description)",
                                  definition=="allgone" ~ "all gone",
                                  definition=="water" & category=="food_drink" ~ "water (beverage)",
                                  definition=="water" ~ "water (not beverage)",
                                  definition=="feet" ~ "foot",
                                  definition=="callph" ~ "call (on phone)",
                                  definition=="clean" & category=="action_words" ~ "clean (action)",
                                  definition=="clean" ~ "clean (description)",
                                  definition=="owie  booboo" ~ "owie/boo boo",
                                  definition=="dont" ~ "don't",
                                  definition=="5bowl" ~ "bowl",
                                  definition=="can" & category=="household" ~ "can (object)",
                                  definition=="can" & category=="helping_verbs" ~ "can (auxiliary)",
                                  definition=="rockingchair" ~ "rocking chair",
                                  definition=="alot" ~ "a lot",
                                  definition=="buttocks/bottom" ~ "buttocks/bottom*",
                                  definition=="daddy" ~ "daddy*",
                                  definition=="childname" ~ "child's own name",
                                  definition=="washingmachine" ~ "washing machine",
                                  definition=="try" ~ "try/try to",
                                  definition=="work" & category=="places" ~ "work (place)",
                                  definition=="work" ~ "work (action)",
                                  definition=="giveme five" ~ "give me five!",
                                  definition=="mommy" ~ "mommy*",
                                  definition=="grandma" ~ "grandma*",
                                  definition=="church" ~ "church*",
                                  definition=="grandpa" ~ "grandpa*",
                                  definition=="patty cake" ~ "pattycake",
                                  definition=="dry" & category=="action_words" ~ "dry (action)",
                                  definition=="dry" ~ "dry (description)",
                                  definition=="lemme" ~ "lemme/let me",
                                  definition=="tissklee" ~ "tissue/kleenex",
                                  definition=="did" ~ "did/did ya",
                                  definition=="gonna get  you" ~ "gonna get you!",
                                  definition=="peanutbutter" ~ "peanut butter",
                                  definition=="playpen" ~ "play pen",
                                  definition=="potatochip" ~ "potato chip",
                                  definition=="wanna" ~ "wanna/want to",
                                  definition=="watch" & category=="action_words" ~ "watch (action)",
                                  definition=="watch" ~ "watch (object)",
                                  definition=="dress" ~ "dress (object)",
                                  definition=="gonna" ~ "gonna/going to",
                                  definition=="gotta" ~ "gotta/got to",
                                  definition=="hafta" ~ "hafta/have to",
                                  definition=="highchair" ~ "high chair",
                                  definition=="lawnmower" ~ "lawn mower",
                                  definition=="little" ~ "little (description)",
                                  definition=="petname" ~ "pet's name",
                                  definition=="so big" ~ "so big!",
                                  definition=="need" ~ "need/need to",
                                  definition=="shush" ~ "shh/shush/hush",
                                  definition=="swing" & category=="action_words" ~ "swing (action)",
                                  definition=="swing" ~ "swing (object)",
                                  definition=="slide" & category=="action_words" ~ "slide (action)",
                                  definition=="slide" ~ "slide (object)",
                                  TRUE ~ definition))
}

wg_dictionary <- function(desc, items) {
  # Descriptions are ambiguous for homonyms (e.g. two "chicken"s); make.unique()
  # in column order reproduces the original disambiguation ("chicken_1" = food).
  tibble(var = items,
         definition = make.unique(unname(desc[items]), sep = "_")) %>%
    mutate(definition = case_when(
      var == "mcg_vc18_back" ~ "back", # NDA description file mislabels this "backyard"
      definition == "MacArthur Words and Gestures: Vocabulary Checklist: House" ~ "house",
      definition=="bye or bye bye" ~ "bye",
      definition=="chicken" ~ "chicken (animal)",
      definition=="chicken_1" ~ "chicken (food)",
      definition=="peek-a-boo" ~ "peekaboo",
      definition=="water" ~ "water (beverage)",
      definition=="water_1" ~ "water (not beverage)",
      definition=="church" ~ "church*",
      definition=="clean" ~ "clean (action)",
      definition=="clean_1" ~ "clean (description)",
      definition=="daddy" ~ "daddy*",
      definition=="dress" ~ "dress (object)",
      definition=="towl" ~ "towel",
      definition=="grandpa" ~ "grandpa*",
      definition=="grandma" ~ "grandma*",
      definition=="mommy" ~ "mommy*",
      definition=="owie/ boo boo" ~ "owie/boo boo",
      definition=="little" ~ "little (description)",
      definition=="drink" ~ "drink (beverage)",
      definition=="drink_1" ~ "drink (action)",
      definition=="dry" ~ "dry (description)",
      definition=="fire truck" ~ "firetruck",
      definition=="fish" ~ "fish (animal)",
      definition=="fish_1" ~ "fish (food)",
      definition=="toy" ~ "toy (object)",
      definition=="teddy bear" ~ "teddybear",
      definition=="swing" ~ "swing (object)",
      definition=="swing_1" ~ "swing (action)",
      definition=="work" ~ "work (place)",
      definition=="orange" ~ "orange (food)",
      definition=="patty cake" ~ "pattycake",
      definition=="slide" ~ "slide (object)",
      definition=="watch" ~ "watch (object)",
      definition=="watch_1" ~ "watch (action)",
      TRUE ~ definition))
}

# ---- response coding ---------------------------------------------------------

item_matrix <- function(d, items) {
  m <- suppressWarnings(sapply(d[, items], as.integer))
  m[!(m %in% 0:3)] <- NA  # blanks and stray codes -> missing
  matrix(m, nrow = nrow(d), dimnames = list(NULL, items))
}

parse_total <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(x %in% 999, NA, x)  # 999 = NDA missing-value code
}

total_agrees <- function(stored, recomputed) {
  tol <- pmax(QC$total_tol_abs, QC$total_tol_rel * stored)
  ifelse(is.na(stored), NA, abs(stored - recomputed) <= tol)
}

# WS per NDA data dictionary: 0 = No; 1 = Says & Understands; 2 = Understands; 3 = Says.
# Only 1 and 3 are production. (Checked: stored totals match count of 1s.)
ws_produce_codes <- function() c(1L, 3L)

# WG: the NDA dictionary is silent, and datasets disagree. Some use
# 1 = understands, 2 = understands & says (standard paper form); others use
# 1 = says (& understands), 2 = understands only. Infer per dataset_id by which
# code count reproduces the stored "words produced" total.
infer_wg_coding <- function(d, m) {
  pr <- parse_total(d$mcg_vc_totpr)
  c1 <- rowSums(m == 1, na.rm = TRUE)
  c2 <- rowSums(m == 2, na.rm = TRUE)
  has_items <- rowSums(!is.na(m)) > 0 & rowSums(m > 0, na.rm = TRUE) > 0
  tibble(dataset_id = d$dataset_id, pr, c1, c2, has_items) %>%
    filter(has_items, !is.na(pr)) %>%
    group_by(dataset_id) %>%
    summarise(n_check = n(),
              agree_code2 = mean(total_agrees(pr, c2)),
              agree_code1 = mean(total_agrees(pr, c1)),
              .groups = "drop") %>%
    mutate(produce_code = if_else(agree_code1 > agree_code2, 1L, 2L),
           coding_agreement = pmax(agree_code1, agree_code2),
           coding_margin = abs(agree_code1 - agree_code2))
}

# ---- administration-level QC -------------------------------------------------

score_admins <- function(d, m, form, produce_code) {
  # produce_code: vector (length nrow(d)) of codes meaning "produces" (WG), or NULL for WS
  n_items <- ncol(m)
  answered <- rowSums(!is.na(m))
  if (form == "WS") {
    produced <- rowSums(m == 1L | m == 3L, na.rm = TRUE)
    stored <- parse_total(d$mcs_vc_total)
    age_rng <- QC$age_ws
  } else {
    produced <- rowSums(m == produce_code, na.rm = TRUE)
    stored <- parse_total(d$mcg_vc_totpr)
    age_rng <- QC$age_wg
  }
  any_nonzero <- rowSums(m > 0, na.rm = TRUE) > 0
  # a dataset whose answered items are all 0 for every child was zero-filled
  # (e.g. WG collection 1600: 840 rows of all-0 items, stored totals up to 395)
  ds_nonzero <- tapply(any_nonzero, d$dataset_id, any)
  ds_zero_filled <- as.vector(!ds_nonzero[d$dataset_id]) & answered > 0
  resp_key <- apply(m, 1, function(r) paste(ifelse(is.na(r), ".", r), collapse = ""))
  id_col <- if (form == "WS") "mci_sentences02_id" else "mci_words_gestures01_id"

  a <- tibble(
    form = form,
    row = seq_len(nrow(d)),
    id = d[[id_col]],
    GUID = d$subjectkey,
    src_subject_id = d$src_subject_id,
    collection_id = as.numeric(d$collection_id),
    dataset_id = d$dataset_id,
    age = as.numeric(d$interview_age),
    sex = d$sex,
    completeness = answered / n_items,
    produced = produced,
    stored_total = stored,
    resp_key = resp_key
  ) %>%
    mutate(
      items_blank   = answered == 0,
      # all items 0 but the lab's own total says otherwise: item data were zero-filled
      items_zero_filled = unname(ds_zero_filled) |
        (answered > 0 & !any_nonzero & !is.na(stored) & stored > QC$total_tol_abs),
      total_check   = case_when(items_blank ~ "no_items",
                                is.na(stored) ~ "unverifiable",
                                total_agrees(stored, produced) ~ "agree",
                                TRUE ~ "disagree"),
      # Several labs leave "no" items blank instead of 0. When the items that
      # are filled in reproduce the lab's stored total, treat blanks as "no".
      blanks_as_no  = !items_blank & completeness < 1 & total_check == "agree",
      incomplete    = !items_blank & completeness < QC$min_completeness & !blanks_as_no,
      age_ok        = !is.na(age) & age >= age_rng[1] & age <= age_rng[2],
      has_sumscore  = !is.na(stored)
    )

  # identical, non-trivial response vectors shared by *different* children
  clones <- a %>%
    filter(!items_blank, !incomplete, produced >= QC$clone_min_words) %>%
    group_by(resp_key) %>%
    filter(n_distinct(GUID) > 1) %>%
    pull(row)
  a$clone_across_children <- a$row %in% clones
  a
}

# Within a child x age (across collections/datasets, within form):
#  - identical responses -> exact duplicates, keep one
#  - differing responses -> conflict; keep the best-verified, most complete one
#    if it is uniquely best, otherwise drop all (can't tell which is real)
dedupe_admins <- function(a) {
  a <- a %>%
    mutate(item_ok = !items_blank & !items_zero_filled & !incomplete &
                     total_check != "disagree" & !clone_across_children,
           check_rank = match(total_check, c("agree", "unverifiable", "disagree", "no_items")))

  cand <- a %>% filter(item_ok) %>%
    group_by(GUID, age) %>%
    mutate(n_rows = n(), n_versions = n_distinct(resp_key)) %>%
    arrange(check_rank, desc(completeness), .by_group = TRUE) %>%
    mutate(best = row_number() == 1,
           tie = n_versions > 1 &
                 sum(check_rank == first(check_rank) & completeness == first(completeness) &
                     resp_key != first(resp_key)) > 0) %>%
    ungroup()

  keep_rows <- cand %>% filter(best, !tie) %>% pull(row)
  dup_info <- cand %>% select(row, n_rows, n_versions, tie)

  a %>% left_join(dup_info, by = "row") %>%
    mutate(n_rows = replace_na(n_rows, 1L), n_versions = replace_na(n_versions, 1L),
           tie = replace_na(tie, FALSE),
           keep_items = row %in% keep_rows & age_ok,
           exclusion = case_when(
             keep_items ~ "kept",
             items_blank ~ "no item data",
             items_zero_filled ~ "items zero-filled",
             incomplete ~ "incomplete items",
             total_check == "disagree" ~ "items disagree with stored total",
             clone_across_children ~ "identical to another child",
             tie ~ "conflicting duplicate",
             !age_ok & row %in% keep_rows ~ "age out of range",
             TRUE ~ "duplicate of kept administration")) %>%
    select(-check_rank)
}

# one administration per child (for analyses assuming independent children)
pick_one_per_child <- function(a, seed = 42) {
  set.seed(seed)
  chosen <- a %>% filter(keep_items) %>%
    group_by(GUID) %>% slice_sample(n = 1) %>% pull(row)
  a %>% mutate(one_per_child = row %in% chosen)
}

# ---- collection-level trust --------------------------------------------------
#
# Components (0-1), computed over all rows the collection submitted:
#   item_coverage : share of rows with usable item-level data (not blank,
#                   not zero-filled, >= 90% complete)
#   verification  : of rows with items AND a stored total, share whose items
#                   reproduce the total (NA if nothing verifiable)
#   consistency   : 1 - share of child x age groups with conflicting versions
#   coding        : WG only -- how well the inferred response coding reproduces
#                   stored totals (min across datasets); 1 for WS
#   uniqueness    : distinct child x age / rows (reported, NOT in score:
#                   exact duplicates are harmless once collapsed)
#
# trust = mean(item_coverage, verification, consistency, coding), with
# unverifiable verification counted as 0.5 (no evidence either way).
# Tier A >= .85 ; B >= .6 ; C < .6 ; "summary only" if no usable item data.
collection_trust <- function(a, coding = NULL) {
  out <- a %>%
    group_by(form, collection_id) %>%
    summarise(
      rows = n(),
      children = n_distinct(GUID),
      child_ages = n_distinct(paste(GUID, age)),
      datasets = n_distinct(dataset_id),
      item_rows = sum(!items_blank & !items_zero_filled & !incomplete),
      item_coverage = item_rows / rows,
      n_verifiable = sum(total_check %in% c("agree", "disagree") & !items_zero_filled),
      verification = ifelse(n_verifiable > 0,
                            sum(total_check == "agree") / n_verifiable, NA),
      consistency = 1 - n_distinct(paste(GUID, age)[tie]) / child_ages,
      uniqueness = child_ages / rows,
      with_sumscore = sum(has_sumscore),
      kept_admins = sum(keep_items),
      kept_children = n_distinct(GUID[keep_items]),
      .groups = "drop")

  if (!is.null(coding)) {
    cod <- coding %>%
      left_join(distinct(a, dataset_id, collection_id), by = "dataset_id") %>%
      group_by(collection_id) %>%
      summarise(coding = min(coding_agreement), .groups = "drop") %>%
      mutate(form = "WG")
    out <- out %>% left_join(cod, by = c("form", "collection_id"))
  }
  if (!"coding" %in% names(out)) out$coding <- NA_real_
  out %>%
    mutate(coding = ifelse(form == "WS", 1, coding),
           trust = ifelse(item_rows == 0, NA,
                          (item_coverage + coalesce(verification, 0.5) +
                           consistency + coalesce(coding, 0.5)) / 4),
           tier = case_when(item_rows == 0 & with_sumscore > 0 ~ "summary only",
                            item_rows == 0 ~ "unusable",
                            trust >= .85 ~ "A",
                            trust >= .60 ~ "B",
                            TRUE ~ "C")) %>%
    arrange(form, tier, desc(trust))
}

# ---- long format -------------------------------------------------------------

to_long <- function(a, m, dict, form, produce_code = NULL) {
  keep <- a$keep_items
  mm <- m[keep, , drop = FALSE]
  meta <- a[keep, ] %>% select(row, id, GUID, age, sex, collection_id, dataset_id,
                               one_per_child, blanks_as_no)
  pc <- if (form == "WS") NULL else produce_code[keep]
  long <- as_tibble(mm) %>%
    mutate(row = meta$row, .pc = if (is.null(pc)) NA_integer_ else pc,
           .bn = meta$blanks_as_no) %>%
    pivot_longer(-c(row, .pc, .bn), names_to = "var", values_to = "code") %>%
    mutate(code = ifelse(is.na(code) & .bn, 0L, code)) %>%
    select(-.bn) %>%
    mutate(value = if (form == "WS") code %in% c(1L, 3L) else code == .pc,
           value = ifelse(is.na(code), NA, value))
  if (form == "WG") long <- long %>% mutate(understands = ifelse(is.na(code), NA, code %in% c(1L, 2L)))
  long %>% select(-.pc) %>%
    left_join(meta, by = "row") %>%
    left_join(dict, by = "var") %>%
    relocate(id, GUID, age, sex, collection_id, dataset_id)
}
