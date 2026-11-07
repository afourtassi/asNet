# Data fixes on `data-cleaning-trust-scores`

This file describes what changed in how the NDAR CDI data is cleaned and analysed on this branch, why each change was made, and what it did to the results. It covers commits `3c66b3b`, `0133d09` and `f61cd39`.

Every number here is an aggregate. No child identifiers or item-level NDA data appear in this file or anywhere else in the repo's new files. NDA-derived outputs go to `data/nda_private/`, which is gitignored because the repo is public.

## 1. Cleaning rebuilt around individual administrations

**The problem.** The old pipeline kept or dropped data a whole collection at a time. It also had coding errors, and duplicate submissions of the same child at the same age stayed in the data.

**What changed.** The QC now lives in `helper_functions/ndar_qc.R` and is run by `01_prep_data.Rmd`. Each administration (one child at one age) is judged on its own, so a clean child who isn't duplicated is kept even if their collection has problems.

### Per-administration QC flags

| Flag | Rule |
|---|---|
| No item data | every vocabulary item is blank |
| Zero-filled | items are all 0 while the stored total is above 0 |
| Incomplete | fewer than 90% of vocabulary items are answered |
| Disagrees with total | the recomputed total differs from the stored total by more than max(5 words, 5%) |
| Clone | an identical response vector appears for a different child (only checked above 10 words) |
| Age out of range | outside 12–48 months for WS, or 8–36 months for WG |

### Duplicates

Duplicates are grouped by child × age within a form, across collections and datasets.
- If the responses are identical, one copy is kept.
- If the responses differ, the best-verified, most complete version is kept, but only if exactly one version is best. If two versions tie, all of them are dropped as a *conflicting duplicate*.
- The `one_per_child` flag picks one administration per child at random (seed 42). Analyses that assume independent children use only those.

### Coding fixes

- **WS:** only codes 1 and 3 count as *produces*. Code 2 means *understands only* and is no longer counted.
- **WG:** NDA datasets disagree on whether 1 or 2 means *produces*. The coding is now inferred separately for each `dataset_id`, by checking which coding reproduces the stored totals.
- **WG comprehension:** most datasets use 1 = understands, 2 = understands & says. Datasets 14747 and 16640 (collections 19 and 2027) instead use 0 = understands only, 1 = says, 2 or blank = neither: their stored "words understood" total equals the count of codes 0 + 1 for every child. The prep had read code 2 as "understands only" there, which made most of those children's comprehension equal to their production. `infer_wg_coding()` now also chooses, per dataset, the comprehension scheme that reproduces `mcg_vc_totcom`. Only the `understands` column changes; production and every result in `03`–`07` are identical. Found while building `08_wg_gestures_gap.Rmd`.
- **Blank items** are treated as "no", but only when the answered items already reproduce the stored total.

### Resulting exclusions (all administrations)

| | WS | WG |
|---|---:|---:|
| Kept | 717 | 1013 |
| Duplicate of kept administration | 2020 | 1617 |
| No item data | 1486 | 4718 |
| Items zero-filled | – | 851 |
| Incomplete items | 430 | 34 |
| Items disagree with stored total | 256 | 7 |
| Age out of range | 147 | 241 |
| Identical to another child | 24 | 7 |
| Conflicting duplicate | 20 | – |
| **One per child (analysis sample)** | **385** | **630** |

## 2. Collection trust scores

Each collection gets a score that summarises how far its data can be trusted. Children are still kept or dropped individually; the score is reported so that the analyses can be re-run on high-trust collections as a sensitivity check.

The score is built from four components, each between 0 and 1, computed over every row the collection submitted:

- **item_coverage:** the share of rows with usable item data (not blank, not zero-filled, at least 90% complete).
- **verification:** among rows that have both items and a stored total, the share whose items reproduce the total. If nothing can be verified, this counts as 0.5.
- **consistency:** 1 minus the share of child × age groups with conflicting versions.
- **coding:** for WG only, how well the inferred coding reproduces stored totals. This is the minimum across the collection's datasets. It is always 1 for WS.

`trust` is the mean of the four components. Tiers are:
- **A:** at least .85
- **B:** at least .60
- **C:** below .60
- **summary only:** no usable item data but stored totals exist
- **unusable:** no usable item data and no stored totals

`uniqueness` (distinct child × age divided by rows) is reported but not included in the score, because exact duplicates do no harm once they are collapsed.

Current tiers among collections that contribute data:
- **WS:** 5 collections in tier A, 3 summary only, 2 unusable.
- **WG:** 7 in tier A, 1 in tier B, 11 summary only, 1 unusable.

## 3. Item-profile check

The trust score can't detect data that is internally consistent but attached to the wrong items. The item-profile check catches that case.

1. Fit a logistic model `value ~ log(vocabulary)` for each item in the TD (Wordbank) data.
2. For each ASD collection, compare every item's observed production rate with the rate expected at that collection's vocabulary sizes. Only children with at least 20 words and at most 90% of items produced are used.
3. Flag a collection if more than 2% of items are grossly off (|observed − expected| > .30) **and** its item residuals replicate across random split halves (r > .70).

A flagged collection gets `item_profile_ok = FALSE` and drops to tier C. Flagged collections are left out of item-level analyses by default; the `include_flagged` parameter puts them back.

## 4. Collection 2024: off-by-one column shift (repaired)

**Finding.** WS collection 2024 failed the item-profile check, with 18 grossly off items and split-half r = .82. Its residuals looked like each column held the answer to the next item on the form.

**Locating the shift.**
- Letting the data choose any column-to-item matching overfit: the matchings didn't agree between random halves of the children, so that approach was rejected.
- A scan for runs of columns that fit the *next* item better found one clear run, from `wind` (outside things) through `slide` (action words). That is 154 columns.
- The shift improves fit by 25.6 log-likelihood units per child.
- Two random halves of the children independently place the start at `wind` and the end within a few items of `slide`.
- No other collection shows a run longer than 2 columns, and none gains more than 0.5 per child.

**The repair.**
- The run is recorded in `KNOWN_SHIFTS` (`helper_functions/ndar_qc.R`).
- `fix_column_shifts()` moves each value back one column for that collection's rows.
- `wind` has no recoverable response, so it is set to missing.
- The five items at the uncertain end of the run (`slide`, `smile`, `spill`, `splash`, `stand`) are also set to missing.
- These missing values are kept as missing after the blanks-as-no rule runs, so that rule can't turn them into "no".

**Check.** Production rates in collection 2024 among children with 150–500 words:

| Item | Before | After | Other collections |
|---|---:|---:|---:|
| mommy | 31% | 79% | 97% |
| daddy | 54% | 69% | 97% |
| grandpa | 28% | 92% | 84% |
| no | 56% | 92% | 94% |
| cowboy | 69% | 10% | 6% |
| nurse | 56% | 31% | 5% |
| person | 33% | 13% | 5% |

After the repair, collection 2024 passes the item-profile check with no grossly off items. It is back in the analyses, which brings the WS sample to 385 children.

**Still open.**
- The repair was inferred from the data. It should be confirmed with the submitting lab or NDA.
- Whether 2024's children have a confirmed ASD diagnosis is not settled. The `exclude_dx_unconfirmed` sensitivity run drops them.

## 5. Analysis fixes

### DIF (`04_DIF_analysis.Rmd`, `helper_functions/DIF_helpers.R`)

- **Group fitting.** The Rasch model is now fit separately for each group with `mirt()`. In mirt 1.46, `multipleGroup()` rejects `free_var` and returns NULL when there are more than about 675 items.
- **Matrices.** The TD and ASD response matrices are now built by item name, so the two groups' columns line up even when their item order differs.
- **Flagging.** Easiness differences are now centered before flagging (`flag_dif()`): `d_diff_c = d_diff − mean(d_diff)`, flagged when |d_diff_c| > 2 SD.
  - Each group's latent mean is fixed at 0, so the overall ability gap shows up as a uniform shift in `d_diff`.
  - Before centering, that shift made the WG flags fall almost entirely on one side.
- **Parameters.** New parameters `tier_A_only`, `exclude_dx_unconfirmed` and `include_flagged` control the sample. Item tables are saved to `data/nda_private/dif_items_*.Rds`.

### Networks (`03_networks.Rmd`, `helper_functions/network_growth.R`)

- **Rewritten.** The notebook no longer needs rwebppl or espeak.
- **AoA** comes from the cleaned data, using a per-item logistic regression of production on age.
- **Networks.** Both groups use the same semantic network (free association, undirected) and phonological network (IPA forms within edit distance 2), over the 312 CDI:WS nouns.
- **Static models.** AoA regressions include predictor × group interactions.
- **Growth models.** The PAC and PAT growth models are fit by maximum likelihood. They use the same softmax likelihood as the old webppl models, with standard errors from the Hessian.
- **Restored file.** `in_files/association_cue_target.csv` (the free-association norms) was restored from git history.

## 6. Effect on results

| | Before this branch | Now |
|---|---|---|
| WS DIF items flagged | inflated by the 2024 shift | 35 of 680 (r = .96 between TD and ASD item easiness) |
| mommy / daddy DIF (centered logits) | −3.9 / −3.4 | −1.8 / −2.1 (−1.4 / −1.3 without 2024) |
| WG DIF items flagged | one-sided because of the uncentered shift | 19 (r = .92); `I`, `that`, `this`, `what` and `who` are the hardest for ASD |
| Noun AoA, ASD vs TD | — | r = .93; ASD median 28.3 vs TD 23.7 months |
| Network predictors | — | sem/phono PAC positive in both groups; PAT null; no group differences (all p > .4) |

**Robust WS DIF.** These items are flagged both in the main run and in the dx-confirmed run, where the two profiles correlate r = .89 and 25 flagged items overlap.
- **Harder for ASD:** mommy, daddy, coke, soda/pop, rocking chair, bib, belt, sweater, penny.
- **Easier for ASD:** penguin, play dough, stairs.

## 7. Known remaining issues

- Several NDA-derived files are still tracked in this public repo: `data/ASD_WS.Rdata`, `data/ASD_WG.Rdata`, `data/cached_data.Rds` and the IRT `.Rds` model files. They were left in place for now and should be untracked, and possibly purged from history.
- The collection 2024 repair needs confirming with the data submitter.
- **IRT follow-ups:**
  - likelihood-ratio DIF tests with an anchor set (`mirt::DIF`) and bootstrap intervals
  - comparisons matched on age or vocabulary
  - leave-one-collection-out reruns
  - a 2PL check

## Reproducing

Render the notebooks in this order. The raw exports must be in `data/ASD CDI data/`, which is gitignored.

1. `01_prep_data.Rmd` writes `data/nda_private/cached_data.Rds`.
2. `03_networks.Rmd` and `04_DIF_analysis.Rmd`. For the sensitivity runs, set the parameters `exclude_dx_unconfirmed`, `tier_A_only` or `include_flagged` to `TRUE`.
3. `Rscript helper_functions/irt_report_data.R <out.json>` writes the aggregate results used by the report.
