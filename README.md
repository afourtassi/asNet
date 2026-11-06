# asNet: early vocabulary in autism

This project compares the early vocabularies of autistic children with those of typically developing (TD) children. It uses MacArthur-Bates CDI data from two sources:

- **ASD:** CDI administrations shared through the [NIMH Data Archive (NDA/NDAR)](https://nda.nih.gov/).
- **TD:** English (American) norms from [Wordbank](http://wordbank.stanford.edu/).

Both CDI forms are analysed: Words & Sentences (WS, 680 items) and Words & Gestures (WG, 396 items).

The project asks two questions:

1. **Item-level differences (IRT / DIF).** Do autistic and TD children find the same words easy or hard, once overall vocabulary size is accounted for? Which words depart from the shared ordering?
2. **Network growth.** Do semantic and phonological network structure predict the order in which nouns are learned in the same way for both groups?

## Main results so far

These results are from the cleaned data on the `data-cleaning-trust-scores` branch. `05_IRT_summary.Rmd` has the full write-up.

- **Same order of difficulty.** Item easiness is highly correlated between groups: r = .96 on WS and r = .92 on WG.
- **WS: 35 flagged items.** Most of them are harder for autistic children. They include family and people words (*mommy*, *daddy*), drink brands, and household and clothing words.
- **WG: 19 flagged items.** The hardest items for autistic children are deictic pronouns and question words (*I*, *that*, *this*, *what*, *who*).
- **Nouns are learned later but in the same order.** Autistic children learn nouns about 4–5 months later than TD children, in nearly the same order (r = .93). Network predictors of age of acquisition and of vocabulary growth do not differ between groups.

## Repository layout

| Path | Contents |
|---|---|
| `01_prep_data.Rmd` | Reads the raw NDAR exports and applies the cleaning steps below. Writes `data/nda_private/cached_data.Rds`. |
| `02_descriptives.Rmd` | Descriptive plots of the cleaned ASD sample. |
| `03_networks.Rmd` | Age of acquisition, the semantic and phonological noun networks, static regressions on age of acquisition, and network growth models fit by maximum likelihood. |
| `04_DIF_analysis.Rmd` | Fits a Rasch model to each group and flags items with large DIF. |
| `05_IRT_summary.Rmd` | Summary report of the DIF and network results, built from saved outputs. |
| `06_anchored_DIF.Rmd` | DIF against the fixed Wordbank English 2PL item bank: ASD ability on the Wordbank scale, language age, and iteratively purified anchored DIF. |
| `07_soft_label_feasibility.Rmd` | Can the item-level profile soft-label ASD? Draft collection-level ASD priors, sex-implied ASD shares, and held-out separability of NDAR vs θ-matched Wordbank children (profile vs level). |
| `00_investigate_missing_data.Rmd` | Exploratory notes on missing item-level data. |
| `DATA_FIXES.md` | What the cleaning does and why, and how the fixes changed the results. |
| `helper_functions/` | `ndar_qc.R` (QC and trust scores), `network_growth.R`, `DIF_helpers.R` and `irt_report_data.R`, plus older helpers from the original analyses. |
| `in_files/` | Free-association norms used for the semantic network; the Wordbank 2PL item bank and ability norms (exported from `adaptive-cdi` by `helper_functions/export_wordbank_params.R`). |
| `saved_data/` | Cached Wordbank-derived inputs (feather), including IPA forms for the phonological network. |
| `data/` | Wordbank TD data (`eng_ws_raw_data.Rds`, `eng_wg_raw_data.Rds`) and the list of NDA collections (`NDA data sources.csv`). |
| `journal/` | Manuscript draft. |
| `misc/` | Older analysis notebooks, kept for reference. |

`01_prep_data.Rmd` applies these cleaning steps:
- per-administration quality checks
- collapsing duplicate administrations
- inferring the WG response coding
- collection trust scores
- an item-profile check
- repair of a known column shift in one collection

`06_anchored_DIF.Rmd` runs both forms with and without the unconfirmed-diagnosis collections (about a minute in total).

`04_DIF_analysis.Rmd` saves its model fits and item tables to `data/nda_private/`.

## Data access and privacy

**This repository is public. Do not commit NDA data.**

NDA data are covered by the NDA Data Use Certification. The raw exports are not in this repository. Anyone with NDA access can download them; the collections used are listed in `data/NDA data sources.csv`. The two forms are:
- `mci_sentences02.txt` (WS)
- `mci_words_gestures01.txt` (WG)

Put both exports in `data/ASD CDI data/`.

Everything derived from NDA data, including GUIDs, item-level responses and the model fits, is written to `data/nda_private/`. Both data folders and all rendered `*.html` files are gitignored. Only aggregate results should leave your machine.

## Running the analyses

Requirements: R with these packages:
- tidyverse
- mirt
- gamlss
- clue
- feather
- janitor
- ggrepel
- knitr
- rmarkdown

Run the notebooks in this order:

```r
rmarkdown::render("01_prep_data.Rmd")
rmarkdown::render("03_networks.Rmd")
rmarkdown::render("04_DIF_analysis.Rmd")
rmarkdown::render("04_DIF_analysis.Rmd", params = list(exclude_dx_unconfirmed = TRUE),
                  output_file = "04_DIF_analysis_dxconfirmed.html")
rmarkdown::render("05_IRT_summary.Rmd")
rmarkdown::render("06_anchored_DIF.Rmd")
```

`03_networks.Rmd` and `04_DIF_analysis.Rmd` take parameters for sensitivity analyses:

| Parameter | Effect |
|---|---|
| `exclude_dx_unconfirmed` | Drops collections without a confirmed ASD diagnosis. |
| `tier_A_only` | Keeps only the highest-trust collections. |
| `include_flagged` | Puts back collections that fail the item-profile check. |
| `refit` | Refits the IRT models (`04_DIF_analysis.Rmd` only). Otherwise cached fits are reused. |

## Open issues

- **The collection 2024 repair is not yet confirmed.** It was inferred from the data and should be checked with the submitter. That collection's diagnosis status also needs confirming.
- **Next steps for the DIF analysis:**
  - likelihood-ratio tests against an anchor set, with bootstrap intervals
  - age- or vocabulary-matched comparisons
  - leave-one-collection-out reruns
  - a 2PL check
- **NDA-derived files are still tracked.** Several files from before the cleanup are still in git: `data/ASD_WS.Rdata`, `data/ASD_WG.Rdata`, `data/cached_data.Rds` and the IRT `.Rds` fits. They should be removed from tracking.
