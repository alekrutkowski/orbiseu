# orbiseu

> [!WARNING]
> Not properly tests, work in progress. Contributions/correction welcome.

**`orbiseu`** is a data.table-based **[R](https://www.r-project.org/)** package toolkit for making licensed Moody's Orbis firm-level exports usable as representative European firm-year panels. It translates the workflow of Kalemli-Özcan, Sørensen, Villegas-Sanchez, Volosovych, and Yeşiltaş (2024), "How to Construct Nationally Representative Firm-Level Data from the Orbis Global Database", American Economic Journal: Macroeconomics, [doi:10.1257/mac.20220036](https://doi.org/10.1257/mac.20220036), into reusable R functions.

The package does not contain or redistribute Orbis data. It provides tools to clean exports you are licensed to use and to benchmark them against Eurostat data fetched via the CRAN package `eurodata`.

## Main workflow

```r
library(orbiseu)
library(data.table)
library(magrittr)

vintages <- list(
  orbis_2009 = bvd_read("orbis_2009_export.csv"),
  orbis_2013 = bvd_read("orbis_2013_export.csv"),
  amadeus_2014 = bvd_read("amadeus_2014_export.csv")
)

panel <- bvd_make_representative_panel(
  vintages,
  already_long = FALSE,
  n_years = 5,
  account_strategy = "prefer_consolidated",
  drop_switchers = TRUE
)$panel

orbis_pipeline_report(panel)
```

## Eurostat validation with `eurodata`

```r
sbs_turnover <- eurostat_fetch_sbs(
  code = "sbs_sc_sca_r2",
  countries = c("AT", "BE", "CZ", "DE", "ES", "FR", "IT"),
  years = 2008:2012,
  indicator = "V12110",
  size_class = "TOTAL"
)

coverage <- bvd_validate_coverage(
  panel[NACE1 == "C"],
  sbs_turnover[NACE1 == "C"],
  orbis_value = "OPER_TURN",
  official_value = "VALUE"
)
```

## SME shares and concentration

```r
sme <- bvd_sme_shares(panel, by = c("CNTRYCDE", "YEAR"))

ms8 <- bvd_market_share_topn(
  panel,
  n = 8,
  by = c("CNTRYCDE", "NACE2", "YEAR"),
  denominator = "all",
  foreign_col = "foreign_owned"
)

eu_ms8 <- bvd_eu_concentration(ms8, gdp = gdp_weights)
```

## Stata-to-R translation map

See `inst/translation/stata_to_r_crosswalk.csv` for a file-by-file mapping from the uploaded replication do files to package functions. The package covers the practical components: relative-year reshaping, BvD ID handling, vintage merging, account duplicate resolution, ownership aggregation, Eurostat benchmarking, SME shares, and concentration measures.

## Installation

```r
remotes::install_github('alekrutkowski/orbiseu')
```

## Citation

Use `citation("orbiseu")`. Please cite Kalemli-Özcan et al. (2024), https://doi.org/10.1257/mac.20220036, whenever the methodology is used.
