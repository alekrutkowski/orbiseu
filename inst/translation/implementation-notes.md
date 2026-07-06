# Implementation notes

This package translates the reusable parts of the Kalemli-Özcan et al. (2024) article and replication do files into R.

## Core choices

1. Historical vintages are combined rather than relying on a single vintage. `bvd_merge_vintages()` implements the merge/update/replace pattern.
2. Relative-year exports are reshaped with `bvd_reshape_relative_years()`. Calendar year is inferred from the account closing date using the June 1 rule.
3. Firm identifiers are standardized with `bvd_standardize_ids()`. The BvD account suffix is used to infer consolidated or unconsolidated account family when needed.
4. General cleaning is handled by `bvd_clean_financial_panel()`: no core financial data, negative assets, negative employment, implausibly huge employment, negative sales, and negative tangible fixed assets.
5. Account duplicate handling is explicit. Use `prefer_consolidated`, `prefer_unconsolidated`, `longest_timeseries`, or `keep_all` depending on the exercise.
6. Ownership links are parsed with `bvd_prepare_shareholder_links()` and aggregated with `bvd_aggregate_ownership()`. The default foreign threshold is 10 percent.
7. Eurostat data are fetched through `eurodata::importData()` using wrapper functions. The wrappers are intentionally transparent because Eurostat dataset dimensions differ across tables and vintages.

## What is not bundled

The package does not include Orbis, Amadeus, Compustat, OECD STAN, World Bank, or Eurostat data extracts from the replication archive. It includes only code, tiny synthetic examples, and code-list helpers.

## Citation

Cite Kalemli-Özcan et al. (2024), doi:10.1257/mac.20220036, when using the methodology. Cite `orbiseu` when using this R implementation.
