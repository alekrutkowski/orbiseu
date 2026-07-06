library(orbiseu)
library(data.table)
library(magrittr)

# Minimal demo using package example data.
demo <- bvd_read(system.file("extdata", "orbis_demo.csv", package = "orbiseu")) %>%
  bvd_clean_vintage(already_long = TRUE) %>%
  bvd_sample_flags()

demo <- merge(demo, nace_rev2_level2()[, .(NACE2 = nace2, NACE1 = nace1)], by = "NACE2", all.x = TRUE)

ownership <- bvd_read(system.file("extdata", "ownership_demo.csv", package = "orbiseu")) %>%
  bvd_prepare_shareholder_links() %>%
  bvd_aggregate_ownership(threshold = 10)

demo <- bvd_merge_ownership_financials(demo, ownership)

print(orbis_pipeline_report(demo))
print(bvd_sme_shares(demo, by = c("CNTRYCDE", "YEAR")))
print(bvd_market_share_topn(demo, n = 2, by = c("CNTRYCDE", "NACE2", "YEAR"), foreign_col = "foreign_owned"))
