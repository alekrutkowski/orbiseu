test_that("size distribution and concentration work on demo data", {
  demo <- bvd_read(system.file("extdata", "orbis_demo.csv", package = "orbiseu"))
  demo <- bvd_clean_vintage(demo, already_long = TRUE)
  demo <- bvd_sample_flags(demo)
  dist <- bvd_size_distribution(demo, by = c("CNTRYCDE", "YEAR"))
  expect_true(all(c("output_share", "employment_share") %in% names(dist)))
  ms <- bvd_market_share_topn(demo, n = 2, by = c("CNTRYCDE", "NACE2", "YEAR"))
  expect_true(all(ms$top_share <= 1))
})

test_that("coverage uses official denominator", {
  demo <- bvd_read(system.file("extdata", "orbis_demo.csv", package = "orbiseu"))
  demo <- bvd_clean_vintage(demo, already_long = TRUE)
  demo <- merge(demo, nace_rev2_level2()[, .(NACE2 = nace2, NACE1 = nace1)], by = "NACE2", all.x = TRUE)
  official <- data.table::fread(system.file("extdata", "eurostat_demo.csv", package = "orbiseu"))
  cov <- bvd_validate_coverage(demo, official, by = c("CNTRYCDE", "YEAR"))
  expect_true("coverage_ratio" %in% names(cov))
})
