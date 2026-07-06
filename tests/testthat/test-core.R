test_that("calendar year follows June cutoff", {
  expect_equal(bvd_assign_calendar_year(c("2006-05-31", "2006-06-01", "2006-12-31")), c(2005L, 2006L, 2006L))
})

test_that("ownership percentages parse BvD codes", {
  expect_equal(bvd_parse_ownership_pct(c("WO", "MO", "CQP1", "NG", "JO", "10%")), c(100, 50.01, 50.01, 0.01, 50, 10))
})

test_that("identifier standardization creates country and account", {
  x <- data.table::data.table(BVDID = "AT123U", BVDACC = "AT123U", CONSCODE = "U1")
  y <- bvd_standardize_ids(x)
  expect_equal(y$ID_NUMBER, "AT123")
  expect_equal(y$CNTRYCDE, "AT")
  expect_equal(y$CONSCODE2, "U")
})
