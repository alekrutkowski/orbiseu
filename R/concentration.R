rank_within <- function(DT, by, value_col, rank_col = "rank") {
  data.table::setorderv(DT, c(by, value_col), order = c(rep(1L, length(by)), -1L), na.last = TRUE)
  DT[, (rank_col) := seq_len(.N), by = by]
  DT
}

#' Market share of top firms within country-sector-year or EU-sector-year cells
#'
#' @param x Firm-year panel.
#' @param n Number of top firms in the numerator.
#' @param by Cell columns. For country-sector-year use c("CNTRYCDE", "NACE2", "YEAR").
#' @param output_col Output column.
#' @param denominator Denominator: "all", "top20", "top50", "top100", or "top1000".
#' @param account_filter Optional account families to keep, for example "U" or "C".
#' @param account_col Account family column.
#' @param foreign_col Optional foreign ownership indicator.
#' @return A data.table with cell-level concentration measures.
#' @export
bvd_market_share_topn <- function(x,
                                  n = 8L,
                                  by = c("CNTRYCDE", "NACE2", "YEAR"),
                                  output_col = "OPER_TURN",
                                  denominator = c("all", "top20", "top50", "top100", "top1000"),
                                  account_filter = NULL,
                                  account_col = "CONSCODE2",
                                  foreign_col = NULL) {
  denominator <- match.arg(denominator)
  DT <- as_dt(x)
  assert_cols(DT, c(by, output_col))
  if (!is.null(account_filter) && account_col %chin% names(DT)) DT <- DT[get(account_col) %chin% account_filter]
  DT <- DT[!is.na(get(output_col)) & get(output_col) > 0]
  if (!nrow(DT)) return(data.table::data.table())
  DT <- rank_within(DT, by = by, value_col = output_col, rank_col = ".rank__")
  denom_n <- switch(denominator, all = Inf, top20 = 20L, top50 = 50L, top100 = 100L, top1000 = 1000L)
  denom <- DT[.rank__ <= denom_n, .(denominator_output = sum(get(output_col), na.rm = TRUE), denominator_firms = .N), by = by]
  num <- DT[.rank__ <= n, .(top_output = sum(get(output_col), na.rm = TRUE), top_firms = .N), by = by]
  out <- merge(num, denom, by = by, all.x = TRUE, sort = FALSE)
  out[, top_share := safe_divide(top_output, denominator_output)]
  if (!is.null(foreign_col) && foreign_col %chin% names(DT)) {
    fnum <- DT[.rank__ <= n, .(foreign_top_output = sum(fifelse(as.logical(get(foreign_col)), get(output_col), 0), na.rm = TRUE)), by = by]
    out <- merge(out, fnum, by = by, all.x = TRUE, sort = FALSE)
    out[, foreign_top_share := safe_divide(foreign_top_output, denominator_output)]
    out[, domestic_top_share := safe_divide(top_output - foreign_top_output, denominator_output)]
  }
  out[, `:=`(top_n = n, denominator = denominator)]
  out[]
}

#' Concentration measures for top firms
#'
#' Computes top-n market share, CR4, CR8, and Herfindahl within cells. HHI uses
#' all firms included in the denominator subset.
#'
#' @param x Firm-year panel.
#' @param by Cell columns.
#' @param output_col Output column.
#' @param denominator Denominator subset.
#' @param top_n Top-n numerator.
#' @return A data.table.
#' @export
bvd_concentration_topn <- function(x,
                                   by = c("CNTRYCDE", "NACE2", "YEAR"),
                                   output_col = "OPER_TURN",
                                   denominator = c("all", "top50", "top100", "top1000"),
                                   top_n = 8L) {
  denominator <- match.arg(denominator)
  DT <- as_dt(x)
  assert_cols(DT, c(by, output_col))
  DT <- DT[!is.na(get(output_col)) & get(output_col) > 0]
  DT <- rank_within(DT, by = by, value_col = output_col, rank_col = ".rank__")
  denom_n <- switch(denominator, all = Inf, top50 = 50L, top100 = 100L, top1000 = 1000L)
  D <- DT[.rank__ <= denom_n]
  D[, cell_output := sum(get(output_col), na.rm = TRUE), by = by]
  D[, firm_share := safe_divide(get(output_col), cell_output)]
  out <- D[, .(
    top_share = sum(firm_share[.rank__ <= top_n], na.rm = TRUE),
    cr4 = sum(firm_share[.rank__ <= 4L], na.rm = TRUE),
    cr8 = sum(firm_share[.rank__ <= 8L], na.rm = TRUE),
    hhi = sum(firm_share ^ 2, na.rm = TRUE),
    denominator_output = first(cell_output),
    denominator_firms = .N
  ), by = by]
  out[, `:=`(top_n = top_n, denominator = denominator)]
  out[]
}

#' Aggregate concentration to country or EU level
#'
#' The paper reports country-sector concentration and then aggregates by sectoral
#' sales and, for EU-country-weighted means, country GDP. This function implements
#' that two-step aggregation. It also supports EU-wide cells by calling
#' bvd_market_share_topn() with by = c(sector_col, year_col).
#'
#' @param cell_concentration Cell-level output from bvd_market_share_topn() or bvd_concentration_topn().
#' @param gdp Optional table with country-year GDP weights.
#' @param country_col Country column.
#' @param year_col Year column.
#' @param sector_col Sector column.
#' @param measure Measure column to aggregate.
#' @param weight_col Cell weight column, usually denominator_output.
#' @return A list with country_year and eu_country_weighted data.tables.
#' @export
bvd_eu_concentration <- function(cell_concentration,
                                 gdp = NULL,
                                 country_col = "CNTRYCDE",
                                 year_col = "YEAR",
                                 sector_col = "NACE2",
                                 measure = "top_share",
                                 weight_col = "denominator_output") {
  C <- as_dt(cell_concentration)
  assert_cols(C, c(year_col, measure, weight_col))
  if (!sector_col %chin% names(C)) stop("cell_concentration must contain a sector column.", call. = FALSE)
  if (!country_col %chin% names(C)) {
    eu <- C[, .(value = wmean(get(measure), get(weight_col))), by = year_col]
    return(list(country_year = NULL, eu_country_weighted = eu[]))
  }
  cy <- C[, .(value = wmean(get(measure), get(weight_col))), by = c(country_col, year_col)]
  if (is.null(gdp)) {
    eu <- C[, .(value = wmean(get(measure), get(weight_col))), by = year_col]
  } else {
    G <- as_dt(gdp)
    assert_cols(G, c(country_col, year_col, "gdp"))
    cyg <- merge(cy, G[, c(country_col, year_col, "gdp"), with = FALSE], by = c(country_col, year_col), all.x = TRUE)
    eu <- cyg[, .(value = wmean(value, gdp)), by = year_col]
  }
  list(country_year = cy[], eu_country_weighted = eu[])
}
