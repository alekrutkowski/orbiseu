resolve_same_year_reports <- function(DT,
                                      by = c("ID_NUMBER", "YEAR", "CONSCODE2"),
                                      output_col = "OPER_TURN",
                                      close_col = "CLOSEDATE",
                                      financial_cols = bvd_financial_vars()) {
  by <- existing_cols(DT, by)
  if (!length(by)) return(DT)
  cols <- existing_cols(DT, financial_cols)
  DT[, .nonmiss_fin__ := if (length(cols)) rowSums(!data.table::as.data.table(lapply(.SD, is_blank))) else 0L, .SDcols = cols]
  if (output_col %chin% names(DT)) DT[, .out_rank__ := fifelse(is.na(get(output_col)), -Inf, get(output_col))] else DT[, .out_rank__ := 0]
  if (close_col %chin% names(DT)) DT[, .close_rank__ := as.integer(parse_idate(get(close_col)))] else DT[, .close_rank__ := 0L]
  data.table::setorderv(DT, c(by, ".nonmiss_fin__", ".out_rank__", ".close_rank__"), order = c(rep(1L, length(by)), -1L, -1L, -1L), na.last = TRUE)
  out <- unique(DT, by = by, fromLast = FALSE)
  out[, c(".nonmiss_fin__", ".out_rank__", ".close_rank__") := NULL]
  out[]
}

#' Clean one raw BvD financial vintage
#'
#' Performs the reusable parts of the Stata vintage cleaning files: standardizes
#' identifiers, converts relative years when needed, applies units, converts
#' financial strings to numeric values, drops empty rows, removes obvious country
#' mismatches, and resolves duplicate reports within firm-year-account cells.
#'
#' @param x Raw vintage data.
#' @param already_long TRUE if x is already one row per firm-account-year.
#' @param n_years Number of relative years when already_long is FALSE.
#' @param id_col BvD ID column.
#' @param account_col BvD account-number column.
#' @param conscode_col BvD consolidation-code column.
#' @param closing_date_col Account closing date column after reshaping.
#' @param country_col Optional BvD country code column.
#' @param unit_col Units column.
#' @param currency_col Currency column.
#' @param financial_cols Financial columns to make numeric and check.
#' @param vintage Optional vintage label added to the output.
#' @return A data.table.
#' @export
bvd_clean_vintage <- function(x,
                              already_long = TRUE,
                              n_years = 5L,
                              id_col = "BVDID",
                              account_col = "BVDACC",
                              conscode_col = "CONSCODE",
                              closing_date_col = "CLOSEDATE",
                              country_col = "COUNTRY",
                              unit_col = "UNITS",
                              currency_col = "CURRENCY",
                              financial_cols = bvd_financial_vars(),
                              vintage = NULL) {
  DT <- as_dt(x)
  if (!already_long) DT <- bvd_reshape_relative_years(DT, n_years = n_years)
  if (!"YEAR" %chin% names(DT) && closing_date_col %chin% names(DT)) DT[, YEAR := bvd_assign_calendar_year(get(closing_date_col))]
  DT <- bvd_standardize_ids(DT, id_col = id_col, account_col = account_col, conscode_col = conscode_col, country_col = country_col, inplace = TRUE)
  if (!is.null(vintage)) DT[, vintage := vintage]
  num_cols <- existing_cols(DT, financial_cols)
  if (length(num_cols)) DT[, (num_cols) := lapply(.SD, to_numeric), .SDcols = num_cols]
  if (unit_col %chin% names(DT)) DT <- apply_units(DT, unit_col = unit_col, financial_cols = num_cols)
  if (length(num_cols)) {
    keep <- DT[, rowSums(!data.table::as.data.table(lapply(.SD, is_blank)), na.rm = TRUE) > 0L, .SDcols = num_cols]
    DT <- DT[keep]
  }
  DT <- DT[!is_blank(ID_NUMBER)]
  if (country_col %chin% names(DT)) {
    src_country <- normalize_chr(DT[[country_col]])
    DT <- DT[is_blank(src_country) | src_country == CNTRYCDE]
  }
  if (currency_col %chin% names(DT)) DT <- DT[!is_blank(get(currency_col))]
  if (closing_date_col %chin% names(DT)) DT <- DT[!is_blank(get(closing_date_col))]
  DT <- resolve_same_year_reports(DT, by = existing_cols(DT, c("ID_NUMBER", "YEAR", "CONSCODE2")), output_col = "OPER_TURN", close_col = closing_date_col, financial_cols = num_cols)
  data.table::setorderv(DT, existing_cols(DT, c("ID_NUMBER", "YEAR", "CONSCODE2")))
  DT[]
}

#' Clean a merged BvD financial panel
#'
#' Applies the general post-merge filters in Appendix A.5.3: removes rows with
#' no core financial information, drops firms with negative assets, negative or
#' implausibly huge employment, negative sales, negative tangible fixed assets,
#' optional bad unit switches, and fills stable strings within firm histories.
#'
#' @param x Firm-year panel.
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @param asset_col Total assets column.
#' @param revenue_col Operating revenue column.
#' @param sales_col Sales column.
#' @param employment_col Employment column.
#' @param tangible_col Tangible fixed assets column.
#' @param string_cols Stable string columns to fill within firm.
#' @param drop_bad_unit_switches Use the units consistency filter when possible.
#' @return A data.table.
#' @export
bvd_clean_financial_panel <- function(x,
                                      id_col = "ID_NUMBER",
                                      year_col = "YEAR",
                                      asset_col = "TOTASSTS",
                                      revenue_col = "OPER_TURN",
                                      sales_col = "SALE",
                                      employment_col = "EMPL",
                                      tangible_col = "TGFIXEDASSTS",
                                      string_cols = c("CTRYISO", "COUNTRY", "NAME", "CITY", "REGION", "POSTCODE", "LEGALFRM", "DATEINC"),
                                      drop_bad_unit_switches = FALSE) {
  DT <- as_dt(x)
  assert_cols(DT, c(id_col, year_col))
  core <- existing_cols(DT, c(asset_col, revenue_col, sales_col, employment_col))
  if (length(core)) {
    keep <- DT[, rowSums(!data.table::as.data.table(lapply(.SD, is_blank)), na.rm = TRUE) > 0L, .SDcols = core]
    DT <- DT[keep]
  }
  drop_firms <- character()
  add_bad <- function(col, predicate) {
    if (!col %chin% names(DT)) return(invisible(NULL))
    bad <- DT[, .(bad = predicate(get(col))), by = id_col][bad == TRUE, get(id_col)]
    drop_firms <<- union(drop_firms, bad)
    invisible(NULL)
  }
  add_bad(asset_col, function(v) any(v < 0, na.rm = TRUE))
  add_bad(employment_col, function(v) any(v < 0, na.rm = TRUE) || any(v >= 2e6, na.rm = TRUE))
  add_bad(sales_col, function(v) any(v < 0, na.rm = TRUE))
  add_bad(tangible_col, function(v) any(v < 0, na.rm = TRUE))
  if (drop_bad_unit_switches && "UNITS" %chin% names(DT) && asset_col %chin% names(DT)) {
    bad <- unique(DT[flag_bad_unit_switches(DT, id_col = id_col, year_col = year_col, asset_col = asset_col), get(id_col)])
    drop_firms <- union(drop_firms, bad)
  }
  if (length(drop_firms)) DT <- DT[!get(id_col) %chin% drop_firms]
  DT <- fill_by_id(DT, id_col = id_col, year_col = year_col, cols = string_cols)
  data.table::setorderv(DT, c(id_col, year_col))
  DT[]
}
