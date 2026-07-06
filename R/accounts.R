#' Detect firm-year observations with more than one account type
#'
#' @param x A firm-year-account panel.
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @param account_col Account family column, usually CONSCODE2.
#' @return A data.table of duplicated firm-years.
#' @export
bvd_detect_account_duplicates <- function(x, id_col = "ID_NUMBER", year_col = "YEAR", account_col = "CONSCODE2") {
  DT <- as_dt(x)
  assert_cols(DT, c(id_col, year_col, account_col))
  DT[, .(n_rows = .N, n_account_types = data.table::uniqueN(get(account_col)), account_types = paste(sort(unique(get(account_col))), collapse = ";")), by = c(id_col, year_col)][n_rows > 1L | n_account_types > 1L]
}

#' Summarize account types
#'
#' @param x Firm-year-account panel.
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @param account_col Account family column.
#' @return A data.table with counts by year and account type.
#' @export
bvd_account_summary <- function(x, id_col = "ID_NUMBER", year_col = "YEAR", account_col = "CONSCODE2") {
  DT <- as_dt(x)
  assert_cols(DT, c(id_col, year_col, account_col))
  DT[, .(firm_years = .N, firms = data.table::uniqueN(get(id_col))), by = c(year_col, account_col)][order(get(year_col), get(account_col))]
}

#' Resolve consolidated and unconsolidated duplicate account rows
#'
#' The source paper uses account-type rules in several contexts. This function is
#' intentionally explicit: choose a strategy suitable for a validation exercise,
#' concentration exercise, or sensitivity analysis.
#'
#' @param x Firm-year-account panel.
#' @param strategy One of: "prefer_consolidated", "prefer_unconsolidated",
#'   "longest_timeseries", "keep_all".
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @param account_col Account family column.
#' @param output_col Output column used as a tie-breaker.
#' @return A data.table.
#' @export
bvd_resolve_account_duplicates <- function(x,
                                           strategy = c("prefer_consolidated", "prefer_unconsolidated", "longest_timeseries", "keep_all"),
                                           id_col = "ID_NUMBER",
                                           year_col = "YEAR",
                                           account_col = "CONSCODE2",
                                           output_col = "OPER_TURN") {
  strategy <- match.arg(strategy)
  DT <- as_dt(x)
  assert_cols(DT, c(id_col, year_col, account_col))
  if (strategy == "keep_all") return(DT[])
  DT[, .ts_len__ := .N, by = c(id_col, account_col)]
  acc <- normalize_chr(DT[[account_col]])
  pref <- switch(
    strategy,
    prefer_consolidated = fifelse(acc == "C", 3L, fifelse(acc == "U", 2L, 1L)),
    prefer_unconsolidated = fifelse(acc == "U", 3L, fifelse(acc == "C", 2L, 1L)),
    longest_timeseries = fifelse(acc == "U", 2L, fifelse(acc == "C", 1L, 0L))
  )
  DT[, .pref_account__ := pref]
  if (output_col %chin% names(DT)) DT[, .out__ := fifelse(is.na(get(output_col)), -Inf, get(output_col))] else DT[, .out__ := 0]
  order_cols <- c(id_col, year_col, ".pref_account__", ".ts_len__", ".out__")
  data.table::setorderv(DT, order_cols, order = c(1L, 1L, -1L, -1L, -1L), na.last = TRUE)
  out <- unique(DT, by = c(id_col, year_col), fromLast = FALSE)
  out[, c(".ts_len__", ".pref_account__", ".out__") := NULL]
  data.table::setorderv(out, c(id_col, year_col))
  out[]
}

#' Drop or mark firms that switch account family over time
#'
#' Switchers are firms whose observed series combines account families, for
#' example U before 2007 and C after 2007. Dropping them is useful in the paper's
#' concentration application because regulatory changes can otherwise create
#' artificial sales jumps.
#'
#' @param x Firm-year panel.
#' @param action "drop" or "mark".
#' @param id_col Firm ID column.
#' @param account_col Account family column.
#' @return A data.table.
#' @export
bvd_drop_account_switchers <- function(x,
                                       action = c("drop", "mark"),
                                       id_col = "ID_NUMBER",
                                       account_col = "CONSCODE2") {
  action <- match.arg(action)
  DT <- as_dt(x)
  assert_cols(DT, c(id_col, account_col))
  sw <- DT[!is_blank(get(account_col)), .(account_types = data.table::uniqueN(get(account_col))), by = id_col][account_types > 1L, get(id_col)]
  DT[, account_switcher := get(id_col) %chin% sw]
  if (action == "drop") DT <- DT[account_switcher == FALSE]
  DT[]
}
