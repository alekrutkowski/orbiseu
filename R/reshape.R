#' Reshape BvD relative-year exports to firm-year format
#'
#' BvD historical disk exports often store variables as X1, X2, ..., where 1 is
#' latest year and larger suffixes are older relative years. This function turns
#' such wide extracts into a long panel and optionally replaces the relative year
#' with the calendar year inferred from CLOSEDATE.
#'
#' @param x A wide data.frame or data.table.
#' @param stubs Variable stubs to reshape. If NULL, stubs are detected from names
#'   ending in 1:n_years.
#' @param id_cols Non-time-varying columns to keep. If NULL, columns without a
#'   recognized relative-year suffix are kept.
#' @param n_years Number of relative years in the export.
#' @param last_year_col Column containing the BvD latest reporting year.
#' @param relative_index_col Name of the generated relative-year index.
#' @param year_col Output calendar-year column.
#' @param closing_date_stub Stub for closing date columns, typically CLOSEDATE.
#' @param use_closing_date If TRUE, year_col is computed from account closing dates.
#' @return A long data.table.
#' @export
bvd_reshape_relative_years <- function(x,
                                       stubs = NULL,
                                       id_cols = NULL,
                                       n_years = 5L,
                                       last_year_col = "LASTYEAR",
                                       relative_index_col = "REL_YEAR",
                                       year_col = "YEAR",
                                       closing_date_stub = "CLOSEDATE",
                                       use_closing_date = TRUE) {
  DT <- as_dt(x)
  n_years <- as.integer(n_years)
  suffixes <- as.character(seq_len(n_years))
  if (is.null(stubs)) {
    rx <- paste0("^(.+)(", paste(suffixes, collapse = "|"), ")$")
    parts <- regexec(rx, names(DT))
    got <- regmatches(names(DT), parts)
    stubs <- unique(vapply(got[lengths(got) > 0L], `[`, character(1L), 2L))
  }
  if (is.null(id_cols)) {
    time_cols <- unlist(lapply(stubs, function(s) paste0(s, suffixes)), use.names = FALSE)
    id_cols <- setdiff(names(DT), intersect(names(DT), time_cols))
  }
  min_hits <- if (n_years <= 1L) 1L else 2L
  stubs <- stubs[vapply(stubs, function(s) sum(paste0(s, suffixes) %chin% names(DT)) >= min_hits, logical(1L))]
  if (!length(stubs)) stop("No relative-year variable stubs detected. Pass stubs explicitly if your export contains only one relative year.", call. = FALSE)
  longs <- lapply(seq_len(n_years), function(j) {
    src <- paste0(stubs, j)
    keep <- src %chin% names(DT)
    ans <- DT[, ..id_cols]
    if (any(keep)) {
      tmp <- DT[, src[keep], with = FALSE]
      data.table::setnames(tmp, src[keep], stubs[keep])
      ans <- data.table::cbind(ans, tmp)
    }
    ans[, (relative_index_col) := j]
    ans
  })
  out <- data.table::rbindlist(longs, use.names = TRUE, fill = TRUE)
  if (last_year_col %chin% names(out)) out[, (year_col) := as_year(get(last_year_col)) - get(relative_index_col) + 1L]
  if (use_closing_date && closing_date_stub %chin% names(out)) out[, (year_col) := bvd_assign_calendar_year(get(closing_date_stub))]
  varying <- existing_cols(out, setdiff(stubs, closing_date_stub))
  if (length(varying)) {
    keep <- out[, rowSums(!data.table::as.data.table(lapply(.SD, is_blank)), na.rm = TRUE) > 0L, .SDcols = varying]
    out <- out[keep]
  }
  out[]
}
