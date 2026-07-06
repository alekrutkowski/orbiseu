#' Normalize BvD consolidation codes
#'
#' Converts BvD consolidation labels such as C1, C2, U1, U2, LF, NF and account
#' suffixes into a compact account family C, U, L, N, or NA.
#'
#' @param conscode Vector of BvD consolidation codes.
#' @param bvd_account Optional vector of BvD account numbers, whose final letter
#'   often contains C or U.
#' @return Character vector.
#' @export
bvd_normalize_conscode <- function(conscode = NULL, bvd_account = NULL) {
  cc <- if (is.null(conscode)) rep(NA_character_, length(bvd_account)) else normalize_chr(conscode)
  acc <- if (is.null(bvd_account)) rep(NA_character_, length(cc)) else as.character(bvd_account)
  suffix <- ifelse(!is.na(acc) & nzchar(acc), toupper(substr(acc, nchar(acc), nchar(acc))), NA_character_)
  out <- rep(NA_character_, length(cc))
  out[cc %chin% c("C1", "C2", "C", "CONSOLIDATED")] <- "C"
  out[cc %chin% c("U1", "U2", "U", "UNCONSOLIDATED")] <- "U"
  out[cc %chin% c("LF", "NRLF")] <- "L"
  out[cc %chin% c("NF", "NRF")] <- "N"
  out[is.na(out) & suffix %chin% c("C", "U")] <- suffix[is.na(out) & suffix %chin% c("C", "U")]
  out
}

strip_account_suffix <- function(x) {
  z <- as.character(x)
  hit <- !is.na(z) & grepl("[CU]$", z)
  z[hit] <- substr(z[hit], 1L, nchar(z[hit]) - 1L)
  z
}

#' Standardize BvD firm identifiers
#'
#' Creates ID_NUMBER, CNTRYCDE, CONSCODE_DETAIL, and CONSCODE2 in the style of
#' the paper's data construction code. ID_NUMBER removes the final account suffix
#' when a BvD account number is used.
#'
#' @param x A data.frame or data.table.
#' @param id_col BvD ID number column.
#' @param account_col BvD account number column.
#' @param conscode_col BvD consolidation code column.
#' @param country_col Optional country ISO-2 column supplied by BvD.
#' @param inplace Modify by reference.
#' @return A data.table.
#' @export
bvd_standardize_ids <- function(x,
                                id_col = "BVDID",
                                account_col = "BVDACC",
                                conscode_col = "CONSCODE",
                                country_col = NULL,
                                inplace = FALSE) {
  DT <- as_dt(x, copy = !inplace)
  id_col <- if (id_col %chin% names(DT)) id_col else if ("BVD_ID_NUMBER" %chin% names(DT)) "BVD_ID_NUMBER" else id_col
  account_col <- if (account_col %chin% names(DT)) account_col else if ("BVD_ACCOUNT_NUMBER" %chin% names(DT)) "BVD_ACCOUNT_NUMBER" else account_col
  if (!id_col %chin% names(DT) && !account_col %chin% names(DT)) stop("No BvD ID or BvD account column found.", call. = FALSE)
  id <- if (id_col %chin% names(DT)) as.character(DT[[id_col]]) else rep(NA_character_, nrow(DT))
  acc <- if (account_col %chin% names(DT)) as.character(DT[[account_col]]) else rep(NA_character_, nrow(DT))
  cons <- if (conscode_col %chin% names(DT)) as.character(DT[[conscode_col]]) else rep(NA_character_, nrow(DT))
  detail <- normalize_chr(cons)
  detail[is_blank(detail)] <- NA_character_
  base_id <- ifelse(!is_blank(id), id, strip_account_suffix(acc))
  base_id <- strip_account_suffix(base_id)
  DT[, ID_NUMBER := base_id]
  DT[, CONSCODE_DETAIL := detail]
  DT[, CONSCODE2 := bvd_normalize_conscode(detail, acc)]
  DT[, CNTRYCDE := substr(ID_NUMBER, 1L, 2L)]
  if (!is.null(country_col) && country_col %chin% names(DT)) DT[, BVD_COUNTRY_ORIGINAL := as.character(get(country_col))]
  DT
}

#' Assign calendar year from BvD account closing dates
#'
#' The paper assigns the current year if the closing date is on or after June 1,
#' and the previous year otherwise.
#'
#' @param closedate Date-like vector.
#' @param cutoff_month Month number. Default is 6.
#' @param cutoff_day Day of month. Default is 1.
#' @return Integer year vector.
#' @export
bvd_assign_calendar_year <- function(closedate, cutoff_month = 6L, cutoff_day = 1L) {
  d <- parse_idate(closedate)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  dd <- as.integer(format(d, "%d"))
  before <- !is.na(m) & (m < cutoff_month | (m == cutoff_month & dd < cutoff_day))
  y[before] <- y[before] - 1L
  y
}
