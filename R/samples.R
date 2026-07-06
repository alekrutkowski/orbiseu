#' Assign firm size bins
#'
#' @param employment Numeric employment vector.
#' @param include_zero Whether to use a separate zero bin.
#' @return Ordered character vector.
#' @export
bvd_size_bins <- function(employment, include_zero = TRUE) {
  e <- suppressWarnings(as.numeric(employment))
  out <- rep(NA_character_, length(e))
  if (include_zero) out[!is.na(e) & e == 0] <- "0"
  out[!is.na(e) & e >= 1 & e <= 19] <- "1_19"
  out[!is.na(e) & e >= 20 & e <= 249] <- "20_249"
  out[!is.na(e) & e >= 250] <- "250_plus"
  out
}

#' Add total-sample and TFP-sample flags
#'
#' @param x Firm-year panel.
#' @param employment_col Employment column.
#' @param wage_col Wage bill/personnel cost column.
#' @param output_col Output column.
#' @param tangible_col Tangible fixed assets column.
#' @param materials_col Materials column.
#' @return A data.table with total_sample and tfp_sample columns.
#' @export
bvd_sample_flags <- function(x,
                             employment_col = "EMPL",
                             wage_col = "STAF",
                             output_col = "OPER_TURN",
                             tangible_col = "TGFIXEDASSTS",
                             materials_col = "MATE") {
  DT <- as_dt(x)
  getnum <- function(col) if (col %chin% names(DT)) DT[[col]] else rep(NA_real_, nrow(DT))
  emp <- getnum(employment_col)
  wage <- getnum(wage_col)
  out <- getnum(output_col)
  tan <- getnum(tangible_col)
  mat <- getnum(materials_col)
  DT[, total_sample := !is.na(out) & out > 0]
  DT[, tfp_sample := ((!is.na(emp) & emp > 0) | (!is.na(wage) & wage > 0)) & !is.na(out) & out > 0 & !is.na(tan) & tan > 0 & !is.na(mat) & mat > 0]
  DT[]
}

#' Firm size distribution by output and employment
#'
#' @param x Firm-year panel.
#' @param by Grouping columns, usually country and year.
#' @param employment_col Employment column.
#' @param output_col Output column.
#' @return A data.table of bin shares.
#' @export
bvd_size_distribution <- function(x,
                                  by = c("CNTRYCDE", "YEAR"),
                                  employment_col = "EMPL",
                                  output_col = "OPER_TURN") {
  DT <- as_dt(x)
  assert_cols(DT, existing_cols(DT, by))
  if (!employment_col %chin% names(DT)) stop("employment column not found.", call. = FALSE)
  DT[, size_bin := bvd_size_bins(get(employment_col))]
  DT <- DT[!is.na(size_bin)]
  if (!output_col %chin% names(DT)) DT[, (output_col) := NA_real_]
  agg <- DT[, .(
    output = sum(get(output_col), na.rm = TRUE),
    employment = sum(get(employment_col), na.rm = TRUE),
    firms = .N
  ), by = c(by, "size_bin")]
  totals <- agg[, .(output_total = sum(output, na.rm = TRUE), employment_total = sum(employment, na.rm = TRUE), firms_total = sum(firms)), by = by]
  out <- merge(agg, totals, by = by, all.x = TRUE)
  out[, output_share := safe_divide(output, output_total)]
  out[, employment_share := safe_divide(employment, employment_total)]
  out[, firm_share := safe_divide(firms, firms_total)]
  data.table::setorderv(out, c(by, "size_bin"))
  out[]
}

#' SME shares following the paper's bins
#'
#' Computes the output and employment shares of 1 to 19, 20 to 249, and 250 plus
#' employee firms. The paper emphasizes 20 to 249 employees as the SME bin in its
#' main application.
#'
#' @param x Firm-year panel.
#' @param by Grouping columns.
#' @param employment_col Employment column.
#' @param output_col Output column.
#' @return A data.table.
#' @export
bvd_sme_shares <- function(x,
                           by = c("CNTRYCDE", "YEAR"),
                           employment_col = "EMPL",
                           output_col = "OPER_TURN") {
  dist <- bvd_size_distribution(x, by = by, employment_col = employment_col, output_col = output_col)
  dist[, .(
    output_share_1_19 = sum(output_share[size_bin == "1_19"], na.rm = TRUE),
    output_share_20_249 = sum(output_share[size_bin == "20_249"], na.rm = TRUE),
    output_share_1_249 = sum(output_share[size_bin %chin% c("1_19", "20_249")], na.rm = TRUE),
    employment_share_1_19 = sum(employment_share[size_bin == "1_19"], na.rm = TRUE),
    employment_share_20_249 = sum(employment_share[size_bin == "20_249"], na.rm = TRUE),
    employment_share_1_249 = sum(employment_share[size_bin %chin% c("1_19", "20_249")], na.rm = TRUE)
  ), by = by]
}
