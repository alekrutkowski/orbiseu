#' Build a representative Orbis EU panel from cleaned or raw vintages
#'
#' Convenience pipeline that runs vintage cleaning, vintage merge, account
#' duplicate resolution, optional switcher removal, and general post-merge
#' cleaning. For very large Orbis extracts, run the same operations country by
#' country or in database-backed chunks.
#'
#' @param vintages List of raw or cleaned vintage tables.
#' @param already_long Are the input vintages already firm-account-year tables?
#' @param id_changes Optional ID correspondence table.
#' @param account_strategy Account duplicate strategy.
#' @param drop_switchers Drop firms switching account family over time.
#' @param clean_vintages Run bvd_clean_vintage() on each input.
#' @param ... Passed to bvd_clean_vintage().
#' @return A list with panel and diagnostics.
#' @export
bvd_make_representative_panel <- function(vintages,
                                          already_long = TRUE,
                                          id_changes = NULL,
                                          account_strategy = "prefer_consolidated",
                                          drop_switchers = TRUE,
                                          clean_vintages = TRUE,
                                          ...) {
  if (clean_vintages) {
    nms <- names(vintages)
    if (is.null(nms)) nms <- rep("", length(vintages))
    nms[is.na(nms) | !nzchar(nms)] <- paste0("v", which(is.na(nms) | !nzchar(nms)))
    names(vintages) <- nms
    cleaned <- Map(function(x, nm) bvd_clean_vintage(x, already_long = already_long, vintage = nm, ...), vintages, nms)
  } else {
    cleaned <- vintages
  }
  merged <- bvd_merge_vintages(cleaned, id_changes = id_changes)
  dup_before <- bvd_detect_account_duplicates(merged)
  resolved <- bvd_resolve_account_duplicates(merged, strategy = account_strategy)
  if (drop_switchers) resolved <- bvd_drop_account_switchers(resolved, action = "drop")
  panel <- bvd_clean_financial_panel(resolved)
  panel <- bvd_sample_flags(panel)
  list(
    panel = panel,
    diagnostics = list(
      n_vintages = length(vintages),
      rows_after_merge = nrow(merged),
      account_duplicates_before_resolution = dup_before,
      rows_final = nrow(panel),
      firms_final = data.table::uniqueN(panel$ID_NUMBER)
    )
  )
}

#' Create a compact pipeline report
#'
#' @param panel A final firm-year panel.
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @return A data.table with core counts by year.
#' @export
orbis_pipeline_report <- function(panel, id_col = "ID_NUMBER", year_col = "YEAR") {
  DT <- as_dt(panel)
  assert_cols(DT, c(id_col, year_col))
  DT[, .(
    firms = data.table::uniqueN(get(id_col)),
    firm_years = .N,
    output = if ("OPER_TURN" %chin% names(DT)) sum(OPER_TURN, na.rm = TRUE) else NA_real_,
    employment = if ("EMPL" %chin% names(DT)) sum(EMPL, na.rm = TRUE) else NA_real_,
    total_sample = if ("total_sample" %chin% names(DT)) sum(total_sample, na.rm = TRUE) else NA_integer_,
    tfp_sample = if ("tfp_sample" %chin% names(DT)) sum(tfp_sample, na.rm = TRUE) else NA_integer_
  ), by = year_col][order(get(year_col))]
}
