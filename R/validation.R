add_nace_levels <- function(DT, nace_col = "NACE2") {
  if ("NACE1" %chin% names(DT)) return(DT)
  if (!nace_col %chin% names(DT)) return(DT)
  n2 <- nace_rev2_level2()[, .(NACE2 = nace2, NACE1 = nace1)]
  DT[, NACE2 := sprintf("%02d", suppressWarnings(as.integer(substr(as.character(get(nace_col)), 1L, 2L))))]
  merge(DT, n2, by = "NACE2", all.x = TRUE, sort = FALSE)
}

#' Validate Orbis coverage against official aggregates
#'
#' Computes the Orbis-to-official ratio for common country-sector-year cells and
#' aggregates to a selected level. For manufacturing validation, use NACE1 == "C"
#' and Eurostat SBS turnover V12110 as the official denominator.
#'
#' @param orbis Firm-level Orbis panel.
#' @param official Eurostat or other official aggregate table.
#' @param orbis_value Firm-level value column, usually OPER_TURN or EMPL.
#' @param official_value Official value column, usually VALUE.
#' @param firm_sector_col Firm sector column, usually NACE2 or NACE1.
#' @param official_sector_col Official sector column, usually NACE1 or NACE2.
#' @param by Aggregation level for returned ratios.
#' @param common_only If TRUE, only common official cells enter the Orbis numerator.
#' @return A data.table with coverage ratios.
#' @export
bvd_validate_coverage <- function(orbis,
                                  official,
                                  orbis_value = "OPER_TURN",
                                  official_value = "VALUE",
                                  firm_sector_col = "NACE2",
                                  official_sector_col = "NACE1",
                                  by = c("CNTRYCDE", "YEAR"),
                                  common_only = TRUE) {
  O <- as_dt(orbis)
  E <- as_dt(official)
  assert_cols(O, c("CNTRYCDE", "YEAR", orbis_value))
  assert_cols(E, c("CNTRYCDE", "YEAR", official_value))
  O <- add_nace_levels(O, firm_sector_col)
  E <- add_nace_levels(E, official_sector_col)
  sec <- if ("NACE1" %chin% names(O) && "NACE1" %chin% names(E)) "NACE1" else if ("NACE2" %chin% names(O) && "NACE2" %chin% names(E)) "NACE2" else NULL
  cell_by <- c("CNTRYCDE", "YEAR", sec)
  cell_by <- cell_by[!is.na(cell_by)]
  oa <- O[, .(orbis_value = sum(get(orbis_value), na.rm = TRUE)), by = cell_by]
  ea <- E[, .(official_value = sum(get(official_value), na.rm = TRUE)), by = cell_by]
  M <- merge(oa, ea, by = cell_by, all = !common_only)
  out <- M[, .(orbis_value = sum(orbis_value, na.rm = TRUE), official_value = sum(official_value, na.rm = TRUE), n_cells = .N), by = by]
  out[, coverage_ratio := safe_divide(orbis_value, official_value)]
  out[]
}
