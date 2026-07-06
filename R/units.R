parse_units_power <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  z <- tolower(trimws(as.character(x)))
  out <- suppressWarnings(as.integer(z))
  out[z %chin% c("unit", "units", "one", "ones", "1")] <- 0L
  out[z %chin% c("thousand", "thousands", "'000", "000", "k", "10^3")] <- 3L
  out[z %chin% c("million", "millions", "m", "mn", "10^6")] <- 6L
  out[z %chin% c("billion", "billions", "bn", "10^9")] <- 9L
  out[is.na(out) & is_blank(z)] <- 0L
  out
}

apply_units <- function(DT, unit_col = "UNITS", financial_cols = bvd_financial_vars()) {
  if (!unit_col %chin% names(DT)) return(DT)
  cols <- existing_cols(DT, financial_cols)
  if (!length(cols)) return(DT)
  p <- parse_units_power(DT[[unit_col]])
  multiplier <- 10 ^ p
  DT[, (cols) := lapply(.SD, function(v) to_numeric(v) * multiplier), .SDcols = cols]
  DT
}

flag_bad_unit_switches <- function(DT, id_col = "ID_NUMBER", year_col = "YEAR", unit_col = "UNITS", asset_col = "TOTASSTS", lower_ratio = 0.01, upper_ratio = 199) {
  if (!all(c(id_col, year_col, unit_col, asset_col) %chin% names(DT))) return(rep(FALSE, nrow(DT)))
  data.table::setorderv(DT, c(id_col, year_col))
  DT[, .unit_power_tmp__ := parse_units_power(get(unit_col))]
  DT[, .asset_ratio_tmp__ := get(asset_col) / data.table::shift(get(asset_col)), by = id_col]
  DT[, .unit_change_tmp__ := .unit_power_tmp__ != data.table::shift(.unit_power_tmp__), by = id_col]
  bad <- DT$.unit_change_tmp__ %in% TRUE & !is.na(DT$.asset_ratio_tmp__) & DT$.asset_ratio_tmp__ > lower_ratio & DT$.asset_ratio_tmp__ < upper_ratio
  firm_bad <- DT[, .(bad = any(bad, na.rm = TRUE)), by = id_col]
  ans <- DT[[id_col]] %chin% firm_bad[bad == TRUE, get(id_col)]
  DT[, c(".unit_power_tmp__", ".asset_ratio_tmp__", ".unit_change_tmp__") := NULL]
  ans
}
