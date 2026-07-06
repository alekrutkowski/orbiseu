#' Eurostat dataset codes used by the workflow
#'
#' @return A data.table with common SBS and BD dataset codes.
#' @export
eurostat_sbs_codes <- function() {
  data.table::data.table(
    source = c("SBS", "SBS", "SBS", "SBS", "SBS", "BD"),
    code = c("sbs_sc_ind_r2", "sbs_sc_sca_r2", "sbs_sc_1b_se95", "sbs_sc_2d_dade95", "sbs_sc_4d_co95", "bd_9bd_sz_cl_r2"),
    note = c(
      "Annual enterprise statistics for industry by NACE Rev. 2",
      "Annual enterprise statistics by size class for special aggregates of activities",
      "Annual enterprise statistics for services, NACE Rev. 1.1 legacy table",
      "Annual detailed enterprise statistics for trade, NACE Rev. 1.1 legacy table",
      "Annual detailed enterprise statistics for construction, NACE Rev. 1.1 legacy table",
      "Business demography by size class, NACE Rev. 2"
    )
  )
}

#' Import a Eurostat dataset via eurodata as a data.table
#'
#' @param code Eurostat dataset code.
#' @param filters Named list of eurodata filters.
#' @param strings_as_character Convert factors to characters.
#' @param ... Passed to eurodata::importData().
#' @return A data.table.
#' @export
eurodata_import_dt <- function(code, filters = list(), strings_as_character = TRUE, ...) {
  if (!requireNamespace("eurodata", quietly = TRUE)) stop("Package eurodata is required.", call. = FALSE)
  filters <- compact_list(filters)
  raw <- eurodata::importData(code, filters = filters, ...)
  DT <- data.table::as.data.table(raw)
  if (strings_as_character) {
    fac <- names(DT)[vapply(DT, is.factor, logical(1L))]
    if (length(fac)) DT[, (fac) := lapply(.SD, as.character), .SDcols = fac]
  }
  setattr(DT, "eurostat_code", code)
  DT[]
}

#' Standardize Eurostat columns for Orbis validation
#'
#' @param x Eurostat data from eurodata_import_dt().
#' @param value_col Value column.
#' @return A data.table with standard columns when available.
#' @export
eurostat_standardize <- function(x, value_col = "value_") {
  DT <- as_dt(x)
  rename_first <- function(old, new) {
    old <- old[old %chin% names(DT)]
    if (length(old) && !new %chin% names(DT)) data.table::setnames(DT, old[1L], new)
    invisible(NULL)
  }
  rename_first("geo", "CNTRYCDE")
  rename_first(c("TIME_PERIOD", "time"), "YEAR")
  rename_first("nace_r2", "NACE_R2")
  rename_first("nace_r1", "NACE_R1")
  rename_first("nace", "NACE")
  rename_first("sizeclas", "SIZE")
  rename_first(c("indic_sb", "indic_bd"), "INDICATOR")
  rename_first(value_col, "VALUE")
  rename_first("flags_", "FLAGS")
  if ("YEAR" %chin% names(DT)) DT[, YEAR := as_year(YEAR)]
  if ("CNTRYCDE" %chin% names(DT)) DT[, CNTRYCDE := normalize_chr(CNTRYCDE)]
  if ("NACE_R2" %chin% names(DT)) {
    DT[, NACE2 := sprintf("%02d", suppressWarnings(as.integer(gsub("[^0-9]", "", NACE_R2))))]
    DT[is.na(NACE2), NACE2 := as.character(NACE_R2)]
    n2 <- nace_rev2_level2()[, .(NACE2 = nace2, NACE1 = nace1)]
    DT <- merge(DT, n2, by = "NACE2", all.x = TRUE, sort = FALSE)
  }
  if ("VALUE" %chin% names(DT)) DT[, VALUE := to_numeric(VALUE)]
  DT[]
}

#' Fetch Eurostat SBS data via eurodata
#'
#' @param code Eurostat SBS code.
#' @param countries ISO-2 country codes.
#' @param years Years.
#' @param nace NACE filters.
#' @param size_class Size-class filters.
#' @param indicator Indicator code, for example V12110 for turnover or V16110 for persons employed.
#' @param unit Optional Eurostat unit filter.
#' @param extra_filters Additional filter list.
#' @return A standardized data.table.
#' @export
eurostat_fetch_sbs <- function(code = "sbs_sc_sca_r2",
                               countries = NULL,
                               years = NULL,
                               nace = NULL,
                               size_class = NULL,
                               indicator = NULL,
                               unit = NULL,
                               extra_filters = list()) {
  filters <- c(list(
    geo = countries,
    TIME_PERIOD = years,
    nace_r2 = nace,
    sizeclas = size_class,
    indic_sb = indicator,
    unit = unit
  ), extra_filters)
  eurodata_import_dt(code, filters = filters) |> eurostat_standardize()
}

#' Fetch Eurostat Business Demography data via eurodata
#'
#' @param code Eurostat BD code.
#' @param countries ISO-2 country codes.
#' @param years Years.
#' @param nace NACE filters.
#' @param size_class Size-class filters.
#' @param indicator Indicator code.
#' @param extra_filters Additional filter list.
#' @return A standardized data.table.
#' @export
eurostat_fetch_bd <- function(code = "bd_9bd_sz_cl_r2",
                              countries = NULL,
                              years = NULL,
                              nace = NULL,
                              size_class = NULL,
                              indicator = NULL,
                              extra_filters = list()) {
  filters <- c(list(
    geo = countries,
    TIME_PERIOD = years,
    nace_r2 = nace,
    sizeclas = size_class,
    indic_bd = indicator
  ), extra_filters)
  eurodata_import_dt(code, filters = filters) |> eurostat_standardize()
}
