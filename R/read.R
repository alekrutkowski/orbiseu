#' Read an Orbis, Amadeus, or auxiliary file
#'
#' Thin wrapper around data.table::fread() and haven::read_dta() that returns a
#' data.table and optionally upper-cases names. The package never assumes access
#' to Orbis. It processes files that the user has exported under their own BvD
#' licence.
#'
#' @param path File path. CSV, TSV, TXT, and DTA are supported.
#' @param upper_names Convert column names to upper case.
#' @param ... Passed to the underlying reader.
#' @return A data.table.
#' @export
bvd_read <- function(path, upper_names = TRUE, ...) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "dta") {
    if (!requireNamespace("haven", quietly = TRUE)) stop("Package haven is needed to read .dta files.", call. = FALSE)
    DT <- data.table::as.data.table(haven::read_dta(path, ...))
  } else {
    DT <- data.table::fread(path, ...)
  }
  if (upper_names) data.table::setnames(DT, names(DT), toupper(names(DT)))
  DT
}
