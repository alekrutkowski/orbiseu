apply_id_changes <- function(DT, id_changes, id_col = "ID_NUMBER") {
  if (is.null(id_changes)) return(DT)
  X <- as_dt(id_changes)
  nms <- names(X)
  old_col <- if ("old_id" %chin% nms) "old_id" else nms[1L]
  new_col <- if ("new_id" %chin% nms) "new_id" else nms[2L]
  data.table::setnames(X, c(old_col, new_col), c("old_id", "new_id"), skip_absent = TRUE)
  DT[, legacy_id_number := get(id_col)]
  DT[X, on = setNames("old_id", id_col), (id_col) := i.new_id]
  DT
}

#' Merge multiple historical Orbis or Amadeus vintages
#'
#' Appends cleaned vintages, applies optional BvD ID changes, and collapses
#' overlapping observations by taking the first non-missing value from the highest
#' priority vintage. This is the R translation of the Stata merge/update/replace
#' pattern used in the replication code.
#'
#' @param vintages Named or unnamed list of data.frames/data.tables.
#' @param id_changes Optional two-column table old_id, new_id.
#' @param key_cols Observation key. By default firm-year-account.
#' @param priority Optional numeric priority. Larger values win. Defaults to list
#'   order, so later list elements win.
#' @param vintage_col Name for source vintage label.
#' @return A merged data.table.
#' @export
bvd_merge_vintages <- function(vintages,
                               id_changes = NULL,
                               key_cols = c("ID_NUMBER", "YEAR", "CONSCODE2"),
                               priority = NULL,
                               vintage_col = "vintage") {
  if (!is.list(vintages) || !length(vintages)) stop("vintages must be a non-empty list.", call. = FALSE)
  nms <- names(vintages)
  if (is.null(nms)) nms <- rep("", length(vintages))
  nms[is.na(nms) | !nzchar(nms)] <- paste0("v", which(is.na(nms) | !nzchar(nms)))
  names(vintages) <- nms
  if (is.null(priority)) priority <- seq_along(vintages)
  if (length(priority) != length(vintages)) stop("priority must have the same length as vintages.", call. = FALSE)
  pieces <- Map(function(x, nm, pr) {
    DT <- as_dt(x)
    DT[, (vintage_col) := nm]
    DT[, .vintage_priority__ := pr]
    DT
  }, vintages, names(vintages), priority)
  DT <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  DT <- apply_id_changes(DT, id_changes = id_changes, id_col = key_cols[1L])
  key_cols <- existing_cols(DT, key_cols)
  assert_cols(DT, key_cols)
  data.table::setorderv(DT, c(key_cols, ".vintage_priority__"), order = c(rep(1L, length(key_cols)), -1L), na.last = TRUE)
  sd_cols <- setdiff(names(DT), key_cols)
  out <- DT[, lapply(.SD, first_nonblank), by = key_cols, .SDcols = sd_cols]
  out[, .vintage_priority__ := NULL]
  data.table::setorderv(out, key_cols)
  out[]
}
