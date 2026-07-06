# Utility helpers used across the package.

as_dt <- function(x, copy = TRUE) {
  ans <- data.table::as.data.table(x)
  if (copy) data.table::copy(ans) else ans
}

null_or <- function(x, y) {
  if (is.null(x)) y else x
}

compact_list <- function(x) {
  x[!vapply(x, function(z) is.null(z) || length(z) == 0L || all(is.na(z)), logical(1L))]
}

is_blank <- function(x) {
  if (is.character(x) || is.factor(x)) {
    z <- as.character(x)
    is.na(z) | trimws(z) %chin% c("", ".", ":", "-", "n.a.", "n.a", "NA", "N/A", "NULL")
  } else {
    is.na(x)
  }
}

first_nonblank <- function(x) {
  ok <- !is_blank(x)
  if (any(ok)) x[which(ok)[1L]] else x[NA_integer_]
}

last_nonblank <- function(x) {
  ok <- !is_blank(x)
  if (any(ok)) x[tail(which(ok), 1L)] else x[NA_integer_]
}

safe_divide <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

wmean <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    ok <- !is.na(x) & !is.na(w) & is.finite(w) & w > 0
    x <- x[ok]
    w <- w[ok]
  }
  if (!length(x) || !length(w) || sum(w) == 0) return(NA_real_)
  sum(x * w) / sum(w)
}

assert_cols <- function(DT, cols, data_name = deparse(substitute(DT))) {
  miss <- setdiff(cols, names(DT))
  if (length(miss)) {
    stop(data_name, " is missing required column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

existing_cols <- function(DT, cols) intersect(cols, names(DT))

setnames_if_present <- function(DT, old, new) {
  hit <- old %chin% names(DT)
  if (any(hit)) data.table::setnames(DT, old[hit], new[hit])
  invisible(DT)
}

to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  y <- as.character(x)
  y <- gsub("[, ]", "", y)
  y[y %chin% c("", ".", ":", "-", "n.a.", "NA", "N/A")] <- NA_character_
  suppressWarnings(as.numeric(y))
}

as_year <- function(x) {
  y <- suppressWarnings(as.integer(as.character(x)))
  y
}

parse_idate <- function(x) {
  if (inherits(x, "Date")) return(data.table::as.IDate(x))
  if (inherits(x, "IDate")) return(x)
  z <- as.character(x)
  z[is_blank(z)] <- NA_character_
  out <- suppressWarnings(data.table::as.IDate(z, format = "%Y-%m-%d"))
  miss <- is.na(out) & !is.na(z)
  if (any(miss)) out[miss] <- suppressWarnings(data.table::as.IDate(z[miss], format = "%d/%m/%Y"))
  miss <- is.na(out) & !is.na(z)
  if (any(miss)) out[miss] <- suppressWarnings(data.table::as.IDate(z[miss], format = "%m/%d/%Y"))
  miss <- is.na(out) & !is.na(z)
  if (any(miss)) out[miss] <- suppressWarnings(data.table::as.IDate(z[miss], format = "%Y%m%d"))
  out
}

fill_both_vec <- function(x) {
  if (!length(x)) return(x)
  blank <- is_blank(x)
  if (all(blank)) return(x)
  out <- x
  idx <- which(!blank)
  out[seq_len(idx[1L] - 1L)] <- out[idx[1L]]
  idx <- which(!is_blank(out))
  if (length(idx) && idx[length(idx)] < length(out)) {
    out[(idx[length(idx)] + 1L):length(out)] <- out[idx[length(idx)]]
  }
  idx <- which(!is_blank(out))
  if (length(idx) > 1L) {
    starts <- idx[-length(idx)] + 1L
    ends <- idx[-1L] - 1L
    fill_from <- idx[-length(idx)]
    invisible(lapply(seq_along(starts), function(i) {
      if (starts[i] <= ends[i]) out[starts[i]:ends[i]] <<- out[fill_from[i]]
    }))
  }
  out
}

fill_by_id <- function(DT, id_col, year_col, cols) {
  cols <- existing_cols(DT, cols)
  if (!length(cols)) return(DT)
  data.table::setorderv(DT, c(id_col, year_col))
  DT[, (cols) := lapply(.SD, fill_both_vec), by = id_col, .SDcols = cols]
  DT
}

normalize_chr <- function(x) {
  toupper(trimws(as.character(x)))
}

#' Common BvD financial variables
#'
#' Returns a character vector of common financial variable names used by the
#' paper's Stata code and by this package.
#'
#' @return A character vector.
#' @export
bvd_financial_vars <- function() {
  c(
    "FIXEDASSTS", "INTGFIXEDASSTS", "TGFIXEDASSTS", "OTHFIXEDASSTS",
    "CURRENTASSTS", "CA_STOCKS", "CA_DEBTORS", "CA_OTHER", "CASH",
    "TOTASSTS", "SHFUNDS", "SHFUNDS_CAPITAL", "SHFUNDS_OTHER",
    "NONCURRLIAB", "NCL_LONGTERMDEBT", "NCL_OTHER", "CURRENTLIAB",
    "CL_LOANS", "CL_CREDITORS", "CL_OTHER", "SHFUNDLIAB", "WORKINGK",
    "NETCURRASSTS", "ENTERPRISEVALUE", "EMPL", "OPER_TURN", "SALE",
    "COSTGOOD", "GROSSPROFIT", "OTHEROPEREXP", "EBIT", "FINREV",
    "FINEXPEN", "FINP_L", "PL_BEFORETAX", "TAXATION", "PL_AFTERTAX",
    "EXTR_OTHERREV", "EXTR_OTHEREXP", "EXTR_OTHERPL", "NETINCOME",
    "EXPORTS", "MATERIAL", "MATE", "COSTEMPL", "STAF", "DEPREC",
    "INTERESTPAID", "CASHFLOW", "ADDEDVALUE", "EBITDA"
  )
}
