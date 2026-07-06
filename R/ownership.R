#' Parse BvD ownership percentage strings
#'
#' Converts BvD ownership codes to numeric percentages. WO and BR become 100,
#' MO and CQP1 become 50.01, JO becomes 50, and NG becomes 0.01.
#'
#' @param x Character or numeric vector.
#' @return Numeric vector in percentage points.
#' @export
bvd_parse_ownership_pct <- function(x) {
  if (is.numeric(x)) return(x)
  z <- toupper(trimws(as.character(x)))
  out <- suppressWarnings(as.numeric(gsub("[%<>±+ ]", "", z)))
  out[z %chin% c("WO", "BR")] <- 100
  out[z %chin% c("MO", "CQP1", "CQPI")] <- 50.01
  out[z %chin% c("JO")] <- 50
  out[z %chin% c("NG")] <- 0.01
  out[z %chin% c("-", ".", "", "N.A.", "NA", "N/A")] <- NA_real_
  out
}

owner_type_bucket <- function(x) {
  z <- toupper(trimws(as.character(x)))
  data.table::fcase(
    grepl("BANK|FINANC|INSURANCE|PENSION|NOMINEE|TRUST|FOUNDATION|PRIVATE EQUITY|VENTURE|HEDGE", z), "financial",
    grepl("GOVERN|STATE|PUBLIC AUTHORITY", z), "government",
    grepl("INDUSTRIAL|CORPORATE|COMPANY|BRANCH|SELF|EMPLOYEE|PERSONNEL|MANAGER|DIRECTOR", z), "industrial",
    grepl("INDIVIDUAL|FAMILY|MR |MRS |MME |SIR", z), "individual",
    grepl("PUBLIC", z), "public",
    default = "other"
  )
}

#' Prepare shareholder-link ownership data
#'
#' Converts raw ownership links to a tidy link panel with numeric direct and total
#' stakes, foreign-link indicator, and broad owner-type buckets.
#'
#' @param x Raw shareholder-link data.
#' @param id_col Company ID column.
#' @param year_col Year column.
#' @param company_country_col Company ISO-2 country column.
#' @param owner_country_col Shareholder country column.
#' @param direct_pct_col Direct ownership percentage column.
#' @param total_pct_col Total ownership percentage column.
#' @param owner_name_col Shareholder name column.
#' @param owner_type_col Shareholder type column.
#' @param missing_owner_country_domestic Treat missing owner country as domestic.
#' @return A data.table.
#' @export
bvd_prepare_shareholder_links <- function(x,
                                          id_col = "ID_NUMBER",
                                          year_col = "YEAR",
                                          company_country_col = "CNTRYCDE",
                                          owner_country_col = "SHARCOUN",
                                          direct_pct_col = "SHARDPER",
                                          total_pct_col = "SHARTPER",
                                          owner_name_col = "SHARNAME",
                                          owner_type_col = "SHARTYPE",
                                          missing_owner_country_domestic = TRUE) {
  DT <- as_dt(x)
  assert_cols(DT, c(id_col, year_col, company_country_col))
  if (!owner_country_col %chin% names(DT)) DT[, (owner_country_col) := NA_character_]
  if (!direct_pct_col %chin% names(DT)) DT[, (direct_pct_col) := NA_character_]
  if (!total_pct_col %chin% names(DT)) DT[, (total_pct_col) := NA_character_]
  if (!owner_name_col %chin% names(DT)) DT[, (owner_name_col) := NA_character_]
  if (!owner_type_col %chin% names(DT)) DT[, (owner_type_col) := NA_character_]
  DT[, direct_pct := bvd_parse_ownership_pct(get(direct_pct_col))]
  DT[, total_pct := bvd_parse_ownership_pct(get(total_pct_col))]
  DT[, owner_country := normalize_chr(get(owner_country_col))]
  DT[, company_country := normalize_chr(get(company_country_col))]
  DT[, owner_type_bucket := owner_type_bucket(get(owner_type_col))]
  if (missing_owner_country_domestic) DT[is_blank(owner_country), owner_country := company_country]
  DT[, foreign_link := !is_blank(owner_country) & owner_country != company_country]
  DT[!is.na(direct_pct) | !is.na(total_pct)][]
}

#' Aggregate shareholder links to firm-year ownership indicators
#'
#' @param links Output of bvd_prepare_shareholder_links().
#' @param threshold Foreign ownership threshold in percentage points.
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @return A firm-year data.table.
#' @export
bvd_aggregate_ownership <- function(links,
                                    threshold = 10,
                                    id_col = "ID_NUMBER",
                                    year_col = "YEAR") {
  DT <- as_dt(links)
  assert_cols(DT, c(id_col, year_col, "direct_pct", "foreign_link"))
  out <- DT[, .(
    direct_known_pct = sum(direct_pct, na.rm = TRUE),
    direct_foreign_pct = sum(fifelse(foreign_link, direct_pct, 0), na.rm = TRUE),
    direct_domestic_pct = sum(fifelse(!foreign_link, direct_pct, 0), na.rm = TRUE),
    n_owners = .N,
    n_foreign_owners = sum(foreign_link, na.rm = TRUE),
    has_foreign_owner = any(foreign_link, na.rm = TRUE)
  ), by = c(id_col, year_col)]
  out[, direct_known_pct := pmin(direct_known_pct, 100)]
  out[, foreign_owned := direct_foreign_pct >= threshold]
  out[]
}

#' Fill ownership indicators within firm histories
#'
#' Carries observed ownership values forward and backward within firm. This
#' mirrors the pragmatic panel completion step in the replication code.
#'
#' @param ownership Firm-year ownership table.
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @param cols Columns to fill. Defaults to all non-key columns.
#' @return A data.table.
#' @export
bvd_fill_ownership_panel <- function(ownership,
                                     id_col = "ID_NUMBER",
                                     year_col = "YEAR",
                                     cols = NULL) {
  DT <- as_dt(ownership)
  assert_cols(DT, c(id_col, year_col))
  if (is.null(cols)) cols <- setdiff(names(DT), c(id_col, year_col))
  fill_by_id(DT, id_col = id_col, year_col = year_col, cols = cols)
}

#' Merge ownership indicators into a financial panel
#'
#' @param financials Firm-year financial panel.
#' @param ownership Firm-year ownership indicators.
#' @param id_col Firm ID column.
#' @param year_col Year column.
#' @param fill_missing_domestic Assign no ownership record to domestic ownership.
#' @return A data.table.
#' @export
bvd_merge_ownership_financials <- function(financials,
                                           ownership,
                                           id_col = "ID_NUMBER",
                                           year_col = "YEAR",
                                           fill_missing_domestic = TRUE) {
  F <- as_dt(financials)
  O <- as_dt(ownership)
  assert_cols(F, c(id_col, year_col))
  assert_cols(O, c(id_col, year_col))
  out <- merge(F, O, by = c(id_col, year_col), all.x = TRUE, sort = FALSE)
  if (fill_missing_domestic) {
    if (!"foreign_owned" %chin% names(out)) out[, foreign_owned := FALSE]
    out[is.na(foreign_owned), foreign_owned := FALSE]
    if ("direct_foreign_pct" %chin% names(out)) out[is.na(direct_foreign_pct), direct_foreign_pct := 0]
  }
  out[]
}
