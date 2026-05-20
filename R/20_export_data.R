#' Export pipeline results to disk
#'
#' Derives an output path from the protocol file name found in `path`,
#' serialises the full results list as an `.rds` file, and optionally writes
#' selected tibbles to an Excel workbook (one sheet per element) using
#' \pkg{openxlsx}.
#'
#' The output stem is extracted from the protocol filename using the pattern
#' `"GT_\\d{4}_\\d{2}"` (e.g. `"GT_2024_01"` from
#' `"Protokoll_GT_2024_01.xlsx"`). Files are written to the same directory
#' as the protocol file. If no matching stem is found, a warning is issued and
#' `"results"` is used as a fallback stem.
#'
#' @param results A named list as returned by [run_batch_pipeline()].
#' @param path Character scalar. Directory that contains the protocol
#'   `Protokoll*.xlsx` file. Used both to locate the file and to determine
#'   where outputs are written. Default `"."`.
#' @param write_xlsx Logical. If `TRUE`, also write an Excel workbook
#'   (`<stem>_results.xlsx`) with sheets for `yields`, `yields_daily`,
#'   `validity`, and `abbruch_wide`. Default `FALSE`.
#'
#' @return Invisibly returns a named list with elements `rds` (path to the
#'   `.rds` file) and, if `write_xlsx = TRUE`, `xlsx` (path to the workbook).
#'
#' @seealso [run_batch_pipeline()]
#'
#' @examples
#' \dontrun{
#' results <- run_batch_pipeline(protokoll_path = "data")
#' export_data(results, path = "data")
#'
#' # also write an Excel workbook
#' export_data(results, path = "data", write_xlsx = TRUE)
#' }
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
export_data <- function(results, path = ".", write_xlsx = FALSE) {

  # --- locate protocol file to derive the output stem ----------------------
  protocol_file <- list.files(
    path        = path,
    pattern     = "Protokoll.*\\.xlsx$",
    full.names  = TRUE,
    ignore.case = TRUE
  )
  protocol_file <- protocol_file[
    !stringr::str_detect(basename(protocol_file), "^~\\$")
  ]

  if (length(protocol_file) == 0) {
    rlang::abort(
      c("No protocol file found; cannot derive output path.",
        i = paste0("Directory searched: ", path),
        i = "Supply a directory containing a 'Protokoll*.xlsx' file.")
    )
  }

  stem <- stringr::str_extract(protocol_file[1], "GT_\\d{4}_\\d{2}")

  if (is.na(stem)) {
    rlang::warn(
      c("Could not extract 'GT_YYYY_MM' stem from the protocol filename.",
        i = paste0("Filename: ", basename(protocol_file[1])),
        i = "Using 'results' as the output stem.")
    )
    stem <- "results"
  }

  out_dir  <- dirname(protocol_file[1])
  out_stem <- file.path(out_dir, stem)
  output   <- list()

  # --- write RDS -----------------------------------------------------------
  rds_path <- stringr::str_c(out_stem, "_results.rds")
  readr::write_rds(results, file = rds_path)
  message("Results saved to: ", rds_path)
  output$rds <- rds_path

  # --- optionally write Excel workbook via openxlsx ------------------------
  if (write_xlsx) {

    xlsx_sheets <- list(
      yields       = results$yields,
      yields_daily = results$yields_daily,
      validity     = results$validity
    )
    # drop any elements not present in results (e.g. during partial runs)
    xlsx_sheets <- Filter(Negate(is.null), xlsx_sheets)
    xlsx_path <- stringr::str_c(out_stem, "_results.xlsx")

    openxlsx::write.xlsx(
      xlsx_sheets,
      file = xlsx_path,
      as_table = TRUE,
      colWidths = "auto",
      firstRow  = TRUE,
      overwrite = TRUE
    )
  }

  invisible(output)
}
