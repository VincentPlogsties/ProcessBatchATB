#' Run the full batch fermentation pipeline
#'
#' Convenience wrapper that calls all pipeline steps in sequence:
#' [import_analytik_batch()], [import_samples()], [import_gaswerte()],
#' [process_analytik()], [calc_gasvol()], [calc_daily()],
#' [calc_termination()], [calc_yields()], and [check_validity()].
#'
#' @param protokoll_path Character scalar. Path to the protocol `.xlsx` file
#'   containing both the sample sheet and gas measurement sheets. Default `"Data"`.
#' @param analytik_path Character scalar. Path to the directory containing
#'   `Analysen*.xlsx` files, passed to [import_analytik_batch()]. Default `"Data"`.
#' @param bad_channels Character vector. Measurement positions to exclude.
#'   Default `character(0)`.
#' @param analytik_pattern Regular expression for analytics file matching.
#'   Default `"Analysen.*\\.xlsx$"`. Passed to [import_analytik_batch()].
#' @param sample_sheet Character scalar. Sheet name for the sample table.
#'   Default `"Proben"`.
#' @param sample_range Character scalar. Cell range for the sample table.
#'   Default `"A4:BQ71"`.
#' @param gas_sheets Character vector. Gas measurement sheet names.
#'   Default `c("w1","w2","w3","w4","w5","w6")`.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`analytik`}{Processed analytics tibble from [process_analytik()].}
#'   \item{`proben`}{Sample table with joined TS values from [process_analytik()].}
#'   \item{`gas`}{Full per-timestamp gas data from [calc_gasvol()].}
#'   \item{`daily`}{Daily summaries from [calc_daily()].}
#'   \item{`condition`}{Termination criterion flags (`$condition` from [calc_termination()]).}
#'   \item{`abbruch_wide`}{Daily `vol_perc` per position in wide format
#'     (`$abbruch_wide` from [calc_termination()]). Useful for inspection and export.}
#'   \item{`yields`}{Final yield results from [calc_yields()].}
#'   \item{`validity`}{Pairwise replicate deviations from [check_validity()].}
#' }
#'
#' @examples
#' \dontrun{
#' results <- run_batch_pipeline(
#'   protokoll_path = "data/Protokoll_VersuchXY.xlsx",
#'   analytik_path  = "data/Analysen",
#'   bad_channels   = c("w1-1", "w3-12")
#' )
#'
#' results$yields
#' results$validity
#' }
#'
#' @export
#'

# devtools::document()   # regenerates man/ files from roxygen comments
# devtools::check()      # runs R CMD CHECK — catches missing imports, broken docs, etc.
# devtools::test()       # runs the tests in tests/testthat/

run_batch_pipeline <- function(
    protokoll_path = "data",
    analytik_path  = "data",
    bad_channels     = character(0)
)
  {
  message("=== ProcessBatchATB Pipeline ===")

  message("[1/8] Importing analytics...")
  raw_analytik <- import_analytik_batch(path = analytik_path)

  message("[2/8] Importing sample protocol...")
  samples <- import_samples(protokoll_path)

  message("[3/8] Importing gas measurements...")
  gaswerte <- import_gaswerte(protokoll_path)

  message("[4/8] Processing analytics...")
  analytik_result <- process_analytik(raw_analytik, samples)

  message("[5/8] Calculating normalised gas volumes...")
  df_gas <- calc_gasvol(gaswerte, analytik_result$proben, bad_channels = bad_channels)

  message("[6/8] Aggregating daily summaries...")
  df_daily <- calc_daily(df_gas)

  message("[7/8] Evaluating VDI 4630 termination criterion...")
  termination_result <- calc_termination(df_gas, df_daily)

  message("[8/8] Computing yields and validity check...")
  df_yields   <- calc_yields(df_daily, termination_result$condition)
  df_validity <- check_validity(df_yields, analytik_result$proben)

  list(
    analytik     = analytik_result$analytik,
    proben       = analytik_result$proben,
    gas          = df_gas,
    daily        = df_daily,
    condition    = termination_result$condition,
    abbruch_wide = termination_result$abbruch_wide,
    yields       = df_yields,
    validity     = df_validity
  )
}
