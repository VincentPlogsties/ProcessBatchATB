#' Run the full batch fermentation pipeline
#'
#' Convenience wrapper that calls all pipeline steps in sequence:
#' [import_samples()],
#' [calc_gasvol()], [calc_daily()],
#' [calc_termination()], [calc_yields()], and [check_validity()].
#'
#' @param protokoll_path Character scalar. Path to the protocol `.xlsx` file
#'   containing both the sample sheet and gas measurement sheets. Default `"."`.
#' @param bad_channels Character vector. Measurement positions to exclude.
#'   Default `character(0)`.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`proben`}{Sample table.}
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
#'   protokoll_path = "./GT_JJJ_##_Protokoll.xlsx",
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
    protokoll_path = ".",
    bad_channels     = character(0),
    export_xlsx = FALSE # in addition to standard .rds file
)
{
  message("=== ProcessBatchATB Pipeline ===")

  message("1/8] Importing sample protocol...")
  samples <- import_samples(protokoll_path)

  message("[2/8] Importing gas measurements...")
  gaswerte <- import_gaswerte(protokoll_path)

  message("[3/8] Calculating normalised gas volumes...")
  df_gas <- calc_gasvol(gaswerte, samples, bad_channels = bad_channels)

  message("[4/8] Aggregating daily summaries...")
  df_daily <- calc_daily(df_gas)

  message("[5/8] Evaluating VDI 4630 termination criterion...")
  termination_result <- calc_termination(df_daily)

  message("[6/8] Computing yields and validity check...")
  df_yields   <- calc_yields(df_daily, termination_result$condition)
  df_validity <- check_validity(df_yields$yields)

  message("[7/8] plotting cummulative daily volumes...")
  plot_qc(df_daily, termination_result$abbruch)

  results <- list(
    yields_daily = df_yields$yields_daily,
    yields       = df_yields$yields,
    validity     = df_validity,
    abbruch      = termination_result$abbruch,
    abbruch_wide = termination_result$abbruch_wide
  )
  message("[8/8] Exporting results...")
  export_data(results, path = protokoll_path, write_xlsx = export_xlsx)

  invisible(results)
}
