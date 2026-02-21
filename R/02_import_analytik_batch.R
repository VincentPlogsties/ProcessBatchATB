#' Import analytics export files for a batch assay
#'
#' A thin wrapper around [ProcessLabAssistant::import_analytik()] with a
#' default file pattern matching and path.
#'
#' @param path Character scalar. Path to the directory containing the
#'   analytics export files. Default `"Data"`
#' @param pattern Regular expression to match analytics files. Matching is
#'   case-insensitive. Default `"Analysen.*\\.xlsx$"`.
#' @param dedup_col Character scalar. Column used for deduplication.
#'   Default `"probennummer"`. Passed through to
#'   [ProcessLabAssistant::import_analytik()].
#'
#' @return A tibble with all unique rows from all matched files, all columns
#'   character type. Pass directly to [process_analytik()].
#'
#' @seealso [process_analytik()], [import_samples()], [import_gaswerte()]
#'
#' @examples
#' \dontrun{
#' raw <- import_analytik_batch("data/Analysen")
#' }
#'
#' @export
import_analytik_batch <- function(
    path      = "data",
    pattern   = "Analysen.*\\.xlsx$",
    dedup_col = "probennummer"
) {
  ProcessLabAssistant::import_analytik(
    path      = path,
    pattern   = pattern,
    dedup_col = dedup_col
  )
}
