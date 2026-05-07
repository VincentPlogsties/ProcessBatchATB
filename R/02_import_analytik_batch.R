#' Import analytics export files for a batch assay
#'
#' Imports \pkg{ProcessLabAssistant} output \code{.rds} files, using a
#' default file pattern matching and path.
#'
#' @param path Character scalar. Path to the directory containing the
#'   analytics export files. Default `"Data"`
#' @param pattern Regular expression to match analytics files. Matching is
#'   case-insensitive. Default `"(Analytik|Analysen).*\\.rds$"`.
#'
#' @return A tibble with all rows from all matched files, all columns
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
# import_analytik_batch <- function(
#     path      = "data",
#     pattern   = "(Analytik|Analysen).*\\.rds$",
#     dedup_col = "probennummer"
# ) {
#   ProcessLabAssistant::import_analytik(
#     path      = path,
#     pattern   = pattern,
#     dedup_col = dedup_col
#   )
# }

import_analytik_batch <- function(
    path    = "data",
    pattern = "(Analytik|Analysen).*\\.rds$"
) {
  files <- list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  # exclude temporary lock files
  files <- files[!stringr::str_detect(basename(files), "^~\\$")]

  if (length(files) == 0) {
    rlang::abort(
      c("No matching files found.",
        i = paste0("Directory: ", path),
        i = paste0("Pattern:   ", pattern))
    )
  }

  files |>
    purrr::map(\(f) readRDS(f)$data
    ) |>
    dplyr::bind_rows()
}
