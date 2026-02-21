# Internal helpers --------------------------------------------------------

#' Find a protocol file in a directory
#'
#' Scans a directory for files matching `"Protokoll.*\\.xlsx$"` (case-
#' insensitive), excludes Excel lock files, and returns the path. If multiple
#' files are found the first is used with a warning. If none are found an
#' error is thrown.
#'
#' @param dir Character scalar. Directory to search. Default `"data"`.
#' @param pattern Regular expression. Default `"Protokoll.*\\.xlsx$"`.
#'
#' @return Character scalar. Full path to the protocol file.
#'
#' @keywords internal
find_protokoll <- function(dir = "data", pattern = "Protokoll.*\\.xlsx$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      ignore.case = TRUE)
  files <- files[!stringr::str_detect(basename(files), "^~\\$")]

  if (length(files) == 0) {
    rlang::abort(
      c("No protocol file found.",
        i = paste0("Directory: ", dir),
        i = paste0("Pattern:   ", pattern, "  (case-insensitive)"))
    )
  }

  if (length(files) > 1) {
    rlang::warn(
      c(paste0(length(files), " protocol files found, using the first."),
        i = paste0("Using:    ", files[1]),
        i = "Supply `path` explicitly to avoid this.")
    )
  }

  files[1]
}
