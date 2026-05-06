#' Process analytics data for a batch assay
#'
#' Cleans and transforms raw analytics data via \pkg{ProcessLabAssistant},
#' computes the inoculum blank mean, and joins corrected dry-matter values
#' (`ts_k_perc_fm`, `ots_perc_ts_k`) back to the sample table so that
#' `otm_k_fm` and inoculum-normalised weighings (`einwaage_in_g_odm`, `p_i`)
#' can be derived.
#'
#' @section Inoculum identification:
#' Rows are treated as inoculum if `probenart == "inokulum"` **or** if `probe`
#' starts with `"GT"` (case-insensitive). The mean of all inoculum positions
#' is stored as `probe = "blind_mean"` and appended to the returned table so
#' it can be used for gas blank correction downstream.
#'
#' @param df_import_analytik A tibble of raw analytics data as returned by
#'   [import_analytik_batch()] (all columns character).
#' @param df_samples A tibble as returned by [import_samples()], used to
#'   join corrected TS values and compute `p_i`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`analytik`}{Tibble with one row per sample (plus `blind_mean`),
#'     all numeric columns expressed as mean `%TS-k`. `NaN` replaced by `NA`.}
#'   \item{`proben`}{`df_samples` extended with `ts_k_perc_fm`,
#'     `ots_perc_ts_k`, `otm_k_fm`, `einwaage_in_g_odm`, and `p_i`.}
#' }
#'
#' @seealso [import_analytik_batch()], [import_samples()],
#'
#' @examples
#' \dontrun{
#' raw     <- import_analytik_batch("data/Analysen")
#' samples <- import_samples("data/Protokoll.xlsx")
#' result  <- process_analytik(raw, samples)
#' df_analytik <- result$analytik
#' df_proben   <- result$proben
#' }
#'
#' @export
process_analytik <- function(df_import_analytik, df_samples) {

  # --- clean & transform via ProcessLabAssistant ---------------------------
  analytik_all <- df_import_analytik |>
    # ProcessLabAssistant::clean_analytik() |>
    # ProcessLabAssistant::transform_analytik() |>
    dplyr::group_by(datum, probenart, probe) |>
    dplyr::summarise(
      analytik_nr = dplyr::if_else(
        dplyr::n() == 1L,
        dplyr::first(analytik_nr),
        paste0(
          stringr::str_extract(dplyr::first(analytik_nr), "^\\d{4}"), "/",
          stringr::str_extract(dplyr::first(analytik_nr), "(?<=-)\\d+"), "-",
          stringr::str_extract(dplyr::last(analytik_nr),  "(?<=-)\\d+")
        )
      ),
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # --- inoculum / blank mean -----------------------------------------------
  blind_mean <- analytik_all |>
    dplyr::filter(
      stringr::str_detect(probe, "(?i)^GT") | probenart == "inokulum"
    ) |>
    dplyr::mutate(probenart = "inokulum") |>
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE))
    ) |>
    dplyr::mutate(
      probenart   = "inokulum",
      probe       = "blind_mean",
      analytik_nr = "blind_mean"
    ) |>
    janitor::remove_empty(which = "cols")

  df_analytik <- dplyr::bind_rows(analytik_all, blind_mean) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ dplyr::na_if(.x, NaN))
    )

  # --- join TS values to sample table and derive weighings -----------------
  df_proben <- df_samples |>
    dplyr::left_join(
      dplyr::select(df_analytik, analytik_nr, ts_k_perc_fm, ots_perc_ts_k),
      by = "analytik_nr"
    ) |>
    dplyr::mutate(
      otm_k_fm          = ts_k_perc_fm * ots_perc_ts_k / 100,
      einwaage_in_g_odm = einwaage_in_g_fm * ts_k_perc_fm / 100 * ots_perc_ts_k / 100,
      p_i = dplyr::if_else(
        stringr::str_detect(analytik_nr, "blind_mean"),
        NA_real_,
        einwaage_in_g_odm /
          mean(einwaage_in_g_odm[analytik_nr == "blind_mean"], na.rm = TRUE)
      ),
      .after = otm_k_fm
    )

  list(analytik = df_analytik, proben = df_proben)
}
