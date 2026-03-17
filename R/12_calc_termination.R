#' Evaluate the VDI 4630 termination criterion
#'
#' Determines when each substrate variant has met the stop condition defined
#' in VDI 4630: the daily specific biogas production rate drops to ≤ 0.5% of
#' the cumulative production per day for **three consecutive days** with a minimum
#' test duration of **25 days**.
#'
#' @section Algorithm:
#' 1. For each measurement position, compute `vol_perc` = daily volume as a
#'    percentage of cumulative volume divided by the elapsed time interval
#'    (days). Set to 100% on day 0.
#' 2. A position meets the criterion on a given day if `vol_perc <= 0.5`
#'    (`condition_probe`).
#' 3. A **variant** (all replicates of a `probe`) meets the criterion on day
#'    `d` if **all** positions satisfy it (`condition_variant`).
#' 4. The test should **continue** (`continue_test = TRUE`) unless the variant
#'    criterion was met on each of the three preceding days and day > 25.
#' 5. `keep_values` propagates the stop forward using [dplyr::cumall()]:
#'    once `continue_test` flips to `FALSE` for a variant, all subsequent
#'    days are also `FALSE` And will not be considered in the evaluation.
#'
#' @param df_gas A tibble as returned by [calc_gasvol()], used to obtain
#'   `run_time` per `(messplatz, datum)`.
#' @param df_daily A tibble as returned by [calc_daily()], containing
#'   `wanne`, `platz`, `messplatz`, `probe`, `datum`, `day`,
#'   `vol_norm_net`, and `vol_norm_net_cum`.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{`condition`}{Tibble with one row per `(probe, day)` and columns
#'     `condition_variant`, `continue_test`, and `keep_values`. Pass to
#'     [calc_yields()].}
#'   \item{`abbruch`}{Long-format tibble with `vol_perc` per `(messplatz, day)`.
#'     Pass to [plot_qc()].}
#'   \item{`abbruch_wide`}{Wide-format version of `abbruch` with one column
#'     per `messplatz`. Useful for export.}
#' }
#'
#' @references
#' VDI 4630 (2016), Section 5.3.3.
#'
#' @seealso [calc_daily()], [calc_yields()]
#'
#' @examples
#' \dontrun{
#' daily  <- calc_daily(df_gas)
#' result <- calc_termination(df_gas, daily)
#'
#' result$condition     # used by calc_yields()
#' result$abbruch_wide  # vol_perc per position per day, wide format
#' }
#'
#' @export
calc_termination <- function(df_gas, df_daily) {

  # --- step 1: vol_perc per measurement position ---------------------------
  df_abbruch <- df_daily |>
    dplyr::select(wanne, platz, messplatz, probe, datum, day,
                  vol_norm_net, vol_norm_net_cum) |>
    dplyr::filter(!stringr::str_detect(probe, "blind_w")) |>
    dplyr::left_join(
      dplyr::select(df_gas, messplatz, datum, run_time),
      by = c("messplatz", "datum"),
      multiple = "last"
    ) |>
    dplyr::group_by(messplatz) |>
    dplyr::mutate(
      vol_perc = dplyr::if_else(
        day == 0,
        100,
        vol_norm_net / vol_norm_net_cum /
          (run_time - dplyr::lag(run_time, default = 0)) * 100
      )
    ) |>
    dplyr::ungroup()

  # --- step 2: wide format for inspection / export -------------------------
  abbruch_wide <- df_abbruch |>
    dplyr::arrange(wanne, platz) |>
    tidyr::pivot_wider(
      id_cols     = c("datum", "day"),
      names_from  = messplatz,
      values_from = vol_perc
    )

  # --- step 3: termination flags per variant -------------------------------
  condition <- df_abbruch |>
    dplyr::mutate(condition_probe = vol_perc <= 0.5) |>
    dplyr::group_by(probe, day) |>
    dplyr::summarise(
      condition_variant = all(condition_probe),
      .groups = "drop"
    ) |>
    dplyr::arrange(probe, day) |>
    dplyr::group_by(probe) |>
    dplyr::mutate(
      # should the test continue? FALSE once criterion met for 3 days after day 25
      continue_test = dplyr::if_else(
        dplyr::lag(condition_variant, default = FALSE) == TRUE &
          dplyr::lag(condition_variant, n = 2, default = FALSE) == TRUE &
          dplyr::lag(condition_variant, n = 3, default = FALSE) == TRUE &
          dplyr::lag(day, default = TRUE) >= 25,
        FALSE,
        TRUE
      ),
      # propagate stop forward: all days after first FALSE are also FALSE
      keep_values = dplyr::cumall(continue_test)
    ) |>
    dplyr::ungroup()

  list(condition = condition, abbruch_wide = abbruch_wide, abbruch = df_abbruch)
}
