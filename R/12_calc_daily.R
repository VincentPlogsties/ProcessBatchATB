#' Compute daily biogas summaries
#'
#' Aggregates the per-timestamp gas data to daily totals per measurement
#' position, computes cumulative sums, and adds a `day` counter (integer days
#' elapsed since the first measurement of each position).
#'
#' @param df_gas A tibble as returned by [calc_gasvol()].
#'
#' @return A tibble with one row per `(messplatz, datum)` containing daily
#'   and cumulative normalised biogas and methane volumes, the `day` and `run_time` counter,
#'   and identifier columns (`gaertest`, `projekt`, `wanne`, `platz`, `probe`,
#'   `einwaage_in_g_fm`).
#'
#' @seealso [calc_gasvol()], [calc_termination()], [calc_yields()]
#'
#' @examples
#' \dontrun{
#' daily <- calc_daily(df_gas)
#' }
#'
#' @export
calc_daily <- function(df_gas) {
  df_gas |>
    dplyr::group_by(
      gaertest, projekt, wanne, platz, messplatz, analytik_nr, probe,
      einwaage_in_g_fm, datum
    ) |>
    dplyr::summarise(
      run_time            = dplyr::last(run_time),
      vol_norm            = sum(vol_norm),
      blind_mean          = sum(blind_mean),
      vol_norm_net        = sum(vol_norm_net),
      vol_norm_net_methan = dplyr::if_else(
        all(is.na(vol_norm_net_methan)),
        NA_real_,
        sum(vol_norm_net_methan, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(messplatz) |>
    dplyr::mutate(
      day = cumsum(
        as.numeric(datum - dplyr::lag(datum, default = dplyr::first(datum)))
      ),
      .after = datum,
      vol_norm_net_cum = cumsum(vol_norm_net),
      vol_norm_net_methan_cum = {
        x <- tidyr::replace_na(vol_norm_net_methan, 0) |> cumsum()
        x[is.na(vol_norm_net_methan)] <- NA
        x
      },
      methane_perc = dplyr::if_else(
        day == 0, 0,
        vol_norm_net_methan_cum / vol_norm_net_cum * 100
      )
    ) |>
    dplyr::mutate(
      vol_norm_net_methan_cum = dplyr::if_else(day == 0, 0, vol_norm_net_methan_cum)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c("blind_mean", "vol_norm")))
}


