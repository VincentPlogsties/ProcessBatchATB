#' Compute daily biogas summaries
#'
#' Aggregates the per-timestamp gas data to daily totals per measurement
#' position, computes cumulative sums, and adds a `day` counter (integer days
#' elapsed since the first measurement of each position).
#'
#' @param df_gas A tibble as returned by [calc_gasvol()].
#'
#' @return A tibble with one row per `(messplatz, datum)` containing daily
#'   and cumulative normalised biogas and methane volumes, the `day` counter,
#'   and identifier columns (`projekt`, `wanne`, `platz`, `probe`,
#'   `einwaage_in_g_fm`, `otm_k_fm`).
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
      projekt, wanne, platz, messplatz, analytik_nr, probe,
      einwaage_in_g_fm, otm_k_fm, datum
    ) |>
    dplyr::summarise(
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
      .before = vol_norm,
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


#' Compute biogas and methane yields per ODM
#'
#' Applies the VDI 4630 termination criterion to the daily data and calculates
#' cumulative biogas and methane yields normalised to organic dry matter
#' (ODM / oTM). Days flagged as invalid by [calc_termination()] are removed.
#'
#' @param df_daily A tibble as returned by [calc_daily()].
#' @param df_termination The `$condition` element of the list returned by
#'   [calc_termination()], containing columns `probe`, `day`,
#'   `condition_variant`, `continue_test`, and `keep_values`.
#'
#' @return A tibble of daily values filtered to `keep_values == TRUE`, with
#'   additional columns:
#' \describe{
#'   \item{`biogas_yield_odm`}{Cumulative normalised biogas volume per g ODM
#'     (mL/g VS).}
#'   \item{`methane_yield_odm`}{Cumulative normalised methane volume per g ODM
#'     (mL/g VS).}
#' }
#'
#' @seealso [calc_daily()], [calc_termination()], [check_validity()]
#'
#' @examples
#' \dontrun{
#' daily      <- calc_daily(df_gas)
#' termresult <- calc_termination(df_gas, daily)
#' results    <- calc_yields(daily, termresult$condition)
#' }
#'
#' @export
calc_yields <- function(df_daily, df_termination) {
  df_daily |>
    dplyr::left_join(df_termination, by = c("probe", "day")) |>
    dplyr::filter(keep_values == TRUE) |>
    dplyr::select(-c(keep_values, continue_test, condition_variant)) |>
    dplyr::mutate(
      biogas_yield_odm  = vol_norm_net_cum /
        (einwaage_in_g_fm * otm_k_fm / 100),
      methane_yield_odm = vol_norm_net_methan_cum /
        (einwaage_in_g_fm * otm_k_fm / 100)
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "einwaage_in_g_fm", "otm_k_fm",
        "vol_norm_net", "vol_norm_net_methan",
        "vol_norm_net_cum", "vol_norm_net_methan_cum"
      ))
    )
}
