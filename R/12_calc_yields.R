#' Compute biogas and methane yields per ODM
#'
#' Applies the VDI 4630 termination criterion to the daily data and calculates
#' cumulative biogas and methane yields (fresh matter based)
#' Days flagged as invalid by [calc_termination()] are removed.
#'
#' @param df_daily A tibble as returned by [calc_daily()].
#' @param df_termination The `$condition` element of the list returned by
#'   [calc_termination()], containing columns `probe`, `day`,
#'   `condition_variant`, `continue_test`, and `keep_values`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`yields_daily`}{Tibble of daily values filtered to
#'     `keep_values == TRUE`, with additional columns `biogas_yield_fm`
#'     (mL/g VS), `methane_yield_fm` (mL/g VS), and `methane_perc` (%).}
#'   \item{`yields`}{Tibble of final summarised values per measurement
#'     position (`last` day per `messplatz`), with the same yield columns
#'     as `yields_daily`.}
#' }
#'
#' @seealso [calc_gasvol()], [calc_daily()], [calc_termination()], [check_validity()]
#'
#' @examples
#' \dontrun{
#' daily  <- calc_daily(df_gas)
#' term   <- calc_termination(df_gas, daily)
#' result <- calc_yields(daily, term$condition)
#'
#' result$yields_daily  # one row per (messplatz, day)
#' result$yields        # one row per messplatz, final values only
#' }
#'
#' @export

calc_yields <- function(df_daily, df_termination) {
  yields_daily <- df_daily |>
    dplyr::left_join(df_termination, by = c("probe", "day")) |>
    dplyr::filter(keep_values == TRUE) |>
    dplyr::select(-c(keep_values, continue_test, condition_variant)) |>
    dplyr::mutate(
      biogas_yield_fm  = vol_norm_net_cum / einwaage_in_g_fm,
      methane_yield_fm  = vol_norm_net_methan_cum / einwaage_in_g_fm
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "einwaage_in_g_fm",
        "vol_norm_net", "vol_norm_net_methan",
        "vol_norm_net_cum", "vol_norm_net_methan_cum"
      ))
    )
  yields <- yields_daily |>
    dplyr::group_by(projekt, wanne, platz, messplatz, analytik_nr, probe)|>
    dplyr::summarise(
      dplyr::across(
        c("day", "methane_perc", "biogas_yield_fm", "methane_yield_fm"),
        dplyr::last
      ),
      .groups = "drop"
    )
  list(yields_daily = yields_daily, yields =yields)
}
