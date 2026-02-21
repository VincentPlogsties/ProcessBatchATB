#' Check replicate validity per VDI 4630
#'
#' Computes pairwise deviations between all replicate measurement positions
#' within each substrate variant and trough (`wanne`). VDI 4630 requires that
#' the deviation between any two replicates does not exceed defined thresholds
#' (typically < 5–15% depending on the number of replicates and gas volume).
#'
#' All pairwise combinations are generated with [utils::combn()], so the
#' function works for 2 or more replicates. For single-replicate positions, the
#' position ID is returned with `NA` deviation values.
#'
#' @param df_yields A tibble as returned by [calc_yields()], containing final
#'   cumulative volumes per measurement position.
#' @param df_samples A tibble as returned by [import_samples()], used to
#'   obtain the `variante` column for grouping.
#'
#' @return A tibble with one row per pair of replicates per variant per trough,
#'   containing columns `projekt`, `probe`, `wanne`, `pruefglied` (pair of
#'   `messplatz` IDs), `mean_BG` (mean cumulative biogas of the pair),
#'   `diff_BG` (absolute difference), and `diff_mean_perc` (relative
#'   difference in %).
#'
#' @references
#' VDI 4630 (2016), Section 5.3.4.
#'
#' @seealso [calc_yields()]
#'
#' @examples
#' \dontrun{
#' validity <- check_validity(df_yields, df_samples)
#' }
#'
#' @export
check_validity <- function(df_yields, df_samples) {

  finals <- df_yields |>
    dplyr::group_by(projekt, messplatz, probe) |>
    dplyr::summarise(
      biogas_yield_odm = dplyr::last(biogas_yield_odm),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      dplyr::select(df_samples, dplyr::any_of(c("messplatz", "wanne", "variante"))),
      by = "messplatz"
    )

  finals |>
    dplyr::group_by(projekt, probe, wanne) |>
    dplyr::reframe(
      pruefglied = list(
        if (dplyr::n() > 1)
          utils::combn(messplatz, 2, paste0, collapse = "-")
        else messplatz
      ),
      mean_BG = list(
        if (dplyr::n() > 1)
          utils::combn(biogas_yield_odm, 2, mean)
        else biogas_yield_odm
      ),
      diff_BG = list(
        if (dplyr::n() > 1)
          abs(utils::combn(biogas_yield_odm, 2, diff))
        else NA_real_
      )
    ) |>
    tidyr::drop_na() |>
    tidyr::unnest(cols = c(pruefglied, mean_BG, diff_BG)) |>
    dplyr::mutate(diff_mean_perc = diff_BG / mean_BG * 100)
}
