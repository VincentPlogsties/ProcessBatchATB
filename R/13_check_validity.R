#' Check replicate validity per VDI 4630
#'
#' Computes pairwise deviations between all replicate measurement positions
#' within each substrate variant and trough (`wanne`). VDI 4630 requires that
#' the deviation between any two replicates does not exceed defined thresholds
#' (typically < 5–20% depending on the homogeneity of the substrate).
#'
#' All pairwise combinations are generated with [utils::combn()], so the
#' function works for 2 or more replicates. For single-replicate positions, the
#' position ID is returned with `NA` deviation values.
#'
#' @param df_yields$yields A tibble as returned by [calc_yields()], containing final
#'   cumulative volumes per measurement position.
#'
#' @return A tibble with one row per pair of replicates per variant per trough,
#'   containing columns `projekt`, `probe`, `wanne`, `pruefglied` (pair of
#'   `messplatz` IDs), `biogas_mean` and `ch4_mean`  (mean cumulative biogas yield and mean methane content of the pair),
#'   `biogas_diff` and `ch4_diff`(absolute difference of the pair), `biogas_dev_pct` and `ch4_dev_pct` (relative
#'   difference of the pair in %).
#'
#' @references
#' VDI 4630 (2016), Section 5.3.4.
#'
#' @seealso [calc_yields()]
#'
#' @examples
#' \dontrun{
#' validity <- check_validity(df_yields$yields)
#' }
#'
#' @export
check_validity <- function(yields) {

  yields |>
    dplyr::group_by(projekt, probe, wanne) |>
    dplyr::reframe(
      pruefglied  = list(
        if (dplyr::n() > 1)
          utils::combn(messplatz, 2, paste0, collapse = "-")
        else messplatz
      ),
      biogas_mean = list(
        if (dplyr::n() > 1) utils::combn(biogas_yield_fm, 2, mean)
        else biogas_yield_fm
      ),
      biogas_diff = list(
        if (dplyr::n() > 1) abs(utils::combn(biogas_yield_fm, 2, diff))
        else NA_real_
      ),
      ch4_mean    = list(
        if (dplyr::n() > 1) utils::combn(methane_perc, 2, mean)
        else methane_perc
      ),
      ch4_diff    = list(
        if (dplyr::n() > 1) abs(utils::combn(methane_perc, 2, diff))
        else NA_real_
      )
    ) |>
    tidyr::drop_na() |>
    tidyr::unnest(cols = c(pruefglied, biogas_mean, biogas_diff, ch4_mean, ch4_diff)) |>
    dplyr::mutate(
      biogas_dev_pct = biogas_diff / biogas_mean * 100,
      ch4_dev_pct    = ch4_diff    / ch4_mean    * 100
    )
}
