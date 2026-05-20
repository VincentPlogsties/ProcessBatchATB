#' Calculate normalised gas volumes with blank correction
#'
#' Takes the combined gas measurement and sample data and computes:
#'
#' 1. **Normalisation factor** – corrects for temperature and pressure
#'    deviations from standard conditions (0 °C, 1013.25 hPa) using an empiric
#'    water vapour pressure formula (pw = -4.3965 + 9.7962 * exp(0.0521 * temp)).
#' 2. **Incremental normalised volume** (`vol_norm`) – change in bell height
#'    multiplied by cylinder cross-section and normalisation factor.
#' 3. **Blank correction** (`vol_norm_net`) – subtracts the mean of all
#'    blank/blind positions in the same tub (`wanne`) at the same time
#'    point. Blank positions are identified by `probe` matching `"blind"`
#'    (case-insensitive). Blank positions themselves retain their raw
#'    `vol_norm`.
#' 4. **Corrected methane fraction** (`methane_perc`) – CH4% / (CH4% + CO2%)
#'     `NA` when no measurement.
#' 5. **Cumulative volumes** and **cumulative methane** per measurement
#'    position.
#' 6. **Run time** in days (from first measurement per position).
#'
#' @param df_gaswerte A tibble as returned by [import_gaswerte()].
#' @param df_proben A tibble as returned by [import_samples()],
#'   containing `wanne`, `platz`, `probe`, `messplatz`, `d_gasmaus_cm`,
#'   `nullpunkt_cm`, `einwaage_in_g_fm`.
#' @param bad_channels Character vector of `messplatz` identifiers to exclude
#'   from all calculations (e.g. positions with equipment failures).
#'   Default `character(0)`.
#'
#' @return A tibble with one row per measurement position per timestamp,
#'   including all input columns plus `normfaktor`, `vol_norm`,
#'   `vol_norm_net`, `methane_perc`, `vol_norm_net_cum`,
#'   `vol_norm_net_methan` (incremental), `vol_norm_net_methan_cum`, and
#'   `run_time` (days).
#'
#' @seealso [import_samples()], [import_gaswerte()], [calc_termination()]
#'
#' @examples
#' \dontrun{
#' gaswerte <- import_gaswerte("data/Protokoll.xlsx")
#' result   <- process_analytik(raw_analytik, samples)
#' df_gas   <- calc_gasvol(gaswerte, result$proben, bad_channels = c("W1-3"))
#' }
#'
#' @export
calc_gasvol <- function(df_gaswerte, df_proben, bad_channels = character(0)) {

  # --- join sample metadata to gas readings --------------------------------
  df <- df_proben |>
    dplyr::right_join(df_gaswerte, by = c("wanne", "platz")) |>
    tidyr::drop_na(probe)

  # --- exclude bad channels ------------------------------------------------
  df <- dplyr::filter(df, !messplatz %in% bad_channels)

  # --- normalisation factor & incremental volume ---------------------------
  df <- df |>
    dplyr::group_by(messplatz) |>
    dplyr::mutate(

      .t_norm = 273.15,
      .p_norm = 1013.25,
      # .p_vapour = 6.11231 * exp(17.5043 * temp / (241.2 + temp)), # magnus correction from VDI4630 (2016)
      .p_vapour = -4.3965 + 9.7962 * exp(0.0521 * temp),            # legacy formula from ATB Batch tests
      normfaktor = (druck - .p_vapour) * .t_norm / (.p_norm * (.t_norm + temp)),

      .h_2   = (hoehe_mm / 10) - nullpunkt_cm,
      .h_1   = (dplyr::lag(hoehe_mm) / 10) - nullpunkt_cm,
      .A_zyl = pi * (d_gasmaus_cm / 2)^2,

      vol_norm = dplyr::if_else(
        is.na(dplyr::lag(ch4_perc)),
        .h_2 * .A_zyl * normfaktor - .h_1 * dplyr::lag(.A_zyl) * dplyr::lag(normfaktor),
        .h_2 * .A_zyl * normfaktor
      ),
      vol_norm = tidyr::replace_na(vol_norm, 0)
    ) |>
    dplyr::select(-tidyr::starts_with("."))

  # --- blank mean per wanne per timestamp ----------------------------------
  blank_means <- df |>
    dplyr::filter(stringr::str_detect(probe, stringr::regex("blind", ignore_case = TRUE))) |>
    dplyr::group_by(wanne, datum, zeit) |>
    dplyr::summarise(blind_mean = mean(vol_norm), .groups = "drop")

  df <- df |>
    dplyr::left_join(blank_means, by = c("wanne", "datum", "zeit")) |>
    dplyr::mutate(
      vol_norm_net = dplyr::if_else(
        stringr::str_detect(probe, stringr::regex("Blind[ _]W\\d", ignore_case = TRUE)),
        vol_norm,
        vol_norm - blind_mean
      )
    )

  # --- corrected methane fraction ------------------------------------------
  df <- df |>
    dplyr::mutate(
      methane_perc = dplyr::if_else(
        ch4_perc > 0 & co2_perc > 0,
        ch4_perc / (ch4_perc + co2_perc) * 100,
        NA_real_
      )
    )

  # --- cumulative volumes --------------------------------------------------
  df_methan <- df |>
    dplyr::group_by(messplatz) |>
    dplyr::mutate(vol_norm_net_cum = cumsum(vol_norm_net)) |>
    dplyr::filter(!is.na(methane_perc)) |>
    dplyr::mutate(
      vol_norm_net_methan =
        (vol_norm_net_cum - dplyr::lag(vol_norm_net_cum, default = 0)) *
        methane_perc / 100
    ) |>
    dplyr::select(messplatz, datum, zeit, vol_norm_net_methan)

  df |>
    dplyr::left_join(df_methan, by = c("messplatz", "datum", "zeit")) |>
    dplyr::ungroup() |>
    dplyr::arrange(wanne, platz, datum, zeit) |>
    dplyr::group_by(wanne, platz) |>
    dplyr::mutate(
      run_time = cumsum(
        as.numeric(difftime(zeit, dplyr::lag(zeit, default = dplyr::first(zeit)),
                            units = "days"))
      ),
      .after = zeit
    ) |>
    dplyr::ungroup()
}
