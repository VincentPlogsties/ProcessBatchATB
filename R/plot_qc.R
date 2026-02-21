#' Quality control plots for visual inspection of gas data
#'
#' Produces two sets of diagnostic plots, one per substrate (`probe`), to help
#' identify outlier measurement positions, data entry errors, and equipment
#' failures before finalising results.
#'
#' **Plot 1 — Cumulative net biogas volume** (`vol_norm_net_cum` over `day`):
#' One line per `messplatz`, coloured by position. Use to spot positions that
#' diverge from their replicates in cumulative production.
#'
#' **Plot 2 — Daily rate as % of cumulative** (`vol_perc` over `day`):
#' Visualises the termination criterion directly. The dashed reference line at
#' 0.5% marks the VDI 4630 threshold. Use to verify when and whether the stop
#' condition is met per position.
#'
#' @param df_daily A tibble as returned by [calc_daily()].
#' @param df_abbruch The `$abbruch` element of the list returned by
#'   [calc_termination()], containing `vol_perc` per `(messplatz, day)`.
#'
#' @return Invisibly returns `NULL`. Plots are printed to the active graphics
#'   device. Call inside a `pdf()` / `dev.off()` block to save to file.
#'
#' @seealso [calc_daily()], [calc_termination()]
#'
#' @examples
#' \dontrun{
#' daily  <- calc_daily(df_gas)
#' term   <- calc_termination(df_gas, daily)
#'
#' plot_qc(daily, term$abbruch)
#'
#' # save to PDF
#' pdf("output/qc_plots.pdf", width = 10, height = 6)
#' plot_qc(daily, term$abbruch)
#' dev.off()
#' }
#'
#' @export
plot_qc <- function(df_daily, df_abbruch) {

  probes <- levels(factor(df_daily$probe))

  for (i in probes) {

    # --- plot 1: cumulative net biogas volume --------------------------------
    p1 <- ggplot2::ggplot(
      df_daily |>
        dplyr::mutate(messplatz = factor(messplatz)) |>
        dplyr::filter(probe == i),
      ggplot2::aes(x = day, y = vol_norm_net_cum, colour = messplatz)
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = i,
        x     = "Day",
        y     = "Cumulative net volume [mL]"
      ) +
      ggplot2::theme(
        plot.title        = ggplot2::element_text(hjust = 0.5),
        legend.title      = ggplot2::element_blank(),
        legend.position   = "inside",
        legend.position.inside = c(0.8, 0.2)
      )

    print(p1)

    # --- plot 2: daily rate as % of cumulative (termination criterion) ------
    p2 <- ggplot2::ggplot(
      df_abbruch |>
        dplyr::mutate(messplatz = factor(messplatz)) |>
        dplyr::filter(probe == i),
      ggplot2::aes(x = day, y = vol_perc, colour = messplatz)
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(
        yintercept = 0.5,
        linetype   = "dashed",
        colour     = "grey40"
      ) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        name   = "Daily vol / cumulative vol [%]",
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::labs(title = i, x = "Day") +
      ggplot2::theme(
        plot.title        = ggplot2::element_text(hjust = 0.5),
        legend.title      = ggplot2::element_blank(),
        legend.position   = "inside",
        legend.position.inside = c(0.8, 0.2)
      )

    print(p2)
  }

  invisible(NULL)
}
