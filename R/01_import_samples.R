#' Import sample protocol sheet
#'
#' Reads the `"Proben"` sheet from the protocol XLSX-File of standard ATB Biogas Batch Test,
#' standardises column names (replacing German umlauts and special characters),
#'  drops rows without a fresh-mass weighing, and coerces columns to the correct types.
#'
#' @param path Character scalar. Path to the protocol `.xlsx` file.
#' Default `"Data"`. Finds files with pattern = `"Protokoll.*\\.xlsx$"`
#'
#' @return A tibble with one row per sample position. Key columns include
#'    `wanne`, `platz`,  `messplatz`, `probe`, `analytik_nr`,
#'   `einwaage_in_g_fm` and `d_gasmaus_cm`.
#'
#' @seealso [import_gaswerte()], [run_batch_pipeline()]
#'
#' @examples
#' \dontrun{
#' samples <- import_samples("data/Protokoll_VersuchXY.xlsx")
#' }
#'
#' @export
import_samples <- function(path = ".")
{
  protocol_file <- list.files(
    path = path,
    pattern = "Protokoll.*\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  # exclude Excel temporary lock file
  file <- protocol_file[!stringr::str_detect(basename(protocol_file), "^~\\$")]

  readxl::read_xlsx(
    file[1],
    sheet = "Proben",
    range = "A4:k121") |>
    dplyr::rename_with(
      ~ stringr::str_replace_all(
        .x,
        c("ö" = "oe", "ä" = "ae", "ü" = "ue")
      )
    ) |>
    janitor::clean_names() |>
    tidyr::drop_na(einwaage_in_g_fm) |>
    dplyr::mutate(
      dplyr::across(
        -c(platz, gasmaus, d_gasmaus_cm, einwaage_in_g_fm),
        as.character
      ),
      dplyr::across(
        c(platz, gasmaus, d_gasmaus_cm, nullpunkt_cm, einwaage_in_g_fm),
        as.numeric
      ),
      projekt = stringr::str_to_lower(projekt) |> stringr::str_squish()
    ) |>
    janitor::remove_empty(which = "cols")
}
