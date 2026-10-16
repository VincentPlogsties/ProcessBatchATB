#' Import gas measurement sheets
#'
#' Reads all gas measurement sheets from the protocol XLSX-File of standard ATB Biogas Batch Test,
#' combines them, reshapes from wide (one column per measurement position) to
#' long format, and parses timestamps.
#'
#' Each sheet is expected to have the same fixed layout with data starting at row 6
#' (`range = "A6:BQ71"`), with columns in the order: date, time, pressure,
#' temperature, tub (`wanne`), followed by five repeated columns per
#' measurement position (height mm, CH4%, CO2%, O2%, H2S ppm) for positions 1–13.
#'
#' @param path Character scalar. Path to the protocol `.xlsx` file.
#' Default `"."` (project root folder)
#'
#' @param excel_origin Character scalar. Date origin for numeric dates.
#'   Defaults to Excel `"1899-12-30"`.
#'
#' @return A long-format tibble with one row per measurement position per
#'   reading. Columns: `datum`, `zeit` (POSIXct), `druck`, `temp`, `wanne`,
#'   `platz` (integer), `hoehe_mm`, `ch4_perc`, `co2_perc`, `o2_perc`,
#'   `h2s_ppm`.
#'
#' @seealso [import_samples()], [calc_gasvol()]
#'
#' @examples
#' \dontrun{
#' gaswerte <- import_gaswerte("data/Protokoll_VersuchXY.xlsx")
#' }
#'
#' @export
import_gaswerte <- function(path = ".", date_origin = "1899-12-30")
{

  protocol_file <- list.files(
    path = path,
    pattern = "Protokoll.*\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  # exclude Excel temporary lock file
  file <- protocol_file[!stringr::str_detect(basename(protocol_file), "^~\\$")]

  # use only sheets with "w" followed by a number to catch only sheets with trial data
  sheets <- readxl::excel_sheets(file)
  sheets <- sheets[stringr::str_detect(sheets, "w\\d+")]

  sheets |>
    purrr::map(
      ~ suppressWarnings(
        readxl::read_xlsx(
          path      = file[1],
          sheet     = .x,
          range     = "A6:BQ71",
          col_names = FALSE,
          col_types = "numeric",
          .name_repair = "unique_quiet"
        )
      ) |>
        dplyr::mutate(sheet_name = .x)
    ) |>
    dplyr::bind_rows() |>
    dplyr::relocate(sheet_name, .after = 4) |>
    dplyr::rename_with(
      ~ c(
        "datum", "zeit", "druck", "temp", "wanne",
        stringr::str_c(
          rep(1:13, each = 5),
          rep(c("hoehe_mm", "ch4_perc", "co2_perc", "o2_perc", "h2s_ppm"), times = 13),
          sep = "-"
        )
      ),
      dplyr::everything()
    ) |>
    tidyr::pivot_longer(
      cols      = -(datum:wanne),
      names_to  = c("num", ".value"),
      names_sep = "-"
    ) |>
    dplyr::mutate(
      platz = as.integer(num),
      .before = hoehe_mm,
      .keep   = "unused"
    ) |>
    tidyr::drop_na(zeit, hoehe_mm) |>
    (\(d) {
      bad <- d$zeit >= 1
      if (any(bad)) {
        rlang::warn(c(
          "Time cells contain a date component; only the time of day is used.",
          i = paste0("Sheet/date: ",
                     unique(paste(d$wanne[bad],
                                  lubridate::as_date(d$datum[bad], origin = date_origin))),
                     collapse = ", ")
        ))
      }
      d
    })() |>
    dplyr::mutate(
      datum = lubridate::as_date(datum, origin = date_origin),
      zeit  = lubridate::as_datetime(
        datum + lubridate::seconds_to_period((zeit %% 1) * 86400)
      )
    )
}
