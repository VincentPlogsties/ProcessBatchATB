#' ProcessBatchATB: Process Batch Fermentation Assay Data
#'
#' @description
#' A pipeline for batch fermentation (BMP) assay data following VDI 4630.
#' Analytics data (substrate characterisation, inoculum) is handled via
#' \pkg{ProcessLabAssistant}. This package covers everything downstream:
#'
#' 1. **Import** – sample protocol sheets and raw gas measurement workbooks.
#' 2. **Process analytics** – via \pkg{ProcessLabAssistant}, then compute
#'    inoculum blank means and join substrate dry-matter values.
#' 3. **Gas calculations** – normalised volume, blank correction,
#'    cumulative biogas and methane per measurement position.
#' 4. **Termination criterion** – VDI 4630 stop condition
#'    (≤ 0.5 %/d for 3 consecutive days after day 25).
#' 5. **Summarise** – daily and cumulative yields normalised to ODM
#'    (oTM, g VS).
#' 6. **Validity check** – pairwise deviation between replicates per VDI 4630.
#'
#' @section Main functions:
#' * [import_samples()] – read sample protocol sheet
#' * [import_gaswerte()] – read gas measurement sheets (w1–w6)
#' * [process_analytik()] – clean/transform analytics via ProcessLabAssistant,
#'   compute inoculum blank mean, join to sample table
#' * [calc_gasvol()] – normalised volume, blank correction, cumulative sums
#' * [calc_termination()] – VDI 4630 termination criterion
#' * [calc_yields()] – daily biogas and methane yields per ODM
#' * [check_validity()] – pairwise replicate deviation per VDI 4630
#' * [run_batch_pipeline()] – full pipeline wrapper
#'
#' @references
#' VDI 4630 (2016). Fermentation of organic materials – Characterisation of
#' the substrate, sampling, collection of material data, fermentation tests.
#' Verein Deutscher Ingenieure, Düsseldorf.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr across all_of any_of arrange bind_rows case_when cumsum
#'   filter first group_by if_else inner_join join_by lag last left_join mutate
#'   n reframe relocate rename rename_with select summarise ungroup unnest
#' @importFrom tidyr drop_na pivot_longer pivot_wider replace_na unnest
#' @importFrom purrr map
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_c str_detect str_extract str_replace_all str_squish
#'   str_to_lower regex
#' @importFrom lubridate as_date as_datetime seconds_to_period
#' @importFrom janitor clean_names remove_empty
#' @importFrom rlang abort
## usethis namespace: end
NULL
