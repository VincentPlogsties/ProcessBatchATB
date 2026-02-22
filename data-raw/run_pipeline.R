# setup -----------------------------------------------------

# install.packages("devtools")

library(devtools)

devtools::install_github("https://github.com/VincentPlogsties/ProcessBatchATB.git")

library(ProcessBatchATB)

# single pipeline processing ------------------------------------

results <- run_batch_pipeline(
  protokoll_path = "data",
  analytik_path = "data",
  bad_channels = character(0)
)

# stepwise processing --------------------------------------------------------------
raw_analytik <- import_analytik_batch(path = path_test)

samples <- import_samples(path = path_test)

gaswerte <- import_gaswerte(path = path_test)

analytik_result <- process_analytik(raw_analytik, samples)

df_gas <- calc_gasvol(gaswerte, analytik_result$proben, bad_channels = character(0))

df_daily <- calc_daily(df_gas)

termination_result <- calc_termination(df_gas, df_daily)

df_yields   <- calc_yields(df_daily, termination_result$condition)

df_validity <- check_validity(df_yields$yields_daily, analytik_result$proben)

plot_qc(df_daily, termination_result$abbruch)

results <- list(
  analytik     = analytik_result$analytik,
  proben       = analytik_result$proben,
  gas          = df_gas,
  daily        = df_daily,
  condition    = termination_result$condition,
  abbruch_wide = termination_result$abbruch_wide,
  yields_daily = df_yields$yields_daily,
  yields       = df_yields$yields,
  validity     = df_validity
)

# view results--------------------------------------------------------------
tibble::view(results$abbruch_wide)
tibble::view(results$df_validity)
tibble::view(results$yields)

# export ------------------------------------------------------------------

path_output <- stringr::str_c("output/", stringr::str_extract(basename(dirname((getwd()))), "^GT_\\d{4}_\\d{2}"))

readr::write_rds(
  results,
  file = stringr::str_c(path_output, "_results.rds")
)

readr::write_rds(
  df_results_individuals_daily,
  file = stringr::str_c(path_output, "_results_individuals_daily.rds")
)

