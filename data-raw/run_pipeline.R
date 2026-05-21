# setup -----------------------------------------------------

# install.packages("devtools")

# library(devtools)
# devtools::load_all()
# devtools::document()

devtools::install_github("https://github.com/VincentPlogsties/ProcessBatchATB.git")

library(ProcessBatchATB)

# single pipeline processing ------------------------------------

results <- run_batch_pipeline(
  protokoll_path = ".",            # default = R-Project folder -> (".")
  bad_channels = character(0),     # example: c("w1-1", "w6-3")
  export_xlsx = FALSE              # in addition to standard .rds file
)

# stepwise processing --------------------------------------------------------------

samples <- import_samples(path = ".")

gaswerte <- import_gaswerte(path = ".")

df_gas <- calc_gasvol(gaswerte, bad_channels = character(0))

df_daily <- calc_daily(df_gas)

termination_result <- calc_termination(df_daily)

df_yields   <- calc_yields(df_daily, termination_result$condition)

df_validity <- check_validity(df_yields$yields)

plot_qc(df_daily, termination_result$abbruch)

results <- list(
  yields_daily = df_yields$yields_daily,
  yields       = df_yields$yields,
  validity     = df_validity,
  abbruch      = termination_result$abbruch,
  abbruch_wide = termination_result$abbruch_wide
)

# export to rds -----------------------------------------------------------

export_data(results, path = ".", write_xlsx = FALSE)

# view results--------------------------------------------------------------
tibble::view(results$abbruch_wide)
tibble::view(results$df_validity)
tibble::view(results$yields)


