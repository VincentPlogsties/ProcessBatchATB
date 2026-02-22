devtools::load_all()

path_test <- "L:/Abt2/_Labor/Batch/Gärtests/2025/GT_2025_02/R/data"


results <- run_batch_pipeline(
  protokoll_path = path_test,
  analytik_path = path_test,
  bad_channels = character(0)
)

# plot_qc(results$daily, results$abbruch)
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

tibble::view(results$df_validity)
tibble::view(results$yields)
tibble::view(results$condition)
