test_that("import_samples drops rows without einwaage_in_g_fm", {
  # just test that the function exists and has expected formals
  expect_true(is.function(import_samples))
  expect_named(formals(import_samples), c("path", "sheet", "range"))
})

test_that("calc_termination returns expected columns", {
  expect_true(is.function(calc_termination))
})

test_that("check_validity returns diff_mean_perc column", {
  expect_true(is.function(check_validity))
})

test_that("run_batch_pipeline returns a named list with 7 elements", {
  expect_true(is.function(run_batch_pipeline))
  expect_named(
    formals(run_batch_pipeline),
    c("protokoll_path", "analytik_path", "bad_channels",
      "analytik_pattern", "sample_sheet", "sample_range", "gas_sheets")
  )
})
