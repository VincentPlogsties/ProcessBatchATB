# data-raw/run_pipeline.R
# Entry point for running the full batch assay pipeline.
# Adjust paths to match your project layout.

library(ProcessBatchATB)

results <- run_batch_pipeline(
  protokoll_path = "data/Protokoll_VersuchXY.xlsx",
  analytik_path  = "data/Analysen",
  bad_channels   = c()   # e.g. c("W1P3", "W2P7")
)

# Inspect results
results$yields
results$validity
