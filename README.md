# ProcessBatchATB

An R package for processing batch fermentation (BMP) assay data following
VDI 4630. Depends on `ProcessLabAssistant` for analytics data processing.

## Installation

```r
# Install dependency first
devtools::install("path/to/ProcessLabAssistant")

# Then install this package
devtools::install("path/to/ProcessBatchATB")
```

## Usage

```r
library(ProcessBatchATB)

results <- run_batch_pipeline(
  protokoll_path = "data/Protokoll_VersuchXY.xlsx",
  analytik_path  = "data/Analysen",
  bad_channels   = c()
)

results$yields     # biogas + methane yields per ODM
results$validity   # VDI 4630 replicate check
```

## Pipeline

```
import_samples()       ─┐
import_gaswerte()      ─┤
                        ├─► process_analytik()  (uses ProcessLabAssistant)
                        │         │
                        │         ▼
                        └──► calc_gasvol()
                                  │
                                  ▼
                             calc_daily()
                                  │
                                  ▼
                          calc_termination()    (VDI 4630 stop criterion)
                                  │
                                  ▼
                             calc_yields()      (mL/g VS)
                                  │
                                  ▼
                          check_validity()      (pairwise replicate deviation)
```
