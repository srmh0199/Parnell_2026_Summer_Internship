# build_own_benchmark.R
# =========================================================================
# Precomputes the "own herds" side of ParnellBenchmark from the step-1/2
# intermediate parquet files into data/parnell_files/own_data.rds — the same
# monthly aggregate tables the app used to compute at every startup (~90 s,
# needs the ~100 MB cow-level parquet on hand). With the rds present the app
# starts in seconds, and a deployment bundle needs NO cow-level data at all:
# the rds holds only monthly counts per herd label.
#
# Run from the project root after any steps-0..2 pipeline refresh:
#   Rscript build_own_benchmark.R

library(tidyverse)
library(arrow)
library(lubridate)
library(here)

source(here::here("functions", "fxn_parnell_cr.R"))
source(here::here("functions", "fxn_parnell_eligibility.R"))
source(here::here("functions", "fxn_own_herd_measures.R"))

data_dir <- here::here("data", "intermediate_files")
out_path <- here::here("data", "parnell_files", "own_data.rds")

events_formatted  <- read_parquet(file.path(data_dir, "events_formatted.parquet"))
animal_lactations <- read_parquet(file.path(data_dir, "animal_lactations.parquet"))

own_data <- fxn_compute_own_measures(events_formatted, animal_lactations, vwp = 50L)

# No cow-level identifiers may ship in this file.
banned_cols <- c("cowid_lact", "id_animal", "id_animal_lact", "ID")
for (nm in setdiff(names(own_data), "computed_at")) {
  stopifnot(
    "Cow-level column leaked into own_data.rds" =
      !any(banned_cols %in% names(own_data[[nm]]))
  )
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_rds(own_data, out_path, compress = "gz")

cat(sprintf("Wrote %s (%s bytes)\n", out_path, format(file.info(out_path)$size, big.mark = ",")))
for (nm in c("cr_monthly", "ir_pr_monthly", "ir_pr_monthly_dnbx",
             "rebreed_monthly", "abortion_monthly", "dim_milestone")) {
  cat(sprintf("  %-18s %6d rows, %d herds\n", nm, nrow(own_data[[nm]]),
              n_distinct(own_data[[nm]]$herd_label)))
}
print(own_data$meta)
