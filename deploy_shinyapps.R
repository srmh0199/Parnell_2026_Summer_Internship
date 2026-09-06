# deploy_shinyapps.R
# =========================================================================
# Deploys ParnellBenchmark to shinyapps.io as "IRandPR_plus" on the parnell
# account (same pattern as the mySYNCH_* apps: a self-contained app folder).
#
# Before deploying it syncs the canonical function files and the two
# precomputed aggregate rds files INTO ParnellBenchmark/ so the bundle is
# self-contained. Only monthly aggregates ship — no cow-level data.
#
# Run from the project root: Rscript deploy_shinyapps.R

app_dir <- "ParnellBenchmark"

# ---- Sync canonical sources into the app folder ---------------------------
dir.create(file.path(app_dir, "functions"), showWarnings = FALSE)
dir.create(file.path(app_dir, "data", "parnell_files"), recursive = TRUE, showWarnings = FALSE)

sync <- c(
  "functions/fxn_parnell_cr.R",
  "functions/fxn_parnell_eligibility.R",
  "functions/fxn_own_herd_measures.R",
  "data/parnell_files/benchmark_data_v2.rds",
  "data/parnell_files/own_data.rds"
)
for (f in sync) {
  stopifnot(file.exists(f))
  ok <- file.copy(f, file.path(app_dir, f), overwrite = TRUE)
  stopifnot(ok)
}
cat("Synced", length(sync), "files into", app_dir, "\n")

# ---- Deploy ---------------------------------------------------------------
rsconnect::deployApp(
  appDir      = app_dir,
  appName     = "IRandPR_plus",
  account     = "parnell",
  forceUpdate = TRUE
)
