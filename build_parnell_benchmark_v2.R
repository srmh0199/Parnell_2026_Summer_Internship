# build_parnell_benchmark_v2.R
# =========================================================================
# Second-generation peer benchmark build. Same shape and anonymization as
# build_parnell_benchmark.R, with three deliberate changes:
#
#  1. Breedings come from <GUID>_events_bred_no_qc_filter.parquet — the
#     complete breeding stream. The QC-filtered events_bred.parquet drops
#     incomplete lactations at the beginning of each herd's pull window
#     (so the service number is always known), which also drops real cows
#     from the early months. The measures here don't use service number,
#     and the app trims thin edge months anyway, so the complete file wins.
#  2. Each herd's full <GUID>_events.parquet contributes DNB dates, and
#     IR/PR is computed BOTH ways per herd:
#       ir_pr_monthly      — proxy eligibility (identical to v1)
#       ir_pr_monthly_dnbx — same, minus cows from their first DNB onward
#     so ParnellBenchmark/app.R can toggle a SYMMETRIC DNB exclusion
#     (own herds and peers under the same definition).
#  3. Input comes from a working copy of the silver layer downloaded from
#     the parnell Azure blob (container "silver-files"), pointed to by the
#     SILVER_DIR environment variable. Those raw files carry real farm
#     names/addresses — they must never live in the repo; only this
#     script's anonymized output does.
#
# Output: data/parnell_files/benchmark_data_v2.rds — six tables keyed by
# an anonymous herd_label (Peer-001.. in GUID-sort order of THIS build; no
# name, address, or HerdKey survives).
#
# Run: set SILVER_DIR to the folder of downloaded silver files, then
#   Rscript build_parnell_benchmark_v2.R

library(tidyverse)
library(arrow)
library(lubridate)
library(here)

source(here::here("functions", "fxn_parnell_utils.R"))
source(here::here("functions", "fxn_parnell_cr.R"))
source(here::here("functions", "fxn_parnell_eligibility.R"))

silver_dir <- Sys.getenv("SILVER_DIR")
stopifnot("Set SILVER_DIR to the downloaded silver-files folder" = nzchar(silver_dir) && dir.exists(silver_dir))
out_path <- here::here("data", "parnell_files", "benchmark_data_v2.rds")

VWP <- 50L  # matches ParnellBenchmark/app.R

guid_pattern <- "^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"

lact_map <- c(
  "1st (LACT=1)" = "Lact 1",
  "2nd (LACT=2)" = "Lact 2",
  "3+ (LACT>2)"  = "Lact 3+"
)

bred_files <- list.files(silver_dir, pattern = "_events_bred_no_qc_filter\\.parquet$", full.names = FALSE)
guids <- str_extract(bred_files, guid_pattern)
guids <- sort(unique(guids[!is.na(guids)]))
stopifnot("No GUID-prefixed events_bred_no_qc_filter files found" = length(guids) > 0)
log_msg(sprintf("Found %d herd files under %s", length(guids), silver_dir))

# ---- Per-herd calculations -------------------------------------------------

process_one_herd <- function(guid) {
  eb_path  <- file.path(silver_dir, paste0(guid, "_events_bred_no_qc_filter.parquet"))
  al_path  <- file.path(silver_dir, paste0(guid, "_animal_lactations.parquet"))
  ev_path  <- file.path(silver_dir, paste0(guid, "_events.parquet"))

  eb <- read_parquet(eb_path, col_select = c("cowid_lact", "date_event", "R", "LACT_grp_set2"))
  al <- read_parquet(al_path, col_select = c("cowid_lact", "LACT", "date_fresh", "date_dry", "date_archive"))
  dnb <- read_parquet(ev_path, col_select = c("cowid_lact", "Event", "Date")) %>%
    filter(Event == "DNB", !is.na(Date)) %>%
    group_by(cowid_lact = as.character(cowid_lact)) %>%
    summarise(date_dnb = min(as.Date(Date)), .groups = "drop")

  # Per-herd file stacking trap: a column empty for this one herd can come
  # back typed <logical> from arrow instead of <character>, and recoding it
  # would then abort. Coerce before any case_when/recode.
  eb <- eb %>% mutate(
    cowid_lact = as.character(cowid_lact),
    R          = as.character(R),
    LACT_grp_set2 = as.character(LACT_grp_set2)
  )
  al <- al %>% mutate(cowid_lact = as.character(cowid_lact))

  eb_cows <- eb %>%
    filter(LACT_grp_set2 %in% names(lact_map), !is.na(date_event)) %>%
    mutate(lact_group = unname(lact_map[LACT_grp_set2]), month = floor_date(date_event, "month"))

  al_g <- al %>%
    filter(LACT > 0) %>%
    mutate(lact_group = case_when(
      LACT == 1 ~ "Lact 1", LACT == 2 ~ "Lact 2", LACT >= 3 ~ "Lact 3+", TRUE ~ NA_character_
    )) %>%
    filter(!is.na(lact_group))

  if (nrow(eb_cows) == 0 || nrow(al_g) == 0) return(NULL)

  # -- Conception Rate, monthly by lact_group (canonical CR definition) ----
  eb_std <- fxn_standardize_for_CR(eb_cows)
  n_error <<- n_error + sum(eb_std$Rstandard == "ERROR", na.rm = TRUE)
  cr_monthly <- eb_std %>%
    group_by(month, lact_group, Rstandard) %>%
    fxn_calculate_CR_main() %>%
    ungroup() %>%
    select(month, lact_group, std_Open, std_Pregnant, std_Abort, std_Other, deno, preg, other, total)

  # -- Insemination Rate / Pregnancy Rate, monthly by lact_group -----------
  # Both definitions per herd: the v1 proxy, and DNB-excluded.
  months_vec <- seq(min(eb_cows$month), max(eb_cows$month), by = "month")
  ir_pr_monthly      <- fxn_monthly_ir_pr(al_g, eb_cows, months_vec, VWP)
  ir_pr_monthly_dnbx <- fxn_monthly_ir_pr(al_g, eb_cows, months_vec, VWP, dnb_dates = dnb)

  # -- Rebreed rate, monthly, herd-wide (matches ParnellRepro/app.R) -------
  rebreed_monthly <- eb %>%
    filter(!is.na(date_event)) %>%
    mutate(month = floor_date(date_event, "month")) %>%
    group_by(month) %>%
    summarise(n_total_bred = n(), n_rebreeds = sum(R == "R", na.rm = TRUE), .groups = "drop")

  # -- Abortion cohort, monthly, herd-wide ----------------------------------
  abortion_monthly <- eb %>%
    fxn_standardize_for_CR() %>%
    filter(Rstandard %in% c("std_Pregnant", "std_Abort")) %>%
    mutate(month = floor_date(date_event, "month")) %>%
    group_by(month) %>%
    summarise(total_pregnancies = n(), n_abortions = sum(Rstandard == "std_Abort"), .groups = "drop")

  # -- DIM milestones (100/150/200), herd-wide ------------------------------
  date_max_pull <- max(eb$date_event, na.rm = TRUE)
  dim_milestone <- bind_rows(
    fxn_dim_milestone(al, eb, 100, date_max_pull),
    fxn_dim_milestone(al, eb, 150, date_max_pull),
    fxn_dim_milestone(al, eb, 200, date_max_pull)
  )

  list(
    cr_monthly         = cr_monthly         %>% mutate(HerdKey = guid),
    ir_pr_monthly      = ir_pr_monthly      %>% mutate(HerdKey = guid),
    ir_pr_monthly_dnbx = ir_pr_monthly_dnbx %>% mutate(HerdKey = guid),
    rebreed_monthly    = rebreed_monthly    %>% mutate(HerdKey = guid),
    abortion_monthly   = abortion_monthly   %>% mutate(HerdKey = guid),
    dim_milestone      = dim_milestone      %>% mutate(HerdKey = guid)
  )
}

# ---- Chunked execution (optional) ------------------------------------------
# For running the build in supervised slices (e.g. a harness that caps how
# long one process may run): set HERD_START / HERD_END (1-based, inclusive)
# and CHUNK_DIR to process a slice and save it, then run once more with
# FINALIZE=1 (and CHUNK_DIR) to combine every saved slice into the final
# rds. With none of these set, the script runs end-to-end in one pass.
chunk_dir     <- Sys.getenv("CHUNK_DIR", "")
finalize_only <- nzchar(Sys.getenv("FINALIZE"))
herd_start    <- as.integer(Sys.getenv("HERD_START", "1"))
herd_end      <- as.integer(Sys.getenv("HERD_END", as.character(length(guids))))

n_failed <- 0L
n_error  <- 0L

if (finalize_only) {
  stopifnot("FINALIZE=1 requires CHUNK_DIR" = nzchar(chunk_dir))
  chunk_files <- list.files(chunk_dir, pattern = "^chunk_.*\\.rds$", full.names = TRUE)
  stopifnot("No chunk files found in CHUNK_DIR" = length(chunk_files) > 0)
  chunks   <- map(chunk_files, readRDS)
  results  <- do.call(c, map(chunks, "results"))
  n_failed <- sum(map_int(chunks, "n_failed"))
  n_error  <- sum(map_int(chunks, "n_error"))
  log_msg(sprintf("Finalize: %d chunk files, %d herd results, %d failed",
                  length(chunk_files), length(results), n_failed))
} else {
  stopifnot(herd_start >= 1, herd_end <= length(guids), herd_start <= herd_end)
  idx <- herd_start:herd_end
  results <- vector("list", length(idx))
  for (j in seq_along(idx)) {
    i <- idx[j]
    g <- guids[i]
    res <- tryCatch(
      process_one_herd(g),
      error = function(e) {
        log_msg(sprintf("[%d/%d] %s FAILED: %s", i, length(guids), g, conditionMessage(e)))
        n_failed <<- n_failed + 1L
        NULL
      }
    )
    results[[j]] <- res
    if (j %% 25 == 0 || j == length(idx)) {
      log_msg(sprintf("[herd %d/%d overall] chunk progress %d/%d (%d failed so far)",
                      i, length(guids), j, length(idx), n_failed))
    }
  }
  results <- results[!map_lgl(results, is.null)]
  if (nzchar(chunk_dir)) {
    dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
    cf <- file.path(chunk_dir, sprintf("chunk_%04d_%04d.rds", herd_start, herd_end))
    saveRDS(list(results = results, n_failed = n_failed, n_error = n_error), cf)
    log_msg(sprintf("Saved %s (%d herd results); run with FINALIZE=1 to combine", cf, length(results)))
    quit(save = "no", status = 0)
  }
}

stopifnot("No herd produced any output" = length(results) > 0)

combine <- function(name) map_dfr(results, ~ .x[[name]])
all_cr        <- combine("cr_monthly")
all_ir_pr     <- combine("ir_pr_monthly")
all_ir_pr_dnb <- combine("ir_pr_monthly_dnbx")
all_rebreed   <- combine("rebreed_monthly")
all_abortion  <- combine("abortion_monthly")
all_dim       <- combine("dim_milestone")

herds_out <- unique(all_cr$HerdKey)
log_msg(sprintf(
  "Processed %d herds, %d contributed rows, %d failed",
  length(guids), length(herds_out), n_failed
))
log_msg(sprintf(
  "%d BRED rows across all herds had an R code outside the known set (Rstandard = 'ERROR') and were excluded from CR",
  n_error
))

# ---- Anonymize -------------------------------------------------------------
# Opaque stable-within-this-build labels in GUID-sort order; the real
# HerdKey is dropped from every shipped table.
herd_labels <- tibble(HerdKey = sort(herds_out)) %>%
  mutate(herd_label = sprintf("Peer-%03d", row_number()))

anonymize <- function(df) {
  df %>%
    inner_join(herd_labels, by = "HerdKey") %>%
    select(-HerdKey) %>%
    relocate(herd_label)
}

benchmark_data <- list(
  cr_monthly         = anonymize(all_cr),
  ir_pr_monthly      = anonymize(all_ir_pr),
  ir_pr_monthly_dnbx = anonymize(all_ir_pr_dnb),
  rebreed_monthly    = anonymize(all_rebreed),
  abortion_monthly   = anonymize(all_abortion),
  dim_milestone      = anonymize(all_dim)
)

# ---- Anonymity assertions (not just a docstring — actually check) --------
banned_cols <- c("HerdKey", "HerdName", "FarmName", "City", "AddressLine1",
                  "AddressLine2", "State", "PostalCode", "FarmId", "HerdId", "cowid_lact")
for (nm in names(benchmark_data)) {
  stopifnot(
    "Identifying column leaked into shipped object" =
      !any(banned_cols %in% names(benchmark_data[[nm]]))
  )
}
stopifnot("herd_label is not unique per herd" = n_distinct(herd_labels$herd_label) == nrow(herd_labels))

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_rds(benchmark_data, out_path, compress = "gz")

log_msg(sprintf(
  "Wrote %s: %s",
  out_path, format(file.info(out_path)$size, big.mark = ",")
))
for (nm in names(benchmark_data)) {
  log_msg(sprintf("  %-18s %6d rows, %3d herds", nm, nrow(benchmark_data[[nm]]), n_distinct(benchmark_data[[nm]]$herd_label)))
}
