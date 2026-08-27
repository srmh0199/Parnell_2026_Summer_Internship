# build_parnell_benchmark.R
# =========================================================================
# Precompute a small, ANONYMIZED cross-herd summary from
# data/parnell_files/silver/ so ParnellBenchmark/app.R never has to touch
# the 453-herd, ~1.8 GB silver layer at runtime.
#
# Each herd in data/parnell_files/silver contributes three files named
# <GUID>_animals.parquet / _animal_lactations.parquet / _events_bred.parquet,
# where the GUID prefix is that herd's HerdKey (same v2 silver-layer naming
# convention used elsewhere at Parnell). Only animal_lactations.parquet and
# events_bred.parquet are read — animals.parquet isn't needed for any of the
# measures below.
#
# The peer extract ships BRED events only (no OPEN/DNB event types), so
# eligibility for Insemination Rate / Pregnancy Rate is derived from each
# BRED event's own R outcome code (P/A/O/R/E/...) via
# functions/fxn_parnell_eligibility.R, rather than from ParnellRepro/app.R's
# OPEN/DNB-event-based logic. The same proxy method is applied to Monte
# Vista in ParnellBenchmark/app.R so both sides of the comparison are
# computed identically. Conception Rate, Rebreed rate, and Abortion rate
# follow the canonical Parnell CR definition in functions/fxn_parnell_cr.R.
#
# Output: data/parnell_files/benchmark_data.rds — a named list of five
# small tables (cr_monthly, ir_pr_monthly, rebreed_monthly, abortion_monthly,
# dim_milestone), all keyed by an anonymous herd_label (Peer-001..) assigned
# in GUID-sort order. No herd name, farm name, address, or HerdKey survives
# into the saved object.
#
# Run from the project root: Rscript build_parnell_benchmark.R

library(tidyverse)
library(arrow)
library(lubridate)
library(here)

source(here::here("functions", "fxn_parnell_utils.R"))
source(here::here("functions", "fxn_parnell_cr.R"))
source(here::here("functions", "fxn_parnell_eligibility.R"))

silver_dir <- here::here("data", "parnell_files", "silver")
out_path   <- here::here("data", "parnell_files", "benchmark_data.rds")

VWP <- 50L  # matches ParnellRepro/app.R's default voluntary waiting period

guid_pattern <- "^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"

lact_map <- c(
  "1st (LACT=1)" = "Lact 1",
  "2nd (LACT=2)" = "Lact 2",
  "3+ (LACT>2)"  = "Lact 3+"
)

bred_files <- list.files(silver_dir, pattern = "_events_bred\\.parquet$", full.names = FALSE)
guids <- str_extract(bred_files, guid_pattern)
guids <- sort(unique(guids[!is.na(guids)]))
stopifnot("No GUID-prefixed events_bred files found" = length(guids) > 0)
log_msg(sprintf("Found %d herd files under %s", length(guids), silver_dir))

# ---- Per-herd calculations -------------------------------------------------
# fxn_dim_milestone() comes from functions/fxn_parnell_eligibility.R, shared
# with ParnellBenchmark/app.R so Monte Vista is scored the same way.

process_one_herd <- function(guid) {
  eb_path <- file.path(silver_dir, paste0(guid, "_events_bred.parquet"))
  al_path <- file.path(silver_dir, paste0(guid, "_animal_lactations.parquet"))

  eb <- read_parquet(eb_path, col_select = c("cowid_lact", "date_event", "R", "LACT_grp_set2"))
  al <- read_parquet(al_path, col_select = c("cowid_lact", "LACT", "date_fresh", "date_dry", "date_archive"))

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
  months_vec <- seq(min(eb_cows$month), max(eb_cows$month), by = "month")
  ir_pr_monthly <- fxn_monthly_ir_pr(al_g, eb_cows, months_vec, VWP)

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
    cr_monthly       = cr_monthly       %>% mutate(HerdKey = guid),
    ir_pr_monthly    = ir_pr_monthly    %>% mutate(HerdKey = guid),
    rebreed_monthly  = rebreed_monthly  %>% mutate(HerdKey = guid),
    abortion_monthly = abortion_monthly %>% mutate(HerdKey = guid),
    dim_milestone    = dim_milestone    %>% mutate(HerdKey = guid)
  )
}

results <- vector("list", length(guids))
n_failed <- 0L
n_error  <- 0L

for (i in seq_along(guids)) {
  g <- guids[i]
  res <- tryCatch(
    process_one_herd(g),
    error = function(e) {
      log_msg(sprintf("[%d/%d] %s FAILED: %s", i, length(guids), g, conditionMessage(e)))
      n_failed <<- n_failed + 1L
      NULL
    }
  )
  results[[i]] <- res
  if (i %% 50 == 0 || i == length(guids)) {
    log_msg(sprintf("[%d/%d] herds processed (%d failed so far)", i, length(guids), n_failed))
  }
}

results <- results[!map_lgl(results, is.null)]
stopifnot("No herd produced any output" = length(results) > 0)

combine <- function(name) map_dfr(results, ~ .x[[name]])
all_cr       <- combine("cr_monthly")
all_ir_pr    <- combine("ir_pr_monthly")
all_rebreed  <- combine("rebreed_monthly")
all_abortion <- combine("abortion_monthly")
all_dim      <- combine("dim_milestone")

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
# Assign an opaque, stable-within-this-build label in GUID-sort order and
# drop the real HerdKey entirely from every shipped table. No name, farm
# name, address, or identifying key travels with the app from this point on.
herd_labels <- tibble(HerdKey = sort(herds_out)) %>%
  mutate(herd_label = sprintf("Peer-%03d", row_number()))

anonymize <- function(df) {
  df %>%
    inner_join(herd_labels, by = "HerdKey") %>%
    select(-HerdKey) %>%
    relocate(herd_label)
}

benchmark_data <- list(
  cr_monthly       = anonymize(all_cr),
  ir_pr_monthly    = anonymize(all_ir_pr),
  rebreed_monthly  = anonymize(all_rebreed),
  abortion_monthly = anonymize(all_abortion),
  dim_milestone    = anonymize(all_dim)
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
  log_msg(sprintf("  %-16s %6d rows, %3d herds", nm, nrow(benchmark_data[[nm]]), n_distinct(benchmark_data[[nm]]$herd_label)))
}
