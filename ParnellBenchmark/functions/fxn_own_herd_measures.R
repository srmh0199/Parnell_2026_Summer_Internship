# Own-herd measure computation for ParnellBenchmark, shared by
# build_own_benchmark.R (precompute at build time -> data/parnell_files/
# own_data.rds) and ParnellBenchmark/app.R (live fallback when that rds is
# absent). Requires functions/fxn_parnell_cr.R and
# functions/fxn_parnell_eligibility.R to be sourced first.
#
# The intermediate files can hold SEVERAL herds at once: Parnell-style pulls
# processed with fxn_assign_id_animal_parnell prefix every id_animal with the
# herd GUID, and that prefix splits the data back into herds here. Data built
# with the default id function (e.g. Monte Vista's single CSV) has no GUID
# prefix and falls back to one herd labeled "My Herd".

fxn_own_herd_label <- function(id) {
  key <- str_extract(id, "^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4,12}")
  if_else(is.na(key), "My Herd", paste("Herd", str_sub(key, 1, 8)))
}

# events_formatted / animal_lactations: the two step-1/2 parquet tables.
# Returns a named list of the six monthly tables (per herd_label) plus meta:
#   cr_monthly, ir_pr_monthly, ir_pr_monthly_dnbx, rebreed_monthly,
#   abortion_monthly, dim_milestone, meta (herd_label, date_min_event,
#   date_max_event), computed_at.
fxn_compute_own_measures <- function(events_formatted, animal_lactations, vwp = 50L) {
  own_lact_map <- c("LACT 1" = "Lact 1", "LACT 2" = "Lact 2", "LACT 3+" = "Lact 3+")

  # events_formatted already carries its own lact_group, so Conception Rate
  # needs no join to animal_lactations.
  cr_monthly <- events_formatted %>%
    filter(event == "BRED", lact_group %in% names(own_lact_map)) %>%
    mutate(
      herd_label = fxn_own_herd_label(id_animal),
      lact_group = unname(own_lact_map[lact_group]),
      month      = floor_date(date_event, "month")
    ) %>%
    fxn_standardize_for_CR() %>%
    group_by(herd_label, month, lact_group, Rstandard) %>%
    fxn_calculate_CR_main() %>%
    ungroup() %>%
    select(herd_label, month, lact_group, std_Open, std_Pregnant, std_Abort, std_Other, deno, preg, other, total)

  # IR / PR / Rebreed / Abortion / DIM milestones need date_fresh/date_dry/
  # date_archive, reshaped to the shape the eligibility helpers expect.
  lact_data <- animal_lactations %>%
    filter(lact_number > 0) %>%
    transmute(
      herd_label = fxn_own_herd_label(id_animal),
      cowid_lact = id_animal_lact,
      lact_group = case_when(
        lact_number == 1 ~ "Lact 1", lact_number == 2 ~ "Lact 2",
        lact_number >= 3 ~ "Lact 3+", TRUE ~ NA_character_
      ),
      date_fresh, date_dry, date_archive
    ) %>%
    filter(!is.na(lact_group))

  bred_events <- events_formatted %>%
    filter(event == "BRED") %>%
    transmute(herd_label = fxn_own_herd_label(id_animal), cowid_lact = id_animal_lact, date_event, R)

  herds <- sort(unique(bred_events$herd_label))

  # DNB dates per cow-lactation: the full event stream is available for own
  # herds, so IR/PR can also be computed with cows dropped from their first
  # DNB event onward (the ir_pr_monthly_dnbx variant).
  dnb_dates <- events_formatted %>%
    filter(toupper(event) == "DNB", !is.na(date_event)) %>%
    group_by(cowid_lact = id_animal_lact) %>%
    summarise(date_dnb = min(date_event), .groups = "drop")

  # Month range and date_max_pull are per herd — pulls can be days or weeks
  # apart, and sharing a calendar range would fabricate empty months.
  compute_ir_pr <- function(dnb = NULL) {
    map_dfr(herds, function(h) {
      bred_h <- bred_events %>% filter(herd_label == h)
      lact_h <- lact_data %>% filter(herd_label == h)
      months_h <- seq(
        floor_date(min(bred_h$date_event, na.rm = TRUE), "month"),
        floor_date(max(bred_h$date_event, na.rm = TRUE), "month"),
        by = "month"
      )
      fxn_monthly_ir_pr(lact_h, bred_h, months_h, vwp, dnb_dates = dnb) %>%
        mutate(herd_label = h)
    })
  }

  rebreed_monthly <- bred_events %>%
    filter(!is.na(date_event)) %>%
    mutate(month = floor_date(date_event, "month")) %>%
    group_by(herd_label, month) %>%
    summarise(n_total_bred = n(), n_rebreeds = sum(R == "R", na.rm = TRUE), .groups = "drop")

  abortion_monthly <- bred_events %>%
    fxn_standardize_for_CR() %>%
    filter(Rstandard %in% c("std_Pregnant", "std_Abort")) %>%
    mutate(month = floor_date(date_event, "month")) %>%
    group_by(herd_label, month) %>%
    summarise(total_pregnancies = n(), n_abortions = sum(Rstandard == "std_Abort"), .groups = "drop")

  dim_milestone <- map_dfr(herds, function(h) {
    bred_h <- bred_events %>% filter(herd_label == h)
    lact_h <- lact_data %>% filter(herd_label == h)
    dmp_h  <- max(bred_h$date_event, na.rm = TRUE)
    bind_rows(
      fxn_dim_milestone(lact_h, bred_h, 100, dmp_h),
      fxn_dim_milestone(lact_h, bred_h, 150, dmp_h),
      fxn_dim_milestone(lact_h, bred_h, 200, dmp_h)
    ) %>% mutate(herd_label = h)
  })

  meta <- bred_events %>%
    filter(!is.na(date_event)) %>%
    group_by(herd_label) %>%
    summarise(
      date_min_event = min(date_event),
      date_max_event = max(date_event),
      .groups = "drop"
    )

  list(
    cr_monthly         = cr_monthly,
    ir_pr_monthly      = compute_ir_pr(),
    ir_pr_monthly_dnbx = compute_ir_pr(dnb_dates),
    rebreed_monthly    = rebreed_monthly,
    abortion_monthly   = abortion_monthly,
    dim_milestone      = dim_milestone,
    meta               = meta,
    computed_at        = Sys.time()
  )
}
