# Eligible-cow denominator logic for Insemination Rate / Pregnancy Rate,
# adapted to run on data that has ONLY BRED events (no OPEN/DNB event types).
#
# ParnellRepro/app.R derives a cow's current status from her most recent
# OPEN/BRED/DNB *event*. The Parnell peer extract (data/parnell_files/silver)
# ships BRED events only, but each BRED row carries an R outcome code
# (P/A/O/R/E/...) that already records the pregnancy-check result of THAT
# service — so the same status can be derived from R-code history instead of
# from a separate event type. A cow is treated as presumed pregnant from a
# std_Pregnant-coded BRED event until her NEXT BRED event of any code (which
# means she was rechecked and either stayed pregnant — a later P — or
# reopened), or until she goes dry/is archived, whichever comes first.
#
# This is used identically for peer herds and for Monte Vista in
# ParnellBenchmark/app.R so the comparison is computed the same way on both
# sides. It is NOT a replacement for ParnellRepro/app.R's own OPEN/DNB-based
# eligibility, which is more precise when the full event stream is available
# (it can see DNB and heat-observed-but-not-bred cows; this proxy cannot).
#
# Requires functions/fxn_parnell_cr.R (for fxn_standardize_for_CR) to be
# sourced first.

# bred_events: cowid_lact, date_event, R (raw code, standardized here).
# Returns cowid_lact, preg_start, preg_end (preg_end is NA while still open,
# meaning presumed pregnant through the end of the data).
fxn_pregnancy_segments <- function(bred_events) {
  bred_events %>%
    fxn_standardize_for_CR() %>%
    filter(!is.na(date_event)) %>%
    arrange(cowid_lact, date_event) %>%
    group_by(cowid_lact) %>%
    mutate(next_bred_date = lead(date_event)) %>%
    ungroup() %>%
    filter(Rstandard == "std_Pregnant") %>%
    transmute(cowid_lact, preg_start = date_event, preg_end = next_bred_date)
}

# lact_data: cowid_lact, lact_group, date_fresh, date_dry, date_archive
#   (already filtered to LACT > 0 / the lactation groups of interest).
# preg_segments: from fxn_pregnancy_segments().
# months: vector of first-of-month Dates to snapshot.
# Returns one row per eligible cow-lactation-month: cowid_lact, lact_group, month.
fxn_monthly_eligible <- function(lact_data, preg_segments, months, vwp) {
  purrr::map_dfr(months, function(m) {
    m_start <- as.Date(m)
    m_end   <- lubridate::ceiling_date(m_start, "month") - lubridate::days(1)

    elig <- lact_data %>%
      filter(
        date_fresh <= m_end,
        is.na(date_archive) | date_archive >= m_start,
        is.na(date_dry) | date_dry > m_start,
        as.numeric(m_start - date_fresh) > vwp
      )
    if (nrow(elig) == 0) return(NULL)

    preg_now <- preg_segments %>%
      filter(preg_start <= m_start, is.na(preg_end) | preg_end > m_start) %>%
      distinct(cowid_lact)

    elig %>%
      anti_join(preg_now, by = "cowid_lact") %>%
      mutate(month = m_start) %>%
      select(cowid_lact, lact_group, month)
  })
}

# Combines eligibility with that month's BRED/pregnancy flags into the
# monthly IR/PR summary: month, lact_group, n_eligible, n_bred, n_pregnant.
fxn_monthly_ir_pr <- function(lact_data, bred_events, months, vwp) {
  preg_segments <- fxn_pregnancy_segments(bred_events)
  eligible <- fxn_monthly_eligible(lact_data, preg_segments, months, vwp)
  if (nrow(eligible) == 0) return(eligible[0, ] %>% mutate(n_eligible = integer(), n_bred = integer(), n_pregnant = integer()))

  bred_flags <- bred_events %>%
    filter(!is.na(date_event)) %>%
    mutate(month = lubridate::floor_date(date_event, "month")) %>%
    distinct(cowid_lact, month) %>%
    mutate(is_bred = TRUE)

  preg_std <- bred_events %>% fxn_standardize_for_CR()
  preg_flags <- preg_std %>%
    filter(Rstandard %in% c("std_Pregnant", "std_Abort"), !is.na(date_event)) %>%
    mutate(month = lubridate::floor_date(date_event, "month")) %>%
    distinct(cowid_lact, month) %>%
    mutate(is_pregnant = TRUE)

  eligible %>%
    left_join(bred_flags, by = c("cowid_lact", "month")) %>%
    left_join(preg_flags, by = c("cowid_lact", "month")) %>%
    mutate(
      is_bred     = tidyr::replace_na(is_bred, FALSE),
      is_pregnant = tidyr::replace_na(is_pregnant, FALSE)
    ) %>%
    group_by(month, lact_group) %>%
    summarise(
      n_eligible = n(),
      n_bred     = sum(is_bred),
      n_pregnant = sum(is_pregnant),
      .groups = "drop"
    )
}

# Percent pregnant by dim_cutoff DIM (100/150/200), grouped by calving month.
# lact_data: cowid_lact, date_fresh (animal_lactations-shaped, LACT > 0).
# bred_events: cowid_lact, date_event, R (raw code, standardized here).
# date_max_pull: that herd's own most recent event date — a calving cohort is
# only eligible once every cow in it could have reached the DIM cutoff by
# that date.
#
# The most recent calving month is usually a PARTIAL month for this
# purpose: eligibility requires date_fresh <= date_max_pull - dim_cutoff,
# a specific day, not a month boundary, so the denominator for that month
# can cover only its first few days while the numerator (grouped by
# calving_month alone) would still count pregnancies from the WHOLE month
# — inflating perc_preg, in one observed case over 200%. ParnellRepro/
# app.R's preg_by_dim() avoids this with a trim_months cutoff; the same
# fixed trim (3/5/7 months for 100/150/200 DIM) is applied here so a
# calving month is only included once it's guaranteed to be a full month
# on both sides of the ratio.
fxn_dim_milestone <- function(lact_data, bred_events, dim_cutoff, date_max_pull) {
  trim_months <- c(`100` = 3, `150` = 5, `200` = 7)[[as.character(dim_cutoff)]]
  cutoff_end  <- lubridate::floor_date(date_max_pull, "month") %m-% months(trim_months)

  eligible <- lact_data %>%
    filter(date_fresh <= (date_max_pull - dim_cutoff)) %>%
    mutate(calving_month = lubridate::floor_date(date_fresh, "month")) %>%
    filter(calving_month < cutoff_end) %>%
    group_by(calving_month) %>%
    summarise(total_cows = n(), .groups = "drop")

  if (nrow(eligible) == 0) {
    return(eligible[0, ] %>% mutate(pregnant_count = integer(), perc_preg = double(), milestone = character()))
  }

  pregnant <- bred_events %>%
    fxn_standardize_for_CR() %>%
    filter(Rstandard %in% c("std_Pregnant", "std_Abort")) %>%
    inner_join(lact_data %>% select(cowid_lact, date_fresh), by = "cowid_lact") %>%
    mutate(
      dim_at_event  = as.numeric(date_event - date_fresh),
      calving_month = lubridate::floor_date(date_fresh, "month")
    ) %>%
    filter(dim_at_event >= 0, dim_at_event <= dim_cutoff, calving_month < cutoff_end) %>%
    distinct(cowid_lact, calving_month) %>%
    group_by(calving_month) %>%
    summarise(pregnant_count = n(), .groups = "drop")

  eligible %>%
    left_join(pregnant, by = "calving_month") %>%
    mutate(
      pregnant_count = tidyr::replace_na(pregnant_count, 0L),
      perc_preg       = pregnant_count / total_cows,
      milestone       = paste0(dim_cutoff, " DIM")
    )
}
