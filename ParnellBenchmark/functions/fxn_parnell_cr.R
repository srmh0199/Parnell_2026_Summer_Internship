# Conception Rate (CR) standardization, vendored offline from the canonical
# Parnell function so it can run without network access:
# https://raw.githubusercontent.com/ParnellPharma/ParnellFunctionsPublic/refs/heads/main/parnell_calculate_CR_master.R
#
# Kept as close to the original as possible so numbers match anything else in
# the Parnell ecosystem built on the same source function. Requires a column
# named "R" holding the raw breeding-outcome code.

fxn_standardize_for_CR <- function(df) {
  df %>%
    mutate(
      Rstandard = case_when(
        (is.na(R)) ~ "Not Bred Event",
        (R %in% "O") ~ "std_Open",
        (R %in% c("A")) ~ "std_Abort",
        (R %in% c("P", "E")) ~ "std_Pregnant",
        (R %in% c("R", "-", "C", "U", "ABS", " ", "_")) ~ "std_Other",
        TRUE ~ "ERROR"
      )
    )
}

# Summarizes a grouped data frame of standardized breeding events into CR.
# deno excludes "std_Other" / "Not Bred Event" — those outcomes are ambiguous,
# not a real service, so they don't belong in either the numerator or the CR
# denominator (they still count toward `total` and `pct_other`).
fxn_calculate_CR_main <- function(df_grouped) {
  format_data <- tibble(std_Open = 0, std_Pregnant = 0, std_Abort = 0, std_Other = 0, data_type = "Format")

  df_grouped %>%
    summarize(ct_row = sum(n())) %>%
    ungroup() %>%
    pivot_wider(names_from = Rstandard, values_from = ct_row) %>%
    bind_rows(format_data) %>%
    filter(!(data_type %in% "Format")) %>%
    select(-data_type) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    mutate(
      deno  = std_Open + std_Pregnant + std_Abort,
      preg  = std_Pregnant + std_Abort,
      other = std_Other,
      total = std_Open + std_Pregnant + std_Abort + std_Other
    ) %>%
    mutate(
      CR    = preg / deno,
      pct_other = other / total,
      SPC   = deno / preg
    )
}

# Wilson score interval, not the Wald interval the canonical function above
# uses (Wald collapses to zero width at CR = 0 or 1, which reads as false
# certainty on a herd with few services). Used for ranking herds fairly
# regardless of denominator size.
fxn_wilson_ci <- function(k, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  n_safe <- ifelse(n > 0, n, NA_real_)
  p <- k / n_safe
  denom <- 1 + z^2 / n_safe
  center <- (p + z^2 / (2 * n_safe)) / denom
  half_width <- (z / denom) * sqrt((p * (1 - p) / n_safe) + (z^2 / (4 * n_safe^2)))
  tibble(
    estimate = p,
    lower    = ifelse(n > 0, pmax(0, center - half_width), NA_real_),
    upper    = ifelse(n > 0, pmin(1, center + half_width), NA_real_)
  )
}

# Herd size / data-quality labels from the same thresholds as the canonical
# fxn_create_CR_warnings(), applied to any rate's denominator (deno). The
# ">15% Other Outcomes" flag is CR-specific (std_Other isn't a concept for
# IR/PR/Rebreed/Abortion); pass pct_other = NA to skip that check.
fxn_cr_quality_label <- function(deno, pct_other = NA_real_) {
  case_when(
    !is.na(pct_other) & pct_other > 0.15 ~ ">15% Other Outcomes",
    deno < 30  ~ "Very Low (<30)",
    deno < 50  ~ "Low (30-49)",
    deno < 100 ~ "Ok (50-99)",
    TRUE       ~ "Good (>=100)"
  )
}

# Generic Wilson-CI pooling: sums k/n across whatever grouping is passed and
# derives rate/rate_lower/rate_upper from the pooled counts (sum(k)/sum(n),
# not a mean of period shares). Works for CR (preg/deno), IR (n_bred/
# n_eligible), PR (n_pregnant/n_eligible), Rebreed (n_rebreeds/n_total_bred),
# and Abortion (n_abortions/total_pregnancies) alike.
fxn_pool_rate <- function(df, k_col, n_col, ...) {
  pooled <- df %>%
    group_by(...) %>%
    summarise(k = sum(.data[[k_col]]), n = sum(.data[[n_col]]), .groups = "drop")
  ci <- fxn_wilson_ci(pooled$k, pooled$n)
  pooled %>%
    mutate(rate = ci$estimate, rate_lower = ci$lower, rate_upper = ci$upper)
}
