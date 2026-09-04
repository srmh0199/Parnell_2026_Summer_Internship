# ParnellBenchmark — Monte Vista vs. the Parnell peer herd population
# =========================================================================
# Compares Monte Vista's own reproductive KPIs against an anonymized set of
# ~450 other herds (data/parnell_files/silver, precomputed by
# build_parnell_benchmark.R into data/parnell_files/benchmark_data.rds).
#
# The peer file ships BRED events only (no separate OPEN/DNB event types),
# so eligibility for Insemination Rate / Pregnancy Rate is derived from each
# BRED event's own R outcome code (P/A/O/R/E/...) via
# functions/fxn_parnell_eligibility.R, rather than ParnellRepro/app.R's
# OPEN/DNB-event-based logic. That proxy is applied identically to Monte
# Vista here so both sides of every comparison are computed the same way —
# these numbers will differ slightly from ParnellRepro's own IR/PR figures,
# which can see DNB flags and heat-observed-but-not-bred cows this can't.
# Conception Rate, Rebreed rate, and Abortion rate follow the canonical
# Parnell CR definition in functions/fxn_parnell_cr.R (vendored offline from
# ParnellFunctionsPublic).
#
# Peer herds are anonymized in the data itself (build_parnell_benchmark.R)
# — no herd name, farm name, address, or HerdKey is loaded by this app at
# any point; herds are labeled Peer-001..Peer-451.
#
# Run with shiny::runApp("ParnellBenchmark") from the project root.

library(shiny)
library(bslib)
library(tidyverse)
library(arrow)
library(gt)
library(lubridate)
library(scales)
library(here)

source(here::here("functions", "fxn_parnell_cr.R"))
source(here::here("functions", "fxn_parnell_eligibility.R"))

VWP <- 50L  # matches ParnellRepro/app.R's default voluntary waiting period
LACT_GROUPS <- c("Lact 1", "Lact 2", "Lact 3+")

# ---- Load the precomputed, anonymized peer summary -----------------------
# Prefer the v2 build (build_parnell_benchmark_v2.R: complete no-QC-filter
# breedings, plus a DNB-excluded IR/PR variant per peer herd so the DNB
# toggle can be symmetric). Fall back to Sarah's v1 file otherwise.
peer_path_v2 <- here::here("data", "parnell_files", "benchmark_data_v2.rds")
peer_path_v1 <- here::here("data", "parnell_files", "benchmark_data.rds")
peer_path <- if (file.exists(peer_path_v2)) peer_path_v2 else peer_path_v1
if (!file.exists(peer_path)) {
  stop(
    "Missing ", peer_path,
    ".\nRun build_parnell_benchmark.R (or _v2) from the project root first."
  )
}
benchmark_data <- readRDS(peer_path)
peer_cr       <- benchmark_data$cr_monthly
peer_ir_pr    <- benchmark_data$ir_pr_monthly
peer_rebreed  <- benchmark_data$rebreed_monthly
peer_abortion <- benchmark_data$abortion_monthly
peer_dim      <- benchmark_data$dim_milestone
peer_ir_pr_dnbx <- benchmark_data$ir_pr_monthly_dnbx  # NULL on a v1 file
have_peer_dnbx  <- !is.null(peer_ir_pr_dnbx)

n_peer_herds <- n_distinct(peer_cr$herd_label)

# ---- Load Monte Vista's own data and compute every measure the same way --
data_dir  <- here::here("data", "intermediate_files")
req_files <- c("events_formatted.parquet", "animal_lactations.parquet")
missing   <- req_files[!file.exists(file.path(data_dir, req_files))]
if (length(missing) > 0) {
  stop(
    "Missing data file(s) in ", data_dir, ": ", paste(missing, collapse = ", "),
    ".\nRun step0_master_processing_my_data.R from the project root first."
  )
}

events_formatted  <- read_parquet(file.path(data_dir, "events_formatted.parquet"))
animal_lactations <- read_parquet(file.path(data_dir, "animal_lactations.parquet"))

# The intermediate files can hold SEVERAL herds at once: Parnell-style pulls
# processed with fxn_assign_id_animal_parnell prefix every id_animal with the
# herd GUID, so that prefix splits the data back into herds here. Data built
# with the default id function (e.g. Monte Vista's single CSV) has no GUID
# prefix and falls back to one herd labeled "My Herd".
own_herd_label <- function(id) {
  key <- str_extract(id, "^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4,12}")
  if_else(is.na(key), "My Herd", paste("Herd", str_sub(key, 1, 8)))
}

# events_formatted already carries its own lact_group ("Heifer"/"LACT 1"/
# "LACT 2"/"LACT 3+"), so Conception Rate needs no join to animal_lactations.
own_lact_map <- c("LACT 1" = "Lact 1", "LACT 2" = "Lact 2", "LACT 3+" = "Lact 3+")

own_cr_monthly <- events_formatted %>%
  filter(event == "BRED", lact_group %in% names(own_lact_map)) %>%
  mutate(
    herd_label = own_herd_label(id_animal),
    lact_group = unname(own_lact_map[lact_group]),
    month      = floor_date(date_event, "month")
  ) %>%
  fxn_standardize_for_CR() %>%
  group_by(herd_label, month, lact_group, Rstandard) %>%
  fxn_calculate_CR_main() %>%
  ungroup() %>%
  select(herd_label, month, lact_group, std_Open, std_Pregnant, std_Abort, std_Other, deno, preg, other, total)

# IR / PR / Rebreed / Abortion / DIM milestones need date_fresh/date_dry/
# date_archive, which only live in animal_lactations, reshaped to the same
# cowid_lact / lact_group / date_fresh / date_dry / date_archive shape the
# eligibility helpers expect from a peer herd's animal_lactations.parquet.
own_lact_data <- animal_lactations %>%
  filter(lact_number > 0) %>%
  transmute(
    herd_label = own_herd_label(id_animal),
    cowid_lact = id_animal_lact,
    lact_group = case_when(
      lact_number == 1 ~ "Lact 1", lact_number == 2 ~ "Lact 2",
      lact_number >= 3 ~ "Lact 3+", TRUE ~ NA_character_
    ),
    date_fresh, date_dry, date_archive
  ) %>%
  filter(!is.na(lact_group))

own_bred_events <- events_formatted %>%
  filter(event == "BRED") %>%
  transmute(herd_label = own_herd_label(id_animal), cowid_lact = id_animal_lact, date_event, R)

own_herds <- sort(unique(own_bred_events$herd_label))

# DNB dates per cow-lactation. The full event stream is available for own
# herds (unlike the peer extract), so IR/PR can optionally drop cows from
# their first DNB event onward.
own_dnb_dates <- events_formatted %>%
  filter(toupper(event) == "DNB", !is.na(date_event)) %>%
  group_by(cowid_lact = id_animal_lact) %>%
  summarise(date_dnb = min(date_event), .groups = "drop")

# Month range and date_max_pull are per herd — pulls can be days or weeks
# apart, and sharing a calendar range would fabricate empty months.
compute_own_ir_pr <- function(dnb_dates = NULL) {
  map_dfr(own_herds, function(h) {
    bred_h <- own_bred_events %>% filter(herd_label == h)
    lact_h <- own_lact_data %>% filter(herd_label == h)
    months_h <- seq(
      floor_date(min(bred_h$date_event, na.rm = TRUE), "month"),
      floor_date(max(bred_h$date_event, na.rm = TRUE), "month"),
      by = "month"
    )
    fxn_monthly_ir_pr(lact_h, bred_h, months_h, VWP, dnb_dates = dnb_dates) %>%
      mutate(herd_label = h)
  })
}
own_ir_pr_monthly      <- compute_own_ir_pr()
own_ir_pr_monthly_nodnb <- compute_own_ir_pr(own_dnb_dates)

own_rebreed_monthly <- own_bred_events %>%
  filter(!is.na(date_event)) %>%
  mutate(month = floor_date(date_event, "month")) %>%
  group_by(herd_label, month) %>%
  summarise(n_total_bred = n(), n_rebreeds = sum(R == "R", na.rm = TRUE), .groups = "drop")

own_abortion_monthly <- own_bred_events %>%
  fxn_standardize_for_CR() %>%
  filter(Rstandard %in% c("std_Pregnant", "std_Abort")) %>%
  mutate(month = floor_date(date_event, "month")) %>%
  group_by(herd_label, month) %>%
  summarise(total_pregnancies = n(), n_abortions = sum(Rstandard == "std_Abort"), .groups = "drop")

own_date_max_pull <- max(own_bred_events$date_event, na.rm = TRUE)
own_dim_milestone <- map_dfr(own_herds, function(h) {
  bred_h <- own_bred_events %>% filter(herd_label == h)
  lact_h <- own_lact_data %>% filter(herd_label == h)
  dmp_h  <- max(bred_h$date_event, na.rm = TRUE)
  bind_rows(
    fxn_dim_milestone(lact_h, bred_h, 100, dmp_h),
    fxn_dim_milestone(lact_h, bred_h, 150, dmp_h),
    fxn_dim_milestone(lact_h, bred_h, 200, dmp_h)
  ) %>% mutate(herd_label = h)
})

# One stable color per own herd (solid lines); the peer population is always
# grey + dashed so your herds carry the color.
own_line_colors <- setNames(
  rep_len(c("#0B4568", "#C05717", "#5B2C6F", "#1D6E3F", "#8B1E3F", "#B8860B"), length(own_herds)),
  own_herds
)
line_color_scale <- scale_color_manual(
  values = c(own_line_colors, "Peer population (pooled)" = "grey45")
)
line_linetype_scale <- scale_linetype_manual(
  values = c(setNames(rep("solid", length(own_herds)), own_herds), "Peer population (pooled)" = "dashed")
)
line_fill_scale <- scale_fill_manual(
  values = c(own_line_colors, "Peer population (pooled)" = "grey45"), guide = "none"
)

# Flags each line's noisy edge months so the loess trend ignores them (they
# still draw as hollow points — nothing is silently hidden):
#   - the line's most recent trim_recent months, where outcome-dependent
#     rates read falsely low because pregnancy diagnoses lag breedings;
#   - months whose denominator is below min_frac of that line's median
#     (ramp-in at the start of an export, or a thin partial month).
# Grouping columns beyond herd_label (lact_group, milestone) come in via ...;
# time_col is the plot's x (month, or calving_month for DIM).
flag_edge_months <- function(df, trim_recent, min_frac, ..., time_col = month) {
  df %>%
    group_by(herd_label, ...) %>%
    mutate(
      keep = {{ time_col }} <= max({{ time_col }}) %m-% months(trim_recent) &
             n >= min_frac * median(n),
      keep = tidyr::replace_na(keep, FALSE)
    ) %>%
    ungroup()
}

# Drops each herd's most recent trim_recent months entirely — used for the
# "Where It Sits" ranking window so the outcome-lag months don't drag every
# herd's recent rate down (window_recent then anchors on the trimmed max).
drop_recent_months <- function(df, trim_recent, time_col = month) {
  df %>%
    group_by(herd_label) %>%
    filter({{ time_col }} <= max({{ time_col }}) %m-% months(trim_recent)) %>%
    ungroup()
}

# Limits each displayed line to the n_years ending at its most recent KEPT
# month (its last month with a real reported result). Trailing hollow months
# after that anchor still show; a line with no kept months at all drops out.
window_display <- function(df, ..., n_years = 3, time_col = month) {
  df %>%
    group_by(herd_label, ...) %>%
    mutate(.anchor = if (any(keep)) max({{ time_col }}[keep]) else as.Date(NA)) %>%
    filter(!is.na(.anchor), {{ time_col }} >= .anchor %m-% lubridate::years(n_years)) %>%
    select(-.anchor) %>%
    ungroup()
}

# =========================================================================
# Shared calculation helpers
# =========================================================================

# Keeps each id's (herd or "Monte Vista") own most recent n_months of data,
# measured from THAT id's own last month with data — herds don't share a
# data-currency date, so a shared calendar cutoff would penalize whichever
# herd's export is a few months older.
window_recent <- function(df, n_months) {
  df %>%
    group_by(herd_label) %>%
    mutate(cutoff = max(month) %m-% months(n_months - 1)) %>%
    ungroup() %>%
    filter(month >= cutoff) %>%
    select(-cutoff)
}

five_band <- function(x, breaks) {
  labels <- c("Bottom 20%", "Below Average", "Average", "Above Average", "Top 20%")
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE)
}

# Bundles the four reactives a lact_group-faceted rate view needs (trend,
# ranking, distribution) so Insemination/Conception/Pregnancy Rate don't
# each repeat the same reactive logic under a different measure name.
make_rate_views <- function(peer_df, own_df, k_col, n_col, active_groups, n_months, min_deno, selected_herds,
                            trim_lag, min_frac) {
  # own_df and peer_df are both REACTIVES so the DNB-exclusion toggle can
  # swap either side's table.
  trend_data <- reactive({
    peer_line <- peer_df() %>%
      filter(lact_group %in% active_groups()) %>%
      fxn_pool_rate(k_col, n_col, month, lact_group) %>%
      mutate(herd_label = "Peer population (pooled)")
    own_line <- own_df() %>%
      filter(herd_label %in% selected_herds(), lact_group %in% active_groups()) %>%
      fxn_pool_rate(k_col, n_col, herd_label, month, lact_group)
    bind_rows(peer_line, own_line) %>%
      filter(n > 0) %>%
      flag_edge_months(trim_recent = trim_lag(), min_frac = min_frac(), lact_group) %>%
      window_display(lact_group)
  })

  peer_ranked <- reactive({
    window_recent(
      peer_df() %>% filter(lact_group %in% active_groups()) %>% drop_recent_months(trim_lag()),
      n_months()
    ) %>%
      fxn_pool_rate(k_col, n_col, herd_label, lact_group) %>%
      filter(n > 0)
  })

  peer_kept <- reactive({
    peer_ranked() %>% filter(n >= min_deno())
  })

  own_pooled <- reactive({
    window_recent(
      own_df() %>% filter(herd_label %in% selected_herds(), lact_group %in% active_groups()) %>%
        drop_recent_months(trim_lag()),
      n_months()
    ) %>%
      fxn_pool_rate(k_col, n_col, herd_label, lact_group)
  })

  rank_summary <- reactive({
    req(nrow(own_pooled()) > 0)
    peer <- peer_kept()
    req(nrow(peer) > 0)

    peer_by_group <- peer %>%
      group_by(lact_group) %>%
      summarise(
        n_peers     = n(),
        peer_median = median(rate, na.rm = TRUE),
        lcb_list    = list(rate_lower),
        breaks      = list({
          b <- quantile(rate_lower, probs = seq(0, 1, 0.2), na.rm = TRUE, names = FALSE)
          b[1] <- -Inf; b[length(b)] <- Inf
          b
        }),
        .groups = "drop"
      )

    own_pooled() %>%
      inner_join(peer_by_group, by = "lact_group") %>%
      rowwise() %>%
      mutate(
        percentile = mean(lcb_list <= rate_lower, na.rm = TRUE),
        band       = as.character(five_band(rate_lower, breaks))
      ) %>%
      ungroup() %>%
      mutate(quality = fxn_cr_quality_label(n)) %>%
      select(herd_label, lact_group, n, rate, rate_lower, rate_upper, quality, peer_median, n_peers, percentile, band) %>%
      arrange(herd_label, lact_group)
  })

  list(trend_data = trend_data, peer_ranked = peer_ranked, peer_kept = peer_kept,
       own_pooled = own_pooled, rank_summary = rank_summary)
}

# Registers the Trend / Where It Sits outputs for one rate view under
# id_prefix (e.g. "ir_", "cr_", "pr_"). loess_span / show_points / show_se
# are reactives: the shared "Trend sensitivity" slider and the two checkboxes.
attach_rate_outputs <- function(output, id_prefix, views, measure_label, loess_span, show_points, show_se) {
  output[[paste0(id_prefix, "trend_plot")]] <- renderPlot({
    dat <- views$trend_data()
    ggplot(dat, aes(month, rate, color = herd_label, linetype = herd_label)) +
      geom_ribbon(
        data = dat %>% filter(herd_label == "Peer population (pooled)", keep),
        aes(ymin = rate_lower, ymax = rate_upper), alpha = 0.2, color = NA, fill = "grey60"
      ) +
      (if (show_points()) geom_point(aes(shape = keep), size = 1.1, alpha = 0.35)) +
      geom_smooth(
        data = dat %>% filter(keep),
        aes(fill = herd_label, weight = n), method = "loess", span = loess_span(),
        se = show_se(), alpha = 0.15, linewidth = 1
      ) +
      facet_wrap(~lact_group) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
      line_color_scale +
      line_linetype_scale +
      line_fill_scale +
      labs(
        title = paste(measure_label, "by month"),
        subtitle = "Denominator-weighted loess trends; hollow points are trimmed edge months; band is the 95% interval around the pooled peer rate",
        x = NULL, y = measure_label, color = NULL, linetype = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output[[paste0(id_prefix, "n_ranked")]] <- renderText({
    paste(n_distinct(views$peer_kept()$herd_label), "of", n_peer_herds)
  })

  output[[paste0(id_prefix, "n_excluded")]] <- renderText({
    paste(n_distinct(views$peer_ranked()$herd_label) - n_distinct(views$peer_kept()$herd_label))
  })

  output[[paste0(id_prefix, "rank_table")]] <- render_gt({
    views$rank_summary() %>%
      gt() %>%
      tab_header(
        title = sprintf("Your herds' %s vs. the peer population", measure_label),
        subtitle = "Each herd's own most recent window (trailing outcome-lag months excluded), ranked on the lower confidence bound"
      ) %>%
      cols_label(
        herd_label = "Herd", lact_group = "Lactation Group", n = "n", rate = "Rate",
        rate_lower = "Lower Bound", rate_upper = "Upper Bound", quality = "Data Volume",
        peer_median = "Peer Median", n_peers = "Peers Ranked",
        percentile = "Percentile (on lower bound)", band = "Band"
      ) %>%
      fmt_percent(columns = c(rate, rate_lower, rate_upper, peer_median, percentile), decimals = 1) %>%
      tab_options(table.border.left.style = "none", stub.border.style = "none")
  })

  output[[paste0(id_prefix, "density_plot")]] <- renderPlot({
    req(nrow(views$peer_kept()) > 0)
    own <- views$own_pooled()
    ggplot(views$peer_kept(), aes(rate)) +
      geom_histogram(binwidth = 0.02, fill = "grey60", alpha = 0.7, boundary = 0) +
      geom_vline(data = own, aes(xintercept = rate, color = herd_label), linewidth = 1) +
      facet_wrap(~lact_group, scales = "free_y") +
      scale_x_continuous(labels = percent, limits = c(0, 1)) +
      scale_color_manual(values = own_line_colors) +
      labs(
        title = paste("Peer herd", measure_label, "(each herd's own recent window)"),
        subtitle = "Vertical lines mark each of your herds over the same window",
        x = measure_label, y = "Peer herds", color = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
}

# UI for one lact_group-faceted measure: Trend | Where It Sits.
rate_nav_panel <- function(id_prefix, title) {
  nav_panel(
    title,
    navset_tab(
      nav_panel(
        "Trend",
        card(
          card_header(sprintf("Monthly %s — your herds vs. peer population", title)),
          plotOutput(paste0(id_prefix, "trend_plot"), height = "480px")
        )
      ),
      nav_panel(
        "Where It Sits",
        layout_columns(
          fill = FALSE,
          value_box("Peer herds ranked", textOutput(paste0(id_prefix, "n_ranked"), inline = TRUE)),
          value_box("Peer herds excluded (thin data)", textOutput(paste0(id_prefix, "n_excluded"), inline = TRUE))
        ),
        card(
          card_header("Your herds' position in the peer distribution"),
          gt_output(paste0(id_prefix, "rank_table"))
        ),
        card(
          card_header("Distribution of peer herds"),
          plotOutput(paste0(id_prefix, "density_plot"), height = "420px")
        ),
        helpText(
          "Bands are quintiles of the CURRENT peer population — not fixed targets.",
          "One fifth of herds is always in the bottom band, however well everyone is doing.",
          "Ranking uses the lower bound of each herd's confidence interval, not the raw",
          "percentage, so a small, noisy herd can't land in the top band by chance."
        )
      )
    )
  )
}

# =========================================================================
# UI
# =========================================================================
ui <- page_sidebar(
  title = "Your Herds vs. Parnell Peer Herds",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    title = "Settings",
    checkboxGroupInput("own_herds", "Your herds:",
                        choices = own_herds, selected = own_herds),
    checkboxGroupInput("lact_groups", "Lactation groups:",
                        choices = LACT_GROUPS, selected = LACT_GROUPS),
    numericInput("n_months", "Months of recent data used for ranking:",
                 value = 12, min = 3, max = 36, step = 1),
    numericInput("min_deno", "Exclude peer herds below this denominator in that window:",
                 value = 30, min = 0, max = 200, step = 10),
    sliderInput("loess_span", "Trend sensitivity:",
                min = 0.1, max = 1, value = 0.5, step = 0.05),
    helpText("Loess span for the trend lines — lower follows the data more closely."),
    checkboxInput("show_points", "Show monthly points", value = TRUE),
    checkboxInput("show_se", "Show trend confidence band", value = FALSE),
    checkboxInput("exclude_dnb",
                  if (have_peer_dnbx) "Exclude DNB cows (your herds + peers)"
                  else "Exclude DNB cows from my herds (peers unchanged)",
                  value = FALSE),
    if (have_peer_dnbx) {
      helpText("DNB exclusion drops each cow from IR/PR eligibility from her",
               "first DNB event onward — applied identically to your herds and",
               "every peer herd (benchmark_data_v2), so the comparison stays",
               "symmetric either way.")
    } else {
      helpText("DNB exclusion uses your herds' own DNB events for IR/PR eligibility.",
               "The peer extract has no DNB events, so the peer line keeps the",
               "proxy definition — with this on, your herds gain a definitional",
               "edge in IR/PR trends and rankings.")
    },
    numericInput("trim_recent", "Drop most recent months (outcome lag):",
                 value = 2, min = 0, max = 12, step = 1),
    numericInput("min_frac_pct", "Minimum month size (% of a line's typical denominator):",
                 value = 50, min = 0, max = 100, step = 10),
    helpText("Edge trimming: recent months read falsely low because pregnancy",
             "diagnoses lag breedings, and a line's earliest months can be thin",
             "(ramp-in). Trimmed months are excluded from the trend lines but",
             "still show as hollow points."),
    hr(),
    helpText(sprintf("Peer population: %d anonymized herds from data/parnell_files.", n_peer_herds)),
    helpText("Peer herds are identified only as Peer-001, Peer-002, etc. No herd name,",
             "farm name, address, or ID is loaded by this app."),
    helpText("Insemination Rate and Pregnancy Rate here use a proxy eligibility method",
             "(see About) — they will differ slightly from ParnellRepro's own numbers.")
  ),

  navset_card_tab(
    nav_panel(
      "Overview",
      layout_columns(
        fill = FALSE,
        value_box("Your herds", as.character(length(own_herds))),
        value_box("Peer herds", as.character(n_peer_herds)),
        value_box("Own data range", paste(format(min(own_bred_events$date_event, na.rm = TRUE), "%b %Y"),
                                           "–", format(own_date_max_pull, "%b %Y")))
      ),
      card(
        card_header("What's here"),
        markdown(paste(
          "Six reproductive measures, each showing your herds' own monthly trends against the",
          "pooled peer population, and where each herd's recent performance ranks among the",
          "451 peer herds: **Insemination Rate**, **Conception Rate**, **Pregnancy Rate**,",
          "**Rebreed Rate**, **Abortion Rate**, and **DIM Milestones** (100/150/200 days).",
          "See the **About** tab for definitions, data sources, and the method's limitations."
        ))
      )
    ),

    rate_nav_panel("ir_", "Insemination Rate"),
    rate_nav_panel("cr_", "Conception Rate"),
    rate_nav_panel("pr_", "Pregnancy Rate"),

    nav_panel(
      "Rebreeds & Abortions",
      navset_tab(
        nav_panel("Rebreed Trend", card(card_header("Monthly Rebreed Rate — your herds vs. peer population"),
                                         plotOutput("rebreed_trend_plot", height = "420px"))),
        nav_panel("Abortion Trend", card(card_header("Monthly Abortion Rate — your herds vs. peer population"),
                                          plotOutput("abortion_trend_plot", height = "420px")))
      )
    ),

    nav_panel(
      "DIM Milestones",
      card(
        card_header("Pregnancy rate at 100 / 150 / 200 DIM by calving cohort"),
        plotOutput("dim_plot", height = "480px")
      ),
      helpText("Recent calving cohorts are right-censored: cows that haven't reached the",
               "milestone yet, or whose pregnancy check hasn't been recorded, will read low.")
    ),

    nav_panel(
      "About",
      card(
        card_header("What this app shows"),
        markdown(paste(
          "**Conception Rate (CR)** = confirmed-pregnant-or-aborted breedings divided by all",
          "breedings with a usable outcome (Pregnant + Aborted + Open) — ambiguous outcome codes",
          "are excluded from both the numerator and denominator, following the standard Parnell",
          "CR definition (`fxn_standardize_for_CR` / `fxn_calculate_CR_main`, vendored offline in",
          "`functions/fxn_parnell_cr.R`). **Rebreed Rate** and **Abortion Rate** use the same R",
          "outcome codes (`R == \"R\"`, and Abort/(Pregnant+Abort)) applied to all BRED events in",
          "a month, herd-wide.",
          "",
          "**Insemination Rate** and **Pregnancy Rate** need an eligible-cow denominator —",
          "which cows COULD have been bred that month. ParnellRepro/app.R derives that from a",
          "cow's most recent OPEN/BRED/DNB *event*. The peer extract ships BRED events only, so",
          "here eligibility is derived instead from each BRED event's own **R outcome code**",
          "(P/A/O/R/E): a cow is treated as presumed pregnant from a Pregnant-coded breeding",
          "until her next BRED event of any code, or until she goes dry/is archived. She is",
          "eligible if lactating, freshened, not dry/archived, and past the voluntary waiting",
          "period (50 days) — see `functions/fxn_parnell_eligibility.R`. This proxy cannot see a",
          "DNB flag or a heat observed but not bred, so it will read slightly more permissive",
          "than ParnellRepro's own IR/PR. The same proxy is applied to Monte Vista here so both",
          "sides of the comparison use one method.",
          "",
          "**DIM Milestones** (100/150/200 days) — a calving cohort counts toward a milestone's",
          "denominator only once every cow in it could have reached that many days in milk;",
          "the numerator is a confirmed pregnancy (Pregnant/Aborted) recorded by that DIM.",
          "",
          "**Peer data.** `data/parnell_files/silver` holds one `events_bred.parquet` and one",
          "`animal_lactations.parquet` per herd across 451 herds with usable data.",
          "`build_parnell_benchmark.R` reads only those two files per herd, computes every",
          "measure above by month (and lactation group, where relevant), strips every",
          "identifying column (herd name, farm name, address, HerdKey), and relabels each herd",
          "Peer-001..Peer-451 in an order that carries no information about the herd itself.",
          "That anonymized summary — not the raw peer data — is what this app reads.",
          "",
          "**Ranking window.** Each herd (including Monte Vista) is ranked on its OWN most",
          "recent N months of data, not a shared calendar cutoff, because peer exports don't",
          "share a data-currency date."
        ))
      )
    )
  )
)

# =========================================================================
# Server
# =========================================================================
server <- function(input, output, session) {

  active_groups <- reactive({
    req(length(input$lact_groups) > 0)
    input$lact_groups
  })
  n_months <- reactive({ req(!is.na(input$n_months)); input$n_months })
  min_deno <- reactive({ req(!is.na(input$min_deno)); input$min_deno })
  selected_herds <- reactive({
    req(length(input$own_herds) > 0)
    input$own_herds
  })
  loess_span  <- reactive({ req(!is.na(input$loess_span)); input$loess_span })
  show_points <- reactive({ isTRUE(input$show_points) })
  show_se     <- reactive({ isTRUE(input$show_se) })
  min_frac    <- reactive({ req(!is.na(input$min_frac_pct)); input$min_frac_pct / 100 })
  # Outcome-dependent measures (CR/PR/abortion) wait on a pregnancy diagnosis,
  # so they get the full trim; breeding-based measures (IR/rebreed) are known
  # at the breeding itself, so only the trailing partial month is suspect.
  trim_outcome <- reactive({ req(!is.na(input$trim_recent)); as.integer(input$trim_recent) })
  trim_bred    <- reactive({ max(0L, trim_outcome() - 1L) })
  own_ir_pr_active <- reactive({
    if (isTRUE(input$exclude_dnb)) own_ir_pr_monthly_nodnb else own_ir_pr_monthly
  })
  peer_ir_pr_active <- reactive({
    if (isTRUE(input$exclude_dnb) && have_peer_dnbx) peer_ir_pr_dnbx else peer_ir_pr
  })

  ir_views <- make_rate_views(peer_ir_pr_active, own_ir_pr_active, "n_bred", "n_eligible", active_groups, n_months, min_deno, selected_herds, trim_bred, min_frac)
  cr_views <- make_rate_views(reactive(peer_cr), reactive(own_cr_monthly), "preg", "deno", active_groups, n_months, min_deno, selected_herds, trim_outcome, min_frac)
  pr_views <- make_rate_views(peer_ir_pr_active, own_ir_pr_active, "n_pregnant", "n_eligible", active_groups, n_months, min_deno, selected_herds, trim_outcome, min_frac)

  attach_rate_outputs(output, "ir_", ir_views, "Insemination Rate", loess_span, show_points, show_se)
  attach_rate_outputs(output, "cr_", cr_views, "Conception Rate", loess_span, show_points, show_se)
  attach_rate_outputs(output, "pr_", pr_views, "Pregnancy Rate", loess_span, show_points, show_se)

  # -- Rebreeds & Abortions: herd-wide trend only, no lact_group facet -----
  rebreed_trend_data <- reactive({
    bind_rows(
      peer_rebreed %>% fxn_pool_rate("n_rebreeds", "n_total_bred", month) %>% mutate(herd_label = "Peer population (pooled)"),
      own_rebreed_monthly %>% filter(herd_label %in% selected_herds()) %>%
        fxn_pool_rate("n_rebreeds", "n_total_bred", herd_label, month)
    ) %>%
      filter(n > 0) %>%
      flag_edge_months(trim_recent = trim_bred(), min_frac = min_frac()) %>%
      window_display()
  })

  output$rebreed_trend_plot <- renderPlot({
    dat <- rebreed_trend_data()
    ggplot(dat, aes(month, rate, color = herd_label, linetype = herd_label)) +
      geom_ribbon(
        data = dat %>% filter(herd_label == "Peer population (pooled)", keep),
        aes(ymin = rate_lower, ymax = rate_upper), alpha = 0.2, color = NA, fill = "grey60"
      ) +
      (if (show_points()) geom_point(aes(shape = keep), size = 1.1, alpha = 0.35)) +
      geom_smooth(data = dat %>% filter(keep),
                  aes(fill = herd_label, weight = n), method = "loess", span = loess_span(),
                  se = show_se(), alpha = 0.15, linewidth = 1) +
      scale_y_continuous(labels = percent) +
      scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
      line_color_scale +
      line_linetype_scale +
      line_fill_scale +
      labs(title = "Rebreed Rate by month",
           subtitle = "Denominator-weighted loess trends; hollow points are trimmed edge months",
           x = NULL, y = "Rebreed Rate", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  })

  abortion_trend_data <- reactive({
    bind_rows(
      peer_abortion %>% fxn_pool_rate("n_abortions", "total_pregnancies", month) %>% mutate(herd_label = "Peer population (pooled)"),
      own_abortion_monthly %>% filter(herd_label %in% selected_herds()) %>%
        fxn_pool_rate("n_abortions", "total_pregnancies", herd_label, month)
    ) %>%
      filter(n > 0) %>%
      flag_edge_months(trim_recent = trim_outcome(), min_frac = min_frac()) %>%
      window_display()
  })

  output$abortion_trend_plot <- renderPlot({
    dat <- abortion_trend_data()
    ggplot(dat, aes(month, rate, color = herd_label, linetype = herd_label)) +
      geom_ribbon(
        data = dat %>% filter(herd_label == "Peer population (pooled)", keep),
        aes(ymin = rate_lower, ymax = rate_upper), alpha = 0.2, color = NA, fill = "grey60"
      ) +
      (if (show_points()) geom_point(aes(shape = keep), size = 1.1, alpha = 0.35)) +
      geom_smooth(data = dat %>% filter(keep),
                  aes(fill = herd_label, weight = n), method = "loess", span = loess_span(),
                  se = show_se(), alpha = 0.15, linewidth = 1) +
      scale_y_continuous(labels = percent) +
      scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
      line_color_scale +
      line_linetype_scale +
      line_fill_scale +
      labs(title = "Abortion Rate by breeding cohort",
           subtitle = "Denominator-weighted loess trends; hollow points are trimmed edge months",
           x = NULL, y = "Abortion Rate", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  })

  # -- DIM Milestones --------------------------------------------------------
  dim_trend_data <- reactive({
    bind_rows(
      peer_dim %>% fxn_pool_rate("pregnant_count", "total_cows", calving_month, milestone) %>% mutate(herd_label = "Peer population (pooled)"),
      own_dim_milestone %>% filter(herd_label %in% selected_herds()) %>%
        fxn_pool_rate("pregnant_count", "total_cows", herd_label, calving_month, milestone)
    ) %>%
      mutate(milestone = factor(milestone, levels = c("100 DIM", "150 DIM", "200 DIM"))) %>%
      filter(n > 0) %>%
      # fxn_dim_milestone already trims right-censored cohorts, so no
      # trailing trim here — just the thin-denominator floor.
      flag_edge_months(trim_recent = 0L, min_frac = min_frac(), milestone, time_col = calving_month) %>%
      window_display(milestone, time_col = calving_month)
  })

  output$dim_plot <- renderPlot({
    dat <- dim_trend_data()
    ggplot(dat, aes(calving_month, rate, color = herd_label, linetype = herd_label)) +
      geom_hline(yintercept = 0.50, linetype = "dashed", color = "steelblue", linewidth = 0.5, alpha = 0.6) +
      geom_hline(yintercept = 0.75, linetype = "dashed", color = "darkgreen", linewidth = 0.5, alpha = 0.6) +
      geom_hline(yintercept = 0.90, linetype = "dashed", color = "purple4", linewidth = 0.5, alpha = 0.6) +
      (if (show_points()) geom_point(aes(shape = keep), size = 1.0, alpha = 0.35)) +
      geom_smooth(data = dat %>% filter(keep),
                  aes(fill = herd_label, weight = n), method = "loess", span = loess_span(),
                  se = show_se(), alpha = 0.15, linewidth = 1) +
      facet_wrap(~milestone) +
      scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
      line_color_scale +
      line_linetype_scale +
      line_fill_scale +
      labs(
        title = "Pregnancy rate at 100 / 150 / 200 DIM by calving cohort",
        subtitle = "Loess trends (see Trend sensitivity); horizontal dashed lines are target benchmarks (50% / 75% / 90%)",
        x = NULL, y = "Pregnant by milestone", color = NULL, linetype = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
