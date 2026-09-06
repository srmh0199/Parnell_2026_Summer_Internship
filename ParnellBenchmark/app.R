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

# Resolve a project-relative path: an app-local copy first (the deployed
# shinyapps.io bundle is the app folder alone, with functions/ and data/
# synced in by deploy_shinyapps.R), else the repo root for development.
proj_file <- function(...) {
  local <- file.path(...)
  if (file.exists(local)) local else here::here(...)
}

source(proj_file("functions", "fxn_parnell_cr.R"))
source(proj_file("functions", "fxn_parnell_eligibility.R"))

VWP <- 50L  # matches ParnellRepro/app.R's default voluntary waiting period
LACT_GROUPS <- c("Lact 1", "Lact 2", "Lact 3+")

# ---- Load the precomputed, anonymized peer summary -----------------------
# Prefer the v2 build (build_parnell_benchmark_v2.R: complete no-QC-filter
# breedings, plus a DNB-excluded IR/PR variant per peer herd so the DNB
# toggle can be symmetric). Fall back to Sarah's v1 file otherwise.
peer_path_v2 <- proj_file("data", "parnell_files", "benchmark_data_v2.rds")
peer_path_v1 <- proj_file("data", "parnell_files", "benchmark_data.rds")
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
peer_built      <- format(file.mtime(peer_path), "%b %d, %Y")

n_peer_herds <- n_distinct(peer_cr$herd_label)

# ---- Load the own-herd side ----------------------------------------------
# Preferred: the small precomputed aggregate file written by
# build_own_benchmark.R (monthly counts only — no cow-level data, so a
# deployment needs neither event_files nor intermediate_files). Fallback for
# development: compute live from the intermediate parquet (slow, ~90 s).
source(proj_file("functions", "fxn_own_herd_measures.R"))

own_path <- proj_file("data", "parnell_files", "own_data.rds")
if (file.exists(own_path)) {
  message("ParnellBenchmark: loading precomputed own-herd data (", own_path, ")")
  own_data <- readRDS(own_path)
} else {
  data_dir  <- here::here("data", "intermediate_files")
  req_files <- c("events_formatted.parquet", "animal_lactations.parquet")
  missing   <- req_files[!file.exists(file.path(data_dir, req_files))]
  if (length(missing) > 0) {
    stop(
      "Missing ", own_path, " AND the fallback input(s) in ", data_dir, ": ",
      paste(missing, collapse = ", "),
      ".\nRun build_own_benchmark.R (preferred) or steps 0-2 from the project root first."
    )
  }
  message("ParnellBenchmark: own_data.rds not found - computing own-herd measures live (slow); run build_own_benchmark.R to precompute")
  own_data <- fxn_compute_own_measures(
    read_parquet(file.path(data_dir, "events_formatted.parquet")),
    read_parquet(file.path(data_dir, "animal_lactations.parquet")),
    vwp = VWP
  )
}

own_cr_monthly          <- own_data$cr_monthly
own_ir_pr_monthly       <- own_data$ir_pr_monthly
own_ir_pr_monthly_nodnb <- own_data$ir_pr_monthly_dnbx
own_rebreed_monthly     <- own_data$rebreed_monthly
own_abortion_monthly    <- own_data$abortion_monthly
own_dim_milestone       <- own_data$dim_milestone
own_meta                <- own_data$meta
own_herds               <- sort(unique(own_ir_pr_monthly$herd_label))
own_date_max_pull       <- max(own_meta$date_max_event)

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
          value_box("Peer herds ranked", textOutput(paste0(id_prefix, "n_ranked"), inline = TRUE), height = "120px"),
          value_box("Peer herds excluded (thin data)", textOutput(paste0(id_prefix, "n_excluded"), inline = TRUE), height = "120px")
        ),
        card(
          fill = FALSE,
          card_header("Your herds' position in the peer distribution"),
          gt_output(paste0(id_prefix, "rank_table"))
        ),
        card(
          fill = FALSE,
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
    tags$h6("Herds & lactation groups"),
    checkboxGroupInput("own_herds", NULL,
                        choices = own_herds, selected = own_herds),
    checkboxGroupInput("lact_groups", NULL,
                        choices = LACT_GROUPS, selected = LACT_GROUPS),
    hr(),
    tags$h6("Trend display"),
    sliderInput("loess_span", "Trend sensitivity:",
                min = 0.1, max = 1, value = 0.5, step = 0.05),
    helpText("Loess span — lower follows the data more closely."),
    checkboxInput("show_points", "Show monthly points", value = TRUE),
    checkboxInput("show_se", "Show trend confidence band", value = FALSE),
    checkboxInput("exclude_dnb",
                  if (have_peer_dnbx) "Exclude DNB cows (your herds + peers)"
                  else "Exclude DNB cows from my herds (peers unchanged)",
                  value = FALSE),
    if (have_peer_dnbx) {
      helpText("Drops each cow from IR/PR eligibility from her first DNB event",
               "onward — applied identically to your herds and every peer herd,",
               "so the comparison stays symmetric either way.")
    } else {
      helpText("Uses your herds' own DNB events; the peer extract has none, so",
               "with this on your herds gain a definitional edge in IR/PR.")
    },
    hr(),
    tags$h6("Ranking window"),
    numericInput("n_months", "Months of recent data used for ranking:",
                 value = 12, min = 3, max = 36, step = 1),
    numericInput("min_deno", "Exclude peer herds below this denominator in that window:",
                 value = 30, min = 0, max = 200, step = 10),
    hr(),
    tags$h6("Edge trimming"),
    numericInput("trim_recent", "Drop most recent months (outcome lag):",
                 value = 2, min = 0, max = 12, step = 1),
    numericInput("min_frac_pct", "Minimum month size (% of a line's typical denominator):",
                 value = 50, min = 0, max = 100, step = 10),
    helpText("Recent months read falsely low (pregnancy diagnoses lag breedings)",
             "and a line's earliest months can be thin (ramp-in). Trimmed months",
             "leave the trend lines but still show as hollow points."),
    hr(),
    helpText(sprintf("Peer population: %d anonymized herds (Peer-001…). No herd name,", n_peer_herds),
             "farm name, address, or ID is loaded by this app.")
  ),

  navset_card_tab(
    nav_panel(
      "Overview",
      layout_columns(
        fill = FALSE,
        value_box("Your herds", as.character(length(own_herds)), height = "140px"),
        value_box("Peer herds", as.character(n_peer_herds), height = "140px"),
        value_box("Own data through", format(own_date_max_pull, "%b %d, %Y"), height = "140px"),
        value_box("Peer file built", peer_built, height = "140px")
      ),
      card(
        fill = FALSE,
        card_header("Data currency by herd"),
        gt_output("meta_table")
      ),
      card(
        fill = FALSE,
        card_header("What's here"),
        markdown(paste(
          sprintf("Six reproductive measures, each showing your herds' own monthly trends against the"),
          sprintf("pooled peer population, and where each herd's recent performance ranks among the"),
          sprintf("%d peer herds: **Insemination Rate**, **Conception Rate**, **Pregnancy Rate**,", n_peer_herds),
          "**Rebreed Rate**, **Abortion Rate**, and **DIM Milestones** (100/150/200 days).",
          "Trend lines are denominator-weighted loess fits with noisy edge months trimmed",
          "(shown hollow); the DNB toggle switches IR/PR to a do-not-breed-aware eligibility",
          "on both sides. See the **About** tab for definitions, data sources, and limitations."
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
      accordion(
        open = "The six measures",

        accordion_panel(
          "The six measures",
          markdown(paste(
            "- **Conception rate (CR)** — pregnant-or-aborted breedings ÷ breedings with a",
            "  usable outcome (Pregnant + Aborted + Open). Ambiguous outcome codes are excluded",
            "  from both sides — the standard Parnell CR definition (`functions/fxn_parnell_cr.R`).",
            "\n- **Insemination rate (IR)** — eligible cows bred that month ÷ eligible cows.",
            "\n- **Pregnancy rate (PR)** — eligible cows confirmed pregnant that month ÷ eligible cows.",
            "\n- **Rebreed rate** — breedings coded `R` ÷ all breedings, monthly, herd-wide.",
            "\n- **Abortion rate** — aborted ÷ (pregnant + aborted), grouped by the month the cow",
            "  was **bred** (a cohort, not the calendar month of the abortion).",
            "\n- **DIM milestones** — share of a calving cohort confirmed pregnant by 100 / 150 /",
            "  200 days in milk."
          ))
        ),

        accordion_panel(
          "Who counts as eligible (IR and PR)",
          markdown(paste(
            "Eligibility is derived from each BRED event's own **R outcome code** (P/A/O/R/E),",
            "so the same rule can run on the peer extract, which has no OPEN events.",
            "",
            "A cow is **eligible** in a month when she is:",
            "",
            "- lactating and freshened before the month's end,",
            "- not dry and not archived,",
            "- past the 50-day voluntary waiting period,",
            "- not presumed pregnant — a Pregnant-coded breeding marks her pregnant until her",
            "  next BRED event of any code.",
            "",
            "Details: `functions/fxn_parnell_eligibility.R`."
          ))
        ),

        accordion_panel(
          "The DNB toggle",
          markdown(paste(
            "**Exclude DNB cows** additionally drops each cow from eligibility from her first",
            "**do-not-breed** event onward — applied **identically to your herds and every peer",
            "herd**, so the comparison stays symmetric with the toggle on or off.",
            "",
            "Why it exists: planned culls kept milking sit in the denominator never getting",
            "bred, which reads as a falsely low IR/PR. The effect is largest for **Lact 3+**."
          ))
        ),

        accordion_panel(
          "Trend lines and edge trimming",
          markdown(paste(
            "Trend lines are **denominator-weighted loess fits**; the *Trend sensitivity*",
            "slider is the loess span (lower = follows the data more closely).",
            "",
            "Noisy edge months are left out of the fit but still drawn as **hollow points**:",
            "",
            "- the most recent months, where pregnancy diagnoses haven't caught up with",
            "  breedings yet (*Drop most recent months*),",
            "- months thinner than a set share of that line's typical denominator, e.g. the",
            "  ramp-in at the start of an export (*Minimum month size*).",
            "",
            "Each line shows the **3 years** ending at its last kept month."
          ))
        ),

        accordion_panel(
          "How ranking works (Where It Sits)",
          markdown(paste(
            "Each herd — yours and every peer — is ranked on its **own most recent N months**",
            "(*Months of recent data*), after dropping its trailing outcome-lag months; exports",
            "don't share a data-currency date, so a shared calendar cutoff would be unfair.",
            "",
            "- Herds below the denominator floor for the window are excluded (*Exclude peer",
            "  herds below…*).",
            "- Ranking uses the **lower bound of each herd's confidence interval**, not the raw",
            "  rate, so a small noisy herd can't top the table by chance.",
            "- Bands are **quintiles of the current peer population** — one fifth of herds is",
            "  always in the bottom band, however well everyone is doing."
          ))
        ),

        accordion_panel(
          "Peer data and anonymization",
          markdown(paste(
            "Peer data is a precomputed summary (`data/parnell_files/benchmark_data_v2.rds`)",
            "built by `build_parnell_benchmark_v2.R` from the Parnell silver layer:",
            "",
            "- each herd's **complete breeding stream** (`events_bred_no_qc_filter.parquet`),",
            "  lactation table, and DNB dates from its full event file,",
            "- every measure computed per herd, IR/PR under **both** definitions,",
            "- every identifying column stripped (assertion-checked at build time), herds",
            "  relabeled **Peer-001…** in an order that carries no information.",
            "",
            "The app only ever reads that anonymized summary — never raw peer data."
          ))
        ),

        accordion_panel(
          "Own-herd data and refreshing",
          markdown(paste(
            "Your herds are precomputed the same way by `build_own_benchmark.R` into",
            "`data/parnell_files/own_data.rds` — monthly aggregates per herd, labeled by",
            "herd-key prefix. The running app needs **no cow-level data**.",
            "",
            "To refresh after a new export: run the pipeline (step 0), then",
            "`Rscript build_own_benchmark.R`. The Overview tab shows each herd's data currency."
          ))
        )
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

  output$meta_table <- render_gt({
    own_meta %>%
      gt() %>%
      cols_label(herd_label = "Herd", date_min_event = "First event", date_max_event = "Last event") %>%
      fmt_date(columns = c(date_min_event, date_max_event), date_style = "yMMMd") %>%
      tab_options(table.border.left.style = "none")
  })

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
