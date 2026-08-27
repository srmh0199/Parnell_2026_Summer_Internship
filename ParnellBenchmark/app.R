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
peer_path <- here::here("data", "parnell_files", "benchmark_data.rds")
if (!file.exists(peer_path)) {
  stop(
    "Missing ", peer_path,
    ".\nRun build_parnell_benchmark.R from the project root first."
  )
}
benchmark_data <- readRDS(peer_path)
peer_cr       <- benchmark_data$cr_monthly
peer_ir_pr    <- benchmark_data$ir_pr_monthly
peer_rebreed  <- benchmark_data$rebreed_monthly
peer_abortion <- benchmark_data$abortion_monthly
peer_dim      <- benchmark_data$dim_milestone

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

# events_formatted already carries its own lact_group ("Heifer"/"LACT 1"/
# "LACT 2"/"LACT 3+"), so Conception Rate needs no join to animal_lactations.
own_lact_map <- c("LACT 1" = "Lact 1", "LACT 2" = "Lact 2", "LACT 3+" = "Lact 3+")

own_cr_monthly <- events_formatted %>%
  filter(event == "BRED", lact_group %in% names(own_lact_map)) %>%
  mutate(
    lact_group = unname(own_lact_map[lact_group]),
    month      = floor_date(date_event, "month")
  ) %>%
  fxn_standardize_for_CR() %>%
  group_by(month, lact_group, Rstandard) %>%
  fxn_calculate_CR_main() %>%
  ungroup() %>%
  select(month, lact_group, std_Open, std_Pregnant, std_Abort, std_Other, deno, preg, other, total) %>%
  mutate(herd_label = "Monte Vista")

# IR / PR / Rebreed / Abortion / DIM milestones need date_fresh/date_dry/
# date_archive, which only live in animal_lactations, reshaped to the same
# cowid_lact / lact_group / date_fresh / date_dry / date_archive shape the
# eligibility helpers expect from a peer herd's animal_lactations.parquet.
own_lact_data <- animal_lactations %>%
  filter(lact_number > 0) %>%
  transmute(
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
  transmute(cowid_lact = id_animal_lact, date_event, R)

own_months_vec <- seq(
  floor_date(min(own_bred_events$date_event, na.rm = TRUE), "month"),
  floor_date(max(own_bred_events$date_event, na.rm = TRUE), "month"),
  by = "month"
)

own_ir_pr_monthly <- fxn_monthly_ir_pr(own_lact_data, own_bred_events, own_months_vec, VWP) %>%
  mutate(herd_label = "Monte Vista")

own_rebreed_monthly <- own_bred_events %>%
  filter(!is.na(date_event)) %>%
  mutate(month = floor_date(date_event, "month")) %>%
  group_by(month) %>%
  summarise(n_total_bred = n(), n_rebreeds = sum(R == "R", na.rm = TRUE), .groups = "drop") %>%
  mutate(herd_label = "Monte Vista")

own_abortion_monthly <- own_bred_events %>%
  fxn_standardize_for_CR() %>%
  filter(Rstandard %in% c("std_Pregnant", "std_Abort")) %>%
  mutate(month = floor_date(date_event, "month")) %>%
  group_by(month) %>%
  summarise(total_pregnancies = n(), n_abortions = sum(Rstandard == "std_Abort"), .groups = "drop") %>%
  mutate(herd_label = "Monte Vista")

own_date_max_pull <- max(own_bred_events$date_event, na.rm = TRUE)
own_dim_milestone <- bind_rows(
  fxn_dim_milestone(own_lact_data, own_bred_events, 100, own_date_max_pull),
  fxn_dim_milestone(own_lact_data, own_bred_events, 150, own_date_max_pull),
  fxn_dim_milestone(own_lact_data, own_bred_events, 200, own_date_max_pull)
) %>%
  mutate(herd_label = "Monte Vista")

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
make_rate_views <- function(peer_df, own_df, k_col, n_col, active_groups, n_months, min_deno) {
  trend_data <- reactive({
    peer_line <- peer_df %>%
      filter(lact_group %in% active_groups()) %>%
      fxn_pool_rate(k_col, n_col, month, lact_group) %>%
      mutate(herd_label = "Peer population (pooled)")
    own_line <- own_df %>%
      filter(lact_group %in% active_groups()) %>%
      fxn_pool_rate(k_col, n_col, month, lact_group) %>%
      mutate(herd_label = "Monte Vista")
    bind_rows(peer_line, own_line)
  })

  peer_ranked <- reactive({
    window_recent(peer_df %>% filter(lact_group %in% active_groups()), n_months()) %>%
      fxn_pool_rate(k_col, n_col, herd_label, lact_group) %>%
      filter(n > 0)
  })

  peer_kept <- reactive({
    peer_ranked() %>% filter(n >= min_deno())
  })

  own_pooled <- reactive({
    window_recent(own_df %>% filter(lact_group %in% active_groups()), n_months()) %>%
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
      select(lact_group, n, rate, rate_lower, rate_upper, quality, peer_median, n_peers, percentile, band)
  })

  list(trend_data = trend_data, peer_ranked = peer_ranked, peer_kept = peer_kept,
       own_pooled = own_pooled, rank_summary = rank_summary)
}

# Registers the Trend / Where It Sits outputs for one rate view under
# id_prefix (e.g. "ir_", "cr_", "pr_").
attach_rate_outputs <- function(output, id_prefix, views, measure_label) {
  output[[paste0(id_prefix, "trend_plot")]] <- renderPlot({
    views$trend_data() %>%
      filter(n > 0) %>%
      ggplot(aes(month, rate, color = herd_label, linetype = herd_label)) +
      geom_ribbon(
        data = . %>% filter(herd_label == "Peer population (pooled)"),
        aes(ymin = rate_lower, ymax = rate_upper), alpha = 0.15, color = NA, fill = "steelblue"
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.3) +
      facet_wrap(~lact_group) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_color_manual(values = c("Monte Vista" = "#0B4568", "Peer population (pooled)" = "steelblue")) +
      scale_linetype_manual(values = c("Monte Vista" = "solid", "Peer population (pooled)" = "dashed")) +
      labs(
        title = paste(measure_label, "by month"),
        subtitle = "Shaded band is the 95% interval around the pooled peer rate that month",
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
        title = sprintf("Monte Vista's %s vs. the peer population", measure_label),
        subtitle = "Each herd's own most recent window, ranked on the lower confidence bound"
      ) %>%
      cols_label(
        lact_group = "Lactation Group", n = "n", rate = "Monte Vista",
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
      geom_histogram(binwidth = 0.02, fill = "steelblue", alpha = 0.7, boundary = 0) +
      geom_vline(data = own, aes(xintercept = rate), color = "#EF3E33", linewidth = 1) +
      facet_wrap(~lact_group, scales = "free_y") +
      scale_x_continuous(labels = percent, limits = c(0, 1)) +
      labs(
        title = paste("Peer herd", measure_label, "(each herd's own recent window)"),
        subtitle = "Red line marks Monte Vista's rate over the same window",
        x = measure_label, y = "Peer herds"
      ) +
      theme_minimal(base_size = 12)
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
          card_header(sprintf("Monthly %s — Monte Vista vs. peer population", title)),
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
          card_header("Monte Vista's position in the peer distribution"),
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
  title = "Monte Vista vs. Parnell Peer Herds",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    title = "Settings",
    checkboxGroupInput("lact_groups", "Lactation groups:",
                        choices = LACT_GROUPS, selected = LACT_GROUPS),
    numericInput("n_months", "Months of recent data used for ranking:",
                 value = 12, min = 3, max = 36, step = 1),
    numericInput("min_deno", "Exclude peer herds below this denominator in that window:",
                 value = 30, min = 0, max = 200, step = 10),
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
        value_box("Farm", "Monte Vista"),
        value_box("Peer herds", as.character(n_peer_herds)),
        value_box("Own data range", paste(format(min(own_bred_events$date_event, na.rm = TRUE), "%b %Y"),
                                           "–", format(own_date_max_pull, "%b %Y")))
      ),
      card(
        card_header("What's here"),
        markdown(paste(
          "Six reproductive measures, each showing Monte Vista's own monthly trend against the",
          "pooled peer population, and where Monte Vista's recent performance ranks among the",
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
        nav_panel("Rebreed Trend", card(card_header("Monthly Rebreed Rate — Monte Vista vs. peer population"),
                                         plotOutput("rebreed_trend_plot", height = "420px"))),
        nav_panel("Abortion Trend", card(card_header("Monthly Abortion Rate — Monte Vista vs. peer population"),
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

  ir_views <- make_rate_views(peer_ir_pr, own_ir_pr_monthly, "n_bred", "n_eligible", active_groups, n_months, min_deno)
  cr_views <- make_rate_views(peer_cr,    own_cr_monthly,    "preg",   "deno",       active_groups, n_months, min_deno)
  pr_views <- make_rate_views(peer_ir_pr, own_ir_pr_monthly, "n_pregnant", "n_eligible", active_groups, n_months, min_deno)

  attach_rate_outputs(output, "ir_", ir_views, "Insemination Rate")
  attach_rate_outputs(output, "cr_", cr_views, "Conception Rate")
  attach_rate_outputs(output, "pr_", pr_views, "Pregnancy Rate")

  # -- Rebreeds & Abortions: herd-wide trend only, no lact_group facet -----
  rebreed_trend_data <- reactive({
    bind_rows(
      peer_rebreed %>% fxn_pool_rate("n_rebreeds", "n_total_bred", month) %>% mutate(herd_label = "Peer population (pooled)"),
      own_rebreed_monthly %>% fxn_pool_rate("n_rebreeds", "n_total_bred", month) %>% mutate(herd_label = "Monte Vista")
    )
  })

  output$rebreed_trend_plot <- renderPlot({
    rebreed_trend_data() %>%
      filter(n > 0) %>%
      ggplot(aes(month, rate, color = herd_label, linetype = herd_label)) +
      geom_ribbon(
        data = . %>% filter(herd_label == "Peer population (pooled)"),
        aes(ymin = rate_lower, ymax = rate_upper), alpha = 0.15, color = NA, fill = "steelblue"
      ) +
      geom_line(linewidth = 1) + geom_point(size = 1.2) +
      scale_y_continuous(labels = percent) +
      scale_color_manual(values = c("Monte Vista" = "#0B4568", "Peer population (pooled)" = "steelblue")) +
      scale_linetype_manual(values = c("Monte Vista" = "solid", "Peer population (pooled)" = "dashed")) +
      labs(title = "Rebreed Rate by month", x = NULL, y = "Rebreed Rate", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  })

  abortion_trend_data <- reactive({
    bind_rows(
      peer_abortion %>% fxn_pool_rate("n_abortions", "total_pregnancies", month) %>% mutate(herd_label = "Peer population (pooled)"),
      own_abortion_monthly %>% fxn_pool_rate("n_abortions", "total_pregnancies", month) %>% mutate(herd_label = "Monte Vista")
    )
  })

  output$abortion_trend_plot <- renderPlot({
    abortion_trend_data() %>%
      filter(n > 0) %>%
      ggplot(aes(month, rate, color = herd_label, linetype = herd_label)) +
      geom_ribbon(
        data = . %>% filter(herd_label == "Peer population (pooled)"),
        aes(ymin = rate_lower, ymax = rate_upper), alpha = 0.15, color = NA, fill = "steelblue"
      ) +
      geom_line(linewidth = 1) + geom_point(size = 1.2) +
      scale_y_continuous(labels = percent) +
      scale_color_manual(values = c("Monte Vista" = "#0B4568", "Peer population (pooled)" = "steelblue")) +
      scale_linetype_manual(values = c("Monte Vista" = "solid", "Peer population (pooled)" = "dashed")) +
      labs(title = "Abortion Rate by breeding cohort", x = NULL, y = "Abortion Rate", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  })

  # -- DIM Milestones --------------------------------------------------------
  dim_trend_data <- reactive({
    bind_rows(
      peer_dim %>% fxn_pool_rate("pregnant_count", "total_cows", calving_month, milestone) %>% mutate(herd_label = "Peer population (pooled)"),
      own_dim_milestone %>% fxn_pool_rate("pregnant_count", "total_cows", calving_month, milestone) %>% mutate(herd_label = "Monte Vista")
    ) %>%
      mutate(milestone = factor(milestone, levels = c("100 DIM", "150 DIM", "200 DIM")))
  })

  output$dim_plot <- renderPlot({
    dim_trend_data() %>%
      filter(n > 0) %>%
      ggplot(aes(calving_month, rate, color = herd_label, linetype = herd_label)) +
      geom_hline(yintercept = 0.50, linetype = "dashed", color = "steelblue", linewidth = 0.5, alpha = 0.6) +
      geom_hline(yintercept = 0.75, linetype = "dashed", color = "darkgreen", linewidth = 0.5, alpha = 0.6) +
      geom_hline(yintercept = 0.90, linetype = "dashed", color = "purple4", linewidth = 0.5, alpha = 0.6) +
      geom_line(linewidth = 1) + geom_point(size = 1.1) +
      facet_wrap(~milestone) +
      scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_color_manual(values = c("Monte Vista" = "#0B4568", "Peer population (pooled)" = "steelblue")) +
      scale_linetype_manual(values = c("Monte Vista" = "solid", "Peer population (pooled)" = "dashed")) +
      labs(
        title = "Pregnancy rate at 100 / 150 / 200 DIM by calving cohort",
        subtitle = "Dashed lines are target benchmarks (50% / 75% / 90%)",
        x = NULL, y = "Pregnant by milestone", color = NULL, linetype = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
