# ParnellRepro — Dairy Reproductive Performance dashboard
# =========================================================================
# Shiny version of parnell_repro.qmd. It reads the same three parquet files
# and reproduces the report's KPIs (insemination rate, conception rate,
# pregnancy rate, rebreeds, abortions, and % pregnant by 100/150/200 DIM),
# adding interactive controls for the voluntary waiting period, the number
# of 21-day periods, which lactation groups to plot, and how many recent
# (pregnancy-diagnosis-lagged) periods to exclude from the trend graphs.
#
# The calculation logic is ported verbatim from parnell_repro.qmd so the
# numbers match the report. Unlike the report it does NOT source Gerard's
# GitHub "os" functions or compute the sarah/monthly denominators, because
# none of those feed the visible outputs — dropping them lets the app run
# offline. Run step0_master_processing_my_data.R first to build the parquet.

library(shiny)
library(bslib)
library(tidyverse)
library(arrow)
library(gt)
library(lubridate)
library(scales)
library(here)

# ---- Load data once at startup ------------------------------------------
# here::here() resolves to the project root (the folder holding the .Rproj),
# so these paths work even though the app lives in the ParnellRepro/ subfolder.
data_dir  <- here::here("data", "intermediate_files")
req_files <- c("events_formatted.parquet", "animals.parquet", "animal_lactations.parquet")
missing   <- req_files[!file.exists(file.path(data_dir, req_files))]
if (length(missing) > 0) {
  stop(
    "Missing data file(s) in ", data_dir, ": ", paste(missing, collapse = ", "),
    ".\nRun step0_master_processing_my_data.R from the project root first."
  )
}

events_formatted  <- read_parquet(file.path(data_dir, "events_formatted.parquet"))
animals           <- read_parquet(file.path(data_dir, "animals.parquet"))
animal_lactations <- read_parquet(file.path(data_dir, "animal_lactations.parquet"))

date_max_pull <- max(events_formatted$date_event, na.rm = TRUE)
date_min_pull <- min(events_formatted$date_event, na.rm = TRUE)

# ---- Pieces that never change with the inputs ---------------------------
herd_intervals <- animal_lactations |>
  select(id_animal_lact, date_fresh, date_archive, date_dry, lact_number) |>
  mutate(
    lact_group = case_when(
      lact_number == 1 ~ "Lact 1",
      lact_number == 2 ~ "Lact 2",
      lact_number >= 3 ~ "Lact 3+",
      TRUE ~ "Other"
    )
  )

cow_status <- events_formatted |>
  filter(event %in% c("BRED", "OPEN", "DNB")) |>
  group_by(id_animal_lact) |>
  summarise(
    last_bred = max(date_event[event == "BRED"], na.rm = TRUE),
    last_open = max(date_event[event == "OPEN"], na.rm = TRUE),
    last_dnb  = max(date_event[event == "DNB"],  na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(c(last_bred, last_open, last_dnb), ~ if_else(is.infinite(.x), as.Date(NA), .x)))

LACT_GROUPS <- c("Lact 1", "Lact 2", "Lact 3+")

# Drop the k most recent periods from a period-keyed data frame. This
# reproduces the report's `period_start < sort(desc)[2]` (k = 2) rule, which
# hides the newest cows whose pregnancy diagnoses have not landed yet.
drop_recent_periods <- function(df, k) {
  if (is.null(k) || k <= 0) return(df)
  ps <- sort(unique(df$period_start), decreasing = TRUE)
  if (length(ps) <= k) return(df[0, ])
  df |> filter(period_start < ps[k])
}

# =========================================================================
# Static KPIs — these do not depend on any input, so compute them once.
# =========================================================================

# Monthly rebreeding (R) rate
rebreed_summary <- events_formatted |>
  filter(event == "BRED") |>
  mutate(month_bred = floor_date(date_event, "month")) |>
  group_by(month_bred) |>
  summarise(
    n_total_bred = n(),
    n_rebreeds   = sum(R == "R", na.rm = TRUE),
    rebreed_rate = n_rebreeds / n_total_bred,
    .groups = "drop"
  ) |>
  arrange(month_bred)

# Monthly abortion cohort (by month the cow was bred)
abortion_cohort_table <- events_formatted |>
  filter(event == "BRED", R %in% c("P", "A")) |>
  mutate(
    month_bred  = floor_date(date_event, "month"),
    is_abortion = if_else(R == "A", 1, 0)
  ) |>
  group_by(month_bred) |>
  summarise(
    total_pregnancies = n(),
    n_abortions       = sum(is_abortion),
    abortion_rate     = n_abortions / total_pregnancies,
    .groups = "drop"
  ) |>
  arrange(month_bred) |>
  filter(month_bred < floor_date(date_max_pull, "month"))  # drop incomplete current month

# % pregnant by 100 / 150 / 200 DIM, by calving-month cohort.
# Each milestone trims recent cohorts that haven't had time to reach the
# window, using the same offsets as the report.
preg_by_dim <- function(dim_cutoff, trim_months) {
  cutoff_end   <- floor_date(date_max_pull, "month") - months(trim_months)
  cutoff_start <- cutoff_end - months(if (dim_cutoff == 100) 21 else 18)

  eligible <- animal_lactations |>
    filter(date_fresh <= (date_max_pull - dim_cutoff)) |>
    mutate(calving_month = floor_date(date_fresh, "month")) |>
    group_by(calving_month) |>
    summarise(total_cows = n(), .groups = "drop")

  pregnant <- events_formatted |>
    filter(event == "BRED", R %in% c("P", "A")) |>
    select(-any_of("date_fresh")) |>
    inner_join(animal_lactations |> select(id_animal_lact, date_fresh), by = "id_animal_lact") |>
    mutate(
      dim_at_event  = as.numeric(date_event - date_fresh),
      calving_month = floor_date(date_fresh, "month")
    ) |>
    filter(dim_at_event >= 0 & dim_at_event <= dim_cutoff) |>
    distinct(id_animal_lact, calving_month) |>
    group_by(calving_month) |>
    summarise(pregnant_count = n(), .groups = "drop")

  eligible |>
    left_join(pregnant, by = "calving_month") |>
    mutate(
      pregnant_count = replace_na(pregnant_count, 0),
      perc_preg      = pregnant_count / total_cows,
      milestone      = paste0(dim_cutoff, " DIM")
    ) |>
    filter(calving_month >= cutoff_start & calving_month < cutoff_end) |>
    arrange(calving_month)
}

combined_preg_summary <- bind_rows(
  preg_by_dim(100, 3),
  preg_by_dim(150, 5),
  preg_by_dim(200, 7)
) |>
  mutate(milestone = factor(milestone, levels = c("100 DIM", "150 DIM", "200 DIM")))

# =========================================================================
# UI
# =========================================================================
ui <- page_sidebar(
  title = "Dairy Reproductive Performance",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    title = "Settings",
    textInput("farm", "Farm name:", value = "Monte Vista"),
    numericInput("vwp", "Voluntary waiting period (days):",
                 value = 50, min = 0, max = 150, step = 5),
    numericInput("n_periods", "Number of 21-day periods:",
                 value = 24, min = 4, max = 48, step = 1),
    checkboxGroupInput("plot_groups", "Lactation groups on graphs:",
                       choices = LACT_GROUPS, selected = LACT_GROUPS),
    numericInput("drop_recent", "Recent periods to exclude from trend graphs:",
                 value = 2, min = 0, max = 6, step = 1),
    hr(),
    helpText("Tables show all lactation groups. The lactation-group and",
             "excluded-periods settings affect the trend and density graphs only."),
    helpText("Data is read from data/intermediate_files. Rebuild it with",
             "step0_master_processing_my_data.R.")
  ),

  navset_card_tab(
    nav_panel(
      "Overview",
      layout_columns(
        fill = FALSE,
        value_box("Farm", textOutput("vb_farm", inline = TRUE)),
        value_box("Data range", textOutput("vb_range", inline = TRUE)),
        value_box("Animals", textOutput("vb_animals", inline = TRUE)),
        value_box("Breeding events", textOutput("vb_bred", inline = TRUE))
      ),
      card(
        card_header("About this dashboard"),
        markdown(
          paste(
            "Reproductive KPIs for the selected herd, calculated over rolling",
            "21-day periods counted back from the most recent event in the data.",
            "A `BRED` event coded `P` or `A` counts as a conception (an `A` is an",
            "abortion of a real conception, so it also feeds the abortion cohort).",
            "The most recent periods are excluded from trend graphs because those",
            "cows have not yet been pregnancy-checked — including them makes recent",
            "performance look like a crash that is not real."
          )
        )
      )
    ),

    nav_panel(
      "Insemination Rate",
      card(card_header("Insemination Rate (per 21-day period)"), gt_output("ir_table"))
    ),

    nav_panel(
      "Conception Rate",
      card(card_header("Conception Rate by Lactation Group"), gt_output("cr_table")),
      layout_columns(
        card(card_header("Conception rate trend"), plotOutput("cr_trend")),
        card(card_header("Conception rate distribution"), plotOutput("cr_density"))
      )
    ),

    nav_panel(
      "Pregnancy Rate",
      card(card_header("Pregnancy Rate by Lactation Group"), gt_output("pr_table")),
      layout_columns(
        card(card_header("Pregnancy rate trend"), plotOutput("pr_trend")),
        card(card_header("Pregnancy rate distribution"), plotOutput("pr_density"))
      )
    ),

    nav_panel(
      "Rebreeds & Abortions",
      layout_columns(
        card(card_header("Monthly Rebreeding (R) Rate"), gt_output("rebreed_table")),
        card(card_header("Monthly Abortion Cohort"), gt_output("abortion_table"))
      ),
      card(card_header("Abortion rate by breeding cohort"), plotOutput("abortion_plot"))
    ),

    nav_panel(
      "DIM Milestones",
      card(
        card_header("Pregnancy rate at 100 / 150 / 200 DIM by calving cohort"),
        plotOutput("dim_plot", height = "480px")
      ),
      card(card_header("Underlying data"), gt_output("dim_table"))
    )
  )
)

# =========================================================================
# Server
# =========================================================================
server <- function(input, output, session) {

  farm_name <- reactive({
    fn <- if (!is.null(input$farm) && nzchar(input$farm)) {
      input$farm
    } else {
      unique(events_formatted$location_event)[1]
    }
    fn <- basename(fn)
    fn <- tools::file_path_sans_ext(fn)
    str_replace_all(fn, "[_|-]", " ") |> str_to_title()
  })

  # -- rolling 21-day periods --------------------------------------------
  periods_df <- reactive({
    n <- as.integer(input$n_periods)
    req(n >= 1)
    tibble(
      period_id    = seq_len(n),
      period_end   = date_max_pull - ((0:(n - 1)) * 21),
      period_start = date_max_pull - ((0:(n - 1)) * 21) - 20
    )
  })

  # -- eligible cows per period (depends on VWP) -------------------------
  eligible_cows_by_period <- reactive({
    vwp <- input$vwp
    req(!is.na(vwp))
    periods_df() |>
      cross_join(herd_intervals) |>
      left_join(cow_status, by = "id_animal_lact") |>
      filter(
        lact_number > 0,
        date_fresh <= period_end,
        (is.na(date_archive) | date_archive >= period_end),
        (is.na(date_dry) | date_dry > period_end),
        as.numeric(period_start - date_fresh) > vwp,
        is.na(last_bred) | (!is.na(last_open) & last_open > last_bred) | (last_bred > period_start),
        is.na(last_dnb) | last_dnb > period_end
      ) |>
      group_by(period_id, id_animal_lact) |>
      slice_max(date_fresh, with_ties = FALSE) |>
      ungroup()
  })

  # -- Insemination Rate --------------------------------------------------
  ir_summary <- reactive({
    bred_flags <- events_formatted |>
      filter(event == "BRED") |>
      select(id_animal_lact, date_event, R) |>
      cross_join(periods_df()) |>
      filter(date_event >= period_start, date_event <= period_end) |>
      distinct(id_animal_lact, period_id) |>
      mutate(is_bred = TRUE)

    eligible_cows_by_period() |>
      left_join(bred_flags, by = c("id_animal_lact", "period_id")) |>
      mutate(is_bred = replace_na(is_bred, FALSE)) |>
      group_by(period_id, period_start, period_end) |>
      summarise(
        n_eligible        = n(),
        n_bred            = sum(is_bred),
        insemination_rate = n_bred / n_eligible,
        .groups = "drop"
      )
  })

  output$ir_table <- render_gt({
    ir_summary() |>
      arrange(desc(period_start)) |>
      gt() |>
      tab_header(title = "Insemination Rate Report", subtitle = "Calculated per 21-Day Period") |>
      cols_label(
        period_id = "Period", period_start = "Start", period_end = "End",
        n_eligible = "Eligible Cows", n_bred = "Bred Cows", insemination_rate = "IR %"
      ) |>
      fmt_percent(columns = insemination_rate, decimals = 1)
  })

  # -- Conception Rate ----------------------------------------------------
  cr_summary_grouped <- reactive({
    breeding_outcomes <- events_formatted |>
      filter(event == "BRED") |>
      select(id_animal_lact, date_event, R) |>
      mutate(is_conception = if_else(R %in% c("P", "A"), TRUE, FALSE, missing = FALSE))

    breeding_outcomes |>
      cross_join(periods_df()) |>
      filter(date_event >= period_start, date_event <= period_end) |>
      left_join(herd_intervals |> select(id_animal_lact, lact_group), by = "id_animal_lact") |>
      filter(lact_group %in% LACT_GROUPS) |>
      group_by(period_id, period_start, period_end, lact_group) |>
      summarise(
        n_conceptions   = sum(is_conception),
        n_services      = n(),
        conception_rate = n_conceptions / n_services,
        .groups = "drop"
      )
  })

  output$cr_table <- render_gt({
    grouped <- cr_summary_grouped()
    cr_wide <- grouped |>
      select(period_id, period_start, period_end, lact_group, conception_rate) |>
      pivot_wider(names_from = lact_group, values_from = conception_rate)
    herd_avg <- grouped |>
      group_by(period_id) |>
      summarise(Overall = sum(n_conceptions) / sum(n_services), .groups = "drop")

    cr_wide |>
      left_join(herd_avg, by = "period_id") |>
      arrange(desc(period_start)) |>
      gt() |>
      tab_header(title = "Conception Rate by Lactation Group",
                 subtitle = "Percent Conception (P & A) per 21-Day Period") |>
      cols_label(period_id = "Period", period_start = "Start Date",
                 period_end = "End Date", Overall = "Herd Avg") |>
      fmt_percent(columns = any_of(c(LACT_GROUPS, "Overall")), decimals = 1) |>
      sub_missing(columns = everything(), missing_text = "0.0%") |>
      tab_options(table.border.left.style = "none", stub.border.style = "none")
  })

  output$cr_trend <- renderPlot({
    req(length(input$plot_groups) > 0)
    cr_summary_grouped() |>
      filter(lact_group %in% input$plot_groups) |>
      drop_recent_periods(input$drop_recent) |>
      ggplot(aes(period_start, conception_rate, color = lact_group, group = lact_group)) +
      geom_line(linewidth = 1) + geom_point() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Conception Rate Trend by Lactation Group",
           x = "Period Start Date", y = "Conception Rate", color = "Lactation Group") +
      theme_minimal()
  })

  output$cr_density <- renderPlot({
    req(length(input$plot_groups) > 0)
    cr_summary_grouped() |>
      filter(lact_group %in% input$plot_groups) |>
      drop_recent_periods(input$drop_recent) |>
      ggplot(aes(conception_rate, fill = lact_group)) +
      geom_density(alpha = 0.5) +
      scale_x_continuous(labels = scales::percent) +
      labs(title = "Distribution of Conception Rates",
           x = "Conception Rate", y = "Density", fill = "Lactation Group") +
      theme_minimal()
  })

  # -- Pregnancy Rate -----------------------------------------------------
  pr_summary <- reactive({
    conception_events <- events_formatted |>
      filter(event == "BRED", R %in% c("P", "A")) |>
      select(id_animal_lact, date_event) |>
      mutate(is_preg_event = TRUE)

    eligible_cows_by_period() |>
      left_join(conception_events, by = "id_animal_lact") |>
      mutate(is_preg = if_else(
        !is.na(is_preg_event) & date_event >= period_start & date_event <= period_end,
        TRUE, FALSE
      )) |>
      distinct(period_id, id_animal_lact, .keep_all = TRUE) |>
      group_by(period_id, period_start, period_end, lact_group) |>
      summarise(
        n_eligible = n(),
        n_pregs    = sum(is_preg),
        preg_rate  = n_pregs / n_eligible,
        .groups = "drop"
      )
  })

  output$pr_table <- render_gt({
    summ <- pr_summary()
    pr_wide <- summ |>
      filter(lact_group %in% LACT_GROUPS) |>
      select(period_id, period_start, period_end, lact_group, preg_rate) |>
      pivot_wider(names_from = lact_group, values_from = preg_rate)
    herd_avg <- summ |>
      group_by(period_id) |>
      summarise(Overall = sum(n_pregs) / sum(n_eligible), .groups = "drop")

    pr_wide |>
      left_join(herd_avg, by = "period_id") |>
      arrange(desc(period_start)) |>
      gt() |>
      tab_header(title = "Pregnancy Rate by Lactation Group",
                 subtitle = "Confirmed Conceptions / Eligible Cows per 21-Day Period") |>
      cols_label(period_id = "Period", period_start = "Start Date",
                 period_end = "End Date", Overall = "Herd Avg") |>
      fmt_percent(columns = any_of(c(LACT_GROUPS, "Overall")), decimals = 1) |>
      sub_missing(columns = everything(), missing_text = "0.0%") |>
      tab_options(table.border.left.style = "none", stub.border.style = "none")
  })

  output$pr_trend <- renderPlot({
    req(length(input$plot_groups) > 0)
    pr_summary() |>
      filter(lact_group %in% input$plot_groups) |>
      drop_recent_periods(input$drop_recent) |>
      ggplot(aes(period_start, preg_rate, color = lact_group, group = lact_group)) +
      geom_line(linewidth = 1) + geom_point() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Pregnancy Rate Trend by Lactation Group",
           x = "Period Start Date", y = "Pregnancy Rate", color = "Lactation Group") +
      theme_minimal()
  })

  output$pr_density <- renderPlot({
    req(length(input$plot_groups) > 0)
    pr_summary() |>
      filter(lact_group %in% input$plot_groups) |>
      drop_recent_periods(input$drop_recent) |>
      ggplot(aes(preg_rate, fill = lact_group)) +
      geom_density(alpha = 0.5) +
      scale_x_continuous(labels = scales::percent) +
      labs(title = "Distribution of Pregnancy Rates",
           x = "Pregnancy Rate", y = "Density", fill = "Lactation Group") +
      theme_minimal()
  })

  # -- Rebreeds & Abortions (static) -------------------------------------
  output$rebreed_table <- render_gt({
    rebreed_summary |>
      gt() |>
      tab_header(title = "Monthly Rebreeding (R) Rate",
                 subtitle = "Share of breeding events flagged 'R' per month") |>
      cols_label(month_bred = "Month", n_total_bred = "Total Bred Events",
                 n_rebreeds = "Rebreeds (R)", rebreed_rate = "Rebreed Rate") |>
      fmt_percent(columns = rebreed_rate, decimals = 1) |>
      fmt_date(columns = month_bred, date_style = "yMMMM")
  })

  output$abortion_table <- render_gt({
    abortion_cohort_table |>
      gt() |>
      tab_header(title = "Monthly Abortion Cohort Analysis",
                 subtitle = "Abortion rate by month of original conception") |>
      cols_label(month_bred = "Month Bred", total_pregnancies = "Total Pregnancies (P+A)",
                 n_abortions = "Abortions (A)", abortion_rate = "Abortion Rate") |>
      fmt_percent(columns = abortion_rate, decimals = 1) |>
      fmt_date(columns = month_bred, date_style = "yMMM") |>
      tab_options(table.border.left.style = "none", stub.border.style = "none")
  })

  output$abortion_plot <- renderPlot({
    abortion_cohort_table |>
      ggplot(aes(month_bred, abortion_rate)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
      labs(title = "Abortion Rate by Breeding Cohort",
           x = "Month Bred", y = "Abortion Rate") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # -- DIM milestones (static) -------------------------------------------
  output$dim_plot <- renderPlot({
    combined_preg_summary |>
      ggplot(aes(calving_month, perc_preg, color = milestone, group = milestone)) +
      geom_hline(yintercept = 0.50, linetype = "dashed", color = "steelblue", linewidth = 0.6, alpha = 0.7) +
      geom_hline(yintercept = 0.75, linetype = "dashed", color = "darkgreen", linewidth = 0.6, alpha = 0.7) +
      geom_hline(yintercept = 0.90, linetype = "dashed", color = "purple4",   linewidth = 0.6, alpha = 0.7) +
      geom_line(linewidth = 1.1) + geom_point(size = 2) +
      scale_color_manual(values = c("100 DIM" = "steelblue", "150 DIM" = "darkgreen", "200 DIM" = "purple4")) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, by = 0.10)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(
        title = "Reproductive Efficiency Milestones by Calving Cohort",
        subtitle = "Pregnancy rate at 100, 150, and 200 Days in Milk against targets",
        x = "Month of Fresh/Calving", y = "Pregnancy Rate", color = "DIM Milestone",
        caption = "Dashed lines are target benchmarks (50% at 100 DIM, 75% at 150 DIM, 90% at 200 DIM)."
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
  })

  output$dim_table <- render_gt({
    combined_preg_summary |>
      select(calving_month, milestone, perc_preg) |>
      pivot_wider(names_from = milestone, values_from = perc_preg) |>
      arrange(desc(calving_month)) |>
      gt() |>
      tab_header(title = "Percent Pregnant by DIM Milestone",
                 subtitle = "By calving-month cohort") |>
      cols_label(calving_month = "Calving Month") |>
      fmt_percent(columns = -calving_month, decimals = 1) |>
      fmt_date(columns = calving_month, date_style = "yMMM") |>
      sub_missing(columns = everything(), missing_text = "—")
  })

  # -- Overview value boxes ----------------------------------------------
  output$vb_farm    <- renderText(farm_name())
  output$vb_range   <- renderText(paste(format(date_min_pull, "%b %Y"), "–", format(date_max_pull, "%b %Y")))
  output$vb_animals <- renderText(scales::comma(n_distinct(events_formatted$id_animal)))
  output$vb_bred    <- renderText(scales::comma(sum(events_formatted$event == "BRED", na.rm = TRUE)))
}

shinyApp(ui, server)
