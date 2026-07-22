library(shiny)
library(bslib)
library(broom)
library(tidyverse)
library(dtplyr)
library(gt)
library(arrow)
library(rmarkdown)
library(lubridate)
library(quarto) 
library(survival)
library(survminer)
library(ggsurvfit)
library(flextable)
library(gtsummary)
library(marginaleffects)
library(scales)
library(zoo)
library(here)

# --- 1. LOAD DATA (Directly from your pipeline) ---
events_formatted <- read_parquet(here::here('data/intermediate_files/events_formatted.parquet'))
animals <- read_parquet(here::here('data/intermediate_files/animals.parquet'))
animal_lactations <- read_parquet(here::here('data/intermediate_files/animal_lactations.parquet'))

# --- 2. SOURCE CUSTOM FUNCTIONS ---
source(here::here("functions/fxn_load_os_fxns.R"))
fxn_load_os_fxns()

source(here::here('functions/fxn_denos_sarah.R'))
herd_deno <- fxn_denos_sarah(
  lact_data = animal_lactations, 
  events_df = events_formatted, 
  animal_data = animals
)

source(here::here('functions/fxn_denominators_monthly.R'))
monthly_deno_data <- fxn_get_monthly_denominators(animal_lactations, animals)

# --- 3. DYNAMIC VARIABLES ---
date_max_pull <- max(events_formatted$date_event, na.rm = TRUE)

# If you want to retain farm selection flexibility, you can default to the 
# first location or let a Shiny input dropdown override it later:
farm_name <- unique(events_formatted$location_event)[1]
farm_name <- basename(farm_name)
farm_name <- tools::file_path_sans_ext(farm_name)
farm_name <- str_replace_all(farm_name, "[_|-]", " ") |> str_to_title()

# --- UI ---
ui <- page_sidebar(
  title = "Dairy Reproductive Performance Dashboard",
  sidebar = sidebar(
    title = "Filters",
    # Input to separate or filter groups based on your workflow needs
    checkboxGroupInput(
      inputId = "selected_groups", 
      label = "Select Lactation Groups:",
      choices = c("Heifers", "Adult Cows"),
      selected = c("Heifers", "Adult Cows")
    ),
    selectInput(
      inputId = "status_filter",
      label = "Pregnancy Status:",
      choices = c("All", "Pregnant", "Open", "Protocol"),
      selected = "All"
    ),
    hr(),
    helpText("Data refreshed from local operational processing pipeline.")
  ),
  
  # Main Panel Layout
  layout_columns(
    col_widths = c(12, 12),
    card(
      card_header("Reproductive Status Distribution"),
      plotOutput("rep_plot")
    ),
    card(
      card_header("Herd Summary Table"),
      gt_output("summary_table")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Reactive filtering based on user inputs
  filtered_data <- reactive({
    data <- df_herd %>%
      filter(lactation_group %in% input$selected_groups)
    
    if (input$status_filter != "All") {
      data <- data %>% filter(pregnancy_status == input$status_filter)
    }
    
    data
  })
  
  # Render ggplot (reusing your styling logic)
  output$rep_plot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = days_in_milk, fill = pregnancy_status)) +
      geom_density(alpha = 0.6) +
      theme_minimal(base_size = 14) +
      labs(
        x = "Days in Milk (DIM)", 
        y = "Density", 
        fill = "Pregnancy Status"
      ) +
      scale_fill_brewer(palette = "Set2")
  })
  
  # Render GT Table (reusing your reporting formats)
  output$summary_table <- render_gt({
    filtered_data() %>%
      group_by(lactation_group, pregnancy_status) %>%
      summarize(
        Head_Count = n(),
        Avg_DIM = round(mean(days_in_milk, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      gt() %>%
      tab_header(
        title = "Herd Reproductive Breakdown",
        subtitle = "Filtered cohort metrics"
      ) %>%
      cols_label(
        lactation_group = "Cohort",
        pregnancy_status = "Status",
        Head_Count = "Head Count",
        Avg_DIM = "Average DIM"
      ) %>%
      opt_stylize(style = 6, color = "cyan")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
