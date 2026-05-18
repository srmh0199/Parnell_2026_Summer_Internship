fxn_denos_sarah <- function(lact_data, events_df, animal_data) {
  
  # 1. Define the timeline based on raw events
  data_min_date <- min(events_df$date_event, na.rm = TRUE)
  data_max_date <- max(animal_data$data_pull_date_max, na.rm = TRUE)
  
  start_year <- year(data_min_date)
  end_year   <- year(data_max_date)
  
  years_to_process <- seq(start_year, end_year)
  
  # 2. The Internal Year Processor
  process_single_year <- function(target_year) {
    # Use the last day of the year (or today's date if it's the current year)
    snapshot_date <- as.Date(paste0(target_year, "-12-31"))
    if(target_year == year(data_max_date)) snapshot_date <- data_max_date
    
    deno_temp <- lact_data |>
      filter(lact_number > 0) |>
      filter(
        # The cow must have freshened BEFORE the snapshot
        date_lact_first_event <= snapshot_date & 
          # AND she must not have been archived yet ON that snapshot date
          (is.na(date_archive) | date_archive >= snapshot_date)
      ) |>
      group_by(id_animal) |>
      filter(lact_number == max(lact_number)) |>
      ungroup() |>
      mutate(time_period = as.character(target_year))
    
    # Summarize based on groupings
    calc_lvl <- function(df, col_name) {
      df |>
        group_by(location_lact_list, time_period, !!sym(col_name)) |>
        summarize(count_animals = n_distinct(id_animal), .groups = "drop") |>
        rename(`Lactation Group` = !!sym(col_name))
    }
    
    bind_rows(
      calc_lvl(deno_temp, "lact_group_basic"),
      calc_lvl(deno_temp, "lact_group_5")
    )
  }
  
  # 3. Map across the years
  purrr::map_dfr(years_to_process, process_single_year) |> distinct()
}