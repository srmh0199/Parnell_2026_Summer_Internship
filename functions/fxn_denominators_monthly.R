### Monthly Denominator Engine ###

fxn_get_monthly_denominators <- function(lact_data, animal_data) {
  
  # 1. Determine the overall date range
  date_end <- max(animal_data$data_pull_date_max, na.rm = TRUE)
  # Look back 2 years (720 days), or adjust as needed
  date_start <- date_end - 720 
  
  # Generate a list of the first day of every month in that range
  months_list <- seq(floor_date(date_start, "month"), 
                     floor_date(date_end, "month"), 
                     by = "month")
  
  # 2. Define logic for a single month snapshot
  process_single_month <- function(m_start) {
    m_end <- ceiling_date(m_start, "month") - days(1)
    m_label <- zoo::as.yearmon(m_start)
    
    # Filter for cows active AT ANY POINT during this specific month
    active_in_month <- lact_data |>
      filter(lact_number > 0) |>
      filter(
        date_fresh <= m_end & (is.na(date_archive) | date_archive >= m_start)
      )
    
    # Calculation Helper
    calc_group <- function(df, group_col) {
      df |>
        group_by(location_lact_list, !!sym(group_col)) |>
        summarize(count = n_distinct(id_animal), .groups = "drop") |>
        rename(`Lactation Group` = !!sym(group_col))
    }
    
    # Run for all your grouping types
    res_basic <- calc_group(active_in_month, "lact_group_basic")
    res_5     <- calc_group(active_in_month, "lact_group_5")
    res_3plus <- calc_group(active_in_month, "lact_group") |> 
      filter(`Lactation Group` == "LACT 3+")
    
    # Combine and add the month label
    bind_rows(res_basic, res_5, res_3plus) |>
      mutate(month = m_label) |>
      distinct()
  }
  
  # 3. Map over all months and combine
  final_monthly_denos <- purrr::map_dfr(months_list, process_single_month)
  
  return(final_monthly_denos)
}