library(data.table)
library(tidyverse)

fxn_assign_day_of_phase <- function(df, lookup_tbl) {
  
  # Convert to data.table without modifying originals
  df_dt     <- as.data.table(copy(df))
  lookup_dt <- as.data.table(copy(lookup_tbl))
  
  # Ensure numeric bounds (for Inf handling)
  lookup_dt[, `:=`(
    dop_start = as.numeric(dop_start),
    dop_end   = as.numeric(dop_end)
  )]
  
  # Non-equi join to assign category
  df_dt[
    lookup_dt,
    day_of_phase_group := i.day_of_phase_group,
    on = .(
      `Lactation Group`,
      dim_event >= dop_start,
      dim_event <= dop_end
    )
  ]
  
  return(as.data.frame(df_dt))
}