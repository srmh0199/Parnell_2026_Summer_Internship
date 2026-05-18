library(tidyverse)

#NOTE: this function uses the natural log of the concentration, the units for half life will be the units of the time variable
fxn_calc_halflife<-function(df, time_points){
  df%>%
    mutate(Time = Time_hrs)%>% #define which variable is Time
    #filter(day %in% list_day_to_start_stop)%>%
    arrange(tx_grp, Subject, Time)%>% #arrange data so the lag function gives the starting conc or time
    mutate(lag_subject = lag(Subject), #make sure it is the same animal
           lag_conc = lag(ln_conc), #get the first concentration
           lag_time = lag(Time),  #get the first time
           diff_conc = lag_conc-ln_conc, #calculate the difference between concentrations
           diff_time = Time-lag_time, #calculate the difference in time
           k_el = diff_conc/diff_time, #calculate k_el
           half_life = 0.693/(k_el), #calculate halflife
           type = time_points)%>% #name points used in calculation
    mutate(half_life = case_when(
      (Subject == lag_subject)~half_life, #only use points from the same subject for halflife calcs
      TRUE~as.numeric(NA)
    ))
}

