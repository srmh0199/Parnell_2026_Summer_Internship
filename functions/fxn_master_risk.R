library(tidyverse)

master_risk<-NULL

#i=34

fxn_master_risk<-function(){
for (i in seq_along(df_risk$start_date)){
  df_calves<-data_surv%>%
    #filter(cowid %in% '1000739903/15/22')%>%
    #filter(cowid %in% cows[1:2000])%>%
    mutate(start_date = df_risk$start_date[[i]], 
           end_date = df_risk$end_date[[i]])%>%
    mutate(age_at_start = start_date-bdat, 
           age_at_end = end_date-bdat)%>%
    filter(age_at_end>=set_age_min)%>%
    filter(age_at_start<=set_age_max)%>%
    filter(date_arrived_kdd<=end_date)%>%
    filter(date_censor>=start_date)%>%
    
    mutate(start_elig = date_arrived_kdd<=start_date, 
           end_elig = date_censor>=end_date)%>%
    mutate(period_elig = (start_elig+end_elig))%>%
    filter(period_elig>0)%>%
    mutate(days_elig = case_when(
      (period_elig == 2)~as.numeric(end_date-start_date)+1, 
      (start_elig>0)~as.numeric(date_censor-start_date)+1, 
      (end_elig>0)~as.numeric(end_date-date_arrived_kdd)+1, 
      # (start_elig==FALSE)~as.numeric(date_censor-date_arrived_kdd)+1, 
      # (end_elig==FALSE)~as.numeric(start_date-date_censor)+1,
      TRUE~as.numeric(NA))
    )%>%
    select(days_elig, start_elig, end_elig, period_elig, start_date, end_date, 
           date_arrived_kdd, date_censor, everything())%>%
    group_by(start_date, end_date)%>%
    summarize(#start_date = min(start_date), 
      #end_date = min(end_date), 
      days_at_risk = sum(days_elig, na.rm = T), 
      removed = sum(is.na(days_elig), na.rm = T), 
      animals = n_distinct(cowid))%>%
    ungroup()
  
  
  
  master_risk<<-bind_rows(df_calves, master_risk)
  
}
  
}