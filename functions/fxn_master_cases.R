
library(tidyverse)

master_cases<-NULL
#i=20

fxn_master_cases<-function(){
for (i in seq_along(df_risk$start_date)){
  
  df_cases<-data_surv%>%
    #filter(cowid %in% '1000739903/15/22')%>%
    #filter(cowid %in% cows[1:2000])%>%
    mutate(age_at_disease = dz_col-bdat)%>%
    filter(dz_status == 1)%>%
    #select(age_at_disease, cowid, dz_col, bdat, everything())
    filter(age_at_disease>=set_age_min)%>%
    filter(age_at_disease<=set_age_max)%>%
    #filter(date_arrived_kdd>=min(date_censor, na.rm = T)+days_in_age_range)%>%
    
    mutate(start_date = df_risk$start_date[[i]],
           end_date = df_risk$end_date[[i]])%>%
    filter(dz_col>=start_date)%>%
    filter(dz_col<=end_date)%>%
    # mutate(start_elig = dz_col>=start_date,
    #        end_elig = dz_col<=end_date)%>%
    # filter(start_elig>0)%>%
    # filter(end_elig>0)%>%
    group_by(start_date, end_date)%>%
    summarize(#start_date = min(start_date),
      #end_date = min(end_date),
      case_ct = n_distinct(cowid))%>%
    ungroup()
  
  
  master_cases<<-bind_rows(df_cases, master_cases)
  
  
}
  
}
