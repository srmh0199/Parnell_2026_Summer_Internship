library(tidyverse)

#fucntion to rename variables used within lag function
mutfxn = function(x, vars, prefix){
  
  map_dfc(vars, ~
            x %>% 
            #mutate(key = as.character(NA))%>%
            transmute(!! paste(prefix,"lag",.x,sep = "_") := lag(!!as.name(.x))==!!as.name(.x))) %>% 
    bind_cols(x,.) 
  
}


# ------------------------------------------------------------------------------

#function to create grouping variables
test_fxn1 <- function(x, arrange_var, mutate_var, prefix, gap){
  arrange_var1 <- quos(!!! arrange_var) # quos returns a list of quosures - can handle (...), !!! allows more than 1, date variable must be last
  var_ct<-length(arrange_var1) # Calculate the number of variables
  
  x %>%
    arrange(!!!arrange_var1) %>% #arrange the dataframe
    
    mutfxn(mutate_var, prefix) %>%   # call mutfxn
    
    mutate(date_gap = (difftime(date, lag(date), units = 'days')),  # for long format data: data_last_admin is replaced by Date
           lag_date = ((date_gap >= 0) & (date_gap < gap))) %>%
    
    mutate(count_lags = reduce(select(., contains("lag")), `+`)) %>%  #Identify rows where a new lag group starts
    
    mutate(lag_ct = (count_lags) < (var_ct))%>%
    
    mutate(lag_ct = case_when(is.na(lag_ct)~TRUE,
                              TRUE~lag_ct)) %>% #this is necessary to fill the first row in the dataframe, which cannot be NA 
    
    mutate(key = case_when(is.na(lag_ct)>0~paste0(prefix, rowid),
                           lag_ct>0~paste0(prefix, rowid),
                           TRUE~as.character(NA))) %>%
    fill(key)
}



#------------------------------------------------------
#use lag function to greate construct groupings

#Regimens
# arrange_vars_r <- alist(Dairy, ID, evt_type, Reason3, Treatment, Date) #removed: Reason2, Disease, we need a "stop if list contains 'lag'
# 
# sort_vars_r <- c("Dairy", "ID", "evt_type", 'Reason3', "Treatment") #removed:"Reason2", "Disease",
# 
# df_r<-test_fxn1(x = df_test,
#                 arrange_var = arrange_vars_r,
#                 mutate_var = sort_vars_r, 
#                 prefix = "r", 
#                 gap = 7)%>% #gap set to identify regimens
#   rename(r_key = key, 
#          r_date_gap = date_gap, 
#          r_ct = lag_ct)%>%
#   select(-(contains('lag')))
# 
# head(df_r%>%select(r_key, r_date_gap, r_ct, everything()))
# 
# write_csv(df_r%>%select(r_key, r_date_gap, r_ct, everything()), 'example_data_lag_r.csv')

