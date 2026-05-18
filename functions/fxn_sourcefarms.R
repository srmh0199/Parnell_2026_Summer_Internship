library(tidyverse)
fxn_parse_source_farm<-function(df){
  df%>%
  mutate(source_farm = str_sub(ID, 1,2))
}

fxn_source_state<-function(df){
  df%>%
    mutate(source_state = case_when(
      (source_farm %in% c('26', '91'))~'Kansas', 
      (source_farm %in% c('63', '33', '76', '80', '66', '71', '60', '14', '31', '11',
                          '72', '67', '65', '55', '84', 
                          '61', '24', '64', '16', '22'))~'Wisconsin', 
      (source_farm %in% c('27', '58', '69', '13', '12', '28', '89'))~'Michigan', 
      (source_farm %in% c('15', '88', '39', '83', '45', '54', '77', '74', '56'))~'I-29', 
      (source_farm %in% c('21', '79', '87', '86', '73', '75'))~'Southeast', 
      (source_farm %in% c('70', '42', '92'))~'New York', 
      TRUE~'Unknown'  ))
}

fxn_add_source_farm_custom<-function(df){
  df%>%
    fxn_source_farm()%>%
    fxn_source_state()
}

fxn_add_source_farm_default<-function(df){
  df%>%
    mutate(source_farm = set_farm_name, 
           source_state = set_farm_state)
}
