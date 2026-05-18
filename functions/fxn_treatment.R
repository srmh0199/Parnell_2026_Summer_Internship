library(tidyverse)

fxn_assign_treatment_template<-function(df){
  df%>%
    mutate(treatment = protocols_letters1)
}

fxn_assign_treatment_bred<-function(df){
  df%>%
    mutate(treatment = `R`)
}