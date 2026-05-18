library(tidyverse)

fxn_detect_location_lesion<-function(df){
  df%>%
    mutate(lesion_location=case_when(
      str_detect(lesion_location, "left")~"left",
      str_detect(lesion_location, "right")~"right",
      TRUE~"unknown"
    ))
}