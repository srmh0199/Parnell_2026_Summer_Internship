library(tidyverse)

fxn_locate_lesion<-function(df){
  df%>%
    mutate(
      LF = case_when(
        str_detect(remark, 'LF|.LF|LF.|.LF.|BF|.BF|BF.|BF|BL|.BL|BL.|.BL')~'LF', 
        TRUE~''), 
      RF = case_when(
        str_detect(remark, 'RF|.RF|RF.|.RF.|BF|.BF|BF.|BF|BR|.BR|BR.|.BR')~'RF', 
        TRUE~''), 
      LH = case_when(
        str_detect(remark,'LR|.LR|LR.|.LR.|LH|.LH|LH.|.LH.|BH|.BH|BH.|BH|BL|.BL|BL.|.BL')~'LH', 
        TRUE~''), 
      RH = case_when(
        str_detect(remark,'RR|.RR|RR.|.RR.|RH|.RH|RH.|.RH.|BH|.BH|BH.|BH|BR|.BR|BR.|.BR')~'RH', 
        TRUE~''))%>%
    mutate(locate_lesion = paste0(LF, RF, LH, RH))
}