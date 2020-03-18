#assumptions
# kb contains the thresholds
# lab_result_cm has lab data

#url of S01 file
kb <-read_csv(url)
l_ext<-lab_result_cm %>% left_join(kb,by=c('lab_loinc'='concept_code') 
                                   
#l_ext now has reference ranges

#violating rows can now be identified
                                   
too_high<- l_ext %>% filter(result_num > max_threshold) %>% group_by(lab_loinc)  %>% summarize(n=n())
                                   
#similarly for too_low
                                   
                                   