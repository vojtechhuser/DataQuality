#DQ study centralized processing

#heel stuff
tta<-as.data.frame(table(combHeel$rule_id) )

tta <-combHeel %>% group_by(rule_id) %>% tally()
tta <-combHeel %>% group_by(rule_id,analysis_id) %>% tally()

ttc <-combHeel %>% group_by(rule_id,analysis_id, dset) %>% tally()

ttb <-combHeel %>% group_by(rule_id, dset) %>% tally() %>% ungroup() %>% dplyr::rename(n2=n) %>% group_by(rule_id) %>% tally()



#tta <-combHeel %>% group_by(rule_id,achilles_heel_warning) %>% tally()
lkup_rules<-read_csv(file='https://raw.githubusercontent.com/OHDSI/Achilles/master/inst/csv/achilles_rule.csv')
tta %>% left_join(select(lkup_rules,1,2)) %>% arrange(desc(n)) %>% View()
tta %>% left_join(select(lkup_rules,1:8)) %>% arrange(desc(n)) %>% View()
tta %>% left_join(select(lkup_rules,1:8)) %>% arrange(desc(n)) %>% write_csv('c:/b/rules-2.csv')

write.table(head(combHeel,200),file="clipboard", sep="\t",row.names=F) #paste to excel

#posted on forum about rule splitting
#2017-06 new work
#join comHeel with lkup
ttd<-combHeel %>% left_join(lkup_rules)

#point111
ttb2<-ttb  %>% left_join(lkup_rules)




#DQD Centralized processing
#----------------------------------------
#----------------------------------------
#this script assummes coordinating center infrustructure for loading
#athena dictionaries (.rda files istead of in a database)
#now truly doing 2 sites

#load athena dictionary
library(tidyverse);library(magrittr);options(tibble.print_max = 200)
load('o:/athena/concept.rda')

#lkup<-concept %>% filter(vocabulary_id %in% c('CPT4','ICD9Proc','CDT','HCPCS','ICD9CM','ICD10CM','ICD10PCS'))

#reading a single site data (for now) 
sfiles<-c('c:/temp/dqd/export/1ThresholdsA.csv','c:/temp/dqd/ThresholdsA.csv')
ll<-map(sfiles,read_csv)
ll

#ll<-map(p$pid,doProperty())
ll2<-map2(ll,sfiles,~mutate(.x,site=.y))
d<-bind_rows(ll2)

#add terminology concepts
sconcept<-concept %>% select(concept_id,concept_name)
names(d) <- tolower(names(d))
names(d)
#remove no units rows and expand the CIDs
d2<-d %>% filter(stratum_1 != 0) %>% filter(stratum_2 != 0) %>% left_join(sconcept,by=c('stratum_1'='concept_id')) %>%
  left_join(sconcept,by=c('stratum_2'='concept_id')) 
names(d2)
#remove columns that are not needed
d3<-d2 %>% select(-stratum_3,-stratum_4,-stratum_5,-p25_value,-p75_value) %>% 
  filter(count_value >=100 ) %>% arrange(stratum_1,desc(count_value) )

d3 %>% count(site)
ba<-d3 %>% count(stratum_1,stratum_2)
ba %>% filter(n>=2)

#24 test-unit pairs  have 2 results



#tests with more units
d3 %>% count(stratum_1)


#only where multiple sites
d10<-d3 %>% inner_join(ba %>% filter(n>=2))





#even more removal of data
d4<-d3 %>% select(-count_value,-median_value,-stdev_value,-avg_value,-site)

d4 %>% write_csv('extras/DqdResults/thresholds-list-A.csv')
nrow(d4)
