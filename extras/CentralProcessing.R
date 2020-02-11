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
f<-'d:/OneDrive - National Institutes of Health/temp/dqd/export'

sfiles<-c(file.path(f,'1ThresholdsA.csv'))
sfiles<-c(file.path(f,'1ThresholdsA.csv'),file.path(f,'ThresholdsA.csv'))
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


#read DD checks
library(stats);library(tidyverse);library(magrittr)
#message("\n*** Successfully loaded .Rprofile ***\n")


url='https://raw.githubusercontent.com/OHDSI/DataQualityDashboard/master/inst/csv/OMOP_CDMv5.3.1_Concept_Level.csv'
dqd<-read_csv(url)
str(dqd)
names(dqd)
dqd %>% dplyr::filter(cdmTableName=='MEASUREMENT' & cmdFieldName=='MEASUREMENT_CONCEPT_ID' ) 
dqd %>% dplyr::filter(cdmFieldName=='MEASUREMENT_CONCEPT_ID' ) %>% nrow()
dqd %>% count(cdmTableName,cdmFieldName)


#compare data driven and expert drive sets
d$STRATUM_1 %<>% as.integer()
dqd$unitConceptId %<>% as.integer()
expert <-dqd %>% dplyr::filter(cdmFieldName=='MEASUREMENT_CONCEPT_ID' )
names(expert)
ddriven<-d %>% rename(conceptId=STRATUM_1,unitConceptId=STRATUM_2)  %>% select(conceptId,unitConceptId) %>% unique()

names(d2)
ddriven<-d %>% rename(conceptId=STRATUM_1,unitConceptId=STRATUM_2) 
ddriven<-d2 %>% rename(conceptId=stratum_1,unitConceptId=stratum_2) 
#ddriven %<>% filter(conceptId!=0)
#ddriven %<>% filter(unitConceptId!=0)

over=expert %>% inner_join(ddriven) #58 overlapping
View(over)
expert %>% anti_join(ddriven) #827 are in expert but not in data

not1<-ddriven %>% anti_join(expert) #14 are in data and not in expert


#compare the trehsholds
names(over)
over %>% select(conceptName,unitConceptName,plausibleValueLow,min_value)
over %>% select(conceptName,unitConceptName,plausibleValueHigh,max_value) 
#%>% knitr::kable()


#expert thresholds don't follow unit conversion logic (max and min is same even if units indicate order of magniture difference)
#MEASUREMENT	MEASUREMENT_CONCEPT_ID	3013721	Aspartate aminotransferase [Enzymatic activity/volume] in Serum or Plasma	8713	gram per deciliter	5	5	2000	5	NA	NA	NA	NA	NA	NA	NA	NA
#MEASUREMENT	MEASUREMENT_CONCEPT_ID	3013721	Aspartate aminotransferase [Enzymatic activity/volume] in Serum or Plasma	8840	milligram per deciliter	5	5	2000

#5g/dL into  mg/dL  (is 5000 mg/dL)
#in data is in fact unit/L
