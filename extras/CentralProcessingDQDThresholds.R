

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
f<-'d:/OneDrive - National Institutes of Health/ohdsi/thresholds'

sfiles<-c(file.path(f,'1ThresholdsA.csv'))
sfiles<-c(file.path(f,'test-ThresholdsA.csv'))
sfiles<-c(file.path(f,'1ThresholdsA.csv'),file.path(f,'ThresholdsA.csv'),file.path(f,'test-ThresholdsA.csv'))
#3 sites processing +1
sfiles<-c(file.path(f,'01ThresholdsB.csv')
          ,file.path(f,'02ThresholdsB.csv')
          ,file.path(f,'03ThresholdsB.csv')
          ,file.path(f,'04ThresholdsB.csv')
          ,file.path(f,'05ThresholdsB.csv')
          ,file.path(f,'06ThresholdsB.csv')
)
ll<-map(sfiles,read_csv)
ll

#ll<-map(p$pid,doProperty())
#strip name from full path trick

#make lowercase the column names
llmoded<-map(ll,~{names(.x)<-tolower(names(.x));return(.x)})
#llmoded[[1]]
#ll[[1]]
ll2<-map2(llmoded,basename(sfiles),~mutate(.x,site=.y))
d<-bind_rows(ll2)

#add terminology concepts
sconcept<-concept %>% select(concept_id,concept_name)
names(d) <- tolower(names(d))
names(d)
#remove no units rows and expand the CIDs
#stratum hav suffix id
# d2<-d %>% filter(stratum_1 != 0) %>% filter(stratum_2 != 0) %>% left_join(sconcept,by=c('stratum_1'='concept_id')) %>%
#   left_join(sconcept,by=c('stratum_2'='concept_id')) 


d2<-d %>% filter(count_value >=11 ) %>% filter(stratum1_id != 0) %>% filter(stratum2_id != 0) %>% 
  left_join(sconcept,by=c('stratum1_id'='concept_id')) %>%
  left_join(sconcept,by=c('stratum2_id'='concept_id')) %>% filter(!is.na(concept_name.x))
#test in 2B range are excluded by last filter

names(d2)

#overview of sites
soverview<-d2 %>% count(site)
soverview
#soverview %>% write_csv('extras/DqdResults/S1_overview.csv')




ba<-d2 %>% group_by(stratum1_id,stratum2_id,concept_name.x,concept_name.y) %>% summarize(tcnt=sum(count_value),n=n())
ba %>% filter(n>=2) %>% nrow()
nrow(ba)
#4465 distinct test-unit pairs
#872 test-unit paris have 2+ sites



#tests with more units
ba %>% ungroup() %>%  count(stratum1_id,concept_name.x) %>% filter(n>=2)
#TODO improve later




#end of section


#---------------comparison with expert driven

#read expert driven checks
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
#d$STRATUM_1 %<>% as.integer()
dqd$unitConceptId %<>% as.integer()
expert <-dqd %>% dplyr::filter(cdmFieldName=='MEASUREMENT_CONCEPT_ID' )
nrow(expert)
#856 threshold checks are in expert driven KB
names(expert)
elabs<-expert %>% group_by(conceptId,conceptName) %>% summarise(unitcnt=n(),units=paste(unitConceptName,collapse = "|"))
nrow(elabs)
# for 330 distinct lab tests
#elabs %>% write_csv('extras/DqdResults/DQD-expert-driven-A-lab-list.csv')  

names(expert)

names(d2)
#renaming d2 (data driven) data to prepare for comparison of expert and data driven
ddriven<-d2 %>% rename(conceptId=stratum1_id,unitConceptId=stratum2_id) 

#ddriven2 is on test-unit pair level (only one row per lab-unit pair)
ddriven2<-ba %>% rename(conceptId=stratum1_id,unitConceptId=stratum2_id) 



#ddriven %<>% filter(conceptId!=0)
#ddriven %<>% filter(unitConceptId!=0)

over=expert %>% inner_join(ddriven2) 
nrow(over)
#451 pairs are overlapping between ddriven (data driven) and expert (expert driven)
#View(over)

not2<-expert %>% anti_join(ddriven2) 
nrow(not2)
#405 are in expert list but not in data from any site

#freq rank for lab test

freq<-ddriven %>% group_by(conceptId) %>% summarize(tcnt=sum(count_value)) %>% 
   mutate(lab_test_freq_rank = dense_rank(desc(tcnt)))





#---more analysis in March 2020
#mean  and median should be close, one super high value shifts the mean (could be used in logic)
#3020891 body temp

#list units for each lab test
names(ddriven)

# bb<-ddriven %>% group_by(conceptId,concept_name.x) %>% dplyr::summarize(n=n()                                                            
#                                                                         ,unitIds=paste(unitConceptId,collapse = '|')
#                                                             ,units=paste(concept_name.y,collapse = '|'))
#by lab test view
bb2<-ddriven2 %>% ungroup %>% arrange(desc(n),desc(tcnt)) %>% group_by(conceptId,concept_name.x) %>% dplyr::summarize(                                                            
                                                                        unitIds=paste(unitConceptId,collapse = '|')
                                                                        ,units=paste(concept_name.y,collapse = '|')
                                                                        ,siteCnts=paste(n,collapse='|')
                                                                        ,distinct_unit_cnt=n())

names(ba)
viewSites<-ba %>% ungroup() %>%  count(n)
viewSites
# # A tibble: 6 x 2
# n    nn
# <int> <int>
#   1     1  4593
# 2     2   641
# 3     3   434
# 4     4   247
# 5     5    27
# 6     6     1


#bset<-ba %>% filter(n==3)
#bc<-ddriven %>% inner_join(bset)
#3009542 hematocrit
options(scipen=999) #disable scientific notation
prekb<-ddriven %>% group_by(conceptId,concept_name.x,unitConceptId,concept_name.y) %>% summarize(
  n=n()
  ,sum_count_value=sum(count_value)
  #,kb_min_mean=mean(min_value)
  #,kb_max_mean=mean(max_value)
  ,kb_min_median=median(min_value)
  ,kb_max_median=median(max_value)
  ,kb_p01_value_median=median(p01_value)
  ,kb_p99_value_median=median(p99_value)
  ,kb_p50_median=median(median_value)
  ,kb_stdev_median=median(stdev_value)
)  %>% ungroup() %>% 
    mutate(freq_rank = dense_rank(desc(sum_count_value)))

kb<- prekb%>% filter(n>=2) %>% filter(sum_count_value>=1)

#analysis on lab test only level
#expert has 330 distinct tests


kb %>% write_csv('local/S01-benchmark-data2.csv')
kbExport<-kb %>% filter(sum_count_value>=100) %>% select(-sum_count_value) %>% arrange(freq_rank)
#kbExport %>% write_csv('extras/DqdResults/S01-benchmark-kb-subset.csv')


names(kb)
names(ddriven)
                                        

#analysis on lab test only level
#expert has 330 distinct tests
#bb2 is essentially that
  #labs<-prekb  %>% group_by(conceptId,concept_name.x) %>% summarise(n=n(),sum_rank=sum(freq_rank)) 
#add freq rank to it

bb3<-bb2 %>% left_join(freq %>% select(-tcnt)) %>% arrange(lab_test_freq_rank)
names(bb3)
bb3 %>%  nrow()
bb3 %>% write_csv('extras/DqdResults/S02-KB-lab-test-view.csv')

#test that are in data but  are not in expert driven (on lab test level)
not1<-bb3 %>% anti_join(expert) 
nrow(not1)
#3993 are in data (full KB) but are absent in expert driven KB


bb3 %>% nrow()
#4282 distinct lab test (including data from a single site)

#terms
#benchmark KB = has content only if 2+ sites contribute thresholds
#full KB = provides thresholds even if they come from a single site (is bigger than benchmark)





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



#unitmorph
#Protein [Mass/volume] in Serum or Plasma	7096851	4	gram per deciliter|unit|milligram per deciliter|gram per liter
#	gram per deciliter|   |milligram per deciliter |   gram per liter
names(d3)
bb<-d3 %>% filter(site=='ThresholdsA.csv') %>% group_by(stratum_1,concept_name.x) %>% 
  summarize(tcnt=sum(count_value)
            ,n=n(),units=paste(concept_name.y,collapse = '|')
            ,cnts=paste(count_value ,collapse = '|')
            ,unitcids=paste(stratum_2,collapse = '|')
  )

bb %>% write_csv('local/morphA.csv')

#more unit work
u<-read_csv('local/conv-dev.csv')
names(u)
u %<>% filter(is.na(measurement_concept_id))
u2<-tibble(unit_concept_id=u$target_unit_concept_id
       ,target_unit_concept_id=u$unit_concept_id
       ,factor=1/u$factor)
ub<-bind_rows(u,u2)  %>% 
  left_join(sconcept,by=c('unit_concept_id'='concept_id')) %>% 
  left_join(sconcept,by=c('target_unit_concept_id'='concept_id'))


             