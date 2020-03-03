

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


d2<-d %>% filter(count_value >=11 ) %>% filter(stratum1_id != 0) %>% filter(stratum2_id != 0) %>% left_join(sconcept,by=c('stratum1_id'='concept_id')) %>%
  left_join(sconcept,by=c('stratum2_id'='concept_id')) %>% filter(!is.na(concept_name.x))
#test in 2B range are excluded by last filter

names(d2)

#overview of sites
soverview<-d2 %>% count(site)
soverview
#soverview %>% write_csv('extras/DqdResults/S1_overview.csv')

#remove columns that are not needed
# d3<-d2 %>% select(-stratum_3,-stratum_4,-stratum_5,-p25_value,-p75_value) %>% 
#   filter(count_value >=100 ) %>% arrange(stratum_1,desc(count_value) )

d3<-d2 %>% select(-stratum_3,-stratum_4,-stratum_5) %>% 
   arrange(stratum_1,desc(count_value) )


#d3 %>% count(site)
#names(d3)
ba<-d2 %>% group_by(stratum1_id,stratum2_id,concept_name.x,concept_name.y) %>% summarize(tcnt=sum(count_value),n=n())
ba %>% filter(n>=2) %>% nrow()
nrow(ba)
#4465 distinct test-unit pairs
#872 test-unit paris have 2+ sites



#tests with more units
ba %>% ungroup() %>%  count(stratum1_id,concept_name.x) %>% filter(n>=2)
#TODO improve later
#868 tests have 2+ units

#only where multiple sites
#d10<-d3 %>% inner_join(ba %>% filter(n>=2))





#even more removal of data
#d4<-d3 %>% select(-count_value,-median_value,-stdev_value,-avg_value,-site)

#d4 %>% write_csv('extras/DqdResults/thresholds-list-A.csv')
#nrow(d4)


#end of analysis of


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

# for 330 distinct lab tests
elabs %>% write_csv('extras/DqdResults/DQD-expert-driven-A-lab-list.csv')  

names(expert)
#ddriven<-d %>% rename(conceptId=STRATUM_1,unitConceptId=STRATUM_2)  %>% select(conceptId,unitConceptId) %>% unique()

names(d2)
#ddriven<-d %>% rename(conceptId=STRATUM_1,unitConceptId=STRATUM_2) 
ddriven<-d2 %>% rename(conceptId=stratum1_id,unitConceptId=stratum2_id) 
ddriven2<-ba %>% rename(conceptId=stratum1_id,unitConceptId=stratum2_id) 

#ddriven %<>% filter(conceptId!=0)
#ddriven %<>% filter(unitConceptId!=0)

over=expert %>% inner_join(ddriven2) 
nrow(over)
#331 tests are overlapping between ddriven (data driven) and expert (expert driven)
#View(over)

not2<-expert %>% anti_join(ddriven2) 
nrow(not2)
#525 are in expert list but not in data from any site

not1<-ddriven2 %>% anti_join(expert) 
nrow(not1)
#4134 are in data but are absent in expert driven KB


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
