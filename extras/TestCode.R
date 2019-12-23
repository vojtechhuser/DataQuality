#' @file TestCode.R


library(DataQuality)

workFolder <- 'c:/temp'

#get connection details
source('c:/r/conn.R')  #

#database parameters
cdmDatabaseSchema <-'ccae'
resultsDatabaseSchema <-'ccae' #at most sites this likely will not be the same as cdmDatabaseSchema

workFolder <- 'c:/temp'   #this folder must exist (use forward slashes)


executeDQ(connectionDetails = connectionDetails,cdmDatabaseSchema = cdmDatabaseSchema,workFolder = workFolder)
cd2<-.createConnectionDetails2(cdmDatabaseSchema = cdmDatabaseSchema)


#DatabaseConnector::createConnectionDetails()
#packageResults(connectionDetails,cdmDatabaseSchema,workFolder)

submitResults(exportFolder =file.path(workFolder,'export'),
              studyBucketName = 'ohdsi-study-dataquality',
              key=studyKey,
              secret =studySecret
              )


#DQD section-----------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------
#uncomment the line below to install the latest version of the code and package
#devtools::install_github("vojtechhuser/DataQuality")
#testing it on Eunomia
library(DataQuality)


#replace with your local parameters using the same variable names
#note: it will run some achilles measures (so it may overwrite your current achilles results table)
library(Eunomia)
connectionDetails<-Eunomia::getEunomiaConnectionDetails() 
cdmDatabaseSchema <-'main'
resultsDatabaseSchema <-'main' #at most sites this likely will not be the same as cdmDatabaseSchema
workFolder <- 'c:/temp/dqd'   #this folder must exist (use forward slashes)

#just a helper construct to package all study details into one object (similar as done with database details)
connectionDetails2<-DataQuality:::.createConnectionDetails2(cdmDatabaseSchema = cdmDatabaseSchema
                                              ,resultsDatabaseSchema = resultsDatabaseSchema
                                              ,workFolder = workFolder
                                              )

#execute the core of the DQD helper analysis, outptu will be writen as CSV file into an export folder

DataQuality::dashboardLabThresholds(connectionDetails = connectionDetails,connectionDetails2 = connectionDetails2)

#results will be in workfolder, subfolder export (will be zipped once we have more output)


#testing on other database
#load your environment 
cdmDatabaseSchema='gpc'
#cdmDatabaseSchema='onek'
vocabularyDatabaseSchema='vocab'
resultsDatabaseSchema=resultDatabaseSchema=cohortDatabaseSchema='gpc_results'
#resultDatabaseSchema=cohortDatabaseSchema='onek_results'

#cohortTable='deletecohort'
cohortTable='cohort'


source('MoreOhdsiRTools.R')
library(DatabaseConnector)

connectionDetails<-createConnectionDetails(dbms='postgresql',user=user,password=pw,server=server
                                           ,schema = cdmDatabaseSchema)
#DatabaseConnector::disconnect(connection)
connection <-connect(connectionDetails)

#(tables<-getTableNames(connection,cdmDatabaseSchema))
dashboardLabThresholds(connectionDetails = connectionDetails,connectionDetails2 = connectionDetails2)
