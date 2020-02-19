#' @file CodeToRun.R


#DQD section-----------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


#---LOAD THE R PACKAGE
#install the latest version of the DataQuality package  
#it requires devtools library installed -  install it first if you don't have it
devtools::install_github("vojtechhuser/DataQuality")


#load the package 
library(DataQuality)


#---SPECIFY YOUR LOCAL PARAMTERS BELOW-----
#replace with your local parameters using the same variable names
#note: it will run some achilles measures (so it may overwrite your current achilles results table)


# Specify details for connecting to the server:
# See ?DatabaseConnector::createConnectionDetails for help

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
								server = "some.server.com/ohdsi",
								user = "",
								password = "")



cdmDatabaseSchema <-'main'
resultsDatabaseSchema <-'main' #at most sites this likely will not be the same as cdmDatabaseSchema
workFolder <- 'c:/temp/dqd'   #this folder must exist (use forward slashes)

#just a helper construct to package all study details into one object (similar as done with database connection details)
connectionDetails2<-DataQuality:::.createConnectionDetails2(cdmDatabaseSchema = cdmDatabaseSchema
                                              ,resultsDatabaseSchema = resultsDatabaseSchema
                                              ,workFolder = workFolder
                                              )


#----EXECUTE THE DQD THRESHOLD STUDY
#execute the core of the DQD helper analysis, outptu will be writen as CSV file into an export folder

DataQuality::dashboardLabThresholds(connectionDetails = connectionDetails,connectionDetails2 = connectionDetails2)

#results are in workfolder, subfolder export, inspect it and 
#email the CSV file called Thresholds to the study PI

#for simplicity, the package is not using submission of the results via OHDSI AWS infrustructure 
#(if requested by site, the code can be provided, though)


