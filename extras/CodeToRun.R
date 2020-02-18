#' @file CodeToRun.R


#DQD section-----------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


#---LOAD THE R PACKAGE
#uncomment the line below to install the latest version of the code and package
#devtools::install_github("vojtechhuser/DataQuality")



#load the package 
library(DataQuality)


#---SPECIFY YOUR LOCAL PARAMTERS BELOW-----
#replace with your local parameters using the same variable names
#note: it will run some achilles measures (so it may overwrite your current achilles results table)
connectionDetails<-Eunomia::getEunomiaConnectionDetails() 
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
#email the CSV file Thresholds to the study PI


