% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DatasetDQ.R
\name{createMIAD}
\alias{createMIAD}
\title{Level one data about dataset MIAD stands for minimum information about a dataset
percentage measures are used that hide exact patient counts}
\usage{
createMIAD(
  connectionDetails,
  cdmDatabaseSchema,
  workDatabaseSchema = cdmDatabaseSchema,
  outputFolder,
  level = 3
)
}
\arguments{
\item{connectionDetails}{connection}

\item{cdmDatabaseSchema}{schema}

\item{workDatabaseSchema}{work schema}

\item{outputFolder}{where to put output}

\item{level}{1 2 or 3, indicates level of detail}
}
\description{
Level one data about dataset MIAD stands for minimum information about a dataset
percentage measures are used that hide exact patient counts
}
