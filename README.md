# Data quality

This is an R package that has sume utilities and also supports  informatics studies that focuses on data quality (rather than a clinical question). It may also support development of other tools.


# Support development of Data Quality Dashboard (DQD)

Package has functions that support addition of more rules and knowledge base items to the Data Quality Dashboard. For example, we want to compute 3rd and 97th percentile (or other parameters) for some lab results to use those as thresholds. We also analyze what value_as_concept_ids are used with specific measurement_concept_ids to help OHDSI network to standardize value_as_concept_id for critical measurements.

A site can participate on developing knowledbe base for DQD. Or it can take part on format research study (to be published). Se [extras/protocol](extras/protocol) folder for protocol. 

To run the study, see DQD section in file [extras/CodeToRun.R](extras/CodeToRun.R)


# Versions

## v4.1
Simplified release for DQD purpose of the package  


## v5.0
Addition of functionality to analyze value_as_concept_ids  


