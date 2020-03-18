# Data Quality Dashboard: Thresholds

This folder contains additional results.

# S01

This is subset of the benchmark knowledge base (KB) (only frequent lab test were exported in this subset). This file lists median values for min, max, p01 (percentile 1), p99, median standard deviation. It provides benchmark values for p01 and p99. 

If given OMOP dataset has significantly different values for p01 and p99 (e.g., not within standard deviation of benchmark values), it may indicate a potential data quality (DQ) issue.

Additional other methodologies are possible to do DQA with the KB.

# S02

View by lab test. One row per lab test. File is ordered by listing test by descending frequency in the network aggregate view (from most frequent on top to least frequent at the bottom). It lists units for that lab test (separated by pipes [if multiple]) 

# PCORnet code
This file allows use of KB on PCORNet formatted data. Use dbplyr to access tables as R objects.

# terms used
- benchmark KB = has content only if 2+ sites contribute thresholds
- full KB = provides thresholds even if they come from a single site (is bigger than benchmark)


# Explanation of other folders and functions in this repo/package

The DataQuality package and repo has other content that supported prior projects in Data Quality. DQD results and content is marked by presence of dqd in folder name or function name. 

