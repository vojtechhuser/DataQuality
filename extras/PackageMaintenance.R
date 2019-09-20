#tbd
library(devtools)
use_package('magrittr')
use_package('Achilles')
use_package('OhdsiSharing')



devtools::install_github("r-lib/pkgdown")
pkgdown::build_site()
#remove.packages(xml2)
