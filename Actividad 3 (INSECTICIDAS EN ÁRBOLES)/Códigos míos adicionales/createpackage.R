if(!require("devtools")){install.packages("devtools")}
if(!require("roxygen2")){install.packages("roxygen2")}

library(devtools)
create("balancset_david")

library(roxygen2)
roxygen2::roxygenise("balancset_david")

setwd("balancset_david")
devtools::install()
