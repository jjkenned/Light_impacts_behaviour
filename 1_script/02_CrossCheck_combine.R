############################################################
#### Combine and crosscheck recordings from databases  ##### 
############################################################

dev.off()
rm(list=ls())

# library packages that you will need
library(lubridate)

# Crosschecking with a reference list and keeping only those from each database
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

# start by reading in each file and labelling from each machine