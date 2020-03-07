#######################################################
#### Read in and format data on Windows side only ##### 
#######################################################

dev.off()
rm(list=ls())

# library packages that you will need
library(RODBC)
library(lubridate)

# This code is for pulling songs from 2-3 databases and keeping only the data that has been processed
# Crosschecking with a reference list and keeping only those from each database

knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


# define path of DB1 location 
db<-"C:/Users/jeremiahkennedy/Desktop/OWL_VISUALIZATION_DATABASE_ACTIVE.accdb"
con<-odbcConnectAccess2007(db) # connect via access 


# Read frame data in 
basedata<-sqlFetch(con,"tblBASE")


# Read detection data in
detections<-sqlFetch(con, "tblPANELS")

# you should now be able to close the connection as you have the data read in
# i would do this in order to get rid of the possibility that you accidently change things in the database
odbcCloseAll()


# Convert date within image_time to be correct date from Image_date 
# this uses lubridate
date(basedata$Image_time)<-basedata$Image_date

# identify fields you want to keep
keep<-c("BASE_ID","Directory","Image","Path","site","Processed","Image_time","Image_date")
base<-basedata[keep] # keep columns we want
base$site<-as.character(base$site)

# do the same for detection data
keep<-c("BASE_ID", "Species", "Panel", "Individual", "Strongest")
owls<-detections[keep]

# Write Both in csv with label for which machine this was taken from 
write.csv(final.base,file = "2_pipeline/int_data/thesis_base_Mar7_MACHINE01.csv",row.names = F)
write.csv(observations,file = "2_pipeline/int_data/thesis_song_counts_Mar7_MACHINE01.csv",row.names = F)










