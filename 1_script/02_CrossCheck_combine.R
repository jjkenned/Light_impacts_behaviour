############################################################
#### Combine and crosscheck recordings from databases  ##### 
############################################################

dev.off()
rm(list=ls())

# library packages that you will need
library(lubridate)
library(tidyverse)

# Crosschecking with a reference list and keeping only those from each database
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

# start by reading in each file and labelling from each machine
procd<-read.csv("2_pipeline/int_data/Processing.csv")

# read in data from other ODBC output
base.1<-read.csv("2_pipeline/int_data/thesis_base_Mar7_MACHINE01.csv") # baseframe data for database 01
base.2<-read.csv("2_pipeline/int_data/thesis_base_Mar7_MACHINE02.csv") # baseframe data for database 02
detec.1<-read.csv("2_pipeline/int_data/thesis_song_counts_Mar7_MACHINE01.csv") # detection data for database 01
detec.2<-read.csv("2_pipeline/int_data/thesis_song_counts_Mar7_MACHINE02.csv") # detection data for database 02

# format some columns
procd$site<-as.character(procd$site)
procd$date<-as.Date(procd$date,format = "%d-%b-%y")
procd<-procd[!is.na(procd$ODBC),]

# Keep the sites listeneed to by myself
procd.1<-procd[procd$ODBC==1 & procd$Completed=="X",]


# Start with Base 
# create empty dataframe with same names to fill

base.main.1<-as.data.frame(matrix(data = NA,nrow = 0,ncol = ncol(base.1)))
colnames(base.main.1)<-colnames(base.1)

# we need to do via loop
for (i in 1:nrow(procd.1)){
  temp<-procd.1[i,]
  dat<-base.1[base.1$site==temp$site,]
  out<-dat[
    as.Date(dat$Image_date, format="%Y-%m-%d")==as.Date(temp$date, format="%Y-%m-%d") |
      as.Date(dat$Image_date, format="%Y-%m-%d")==as.Date(temp$date, format="%Y-%m-%d")-1,]
  base.main.1<-rbind(base.main.1,out)
  
  
}


# check anything that isn't complete and address 
base.main.1[!base.main.1$Processed==1,]
# once checked just convert to get rid of anything that isnt completed
comb.1<-base.main.1[base.main.1$Processed==1,]

# we all good so let's move on
# now we will work with '*.2' files 
# Keep the sites listeneed to by myself
procd.2<-procd[procd$ODBC==2 & procd$Completed=="X",]

# Now we can get the main database values from the ones that should have been listend to 
# we'll then check that they've been propperly listened to 

# Start with Base 
# create empty dataframe with same names to fill

base.main.2<-as.data.frame(matrix(data = NA,nrow = 0,ncol = ncol(base.2)))
colnames(base.main.2)<-colnames(base.2)



# we need to do via loop
for (i in 1:nrow(procd.2)){
  temp<-procd.2[i,]
  dat<-base.2[base.2$site==temp$site,]
  out<-dat[
    as.Date(dat$Image_date, format="%Y-%m-%d")==as.Date(temp$date, format="%Y-%m-%d") |
      as.Date(dat$Image_date, format="%Y-%m-%d")==as.Date(temp$date, format="%Y-%m-%d")-1,]
  base.main.2<-rbind(base.main.2,out)
  
  
}


# check anything that isn't complete and address 
base.main.2[!base.main.2$Processed==1,]
table(as.character(base.main.2[base.main.2$Processed==0 & base.main.2$site=="OWGR-015",]$Image_date))


check_comp <- base.2[base.2$Processed==1,] %>% group_by(site,Image_date) %>% 
  count(Processed)

check_incomp <- base.2 %>% group_by(site,Image_date) %>% 
  summarise(Comp=sum(which(Processed==1)),
            inComp=sum(which(Processed==0)))



# now we've checked need to get rid of incompleted values
comb.2<-base.main.2[base.main.2$Processed==1,]

##### Combine and Export #######



# sbase to start
obs.1<-detec.1[detec.1$BASE_ID %in% comb.1$BASE_ID,]
obs.2<-detec.2[detec.2$BASE_ID %in% comb.2$BASE_ID,]

observations<-rbind(obs.1,obs.2)

# now bind the base data and read out
final.base<-rbind(comb.1,comb.2)

# real quick check for problematic incompletions
final.base[final.base$BASE_ID %in% observations$BASE_ID & !final.base$Processed==1,]$BASE_ID
# final.base<-final.base[final.base$Processed==1,]
getwd()
write.csv(final.base,file = "2_pipeline/int_data/thesis_base_Mar8_BOTH.csv",row.names = F)
write.csv(observations,file = "2_pipeline/int_data/thesis_song_counts_Mar8_BOTH.csv",row.names = F)







