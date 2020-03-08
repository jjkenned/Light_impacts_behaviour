######################################################################################
############### trimmed set of functions for analyzing BOOW and GHOW data ############ 
######################################################################################

dev.off()
rm(list=ls())

library(tidyverse)

# read in location data
##### set ID for working directories to be used during analysis #### 
# Much of this data came from thesis processing steps in other repo and project
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


##### read in frame data #####
loc<-read.csv("2_pipeline/int_data/site_loc.csv") # site specific GPS data
base<-read.csv("2_pipeline/int_data/thesis_base.csv") # base data
songcounts<-read.csv("2_pipeline/int_data/thesis_song_counts.csv") # song count data 
polute<-read.csv("2_pipeline/int_data/light_polution.csv") # light polution data 


#### data should be formatted thusly ####
# loc should have 3 columns (site,lat,lon)
# base is a direct readin and re-format from database using the winbows R side

# pre-format loc
loc$site<-as.character(loc$site)
# loc$lat<-as.numeric(loc$lat) # usually not needed, but here if you need it
# loc$lon<-as.numeric(loc$lon) # usually not needed, but here if you need it

# base should have 8 columns ("BASE_ID", "Directory", "Image", "Path", "site", "Processed", "Image_time", "Image_date")
# should use date and time-appropriate formats

base$Image_date<-as.Date(as.character(base$Image_date), format="%Y-%m-%d") # day by day
base$Image_time<-as.POSIXct(as.character(base$Image_time)) # time to time
# all others are factors and integers where appropriate 


# now that everything is ready to go, let's write some functions
sun.sets.func<-function(base,loc){
  site.date<-unique(base[c("site","Image_date")])
  
  # Add lat long for all of this
  
  for (i in 1:nrow(site.date)){
    site.date$lat[i]<-loc[loc$site==site.date$site[i],]$lat
    site.date$lon[i]<-loc[loc$site==site.date$site[i],]$lon
  }
  
  # convert colname to date
  colnames(site.date)<-c("site","date","lat","lon")
  
  # now combine these to calculate sun and mooon values
  
  for (i in 1:nrow(site.date)){
    # Everything we are calculating is relative to the start of the period in the evening and end of period in the morning
    # start with civil sunrise and sunset
    civil.rise<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$sunrise)
    civil.set<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$sunset)
    
    # nautical sunrise and sunset
    naut.rise<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$dawn)
    naut.set<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$dusk)
    
    # astronomical sunrise and sunset
    astro.rise<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$nauticalDawn)
    astro.set<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$nauticalDusk)
    
    # Night start and end
    night.end<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$nightEnd)
    night.start<-(getSunlightTimes(date = as.Date(site.date$date[i]), tz="MST", lat = site.date$lat[i], lon = site.date$lon[i])$night)
    
    

    # Now add all of this to your dataframe
    # Civil
    site.date$civil.rise[i]<-(format(civil.rise, format = "%Y-%m-%d %H:%M:%S"))
    site.date$civil.set[i]<-(format(civil.set, format = "%Y-%m-%d %H:%M:%S"))
    
    # nautical
    site.date$naut.rise[i]<-(format(naut.rise, format = "%Y-%m-%d %H:%M:%S"))
    site.date$naut.set[i]<-(format(naut.set, format = "%Y-%m-%d %H:%M:%S"))
    
    # astronomical
    site.date$astro.rise[i]<-(format(astro.rise, format = "%Y-%m-%d %H:%M:%S"))
    site.date$astro.set[i]<-(format(astro.set, format = "%Y-%m-%d %H:%M:%S"))
    
    # start end
    site.date$night.end[i]<-(format(night.end, format = "%Y-%m-%d %H:%M:%S"))
    site.date$night.start[i]<-(format(night.start, format = "%Y-%m-%d %H:%M:%S"))
    
    
    if (i%%10==0){
      print(i)
    }
  }
  
  return(site.date)
} # function for obtaining crepuscular time periods
song.mean.func<-function(base,songcounts,polute){
  
  # This analysis is going to attempt to account for individual
  colnames(base)
  keep<-c("BASE_ID","Image","site","Image_time","Image_date")
  base<-base[keep]
  
  # songcounts, get rid of non strong songs and unnecissary7 columns
  songs<-songcounts[songcounts$Strongest==1,]
  colnames(songs)
  keep<-c("BASE_ID","Species","Individual")
  songs<-songs[keep]
  
  # Song counts for each individual in each site frame
  counts<-songs %>% 
    group_by(BASE_ID,Species,Individual) %>% 
    summarise(songcount = n())
  
  # Now we are going to make some mean values
  frame.count<-counts %>% 
    group_by(BASE_ID,Species) %>%
    summarise(mean.count = mean(songcount))
  
  # Now we can make a BOOW and GHOW count
  out<-spread(frame.count,Species,mean.count)
  
  # and combine with main dataset
  df<-merge(base,out,by="BASE_ID",all = T)
  df[is.na(df)]<-0 # convert NAs to 0s
  
  
  
  # Now we can combine these things with sunrise sunset times
  # double check that the dates in time field are the same as the date field
  # assign better cal name for date and time
  colnames(df)<-c("BASE_ID","Image","site","time","date","BOOW","GHOW")
  
  
  # as date and time conversions and 
  df$date<-as.Date(as.character(df$date), format="%Y-%m-%d")
  df$time<-as.POSIXct(as.character(df$time), format="%Y-%m-%d %H:%M:%S")
  suntimes$site<-as.character(suntimes$site)
  df$site<-as.character(df$site)
  suntimes$date<-as.Date(as.character(suntimes$date), format="%Y-%m-%d")
  suntimes<-suntimes[suntimes$site %in% df$site,] # keep only the site data you need 
  
  
  
  # convert All time into posixct format
  # change range of j for number of columns with times in them. Maybe add more for moons
  for (j in 5:12){
    suntimes[,j]<-as.POSIXct(as.character(suntimes[,j]), format="%Y-%m-%d %H:%M:%S", tz="MST")
  }
  
  
  
  
  # Next step is to sort into calling periods
  print("group into crepuscular time periods")
  # Group by site 
  for (i in 1:nrow(df)){
    
    # Identify what time and location values are
    site<-df$site[i] # recording site
    time<-df$time[i] # recording time/date
    
    # group by site
    sun<-suntimes[suntimes$site==site,]
    sun<-sun[,5:12]
    
    # new matrix for keeping difference in times 
    cros.ref<-data.frame(matrix(NA,nrow=nrow(sun),ncol = ncol(sun)))
    colnames(cros.ref)<-colnames(sun) # keep colnames the same for ease
    
    # loop through matrix to calculate the time difference between all of these
    
    for (j in 1:nrow(sun)){
      for (k in 1:ncol(sun)){
        cros.ref[j,k]<-difftime(time, sun[j,k], units = "secs")
        
      }
    }
    
    
    # now find the location of the min value (positive) to give which section it's in 
    ref<-as.data.frame(which(cros.ref==min(cros.ref[cros.ref>=0]),arr.ind = T))
    
    df$group[i]<-colnames(cros.ref[ref$col])# group assigmnet
    
    if (i%%100==0){
      print(i)
    }
    
  }
  
  
  # let's rename these bad boys so that we don't have to worry about getting confused
  df$time.frame<-NA
  df[df$group=="civil.rise",]$time.frame<-"day"
  df[df$group=="civil.set",]$time.frame<-"civil"
  df[df$group=="naut.rise",]$time.frame<-"civil"
  df[df$group=="naut.set",]$time.frame<-"nautical"
  df[df$group=="astro.rise",]$time.frame<-"nautical"
  df[df$group=="astro.set",]$time.frame<-"astronomical"
  df[df$group=="night.end",]$time.frame<-"astronomical"
  df[df$group=="night.start",]$time.frame<-"night"
  
  
  # get moon angle
  print("moon height and fraction calculation")
  for (i in 1:nrow(df)){
    
    # Need to reference location for each station
    lat<-loc[loc$site==df$site[i],]$lat
    lon<-loc[loc$site==df$site[i],]$lon
    
    # get height (if negative we can make other value zero)
    height<-(getMoonPosition(date = as.POSIXct(df$time[i], tz), lat = lat, lon = lon)$altitude)
    
    if (height>0){
      # fraction illuminated
      illum<-(getMoonIllumination(date = df$time[i])$fraction)
      df$moon.illum[i]<-illum
    } else {
      df$moon.illum[i]<-0
    }
    
    
    if (i%%100==0){
      print(i)
    }
  }
  
  # round moon illumination values
  df$moon.round<-round(df$moon.illum, digits = 1)
  
  # site polution values merge
  full<-merge(df,polute,by="site")

  # return values
  return(full)
  
  
  
} # get mean song counts for all 
night.analysis<-function(full){
  
  # is owls?
  present<-data.frame(site=unique(full$site))
  i=1
  for (i in 1:nrow(present)){
    boow<-full[full$site==present$site[i] & full$BOOW>0,] # number of frames in the site that have boow
    ghow<-full[full$site==present$site[i] & full$GHOW>0,] # number of frames in the site that have ghow
    present$boow[i]<-nrow(boow) # add count to present frame
    present$ghow[i]<-nrow(ghow) # add count to present frame
  }
  
  # now we add a column to the dataframe to specify what the minimum number of detections for including these birds will be
  for (i in 1:nrow(full)){
    boow<-present[full$site[i]==present$site,]$boow
    ghow<-present[full$site[i]==present$site,]$ghow
    
    if (boow>10){
      full$boow.inc[i]<-1
    } else {full$boow.inc[i]<-0}
    
    if (ghow>10){full$ghow.inc[i]<-1
    } else {full$ghow.inc[i]<-0}
    
    if (i%%100==0){
      print(i)
    }
    
    
  }
  
  # remove non-night times
  night<-full[full$time.frame=="night",]
  return(night)
  
} # get rid of crepuscular and dial time periods


# Apply functions in this order as some outputs are required for next to run
suntimes<-sun.sets.func(base = base,loc = loc)
full<-song.mean.func(base = base, songcounts = songcounts, polute = polute)
night<-night.analysis(full)

# load R data for keeping in normal
# save(full,night,file = "2_pipeline/int_data/yiyang_lightpolution_data.Rdata")

load(file = "2_pipeline/int_data/yiyang_lightpolution_data.Rdata")


ggplot(dat=boow, aes(x=date, y=BOOW, color=moon.illum)) +
  geom_point()

plot(full$BOOW~full$moon.illum)
line(full$BOOW~full$moon.illum)

# run analysis
# parse out species-specific sites
boow<-night[night$boow.inc==1,]
ghow<-night[night$ghow.inc==1,]

# build models




# start with BOOW

# lms (using )
fit.boow.null<-lm(BOOW ~ moon.illum, data = boow)
fit.boow.polut<-lm(BOOW ~ moon.illum*light.polute, data = boow)

# likelihood ratio test
LL.polute<-as.numeric(logLik(fit.boow.polut))
LL.null<-as.numeric(logLik(fit.boow.null))
test.st<-(2*LL.polute-2*LL.null)
p.val<-pchisq(test.st, df=2, lower.tail=FALSE)

# shall we plot? 

plot(BOOW ~ moon.illum, data = boow[boow$light.polute>0.7,],
     xlab="Illuminated Fraction of Moon", ylab="BOOW Song Count (songs/20sec)",
     col="darkgreen", pch=4, cex=0.7)

points(BOOW ~ moon.illum, data = boow[boow$light.polute<0.7,],
     col="darkred", cex=0.7)


# plot this shit up! 
B0=summary(fit.boow.polut)$coefficients[1] # beta not
B1=summary(fit.boow.polut)$coefficients[2] # length parameter
B2=summary(fit.boow.polut)$coefficients[3] # infestation parameter
B3=summary(fit.boow.polut)$coefficients[4] # interaction parameter

int.null=B0 # intercept for 0 infestation
slope.null=B1 # slope for 0 infestation
int.inf=B0+B2 # intercept for 1 infestation
slope.inf=B1+B3 # slope for 1 infestation 
abline(a=int.null, b=slope.null, col="darkred",lwd=2) # fit line for inf=0
abline(a=int.inf, b=slope.inf, col="darkgreen",lwd=2) # fit line for inf=1








# GHOW lms
# lms (using )
fit.ghow.null<-lm(GHOW ~ moon.illum, data = ghow)
fit.ghow.polut<-lm(GHOW ~ moon.illum*light.polute, data = ghow)

# likelihood ratio test
LL.polute<-as.numeric(logLik(fit.ghow.polut))
LL.null<-as.numeric(logLik(fit.ghow.null))
test.st<-(2*LL.polute-2*LL.null)
p.val<-pchisq(test.st, df=2, lower.tail=FALSE)

# shall we plot? 

plot(GHOW ~ moon.illum, data = ghow[ghow$light.polute>0.7,],
     xlab="Illuminated Fraction of Moon", ylab="GHOW Song Count (songs/20sec)",
     col="darkgreen", pch=4, cex=0.7)

points(GHOW ~ moon.illum, data = ghow[ghow$light.polute<0.7,],
       col="darkred", cex=0.7)


# plot this shit up! 
B0=summary(fit.ghow.polut)$coefficients[1] # beta not
B1=summary(fit.ghow.polut)$coefficients[2] # length parameter
B2=summary(fit.ghow.polut)$coefficients[3] # infestation parameter
B3=summary(fit.ghow.polut)$coefficients[4] # interaction parameter

int.null=B0 # intercept for 0 infestation
slope.null=B1 # slope for 0 infestation
int.inf=B0+B2 # intercept for 1 infestation
slope.inf=B1+B3 # slope for 1 infestation 
abline(a=int.null, b=slope.null, col="darkred",lwd=2) # fit line for inf=0
abline(a=int.inf, b=slope.inf, col="darkgreen",lwd=2) # fit line for inf=1





# now GLMs

fit.boow.null<-glm(BOOW ~ moon.illum, data = boow,)
fit.boow.polut<-glm(BOOW ~ moon.illum*light.polute, data = boow,)

# likelihood ratio test
LL.polute<-as.numeric(logLik(fit.boow.polut))
LL.null<-as.numeric(logLik(fit.boow.null))
test.st<-(2*LL.polute-2*LL.null)
p.val<-pchisq(test.st, df=2, lower.tail=FALSE)








