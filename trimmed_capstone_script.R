#I have put all chunks of code (until near the end) into functions with no arguments or return
#values so that all of the code can be run from a "control panel" at the end.
#Select all code up until beginning of SECTION 13 and run it, then can use control panel.
#The code is organized into the following order (I will mark the beginnings by SECTION 1, 
#SECTION 2, etc) :
#1. Install all needed packages and put them into the library, set the default
#   leaflet m (for making maps) and import needed data frames
#2. Record peak data for Maine ebird with manually counted peaks
#3. Create maps from this data
#4. Create data frames to record time at which 50% of the fall traffic through each site
#   (with enough data, which is a minority of the data) occurs, and another data frame for
#   the count of birds that fall up to that point
#5. Create maps from these data frames
#6. Carry out 50% fall traffic analysis for the setophaga genus (warblers) in Maine, whose count is about
#   5 times greater than that of all raptors combined
#7. Make maps from these data frames
#8. Extract date and magnitude of fall peak at each Hawk watch sites
#   Note: due to formatting differences between the Hawk Watch data, some of this extraction was done manually, although
#   most was done using code
#9. Create maps from these data frames
#10. At each Hawkwatch site, record the date of peak fall migration going back
#   as many years as there is reliable data. For each site, next create
#   scatter plots showing year versus day of peak fall migration. This is a different
#   order than I had been doing before, but there wasn't very much data processing
#   to do for this section, so the plots are put with the data processing.
#11. Show correlation between the peak date of Acadia Hawk Watch (in Maine) from 2010 to 2013
#   and the peak of the ebird region surrounding the park from 2010 to 2013
#12. Control panel
#13. Various functions and lines of code I used to get results manually along the
#   way, including the "ImageMagick" functions I use to create GIFs from the maps
#   I created GIFS by creating PNG files from each map and stringing them together.
#   This must be done manually, so I put this code at the end


#SECTION 1

#only install if package has never been installed

#install.packages("animation")
require(animation)
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
#install.packages("rgdal")
library(rgdal)
#install.packages("sp")
library(sp)
#install.packages("dygraphs")
library(dygraphs)
#install.packages("png")
library(png)
library(lubridate)
if(!require(devtools)) install.packages("devtools")
if(!require(leaflet)) install_github("rstudio/leaflet")

#install.packages("calibrate")
library(calibrate)


(m <- leaflet() %>% addTiles()) 

m %>% setView(lng = -68.25, lat = 44.25, zoom = 7) %>%
addMarkers(-68.2733,44.3386) %>%


maine <- read.csv("~/maine.csv")

acadia <- read.csv("~/Acadia.csv")
acadia$year <- as.integer(format(as.Date(acadia$Date, '%d-%b-%Y'),'%Y'))
acadia$Date <-  as.Date(acadia$Date, '%d-%b-%Y')
acadia$Date

#This is the set of latitude/longitude 0.25 degree latitude x 0.25 degree longitude "squares"
#in Maine that have ebird data
x <-  unique( cbind(maine$latitude, maine$longitude))

#################################################################################

#  SECTION 2

#The data are grouped into 0.25 degree latitude by 0.25 degree longitude (curved) "squares".
#The data John Cardente gave me came with all latitudes and longitudes rounded to the nearest quarter.
#I left the data this way, since the ebird data needed to be spatially aggregated in order for 
#any of the "squares" to have enough data to show any patterns.

#These are the dates and number of birds observed on the peak fall migration day in all ebird
#locations that I deemed to have a sufficient amount of data to unambiguously choose a peak.
#I made scatterplots of the number of raptors counted on each day at each location that had
#at least 50 days with observations. Many of these scatterplots did not have discernible peaks.
#The below data are the manually chosen locations each year in which I was able to discern a peak.
#This data might be difficult to reproduce, since my criteria were somewhat subjective.
#However, writing an algorithm to find peaks for me with noisy data did not seem plausible for a
#two-week project.
#Later, I looked at locations and years in ebird raptor data to find the date at which at least
#50% of the year's total raptor count has been observed. This criterion could be analyzed without
#resorting to manual methods, and thus is easily reproducible.

#Different years contain different numbers of locations.

manualpeaks2010 <- data.frame(latitude = double(), longitude = double(),
                              endday = double(), size = integer())

manualpeaks2010<- rbind(manualpeaks2010, c(44.5,-68.75,220,6,0))
manualpeaks2010<- rbind(manualpeaks2010, c(44,-68.75,206,4,0))
manualpeaks2010<- rbind(manualpeaks2010, c(43.5,-70.75,272,13,0))
manualpeaks2010<- rbind(manualpeaks2010, c(44.25,-68.25,264,59,0))
manualpeaks2010<- rbind(manualpeaks2010, c(43.25,-70.5,248,6,0))
manualpeaks2010<- rbind(manualpeaks2010, c(43.75,-69.25,284,127,0))
manualpeaks2010<- rbind(manualpeaks2010, c(44.75,-68.75,250,5,0))
manualpeaks2010<- rbind(manualpeaks2010, c(44.25,-69,196,12,0))
manualpeaks2010<- rbind(manualpeaks2010, c(43.75,-69.75,264,17,0))
manualpeaks2010<- rbind(manualpeaks2010, c(43.75,-70,206,12,0))
manualpeaks2010<- rbind(manualpeaks2010, c(44,-69.75,264,4,0))
manualpeaks2010<- rbind(manualpeaks2010, c(43.5,-70.25,258,26,0))
manualpeaks2010<- rbind(manualpeaks2010, c(44.3386,-68.2733,254,449,1))

manualpeaks2010 <- manualpeaks2010[order(manualpeaks2010[3]),] 


manualpeaks2011 <- data.frame(latitude = double(), longitude = double(),
                              endday = double(), size = integer())

manualpeaks2011<- rbind(manualpeaks2011, c(43.5,-70.75,306,8,0))
manualpeaks2011<- rbind(manualpeaks2011, c(43.75,-69.25,270,50,0))
manualpeaks2011<- rbind(manualpeaks2011, c(43.75,-69.75,280,22,0))
manualpeaks2011<- rbind(manualpeaks2011, c(44.75,-69.5,238,8,0))
manualpeaks2011<- rbind(manualpeaks2011, c(43.75,-70.25,230,26,0))
manualpeaks2011<- rbind(manualpeaks2011, c(43.5,-70.5,278,10,0))
manualpeaks2011<- rbind(manualpeaks2011, c(44,-69.75,206,5,0))
manualpeaks2011<- rbind(manualpeaks2011, c(43.5,-70.25,236,17,0))
manualpeaks2011<- rbind(manualpeaks2011, c(44.3386,-68.2733,253,313,1))


manualpeaks2011 <- manualpeaks2011[order(manualpeaks2011[3]),] 


manualpeaks2012 <- data.frame(latitude = double(), longitude = double(),
                              endday = double(), size = integer())

manualpeaks2012<- rbind(manualpeaks2012, c(44.5,-68.75,224,4,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.75,-69.25,240,4,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.5,-69,248,5,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.25,-68.5,230,11,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44,-69.5,242,20,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44,-68.75,204,13,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.5,-70,202,6,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.5,-70.75,296,15,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.25,-68.25,238,40,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.25,-70.5,280,13,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.75,-69.25,282,140,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.75,-68.75,334,9,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.75,-69.75,280,21,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.5,-69.25,232,11,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44,-69.25,262,6,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.75,-69.5,232,12,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44.75,-69.5,248,8,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.75,-70.25,264,17,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.75,-70,230,10,0))
manualpeaks2012<- rbind(manualpeaks2012, c(44,-69.75,280,9,0))
manualpeaks2012<- rbind(manualpeaks2012, c(43.5,-70.25,254,19,0))      
manualpeaks2012<- rbind(manualpeaks2012, c(44.3386,-68.2733,255,766,1))      

manualpeaks2012 <- manualpeaks2012[order(manualpeaks2012[3]),] 
manualpeaks2012


manualpeaks2013 <- data.frame(latitude = double(), longitude = double(),
                              endday = double(), size = integer())

manualpeaks2013<- rbind(manualpeaks2013, c(44.25,-69,224,11,0))
manualpeaks2013<- rbind(manualpeaks2013, c(43.75,-69.25,228,22,0))
manualpeaks2013<- rbind(manualpeaks2013, c(44,-69.25,230,12,0))
manualpeaks2013<- rbind(manualpeaks2013, c(44.25,-68.75,236,20,0))
manualpeaks2013<- rbind(manualpeaks2013, c(43.5,-70.25,238,17,0))
manualpeaks2013<- rbind(manualpeaks2013, c(44.25,-68.5,242,7,0))
manualpeaks2013<- rbind(manualpeaks2013, c(43.75,-70.25,242,24,0))
manualpeaks2013<- rbind(manualpeaks2013, c(44.5,-69,250,5,0))
manualpeaks2013<- rbind(manualpeaks2013, c(44.25,-68.25,258,48,0))
manualpeaks2013<- rbind(manualpeaks2013, c(43.75,-69.25,270,160,0))
manualpeaks2013<- rbind(manualpeaks2013, c(43.5,-70.5,280,6,0))
manualpeaks2013<- rbind(manualpeaks2013, c(43.75,-70,286,11,0))
manualpeaks2013<- rbind(manualpeaks2013, c(44.3386,-68.2733,259,431,1))

manualpeaks2013 <- manualpeaks2013[order(manualpeaks2013[3]),] 
manualpeaks2013


####################################################################

# SECTION 3

#The following functions plot, for each site in each year it is included, the date at which at
#least 50% of that year's birdcount has been counted (this can only be determined in retrospect).
#The first frame represents the earliest date in a given year, the second frame the next date in that
#year, etc. The actual number of birds observed at each site is represented by the area of the circle
#at the site. If my hypothesis, that a corridor of bird migration in a SSW direction in the fall 
#should be discernible from the timing of 50% annual migration, is correct, then clicking forward
#on the maps (moving forward in time) should show a motion in the SSW motion of the circles.
#Unfortunately this was not observed.

#I ended up writing a function for each year because when I tried to put each year's maps
#as elements of a list, the indexing got too confusing and I couldn't get lapply to work. I 
#had to use lapply because for loops and while loops were unable to run the leaflet package successfully.
#I did the same in every section in which I made maps.

ebird_run2010 <- function() {
map2010 <- function(i) {
  if(manualpeaks2010[i,5]==0) {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>% # map location
      addMarkers(-68.25, 44.25) %>% # add a marker
      addPopups(-68.25, 45.25, popup = paste("ebird: Year = 2010, Day = ",
                                             manualpeaks2010[i,3], ", Count = ", manualpeaks2010[i,4])) %>% # popup
      addCircles(color = "black", lng = manualpeaks2010[i,2],
                 lat = manualpeaks2010[i,1], radius = 2500*sqrt(manualpeaks2010[i,4]))
  #The 2500 factor for the radius was chosen by trial and error to give circles that are
  #neither too large nor too small
  }
  
  else {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("Year = 2010, Day = ",manualpeaks2010[i,3],
                                             ", Count = ", manualpeaks2010[i,4])) %>% 
      addCircles(color = "red", lng = manualpeaks2010[i,2],
                 lat = manualpeaks2010[i,1], radius = 2500*sqrt(manualpeaks2010[i,4]))
  }
  return(m2)
}

#Have to use lapply because for and while loops are
#unable to access leaflets successfully

function2 <- function(k) {
  return(lapply(seq(1,dim(manualpeaks2010)[1],1), map2010)[[k]])
}

lapply(1:dim(manualpeaks2010)[1], function2)
}

ebird_run2011 <- function() {
  
map2011 <- function(i) {
  if(manualpeaks2011[i,5]==0) {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>%
      addMarkers(-68.25, 44.25) %>%
      addPopups(-68.25, 45.25, popup = paste("ebird: Year = 2011,
                         Day = ", manualpeaks2011[i,3], ", Count = ", manualpeaks2011[i,4])) %>%
      addCircles(color = "black", lng = manualpeaks2011[i,2],
                 lat = manualpeaks2011[i,1], radius = 2500*sqrt(manualpeaks2011[i,4]))
  }
  
  else {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>%
      addMarkers(-68.25, 44.25) %>%
      addPopups(-68.25, 45.25, popup = paste("Year = 2011, Day = ",manualpeaks2011[i,3],
                                             ", Count = ", manualpeaks2011[i,4])) %>%
      addCircles(color = "red", lng = manualpeaks2011[i,2],
                 lat = manualpeaks2011[i,1], radius = 2500*sqrt(manualpeaks2011[i,4]))
  }
  return(m2)
}

function2 <- function(k) {
  return(lapply(seq(1,dim(manualpeaks2011)[1],1), map2011)[[k]])
}

lapply(1:dim(manualpeaks2011)[1], function2)
}


ebird_run2012 <- function() {
map2012 <- function(i) {
  if(manualpeaks2012[i,5]==0) {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>%
      addMarkers(-68.25, 44.25) %>%
      addPopups(-68.25, 45.25, popup = paste("ebird: Year = 2012, Day = ",manualpeaks2012[i,3],
                                             ", Count = ", manualpeaks2012[i,4])) %>%
      addCircles(color = "black", lng = manualpeaks2012[i,2],
                 lat = manualpeaks2012[i,1], radius = 2500*sqrt(manualpeaks2012[i,4]))
  }
  
  else {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>%
      addMarkers(-68.25, 44.25) %>%
      addPopups(-68.25, 45.25, popup = paste("Year = 2012, Day = ",
                                   manualpeaks2012[i,3], ", Count = ", manualpeaks2012[i,4])) %>%
      addCircles(color = "red", lng = manualpeaks2012[i,2],
                 lat = manualpeaks2012[i,1], radius = 2500*sqrt(manualpeaks2012[i,4]))
  }
  return(m2)
}

function2 <- function(k) {
  return(lapply(seq(1,dim(manualpeaks2012)[1],1), map2012)[[k]])
}

lapply(1:dim(manualpeaks2012)[1], function2)
}


ebird_run2013 <- function() {
  
map2013 <- function(i) {
  if(manualpeaks2013[i,5]==0) {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>%
      addMarkers(-68.25, 44.25) %>%
      addPopups(-68.25, 45.25, popup = paste("Year = 2013, Day = ",
                              manualpeaks2013[i,3], ", Count = ", manualpeaks2013[i,4])) %>%
      addCircles(color = "black", lng = manualpeaks2013[i,2],
                 lat = manualpeaks2013[i,1], radius = 2500*sqrt(manualpeaks2013[i,4]))
  }
  
  else {
    m2 <- m %>%
      setView(-68.5, 44.4, 7) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("Year = 2013, Day = ",manualpeaks2013[i,3],
                                             ", Count = ", manualpeaks2013[i,4])) %>% 
      addCircles(color = "red", lng = manualpeaks2013[i,2],
                 lat = manualpeaks2013[i,1], radius = 2500*sqrt(manualpeaks2013[i,4]))
  }
  return(m2)
}

function2 <- function(k) {
  return(lapply(seq(1,dim(manualpeaks2013)[1],1), map2013)[[k]])
}

lapply(1:dim(manualpeaks2013)[1], function2)
}


####################################################################
#I made my manual calculations from the below code.
#I also used this code to calculate 50% annual migration time

#  SECTION 4

maine_fall <- maine[which(maine$yday>=245),]
#adjust number in above line to change what day of year to start tracking
track <- data.frame()
#There is no Maine ebird data in 2014 after the 245th day of the year, so 2013 is the last
#year that will be analyzed here
for(k in 2010:2013) {
  start_const <- 2007
  #start_const is chosen so that the first two columns in track are latitude and longitude,
  #then the first year is the third column, second year fourth column, etc.
  #k is year, so have to take k-2007 to have first year go into third column
  for(i in 1:dim(x)[1]) {
    track[i,1] <- x[i,1]
    track[i,2] <- x[i,2]
    counts <-  
      maine_fall %>%
      filter(category=='raptor', year == k, latitude == x[i,1],
             longitude == x[i,2]) %>%
      group_by(yday) %>%
      summarise(birdcount = sum(observation.count))
    
    if ( dim(counts)[1] > 20 ){
      #Make the above dimension requirement higher in order to get less locations by removing
      #those that have 20 or fewer days with observations in the fall
      year_count <- 0
      for(j in 1:length(counts$birdcount)) {
        year_count <- year_count + counts$birdcount[j]
      }
      track[i,k-start_const] <- year_count
    }
    else{
      track[i,k-start_const] <- 0
    }
  }
}

year_half_total <- track
for(k in 2010:2013) {
  start_const <- 2007
  for(i in 1:dim(x)[1]) {
    year_half_total[i,k-start_const] <- ceiling(year_half_total[i,k-start_const]/2)
  }
}
#year_half_total
#doing a "reality check" in Excel, I manually counted for particular places and 
#years and saw that these numbers were correct for randomly selected places/years

year_half_day <- year_half_total
for(k in 2010:2013) {
  start_const <- 2007
  for(i in 1:dim(x)[1]) {
    counts <-  
      maine_fall %>%
      filter(category=='raptor', year == k, latitude == x[i,1],
             longitude == x[i,2]) %>%
      group_by(yday) %>%
      summarise(birdcount = sum(observation.count))
    if (dim(counts)[1] > 20){
      day <- 1
      partial_count <- 0
      done <- FALSE
      while(done == FALSE) {
        partial_count <- partial_count + counts$birdcount[day]
        if(partial_count>=year_half_total[i,k-start_const]){
          done=TRUE
        }
        else{
          day <- day +1
        }
      }
      real_day <- counts$yday[day]
      year_half_day[i,k-start_const] <- real_day
    }
  }
}


nonzero_places <- list()
ebird_years <- list()
for(k in 1:4) {
  ebird_years[[k]] <- data.frame()
  nonzero_count <- 0
  for(j in 1:67) {
    #j goes through all the rows in track that have nonzero entries. If too large, will get error
    #for trying to put values in empty rows
    if(track[j,k+2] > 0) {
      nonzero_count <- nonzero_count+1
      ebird_years[[k]][nonzero_count,1] <- as.integer(k + 2009) #convert index to year
      ebird_years[[k]][nonzero_count,2] <- track[j,1]
      ebird_years[[k]][nonzero_count,3] <- track[j,2]
      ebird_years[[k]][nonzero_count,4] <- track[j,k+2]
      ebird_years[[k]][nonzero_count,5] <- year_half_day[j,k+2]
      ebird_years[[k]][nonzero_count,6] <- year_half_total[j,k+2]
      
    }
  }
  nonzero_places[[k]] <- nonzero_count
  ebird_years[[k]] <- ebird_years[[k]][order(ebird_years[[k]]$V5),] #V5 becomes halfday
}
ebird_years
for(i in 1:4) {
  colnames(ebird_years[[i]]) <- c("year", "latitude", "longitude", "count", "halfday", "halfcount")
}


###########################################################

# SECTION 5

run2010 <- function() {
  make_map2010 <- function(i) {
    
    m2 <- m %>%
      setView(-68.5, 44.4, 6) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("ebird:Year = ",ebird_years[[1]]$year[i], ", Day = ",
                                             ebird_years[[1]]$halfday[i],
                                             ", Count = ", ebird_years[[1]]$halfcount[i])) %>% 
      addCircles(color = "black", lng = ebird_years[[1]]$longitude[i],
                 lat = ebird_years[[1]]$latitude[i], radius = 1500*sqrt(ebird_years[[1]]$halfcount[i]))
    
    return(m2)
  }
  
  function2 <- function(k) {
    return(lapply(1:nonzero_places[[1]],make_map2010)[[k]])
  }
  
  
  lapply(1:nonzero_places[[1]],function2)
}

run2011 <- function() {
  make_map2011 <- function(i) {
    
    m2 <- m %>%
      setView(-68.5, 44.4, 6) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("ebird: Year = ",ebird_years[[2]]$year[i], ", Day = ",
                                             ebird_years[[2]]$halfday[i],
                                             ", Count = ", ebird_years[[2]]$halfcount[i])) %>% 
      addCircles(color = "black", lng = ebird_years[[2]]$longitude[i],
                 lat = ebird_years[[2]]$latitude[i], radius = 1500*sqrt(ebird_years[[2]]$halfcount[i]))
    
    return(m2)
  }
  
  function2 <- function(k) {
    return(lapply(1:nonzero_places[[2]],make_map2011)[[k]])
  }
  
  
  lapply(1:nonzero_places[[2]],function2)
  
}



run2012 <- function() {
  make_map2012 <- function(i) {
    
    m2 <- m %>%
      setView(-68.5, 44.4, 6) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("ebird: Year = ",ebird_years[[3]]$year[i], ", Day = ",
                                             ebird_years[[3]]$halfday[i],
                                             ", Count = ", ebird_years[[3]]$halfcount[i])) %>% 
      addCircles(color = "black", lng = ebird_years[[3]]$longitude[i],
                 lat = ebird_years[[3]]$latitude[i], radius = 1500*sqrt(ebird_years[[3]]$halfcount[i]))
    
    return(m2)
  }
  
  function2 <- function(k) {
    return(lapply(1:nonzero_places[[3]],make_map2012)[[k]])
  }
  
  
  lapply(1:nonzero_places[[3]],function2)
}



run2013 <- function() {
  make_map2013 <- function(i) {
    
    m2 <- m %>%
      setView(-68.5, 44.4, 6) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("ebird: Year = ",ebird_years[[4]]$year[i], ", Day = ",
                                             ebird_years[[4]]$halfday[i],
                                             ", Count = ", ebird_years[[4]]$halfcount[i])) %>% 
      addCircles(color = "black", lng = ebird_years[[4]]$longitude[i],
                 lat = ebird_years[[4]]$latitude[i], radius = 1500*sqrt(ebird_years[[4]]$halfcount[i]))
    
    return(m2)
  }
  
  function2 <- function(k) {
    return(lapply(1:nonzero_places[[4]],make_map2013)[[k]])
  }
  
  lapply(1:nonzero_places[[4]],function2)
}



#################################################################################


#  SECTION 6

#ebird setophaga data

setop <- read.csv("~/setophaga_fall.csv")
setop <- setop[which(setop$yday>=225),]
y <-  unique( cbind(setop$latitude, setop$longitude))
setop$yday


#adjust number in below line to adjust what day of year to start tracking
setop_fall <- setop[which(setop$yday>=245),]
track <- data.frame()
for(k in 2010:2014) {
  start_const <- 2007
  for(i in 1:dim(y)[1]) {
    setop_track[i,1] <- y[i,1]
    setop_track[i,2] <- y[i,2]
    counts <-  
      setop_fall %>%
      filter(year == k, latitude == y[i,1],
             longitude == y[i,2]) %>%
      group_by(yday) %>%
      summarise(birdcount = sum(observation.count))
    
    if ( dim(counts)[1] > 20 ){
      year_count <- 0
      for(j in 1:length(counts$birdcount)) {
        year_count <- year_count + counts$birdcount[j]
      }
      track[i,k-start_const] <- year_count
    }
    else{
      track[i,k-start_const] <- 0
    }
  }
}

year_half_total <- track
for(k in 2010:2014) {
  start_const <- 2007
  for(i in 1:dim(y)[1]) {
    year_half_total[i,k-start_const] <- ceiling(year_half_total[i,k-start_const]/2)
  }
}


year_half_day <- year_half_total
for(k in 2010:2014) {
  start_const <- 2007
  for(i in 1:dim(y)[1]) {
    counts <-  
      setop_fall %>%
      filter(year == k, latitude == y[i,1],
             longitude == y[i,2]) %>%
      group_by(yday) %>%
      summarise(birdcount = sum(observation.count))
    if (dim(counts)[1] > 20){
      day <- 1
      partial_count <- 0
      done <- FALSE
      while(done == FALSE) {
        partial_count <- partial_count + counts$birdcount[day]
        if(partial_count>=year_half_total[i,k-start_const]){
          done=TRUE
        }
        else{
          day <- day +1
        }
      }
      real_day <- counts$yday[day]
      year_half_day[i,k-start_const] <- real_day
    }
  }
}


setop_count <- data.frame()
setop_day <- data.frame()

for(k in 2010:2013) {
  numplaces <- 0
  start_const <- 2007
  for(i in 1:dim(y)[1]) {
    counts <-  
      setop %>%
      filter(year == k, latitude == y[i,1],
             longitude == y[i,2]) %>%
      group_by(yday) %>%
      summarise(birdcount = sum(observation.count))
    
    #Later years have more data, so I made the minimum 
    #data requirement larger for later years
    year_threshold <- 0
    if(k ==2010) {
      year_threshold <- 25
    } else if(k==2011 || k==2012) {
      year_threshold <- 35
    } else {
      year_threshold <- 40
    }
    
    if (dim(counts)[1] > year_threshold){
      numplaces <- numplaces + 1
      setop_count[numplaces, 1] <- y[i,1]
      setop_count[numplaces, 2] <- y[i,2]
      setop_day[numplaces, 1] <- y[i,1]
      setop_day[numplaces, 2] <- y[i,2]
      
      setop_count[numplaces, k-start_const] <- max(counts$birdcount)
      
      for(j in 1:length(counts$birdcount)) {
        if(counts$birdcount[j]==max(counts$birdcount)) {
          setop_day[numplaces,k-start_const] <-  counts$yday[j]
        }
      }
    }
  }
}

colnames(setop_count) <- c("lat", "long", "2010", "2011", "2012", "2013")
colnames(setop_day) <- c("lat", "long", "2010", "2011", "2012", "2013")




#how many locations are available for each year
nonzero_places <- c()
nonzero_places[1] <- 5
nonzero_places[2] <- 6
nonzero_places[3] <- 6
nonzero_places[4] <- 9

#`` marks are required in order to use numbers as column names
setop_count_ten <- setop_count[order(setop_day$`2010`),]
setop_day_ten <- setop_day[order(setop_day$`2010`),]

setop_count_eleven <- setop_count[order(setop_day$`2011`),]
setop_day_eleven <- setop_day[order(setop_day$`2011`),]

setop_count_twelve <- setop_count[order(setop_day$`2012`),]
setop_day_twelve <- setop_day[order(setop_day$`2012`),]

setop_count_thirteen <- setop_count[order(setop_day$`2013`),]
setop_day_thirteen <- setop_day[order(setop_day$`2013`),]




###################################################################

# SECTION 7

setop2010 <- function() {
  make_map2010 <- function(i) {
    m2 <- m %>%
      setView(-68.5, 44.4, 6) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("setophaga: Year = 2010, Day = ",
                                             setop_day_ten$`2010`[i],
                                             ", Count = ", setop_count_ten$`2010`[i])) %>% 
      addCircles(color = "black", lng = setop_count_ten$long[i],
                 lat = setop_count_ten$lat[i], radius = 1500*sqrt(setop_count_ten$`2010`[i]))
    m2
    return(m2)
  }
  lapply(1:nonzero_places[1],make_map2010)
}



setop2011 <- function() {
  make_map2011 <- function(i) {
    m2 <- m %>%
      setView(-68.5, 44.4, 6) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("setophaga: Year = 2011, Day = ",
                                             setop_day_eleven$`2011`[i],
                                             ", Count = ", setop_count_eleven$`2011`[i])) %>% 
      addCircles(color = "black", lng = setop_count_eleven$long[i],
                 lat = setop_count_eleven$lat[i], radius = 1500*sqrt(setop_count_eleven$`2011`[i]))
    m2
    return(m2)
  }
  lapply(1:nonzero_places[1],make_map2011)
}





setop2012 <- function() {
  make_map2012 <- function(i) {
    m2 <- m %>%
      setView(-68.5, 44.4, 6) %>% 
      addMarkers(-68.25, 44.25) %>% 
      addPopups(-68.25, 45.25, popup = paste("setophaga: Year = 2012, Day = ",
                                             setop_day_twelve$`2012`[i],
                                             ", Count = ", setop_count_twelve$`2012`[i])) %>% 
      addCircles(color = "black", lng = setop_count_twelve$long[i],
                 lat = setop_count_twelve$lat[i], radius = 1500*sqrt(setop_count_twelve$`2012`[i]))
    m2
    return(m2)
  }
  lapply(1:nonzero_places[1],make_map2012)
}




setop2013 <- function() {
make_map2013 <- function(i) {
  m2 <- m %>%
    setView(-68.5, 44.4, 6) %>% 
    addMarkers(-68.25, 44.25) %>% 
    addPopups(-68.25, 45.25, popup = paste("setophaga: Year = 2013, Day = ",
                                           setop_day_thirteen$`2013`[i],
                                           ", Count = ", setop_count_thirteen$`2013`[i])) %>% 
    addCircles(color = "black", lng = setop_count_thirteen$long[i],
               lat = setop_count_thirteen$lat[i], radius = 1500*sqrt(setop_count_thirteen$`2013`[i]))
  m2
  return(m2)
}

lapply(1:nonzero_places[4], make_map2013)
}


##############################################################

#  SECTION 8

#loc stores the coordinates of every Hawk Watch site 
  loc <- data.frame()
  
  loc[1,1] <- 44.3386
  loc[1,2] <- -68.2733
  loc[1,3] <- "Acadia"
  loc[2,1] <- 42.5036
  loc[2,2] <- -71.8854
  loc[2,3] <- "Wachusett Mountain"
  loc[3,1] <- 42.4254
  loc[3,2] <- -75.0804
  loc[3,3] <- "Franklin Mountain"
  loc[4,1] <- 41.2415
  loc[4,2] <- -74.2907
  loc[4,3] <- "Mount Peter"
  loc[5,1] <- 40.2768
  loc[5,2] <- -77.2778
  loc[5,3] <- "Waggoner's Gap"
  loc[6,1] <- 39.2232
  loc[6,2] <- -79.1989
  loc[6,3] <- "Allegheny Front"
  
  colnames(loc) <- c("lat", "long", "name")

  #upload data for all Hawk Watch sites
  acadia <- read.csv("~/Acadia.csv")
  acadia$year <- as.integer(format(as.Date(acadia$Date, '%d-%b-%Y'),'%Y'))
  acadia$Date <-  as.Date(acadia$Date, '%d-%b-%Y')
  
  franklin <- read.csv("~/Franklin.csv")
  pilgrim <- read.csv("~/Pilgrim_Heights.csv")
  mpeter <- read.csv("~/Mount_Peter.csv")

  alleg <- read.csv("~/Allegheny_Front.csv")
  
  wach <- read.csv("~/Wachusett_Mountain.csv")
  wag <- read.csv("~/Waggoners_Gap.csv")

  sites <- list()
  sites[[1]] <- acadia
  sites[[2]] <- wach
  sites[[3]] <- franklin
  sites[[4]] <- mpeter
  sites[[5]] <- wag
  sites[[6]] <- alleg
  
  
  
  for(i in 1:length(sites)) {
    sites[[i]]$year <- as.integer(format(as.Date(sites[[i]]$Date, '%d-%b-%Y'),'%Y'))
    sites[[i]]$Date <- format(as.Date(sites[[i]]$Date, '%d-%b-%Y'))
    sites[[i]]$Date
  }
format(as.Date(sites[[5]]$Date), '%d-%b-%Y')
  

  
peak_total <- data.frame()
  peak_day <- data.frame()
  for(k in 2002:2013) {
    start_const <- 1999
    for(i in 2:length(sites)) { #excluding Acadia because Date formatting
      peak_total[i,1] <- loc[i,1] #worked differently in that data
      peak_total[i,2] <- loc[i,2]
      
      peak_day[i,1] <- loc[i,1]
      peak_day[i,2] <-loc[i,2]
      if(i == 6 | i ==2 |i==5) {
      counts <-  #need to make list of data frames here
        sites[[i]] %>%
        filter(year==k) %>%
        group_by(Date) %>%
        summarise(birdcount = sum(TOTAL))
      counts$birdcount
      peak_total[i,k-start_const] <- max(counts$birdcount)
      for(j in 1:length(counts$birdcount)) {
        if(counts$birdcount[j] == max(counts$birdcount)) {
          peak_day[i,k-start_const] <- format(as.Date(counts$Date[j]),'%d-%b-%Y')
        }
      }
      } else {
        counts <-  
          sites[[i]] %>%
          filter(year==k-start_const) %>%
          group_by(Date) %>%
          summarise(birdcount = sum(TOTAL))
       
        peak_total[i,k-start_const] <- max(counts$birdcount) 
        for(j in 1:length(counts$birdcount)) {
          if(counts$birdcount[j] == max(counts$birdcount)) {
            peak_day[i,k-start_const] <- format(as.Date(counts$Date[j]),'%d-%b-%Y')
          }
        }
      }
     
    }
  }
peak_total
  peak_day
  #The first row at this point is empty. This is because the Acadia data was
  #formatted differently than the other Hawk Watch data sets, so the method I used on
  #the other sites didn't work at Acadia
  #I have accordingly done the below manual calculations on the Acadia data
  
  
  
  
  colnames(peak_day) <- c("lat", "long", "two", "three", "four","five", "six", "seven",
                          "eight","nine","ten", "eleven","twelve","thirteen")
  colnames(peak_total) <- c("lat", "long", "two", "three", "four","five", "six", "seven",
                            "eight","nine","ten", "eleven","twelve","thirteen")
  
  peak_day$lat[1] <- 44.3386
  peak_day$long[1] <- -68.2733
  
  peak_total$lat[1] <- 44.3386
  peak_total$long[1] <- -68.2733
  
  peak_day$lat[2] <- 42.0570
  peak_day$long[2] <- -70.1147
  
  peak_total$lat[2] <- 42.0570
  peak_total$long[2] <- -70.1147
  
 
  peak_day$two[1] <- "2002-09-18"
  peak_total$two[1] <- 262
  
  peak_day$three[1] <- "2003-09-08"
  peak_total$three[1] <- 347
  
  peak_day$four[1] <- "2004-09-21"
  peak_total$four[1] <- 497
  
  peak_day$five[1] <- "2005-09-24"
  peak_total$five[1] <- 713
  
  peak_day$six[1] <- "2006-09-25"
  peak_total$six[1] <- 378
  
  peak_day$seven[1] <- "2007-09-17"
  peak_total$seven[1] <- 458
  
  peak_day$eight[1] <- "2008-10-06"
  peak_total$eight[1] <- 235
  
  peak_day$nine[1] <- "2009-09-06"
  peak_total$nine[1] <- 287
  
  peak_day$ten[1] <- "2010-09-11"
  peak_total$ten[1] <- 449
  
  peak_day$eleven[1] <- "2011-09-17"
  peak_total$eleven[1] <- 3200
  
  peak_day$twelve[1] <- "2012-09-11"
  peak_total$twelve[1] <- 766
  
  peak_day$thirteen[1] <- "2013-09-17"
  peak_total$thirteen[1] <- 1482

  peak_day
  peak_total
  



#I need to have the data each year ordered by the date at which the peak occurred,
#but these dates will not be in the same order all years. Latitudes and longitudes would get
#scrambled around incorrectly by re-ordering all years by peak date simultaneously.
#Thus, I am creating data frames specifically for examining each year.

peak_total_two <- peak_total[order(peak_day$two),]
peak_day_two <- peak_day[order(peak_day$two),]
peak_total_two

peak_total_three <- peak_total[order(peak_day$three),]
peak_day_three <- peak_day[order(peak_day$three),]

peak_total_four <- peak_total[order(peak_day$four),]
peak_day_four <- peak_day[order(peak_day$four),]

peak_total_five <- peak_total[order(peak_day$five),]
peak_day_five <- peak_day[order(peak_day$five),]

peak_total_six <- peak_total[order(peak_day$six),]
peak_day_six <- peak_day[order(peak_day$six),]

peak_total_seven <- peak_total[order(peak_day$seven),]
peak_day_seven <- peak_day[order(peak_day$seven),]

peak_total_eight <- peak_total[order(peak_day$eight),]
peak_day_eight <- peak_day[order(peak_day$eight),]

peak_total_nine <- peak_total[order(peak_day$nine),]
peak_day_nine <- peak_day[order(peak_day$nine),]

peak_total_ten <- peak_total[order(peak_day$ten),]
peak_day_ten <- peak_day[order(peak_day$ten),]

peak_total_eleven <- peak_total[order(peak_day$eleven),]
peak_day_eleven <- peak_day[order(peak_day$eleven),]

peak_total_twelve <- peak_total[order(peak_day$twelve),]

peak_day_twelve <- peak_day[order(peak_day$twelve),]

peak_total_thirteen <- peak_total[order(peak_day$thirteen),]
peak_day_thirteen <- peak_day[order(peak_day$thirteen),]

##################################################################

#  SECTION 9

#The function hawktwo (hawkthree, etc) plots the locations of the Hawkwatch sites on a map
#by the order of the peak dates in 2002 (2003, etc). The area of the circle at each site
#represents the actual number of birds observed on the peak date.

#I have put the entire process that outputs a map of 2002 (2003, etc) within the function
#run_hawktwo (run_hawkthree, etc), so that all of the code may be executed by only selecting
#one line of code.
run_hawktwo <- function() {
hawktwo <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2002, ", Day is ",
                                           peak_day_two$two[i],
                                           ", Count is ", peak_total_two$two[i])) %>% 
    addCircles(color = "black", lng = peak_total_two$long[i],
               lat = peak_total_two$lat[i], radius = 1500*sqrt(peak_total_two$two[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawktwo)[[k]])
}

lapply(1:6,function2)
}
run_hawktwo()

run_hawkthree <- function() {
  
hawkthree <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2003, ", Day is ",
                                           peak_day_three$three[i],
                                           ", Count is ", peak_total_three$three[i])) %>%
    addCircles(color = "black", lng = peak_total_three$long[i],
               lat = peak_total_three$lat[i], radius = 1500*sqrt(peak_total_three$three[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkthree)[[k]])
}

lapply(1:6,function2)
}
run_hawkthree()




run_hawkfour <- function() {
  
hawkfour <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2004, ", Day is ",
                                           peak_day_four$four[i],
                                           ", Count is ", peak_total_four$four[i])) %>% 
    addCircles(color = "black", lng = peak_total_four$long[i],
               lat = peak_total_four$lat[i], radius = 1500*sqrt(peak_total_four$four[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkfour)[[k]])
}

lapply(1:6,function2)
}
run_hawkfour()


run_hawkfive <- function() {
  
hawkfive <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2005, ", Day is ",
                                           peak_day_five$five[i],
                                           ", Count is ", peak_total_five$five[i])) %>% 
    addCircles(color = "black", lng = peak_total_five$long[i],
               lat = peak_total_five$lat[i], radius = 1500*sqrt(peak_total_five$five[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkfive)[[k]])
}

lapply(1:6,function2)
}




run_hawksix <- function() {
  
hawksix <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2006, ", Day is ",
                                           peak_day_six$six[i],
                                           ", Count is ", peak_total_six$six[i])) %>% 
    addCircles(color = "black", lng = peak_total_six$long[i],
               lat = peak_total_six$lat[i], radius = 1500*sqrt(peak_total_six$six[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawksix)[[k]])
}

lapply(1:6,function2)
}




run_hawkseven <- function() {
  
hawkseven <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2007, ", Day is ",
                                           peak_day_seven$seven[i],
                                           ", Count is ", peak_total_seven$seven[i])) %>% 
    addCircles(color = "black", lng = peak_total_seven$long[i],
               lat = peak_total_seven$lat[i], radius = 1500*sqrt(peak_total_seven$seven[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkseven)[[k]])
}

lapply(1:6,function2)

}





run_hawkeight <- function() {
  
hawkeight <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2008, ", Day is ",
                                           peak_day_eight$eight[i],
                                           ", Count is ", peak_total_eight$eight[i])) %>% 
    addCircles(color = "black", lng = peak_total_eight$long[i],
               lat = peak_total_eight$lat[i], radius = 1500*sqrt(peak_total_eight$eight[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkeight)[[k]])
}

lapply(1:6,function2)
}




run_hawknine <- function() {
  
hawknine <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2009, ", Day is ",
                                           peak_day_nine$nine[i],
                                           ", Count is ", peak_total_nine$nine[i])) %>% 
    addCircles(color = "black", lng = peak_total_nine$long[i],
               lat = peak_total_nine$lat[i], radius = 1500*sqrt(peak_total_two$nine[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawknine)[[k]])
}

lapply(1:6,function2)
}




run_hawkten <- function() {
  
hawkten <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2010, ", Day is ",
                                           peak_day_ten$ten[i],
                                           ", Count is ", peak_total_ten$ten[i])) %>% 
    addCircles(color = "black", lng = peak_total_ten$long[i],
               lat = peak_total_ten$lat[i], radius = 1500*sqrt(peak_total_ten$ten[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkten)[[k]])
}

lapply(1:6,function2)

}


run_hawkeleven <- function() {
  
hawkeleven <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2011, ", Day is ",
                                           peak_day_eleven$eleven[i],
                                           ", Count is ", peak_total_eleven$eleven[i])) %>% 
    addCircles(color = "black", lng = peak_total_eleven$long[i],
               lat = peak_total_eleven$lat[i], radius = 1500*sqrt(peak_total_eleven$eleven[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkeleven)[[k]])
}

lapply(1:6,function2)
}




run_hawktwelve <- function() {
  
hawktwelve <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2012, ", Day is ",
                                           peak_day_twelve$twelve[i],
                                           ", Count is ", peak_total_twelve$twelve[i])) %>% 
    addCircles(color = "black", lng = peak_total_twelve$long[i],
               lat = peak_total_twelve$lat[i], radius = 1500*sqrt(peak_total_twelve$twelve[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawktwelve)[[k]])
}

lapply(1:6,function2)
}
run_hawktwelve()




run_hawkthirteen <- function() {
  
hawkthirteen <- function(i) {
  
  m2 <- m %>%
    setView(-68.5, 44.4, 4) %>% 
    addPopups(-68.25, 45.25, popup = paste("Year is ",2013, ", Day is ",
                                           peak_day_thirteen$thirteen[i],
                                           ", Count is ", peak_total_thirteen$thirteen[i])) %>% 
    addCircles(color = "black", lng = peak_total_thirteen$long[i],
               lat = peak_total_thirteen$lat[i], radius = 1500*sqrt(peak_total_thirteen$thirteen[i]))
  
  return(m2)
}

function2 <- function(k) {
  return(lapply(1:6,hawkthirteen)[[k]])
}

lapply(1:6,function2)
}




###########################################################

# SECTION 10




wag$year <- as.integer(format(as.Date(wag$Date, '%d-%b-%Y'),'%Y'))
wag$Date <-  as.Date(wag$Date, '%d-%b-%Y')

waggoner_plot <- function() {

  

peak_total_wag <- c()
peak_day_wag <- c()

#1957 is first year at Waggoner's Gap Hawkwatch with an appreciable number of observation days
for(k in 1957:2013) {
  start_const <- 1956 
  #first year goes in first column,
  #since latitude, longitude fixed for this site 
  counts <-  
      wag %>%
      filter(year==k) %>%
      group_by(Date) %>%
      summarise(birdcount = sum(TOTAL))
  
  maxcount <- max(counts$birdcount) 
  counts$Date <- as.Date(counts$Date, '%Y-%b-%d')
  for(i in 1:dim(counts)[1]) {
    if(dim(counts)[1]>0) {
   
   temp_date <- format(as.Date(counts$Date[i],'%Y-%b-%d'))
   doy <- strftime(temp_date, format = "%j")
   
  if(counts$birdcount[i]==maxcount) {
    peak_day_wag[k-1956] <- as.numeric(doy)
    peak_total_wag[k-1956] <- maxcount
    print(paste(k, "  ,  ", peak_day_wag[k-1956]))
    }
  }
  }
}
as.vector(peak_day_wag)

wag_years <- c(1957:2013)
wag_years

plot(wag_years, as.vector(peak_day_wag), main = "Waggoner's Gap. Multiple R squared = 0.04084",
     xlab = "Year", ylab = "Date of peak count")


#Got info on how to do trendline in base package from:
#http://stackoverflow.com/questions/15102254/how-do-i-add-different-trend-lines-in-r
fit <- lm(as.vector(peak_day_wag)~wag_years)
fit
co <- coef(fit)
abline(fit, col="blue", lwd=2)
r2 <- round(summary(fit)$r.squared, 2)
print(summary(fit))
}





mpeter_plot <- function() {

mpeter <- read.csv("~/Mount_Peter.csv")
mpeter$year <- as.integer(format(as.Date(mpeter$Date, '%d-%b-%Y'),'%Y'))
mpeter$Date <-as.Date(mpeter$Date, '%d-%b-%Y')
peak_total_mpeter <- c()
peak_day_mpeter <- c()


for(k in 1960:1999) {
#had to split years into before 2000, and 2000 and later because the year
#was parsed from the date only as the last two digits of the year. Thus, 1999
#becomes 99, 2000 becomes 0. For years before 2000, the filter must be
#year == k-1900, but for 2000 and later, the filter must be year== k-2000
  counts <- 
    mpeter %>%
    filter(year==k-1900) %>%
    group_by(Date) %>%
    summarise(birdcount = sum(TOTAL))
  
  counts$Date <- format(as.Date(counts$Date, '%Y-%b-%d'))
  maxcount <- max(counts$birdcount) #i.e. column 1
  for(i in 1:dim(counts)[1]) {
  if(dim(counts)[1]>0) {
    temp_date <- counts$Date[i]
    #set index at k-1959 so that it starts at 1
    doy <- strftime(temp_date, format = "%j")
    as.numeric(strftime("0000-08-05", format = "%j"))
    
    if(counts$birdcount[i]==maxcount) {
      peak_day_mpeter[k-1959] <- as.numeric(doy)
      peak_total_mpeter[k-1959]<- maxcount
     
    }
  }
}
}


for(k in 2000:2014) {
  
  counts <- 
    mpeter %>%
    filter(year==k-2000) %>%
    group_by(Date) %>%
    summarise(birdcount = sum(TOTAL))
  
    maxcount <- max(counts$birdcount) 
    counts$Date <- format(as.Date(counts$Date, '%Y-%b-%d'))
  for(i in 1:dim(counts)[1]) {
  if(dim(counts)[1]>0) {
    temp_date <- counts$Date[i]
  
    doy <- strftime(temp_date, format = "%j")
    
    if(counts$birdcount[i]==maxcount) {
      peak_day_mpeter[k-1959] <- as.numeric(doy)
      peak_total_mpeter[k-1959]<- maxcount
      
      }
    }
  }
  
}



peak_day_mpeter
peak_total_mpeter


length(mpeter_years <- c(1960:2014))
length(peak_day_mpeter)

plot(mpeter_years, peak_day_mpeter, main = "Mount Peter. Multiple R squared = 0.04944",
     xlab = "Year", ylab = "Date of peak count")
#Got info on how to do trendline in base package from:
#http://stackoverflow.com/questions/15102254/how-do-i-add-different-trend-lines-in-r
fit <- lm(peak_day_mpeter~mpeter_years)
fit
co <- coef(fit)
abline(fit, col="blue", lwd=2)
r2 <- round(summary(fit)$r.squared, 2)
print(summary(fit))
print(r2)
}






pilgrim <- read.csv("~/Pilgrim_Heights.csv")
pilgrim$Date <-  as.Date(pilgrim$Date, '%d-%b-%Y')
pilgrim$year <- as.integer(format(as.Date(pilgrim$Date, '%d-%b-%Y'),'%Y'))
pilgrim$year
pilgrim$Date
pilgrim_plot <- function() {
  
  peak_total_pilgrim <- c()
  peak_day_pilgrim <- c()
  
  for(k in 1998:2014) {

    counts <-  
      pilgrim %>%
      filter(year==k) %>%
      group_by(Date) %>%
      summarise(birdcount = sum(TOTAL))
    countmax <- max(counts$birdcount)
    counts$Date <- as.Date(counts$Date, '%Y-%b-%d')
    
    for(i in 1:dim(counts)[1]) {
    if(dim(counts)[1]>0) {
      
      temp_date <- as.Date(counts$Date[i],'%Y-%b-%d')
      doy <- strftime(temp_date, format = "%j")
      
      if(counts$birdcount[i] == countmax) {
        peak_day_pilgrim[k-1997] <- as.numeric(doy)
        peak_total_pilgrim[k-1997] <- countmax
        print(paste(k, "  ,  ", peak_day_pilgrim[k-1997]))
        
      }
    }
  }
  
  }
  as.vector(peak_day_pilgrim)
  
  pilgrim_years <- c(1998:2014)
  pilgrim_years
  
  
  plot(pilgrim_years, as.vector(peak_day_pilgrim),
       main = "Pilgrim Heights. Multiple R squared = 0.107",
       xlab = "Year", ylab = "Date of peak count")
  
  fit <- lm(peak_day_pilgrim~pilgrim_years)
  fit$model
  co <- coef(fit)
  abline(fit, col="blue", lwd=2)
  r2 <- round(summary(fit)$r.squared, 2)
  print(summary(fit))
}






alleg <- read.csv("~/Allegheny_Front.csv")
alleg$Date <-  as.Date(alleg$Date, '%d-%b-%Y')
alleg$year <- as.integer(format(as.Date(alleg$Date, '%d-%b-%Y'),'%Y'))
alleg$year
alleg$Date
alleg_plot <- function() {
  
  peak_total_alleg <- c()
  peak_day_alleg <- c()
  
  for(k in 2001:2014) {

    counts <- 
      alleg %>%
      filter(year==k) %>%
      group_by(Date) %>%
      summarise(birdcount = sum(TOTAL))
    
    maxcount <- max(counts$birdcount)
    for(i in 1:dim(counts)[1]) {
    if(dim(counts)[1]>0) {
      counts$Date <- as.Date(counts$Date, '%Y-%b-%d')
      
      temp_date <- as.Date(counts$Date[i],'%Y-%b-%d')
      doy <- strftime(temp_date, format = "%j")
      
      if(counts$birdcount[i]==maxcount) {
        peak_day_alleg[k-2000] <- as.numeric(doy)
        peak_total_alleg[k-2000] <- maxcount
        print(paste(k, "  ,  ", peak_day_alleg[k-2000]))
        
      }
    }
    }
  }
  as.vector(peak_day_alleg)
  
  alleg_years <- c(2001:2014)
  alleg_years
  
  
  plot(alleg_years, as.vector(peak_day_alleg),
       main = "Allegheny Front. Multiple R squared = 0.04314",
       xlab = "Year", ylab = "Date of peak count")
  
  fit <- lm(peak_day_alleg~alleg_years)
  fit$model
  co <- coef(fit)
  abline(fit, col="blue", lwd=2)
  r2 <- round(summary(fit)$r.squared, 2)
  print(summary(fit))
}




franklin <- read.csv("~/Franklin.csv")
franklin$Date <-  as.Date(franklin$Date, '%d-%b-%Y')
franklin$year <- as.integer(format(as.Date(franklin$Date, '%d-%b-%Y'),'%Y'))
franklin_plot <- function() {
  
  peak_total_franklin <- c()
  peak_day_franklin <- c()
  
  for(k in 1989:1999) {
   
    counts <-  
      franklin %>%
      filter(year==k-1900) %>%
      group_by(Date) %>%
      summarise(birdcount = sum(TOTAL))
    
    maxcount <- max(counts$birdcount)
    counts$Date <- as.Date(counts$Date, '%Y-%b-%d')
    
    for(i in 1:dim(counts)[1]) {
    if(dim(counts)[1]>0) {
      
      temp_date <- as.Date(counts$Date[i],'%Y-%b-%d')
      doy <- strftime(temp_date, format = "%j")
     
      if(counts$birdcount[i]==maxcount) {
        peak_day_franklin[k-1988] <- as.numeric(doy)
        peak_total_franklin[k-1988] <- maxcount
        print(paste(k, "  ,  ", peak_day_franklin[k-1988]))
        
      }
    }
  }
} 
  
  for(k in 2000:2014) {
  
    counts <- 
      franklin %>%
      filter(year==k-2000) %>%
      group_by(Date) %>%
      summarise(birdcount = sum(TOTAL))
    
    maxcount <- max(counts$birdcount)
    counts$Date <- as.Date(counts$Date, '%Y-%b-%d')
    
    for(i in 1:dim(counts)[1]) {
    if(dim(counts)[1]>0) {
     
      temp_date <- as.Date(counts$Date[k-1988],'%Y-%b-%d')
      doy <- strftime(temp_date, format = "%j")
    
      if(counts$birdcount[i]==maxcount) {
        peak_day_franklin[k-1988] <- as.numeric(doy)
        peak_total_franklin[k-1988] <- maxcount
       
        print(paste(k, "  ,  ", peak_day_franklin[k-1988]))
        
      }
    }
  }
}
  as.vector(peak_day_franklin)
  
  franklin_years <- c(1989:2014)
  franklin_years
  
  
  plot(franklin_years, as.vector(peak_day_franklin),
       main = "Franklin Mountain. Multiple R squared = 0.4498",
       xlab = "Year", ylab = "Date of peak count")
  
  fit <- lm(peak_day_franklin~franklin_years)
  fit$model
  co <- coef(fit)
  abline(fit, col="blue", lwd=2)
  r2 <- round(summary(fit)$r.squared, 2)
  print(summary(fit))
}


acadia_plot <- function() {
  
acadia_format_dates <- c()

#Manually recorded Acadia peak days 2002-2013 because the dplyr package was
#not working correctly on this dataset

acadia_format_dates[1] <- as.numeric(strftime("2002-09-18", format = "%j"))
acadia_format_dates[2] <- as.numeric(strftime("2003-09-08", format = "%j"))
acadia_format_dates[3] <- as.numeric(strftime("2004-09-21", format = "%j"))
acadia_format_dates[4] <- as.numeric(strftime("2005-09-24", format = "%j"))
acadia_format_dates[5] <- as.numeric(strftime("2006-09-25", format = "%j"))
acadia_format_dates[6] <- as.numeric(strftime("2007-09-17", format = "%j"))
acadia_format_dates[7] <- as.numeric(strftime("2008-10-06", format = "%j"))
acadia_format_dates[8] <- as.numeric(strftime("2009-09-06", format = "%j"))
acadia_format_dates[9] <- as.numeric(strftime("2010-09-11", format = "%j"))
acadia_format_dates[10] <- as.numeric(strftime("2011-09-17", format = "%j"))
acadia_format_dates[11] <- as.numeric(strftime("2012-09-11", format = "%j"))
acadia_format_dates[12] <- as.numeric(strftime("2013-09-17", format = "%j"))


acadia_years <- c(2002:2013)
acadia_format_dates
plot(acadia_years, acadia_format_dates,
     main = "Acadia. Multiple R squared = 0.02763",
     xlab = "Year", ylab = "Date of peak count")

fit <- lm(acadia_format_dates~acadia_years)
fit$model
co <- coef(fit)
abline(fit, col="blue", lwd=2)
r2 <- round(summary(fit)$r.squared, 2)
print(r2)
print(summary(fit))
}


########################################################

#  SECTION 11

acadia_correlation <- function() {
  #manually recorded fall peaks  at 44.25, -68.25 location 2010 to 2013
  by_acadia <- c(263,260,245,260)
  
  #manually recorded fall peaks at Acadia location 2010 to 2013
  acadia_peak_day <- c(264,260,238,258)
  
  yearvec  <- seq(2010,2013,1)
  plot(yearvec,acadia_peak_day,type="l",col="blue",
       xlim=c(2010,2013), ylim=c(0,270),
       xlab = "Year", ylab = "day of peak bird count", xaxt = 'n',
       main= "Correlation = 0.9970704")
  lines(yearvec,by_acadia, type="l",col="red")
  axis(1, at=seq(2010,2013, by=1))
  points(2010,263, col = "red")
  points(2011,260, col = "red")
  points(2012,245, col = "red")
  points(2013,260, col = "red")
  points(2010,264, col = "blue")
  points(2011,260, col = "blue")
  points(2012,238, col = "blue")
  points(2013,258, col = "blue")
  
  legend(2010,100,c("Acadia", "nearest ebird site               "),
         lty = c(1,1),lwd=c(2.5,2.5),col=c("blue","red"),cex = 1)
  #cor(acadia_peak_days,by_acadia)
}


#RUN ALL CODE UP TO THIS POINT ALL AT ONE TIME.
#I DID THIS TO MAKE THE PROGRAM EASIER TO USE TO SEE PLOTS
##############################################################################


# SECTION 12

#THE FOLLOWING FUNCTIONS EXECUTE ALL ABOVE CODE

#ebird with manual peaks
ebird_run2010()
ebird_run2011()
ebird_run2012()
ebird_run2013()

#ebird with day of 50% of fall migration
run2010()
run2011() #?
run2012()
run2013()



#(for setophaga genus): ebird with dayof 50% of fall migration
setop2010()
setop2011()
setop2012()
setop2013()



#Dates of peaks of Hawkwatch sites, 2002-2013
#Functions named as they are because having numbers in the names caused problems for
#these functions in particular
run_hawktwo()
run_hawkthree()
run_hawkfour()
run_hawkfive()
run_hawksix()
run_hawkseven()
run_hawkeight() 
run_hawknine()
run_hawkten()
run_hawkeleven()
run_hawktwelve() 
run_hawkthirteen()



#Plots of year versus date of peak for 6 Hawk Watch sites
mpeter_plot()
waggoner_plot()
pilgrim_plot()
alleg_plot()
franklin_plot()
acadia_plot()


#Correlation between peaks at Acadia and ebird region surrounding Acadia
acadia_correlation()

#######################################################

#SECTION 13


#EVERYTHING BELOW IS CODE I USED TO HELP GET RESULTS ALONG THE WAY,
#BUT THEY DO NOT NEED TO BE RUN TO SEE ALL RESULTS OF THIS PROJECT



for(i in 2010:2013) {
  counts <-  
    maine %>%
    filter(category=='raptor', year == i, latitude == 44.25,
           longitude == -68.25) %>%
    group_by(yday) %>%
    summarise(birdcount = sum(observation.count))
  plot(counts$yday,counts$birdcount)
  textxy(counts$yday,counts$birdcount,
         counts$yday)
}
by_acadia <- c(263,260,245,260)


#this code was used to directly obtain the bird counts at Acadia
acadia_counts <-  
  acadia %>%
  filter(year==11) %>%
  group_by(Date) %>%
  summarise(birdcount = sum(TOTAL))
acadia_counts$Date <-  as.Date(acadia_counts$Date, '%d-%b-%Y')
plot(acadia_counts, main = paste("Hawkwatch, ", 2013))
textxy(acadia_counts$Date,acadia_counts$birdcount,
       acadia_counts$Date)





by_acadia_table <- table(2010:2013,acadia_peak_days)
by_acadia_table
total <- 0
for(i in 1:dim(acadia)[1]) {
  if (acadia$Date[i] == '0004-09-21') {
    total <- total+ acadia$TOTAL[i]
  }
}
#total

#make "restart animation" map for ebird2010-ebird2013
m3 <- m %>%
  setView(-68.5, 44.4, 7) %>% # map location
  addMarkers(-68.25, 44.25) %>% # add a marker
  addPopups(-68.25, 45.25, popup = "End of one loop of animation")
m3

im.convert(c("~/ebird2010_peak1.png","~/ebird2010_peak2.png", "~/ebird2010_peak3.png", 
             "~/ebird2010_peak4.png", "~/ebird2010_peak5.png", "~/ebird2010_peak6.png", 
             "~/ebird2010_peak7.png", "~/ebird2010_peak8.png", "~/ebird2010_peak9.png",
             "~/ebird2010_peak10.png", "~/ebird2010_peak11.png", "~/ebird2010_peak12.png", 
             "~/ebird2010_peak13.png", "~/ebird20yy_end.png", "~/ebird20yy_end.png", "~/ebird20yy_end.png"),
           output = "ebird2010_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


im.convert(c("~/ebird2011_peak1.png","~/ebird2011_peak2.png", "~/ebird2011_peak3.png", 
             "~/ebird2011_peak4.png", "~/ebird2011_peak5.png", "~/ebird2011_peak6.png", 
             "~/ebird2011_peak7.png", "~/ebird2011_peak8.png", "~/ebird2011_peak9.png",
             "~/ebird20yy_end.png", "~/ebird20yy_end.png", "~/ebird20yy_end.png"),
           output = "ebird2011_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


im.convert(c("~/ebird2012_peak1.png","~/ebird2012_peak2.png", "~/ebird2012_peak3.png", 
             "~/ebird2012_peak4.png", "~/ebird2012_peak5.png", "~/ebird2012_peak6.png", 
             "~/ebird2012_peak7.png", "~/ebird2012_peak8.png", "~/ebird2012_peak9.png",
             "~/ebird2012_peak10.png", "~/ebird2012_peak11.png", "~/ebird2012_peak12.png", 
             "~/ebird2012_peak13.png", "~/ebird2012_peak14.png", "~/ebird2012_peak15.png", 
             "~/ebird2012_peak16.png", "~/ebird2012_peak17.png", "~/ebird2012_peak18.png", 
             "~/ebird2012_peak19.png", "~/ebird2012_peak20.png", "~/ebird20yy_end.png",
             "~/ebird20yy_end.png", "~/ebird20yy_end.png"),
           output = "ebird2012_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


im.convert(c("~/ebird2013_peak1.png","~/ebird2013_peak2.png", "~/ebird2013_peak3.png", 
             "~/ebird2013_peak4.png", "~/ebird2013_peak5.png", "~/ebird2013_peak6.png", 
             "~/ebird2013_peak7.png", "~/ebird2013_peak8.png", "~/ebird2013_peak9.png",
             "~/ebird2013_peak10.png", "~/ebird2013_peak11.png", "~/ebird2013_peak12.png", 
             "~/ebird2013_peak13.png", "~/ebird20yy_end.png", "~/ebird20yy_end.png", "~/ebird20yy_end.png"),
           output = "ebird2013_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


#restart animation map for run2010-run2013
m4 <- m %>%
  setView(-68.5, 44.4, 6) %>% # map location
  addMarkers(-68.25, 44.25) %>% # add a marker
  addPopups(-68.25, 45.25, popup = "End of one loop of animation")
m4



im.convert(c("~/ebird2010_half1.png","~/ebird2010_half2.png", "~/ebird2010_half3.png", 
             "~/ebird2010_half4.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png",
             "~/ebird20yy_half_end.png"),
           output = "ebird2010_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


im.convert(c("~/ebird2011_half1.png","~/ebird2011_half2.png", "~/ebird2011_half3.png", 
             "~/ebird2011_half4.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png",
             "~/ebird20yy_half_end.png"),
           output = "ebird2011_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


im.convert(c("~/ebird2012_half1.png","~/ebird2012_half2.png", "~/ebird2012_half3.png", 
             "~/ebird2012_half4.png", "~/ebird2012_half5.png", "~/ebird2012_half6.png",
             "~/ebird2012_half7.png", "~/ebird2012_half8.png", "~/ebird2012_half9.png",
             "~/ebird2012_half10.png", "~/ebird2012_half11.png", "~/ebird2012_half12.png",
             "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png"),
           output = "ebird2012_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


im.convert(c("~/ebird2013_half1.png","~/ebird2013_half2.png", "~/ebird2013_half3.png", 
             "~/ebird2013_half4.png", "~/ebird2013_half5.png", "~/ebird2013_half6.png",
             "~/ebird2013_half7.png", "~/ebird2013_half8.png", "~/ebird2013_half9.png",
             "~/ebird2013_half10.png", "~/ebird2013_half11.png",
             "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png"),
           output = "ebird2013_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


#setop "restart animation" map

m5 <- m %>%
  setView(-68.5, 44.4, 6) %>% # map location
  addMarkers(-68.25, 44.25) %>% # add a marker
  addPopups(-68.25, 45.25, popup = "End of one loop of animation")
m5

im.convert(c("~/setop2010_half1.png","~/setop2010_half2.png", "~/setop2010_half3.png", 
             "~/setop2010_half4.png", "~/setop2010_half5.png",
             "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png"),
           output = "setop2010_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/setop2011_half1.png","~/setop2011_half2.png", "~/setop2011_half3.png", 
             "~/setop2011_half4.png", "~/setop2011_half5.png",
             "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png"),
           output = "setop2011_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/setop2012_half1.png","~/setop2012_half2.png", "~/setop2012_half3.png", 
             "~/setop2012_half4.png", "~/setop2012_half5.png",
             "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png"),
           output = "setop2012_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/setop2013_half1.png","~/setop2013_half2.png", "~/setop2013_half3.png", 
             "~/setop2013_half4.png", "~/setop2013_half5.png", "~/setop2013_half6.png",
             "~/setop2013_half7.png", "~/setop2013_half8.png", "~/setop2013_half9.png",
             "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png", "~/ebird20yy_half_end.png"),
           output = "setop2013_half.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)
            

            
#hawkwatch "restart animation" map
m6 <- m %>%
  setView(-68.5, 44.4, 4) %>% # map location
  addMarkers(-68.25, 44.25) %>% # add a marker
  addPopups(-68.25, 45.25, popup = paste("End of one loop of animation"))
m6
                                         

im.convert(c("~/hawk2_1.png", "~/hawk2_2.png", "~/hawk2_3.png", "~/hawk2_4.png",
             "~/hawk2_5.png", "~/hawk2_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk2_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk3_1.png", "~/hawk3_2.png", "~/hawk3_3.png", "~/hawk3_4.png",
             "~/hawk3_5.png", "~/hawk3_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk3_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk4_1.png", "~/hawk4_2.png", "~/hawk4_3.png", "~/hawk4_4.png",
             "~/hawk4_5.png", "~/hawk4_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk4_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk5_1.png", "~/hawk5_2.png", "~/hawk5_3.png", "~/hawk5_4.png",
             "~/hawk5_5.png", "~/hawk5_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk5_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk6_1.png", "~/hawk6_2.png", "~/hawk6_3.png", "~/hawk6_4.png",
             "~/hawk6_5.png", "~/hawk6_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk6_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk7_1.png", "~/hawk7_2.png", "~/hawk7_3.png", "~/hawk7_4.png",
             "~/hawk7_5.png", "~/hawk7_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk7_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk8_1.png", "~/hawk8_2.png", "~/hawk8_3.png", "~/hawk8_4.png",
             "~/hawk8_5.png", "~/hawk8_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk8_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk9_1.png", "~/hawk9_2.png", "~/hawk9_3.png", "~/hawk9_4.png",
             "~/hawk9_5.png", "~/hawk9_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk9_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk10_1.png", "~/hawk10_2.png", "~/hawk10_3.png", "~/hawk10_4.png",
             "~/hawk10_5.png", "~/hawk10_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk10_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk11_1.png", "~/hawk11_2.png", "~/hawk11_3.png", "~/hawk11_4.png",
             "~/hawk11_5.png", "~/hawk11_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk11_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk12_1.png", "~/hawk12_2.png", "~/hawk12_3.png", "~/hawk12_4.png",
             "~/hawk12_5.png", "~/hawk12_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk12_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)

im.convert(c("~/hawk13_1.png", "~/hawk13_2.png", "~/hawk13_3.png", "~/hawk13_4.png",
             "~/hawk13_5.png", "~/hawk13_6.png",
             "hawk20yy_end.png", "hawk20yy_end.png", "hawk20yy_end.png"),
           output = "hawk13_peak.gif" , convert = c("convert", "gm convert"), 
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "", 
           clean = FALSE)


#I used these to find the total count of raptors and setophaga in Maine
#Some of the rows in "Maine.csv" contain counts of more than one bird, thus
#the number of rows is not the number of birds counted

maine_raptors <- maine[which(maine$category=='raptor'),]
counts1 <-  
  maine_raptors %>%
  filter() %>%
  group_by() %>%
  summarise(birdcount = sum(observation.count))
counts1 # = 48081

setophaga <- read.csv("~/setophaga_data.csv")


counts2 <- 
  setophaga %>%
  filter() %>%
  group_by() %>%
  summarise(birdcount = sum(observation.count))
counts2 # = 278776