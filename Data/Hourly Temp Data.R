library(dplyr)
library(lubridate)
library(rnoaa)
library(ggplot2)

options(noaakey = Sys.getenv("NOAAKEY"))

Stations <- read.csv("c:/users/John/Documents/GitHub/Wx_Charts/Data/StationNames.csv",stringsAsFactors = FALSE)

#codes <- Stations$noaa

#zones <- c("FE_OH/Cleveland","DQE/Pittsburgh","PPL/Allentown","PECO/Philadelphia","AEP/Columbus","Toledo","Alliance/Akron")
#hours <- seq(0,23)
#temps <- c("ATSI_temp","DUQ_temp","PPL_temp","PECO_temp","AEP_temp","Toledo_temp","Akron_temp")

StationsRow <- c(32,33)

allData <- list()

for (i in 1:2){
  
  jan_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]), 
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-01-01', enddate = '2010-02-01', limit = 1000)
  
  feb_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]), 
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-02-01', enddate = '2010-03-01', limit = 1000)
  
  mar_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-03-01', enddate = '2010-04-01', limit = 1000)
  
  apr_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-04-01', enddate = '2010-05-01', limit = 1000)
  
  may_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-05-01', enddate = '2010-06-01', limit = 1000)
  
  jun_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-06-01', enddate = '2010-07-01', limit = 1000)
  
  jul_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-07-01', enddate = '2010-08-01', limit = 1000)
  
  aug_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-08-01', enddate = '2010-09-01', limit = 1000)
  
  sep_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-09-01', enddate = '2010-10-01', limit = 1000)
  
  oct_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-10-01', enddate = '2010-11-01', limit = 1000)
  
  nov_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-11-01', enddate = '2010-12-01', limit = 1000)
  
  dec_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
                      datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-12-01', enddate = '2011-01-01', limit = 1000)
  
  normals <- rbind(jan_normals[[2]] %>% filter(month(date) != 2),feb_normals[[2]] %>% filter(month(date) != 3),
                   mar_normals[[2]] %>% filter(month(date) != 4),apr_normals[[2]] %>% filter(month(date) != 5), 
                   may_normals[[2]] %>% filter(month(date) != 6),jun_normals[[2]] %>% filter(month(date) != 7),
                   jul_normals[[2]] %>% filter(month(date) != 8),aug_normals[[2]] %>% filter(month(date) != 9), 
                   sep_normals[[2]] %>% filter(month(date) != 10),oct_normals[[2]] %>% filter(month(date) != 11),
                   nov_normals[[2]] %>% filter(month(date) != 12),dec_normals[[2]] %>% filter(month(date) != 1)) %>%
    mutate(date = ymd_hms(gsub("T"," ",date)) + years(7),
           value = (value/10) ) %>%
    select(date,station,value) %>%
    mutate(value = as.numeric(value), datatype = "Normal")%>%
    mutate(day = floor_date(date,unit = "day"),
           hour = hour(date)) %>%
    mutate(station = as.character(Stations[StationsRow[i],1]))
  
  allData[[i]] <- normals
}

normalAll <-do.call(rbind,allData)

write.csv(normalAll, file = "C:\\Users\\John\\Desktop\\Temp_Norm.csv")

#write.csv(normalAll, file = "C:\Users\John\Documents\GitHub\Wx_Charts\Data\Temp_Normals.csv")

