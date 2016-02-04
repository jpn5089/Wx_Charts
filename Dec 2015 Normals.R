library(dplyr)
library(lubridate)
library(rnoaa)
library(ggplot2)

options(noaakey = "insert key here")

Stations <- read.csv("c:/users/jnicola/desktop/Weather Charts/StationNames.csv",stringsAsFactors = FALSE)

#codes <- Stations$noaa

#zones <- c("FE_OH/Cleveland","DQE/Pittsburgh","PPL/Allentown","PECO/Philadelphia","AEP/Columbus","Toledo","Alliance/Akron")
#hours <- seq(0,23)
#temps <- c("ATSI_temp","DUQ_temp","PPL_temp","PECO_temp","AEP_temp","Toledo_temp","Akron_temp")

StationsRow <- c(6,14,1,13,7,16,5)

allOld <- list()

for (i in 1:7){


dec_normals <- ncdc(datasetid='NORMAL_HLY', stationid = as.character(Stations[StationsRow[i],2]),
    datatypeid = "HLY-TEMP-NORMAL", startdate = '2010-12-01', enddate = '2011-01-01', limit = 1000)

avg <- dec_normals[[2]] %>% filter(month(date) != 1) %>%
  mutate(date = ymd_hms(gsub("T"," ",date)) + years(5),
         value = (value/10) ) %>%
  select(date,station,value) %>%
  mutate(value = as.numeric(value), datatype = "Normal")%>%
  mutate(day = floor_date(date,unit = "day"),
         hour = hour(date)) %>%
  mutate(station = as.character(Stations[StationsRow[i],1]))

allOld[[i]] <- avg
}

normalAll <-do.call(rbind,allOld)

write.csv(normalAll, file = "C:/Users/jnicola/Desktop/Normals.csv")
