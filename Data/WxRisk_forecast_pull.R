#---
# longTermWeather <- do.call(rbind,hist_wdata) %>%
#   filter(station == 'GHCND:USW00093814',
#          datatype == 'TMAX') %>%
#   mutate(min = (1.8*(value/10))+32) %>%
#   select(date, min)

# This scrapes the names of all the files on the FTP site
# Arranges them by date
# Then selects the most recent file for all 16 station_path
# Note:
# To avoid extended passive mode and use the regular passive mode. (increases speed of download)
# This is controlled via the ftp.use.epsv option in calls to curlPerform()
# and we set this to FALSE so that PASV is used rather than EPSV. (http://www.omegahat.org/RCurl/FAQ.html)

library(plyr)
library(dplyr)
library(lubridate)
library(RCurl)
library(httr)
library(data.table)
library(devtools)
library(XML)
library(reshape2)
library(tidyr)


station_path <- getURL("??????",
                       userpwd = "????",
                       ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
  data.table::fread(.,header=FALSE) %>%
  filter(V1 != "Historical_Data") %>%
  rowwise()%>%
  mutate(date = gsub("\\D", "",V1),
         date2 = ymd(paste(paste("20",substr(date,5,6),sep=""), substr(date,1,2),substr(date,3,4), sep = "-"))) %>%
  arrange(date2) %>%
  tail(.,32)



# This for loop grabs all 16 of the latest files for each staion and downloads the data
out <- list()
for (i in 1:32){

  x <- paste("?????", station_path$V1[i], sep ="")

  data <- GET(x,authenticate("??????", "??????")) %>%
    content(.,'text') %>%
    data.table::fread(.)

  name <- substr(station_path$V1[i],0,13)

  out[[""]] <- data

}


weather_fcst<-do.call(rbind,out) 
write.csv(weather_fcst, file = paste("T:\\Scheduling\\Power\\load forecasting\\zzz pjm files folder\\Weather/Weather_Forecast_",Sys.Date(),".csv",sep = ""))

weather_fcst<-do.call(rbind,out) %>%
  mutate(`Valid Date` = ymd_h(`Valid Date`)) %>%
  mutate(`Forecast Date` = ymd_hm(`Forecast Date`)) %>%
  gather(variable, value, -`Valid Date`, -CODE, -`Forecast Date`) %>%
  filter(variable %in% c('TMP')) %>%
  mutate(value = as.numeric(value), datatype = "Forecast")

setnames(weather_fcst,"Valid Date","date")
setnames(weather_fcst, "CODE","station")
setnames(weather_fcst,"Forecast Date","ForecastDate")

weather_fcst$ForecastDate <- ceiling_date(weather_fcst$ForecastDate, "hour")

allFcast <-split(weather_fcst,weather_fcst$ForecastDate) 

  previous <-allFcast[[1]] %>%
    mutate(datatype = "Yesterday's Forecast") %>%
    select(-ForecastDate)
  current <-allFcast[[2]]  %>%
    mutate(datatype = "Today's Forecast") %>%
    select(-ForecastDate)
  
forecast <- rbind(current,previous) %>%
  mutate(day = floor_date(date,unit = "day"),
         hour = hour(date)) 

write.csv(forecast, file = paste("C:/Users/jnicola/Desktop/WxFcst/Forecast_Data_",Sys.Date(),".csv",sep = ""))



