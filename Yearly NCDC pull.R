#https://cran.r-project.org/web/packages/rnoaa/README.html
#https://ropensci.org/tutorials/rnoaa_tutorial.html
#https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf

library(dplyr)
library(lubridate)
library(rnoaa)
library(ggplot2)

options(noaakey = Sys.getenv("NOAAKEY"))

Stations <- read.csv("c:/users/John/Documents/GitHub/Wx_Charts/Data/StationNames.csv",stringsAsFactors = FALSE)

StationsRow <- c(14,17)

cityData_list <- list()

yearly_list <- list()

years <- seq(2015,2016, by=1)

ptm <- proc.time()

for (i in 1:2){
  
  for (j in years) {
  
  jan_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-01-01",sep = ""), enddate = paste("",j,"-02-01",sep = ""), limit = 1000)
  
  temps_jan <- jan_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)
  
  #in mph
  #W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)
  
  winds_jan <- jan_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))
  
  #in inches
  #T(fl_m) = trace of precipitation, snowfall, or snow depth
  #W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)
  
  precip_jan <- jan_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))
  
  #in inches
  #T(fl_m) = trace of precipitation, snowfall, or snow depth
  #W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)
  
  snowfall_jan <- jan_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))
  
  #in inches
  #W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)
  
  snow_depth_jan <- jan_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  january <- rbind(temps_jan, winds_jan, precip_jan, snowfall_jan, snow_depth_jan)

################################################################################################  
    
  feb_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-02-01",sep = ""), enddate = paste("",j,"-03-01",sep = ""), limit = 1000)
  
  temps_feb <- feb_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)
  
  winds_feb <- feb_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))
  
  precip_feb <- feb_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))

  snowfall_feb <- feb_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))

  snow_depth_feb <- feb_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  february <- rbind(temps_feb, winds_feb, precip_feb, snowfall_feb, snow_depth_feb)
 
  ################################################################################################   
   
  mar_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-03-01",sep = ""), enddate = paste("",j,"-04-01",sep = ""), limit = 1000)
  
  temps_mar <- mar_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)
 
  winds_mar <- mar_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))

  precip_mar <- mar_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))

  snowfall_mar <- mar_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))
 
  snow_depth_mar <- mar_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  march <- rbind(temps_mar, winds_mar, precip_mar, snowfall_mar, snow_depth_mar)
  
  ################################################################################################   
  
  apr_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-04-01",sep = ""), enddate = paste("",j,"-05-01",sep = ""), limit = 1000)
  
  temps_apr <- apr_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)
 
  winds_apr <- apr_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))
 
  precip_apr <- apr_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))
 
  snowfall_apr <- apr_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))
 
  snow_depth_apr <- apr_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  april <- rbind(temps_apr, winds_apr, precip_apr, snowfall_apr, snow_depth_apr)
  
  ################################################################################################   
  
  may_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-05-01",sep = ""), enddate = paste("",j,"-06-01",sep = ""), limit = 1000)
  
  temps_may <- may_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)

  winds_may <- may_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))

  precip_may <- may_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))

  snowfall_may <- may_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))

  snow_depth_may <- may_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  may <- rbind(temps_may, winds_may, precip_may, snowfall_may, snow_depth_may)
  
  ################################################################################################   
  
  jun_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-06-01",sep = ""), enddate = paste("",j,"-07-01",sep = ""), limit = 1000)
  
  temps_jun <- jun_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)

  winds_jun <- jun_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))

  precip_jun <- jun_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))

  snowfall_jun <- jun_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))
 
  snow_depth_jun <- jun_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  june <- rbind(temps_jun, winds_jun, precip_jun, snowfall_jun, snow_depth_jun)
  
  ################################################################################################   
  
  jul_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-07-01",sep = ""), enddate = paste("",j,"-08-01",sep = ""), limit = 1000)
  
  temps_jul <- jul_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)
 
  winds_jul <- jul_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))

  precip_jul <- jul_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))

  snowfall_jul <- jul_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))

  snow_depth_jul <- jul_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  july <- rbind(temps_jul, winds_jul, precip_jul, snowfall_jul, snow_depth_jul)
  
  ################################################################################################   
  
  aug_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-08-01",sep = ""), enddate = paste("",j,"-09-01",sep = ""), limit = 1000)
  
  temps_aug <- aug_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)
 
  winds_aug <- aug_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))
 
  precip_aug <- aug_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))
  
  snowfall_aug <- aug_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))
  
  snow_depth_aug <- aug_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  august <- rbind(temps_aug, winds_aug, precip_aug, snowfall_aug, snow_depth_aug)
  
  ################################################################################################   
  
  sept_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-09-01",sep = ""), enddate = paste("",j,"-10-01",sep = ""), limit = 1000)
  
  temps_sept <- sept_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)

  winds_sept <- sept_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))
 
  precip_sept <- sept_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))

  snowfall_sept <- sept_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))
 
  snow_depth_sept <- sept_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  september <- rbind(temps_sept, winds_sept, precip_sept, snowfall_sept, snow_depth_sept)
  
  ################################################################################################   
  
  oct_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-10-01",sep = ""), enddate = paste("",j,"-11-01",sep = ""), limit = 1000)
  
  temps_oct <- oct_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)

  winds_oct <- oct_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))
 
  precip_oct <- oct_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))
 
  snowfall_oct <- oct_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))

  snow_depth_oct <- oct_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  october <- rbind(temps_oct, winds_oct, precip_oct, snowfall_oct, snow_depth_oct)
  
  ################################################################################################   
  
  nov_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-11-01",sep = ""), enddate = paste("",j,"-12-01",sep = ""), limit = 1000)
  
  temps_nov <- nov_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)

  winds_nov <- nov_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))
 
  precip_nov <- nov_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))
  
  snowfall_nov <- nov_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))
 
  snow_depth_nov <- nov_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  november <- rbind(temps_nov, winds_nov, precip_nov, snowfall_nov, snow_depth_nov)
  
  ################################################################################################   
  
  dec_weather <- ncdc(datasetid='GHCND', stationid = as.character(Stations[StationsRow[i],2]), 
                      startdate = paste("",j,"-12-01",sep = ""), enddate = paste("",j,"-12-31",sep = ""), limit = 1000)
  
  temps_dec <- dec_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
    mutate(value = round(((value/10)*1.8)+32),0) %>%
    select(-`0`)

  winds_dec <- dec_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
    mutate(value = round(((value/10)*2.23693629),1))

  precip_dec <- dec_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("PRCP")) %>%
    mutate(value = round(((value/10)*0.03937008),2))
  
  snowfall_dec <- dec_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNOW")) %>%
    mutate(value = round((value*0.03937008),1))

  snow_depth_dec <- dec_weather$data %>%
    mutate(date = ymd_hms(gsub("T"," ",date))) %>%
    mutate(station = as.character(Stations[StationsRow[i],1])) %>%
    filter(datatype %in% c("SNWD")) %>%
    mutate(value = round((value*0.03937008),0))
  
  december <- rbind(temps_dec, winds_dec, precip_dec, snowfall_dec, snow_depth_dec)
  
  yearly_data <- rbind(january %>% filter(month(date) != 2), february %>% filter(month(date) != 3),
                    march %>% filter(month(date) != 4), april %>% filter(month(date) != 5), 
                    may %>% filter(month(date) != 6), june %>% filter(month(date) != 7),
                    july %>% filter(month(date) != 8), august %>% filter(month(date) != 9), 
                    september %>% filter(month(date) != 10), october %>% filter(month(date) != 11),
                    november %>% filter(month(date) != 12), december %>% filter(month(date) != 1))
                   
                   
  yearly_list[[j]] <- yearly_data
  yearly_weather <-do.call(rbind,yearly_list)
  }
 cityData_list[[i]] <- yearly_weather
}

all_weather <- do.call(rbind,cityData_list) %>%
  mutate(month = month(date)) %>%
  mutate(year  = year(date))

proc.time() - ptm

rain <- filter(all_weather, datatype == "PRCP") %>%
  mutate(year = year(date)) %>%
  filter(year == 2016)
#rain <- filter(rain, station == "KTPA")
colnames(rain)[4] <- "Total"

ggplot(rain, aes(date, Total)) +
  geom_point(aes(color=Total)) +
  geom_smooth(color="blue", size=1) +
  scale_colour_gradient() +
  labs(title = "2016 Daily Precipitation",
       y="Rain (in)",
       subtitle = "Tampa",
       caption = "Data Source: NCEI (formerly NCDC)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank()) +
  theme(legend.position="none")