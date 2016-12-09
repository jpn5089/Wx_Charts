#PRCP = Precipitation (mm or inches as per user preference, inches to hundredths on Daily Form pdf file) 
#SNOW = Snowfall (mm or inches as per user preference, inches to tenths on Daily Form pdf file) 
#SNWD = Snow depth (mm or inches as per user preference, inches on Daily Form pdf file) 
#TMAX = Maximum temperature (Fahrenheit or Celsius as per user preference, Fahrenheit to tenths on Daily Form pdf file 
#TMIN = Minimum temperature (Fahrenheit or Celsius as per user preference, Fahrenheit to tenths on Daily Form pdf file 
#AWND = Average daily wind speed (meters per second or miles per hour as per user preference) 
#WDF2 = Direction of fastest 2-minute wind (degrees) 
#WDF5 = Direction of fastest 5-second wind (degrees) 
#WSF2 = Fastest 2-minute wind speed (miles per hour or  meters per second as per user preference) 
#WSF5 = Fastest 5-second wind speed (miles per hour or  meters per second as per user preference) 

#y <- ncdc_stations(datasetid='NORMAL_HLY', limit = 1000)
#Norms_HLY_Stations <- y$data

library(dplyr)
library(lubridate)
library(rnoaa)
library(ggplot2)

options(noaakey = ...)
stations <- ncdc_stations()

rawdata <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823",
             startdate = "1993-03-15", enddate = "1993-03-19", limit = 1000)

#in degrees F
#H(fl_m) = represents highest or lowest hourly temperature (TMAX or TMIN) or average of hourly values (TAVG) 
#W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)
#S(fl_so) = Global Summary of the Day (NCDC DSI-9618) NOTE: "S" values are derived from hourly synoptic reports
#exchanged on the Global Telecommunications System (GTS). Daily values derived in this fashion may differ
#significantly from "true" daily data, particularly for precipitation(i.e., use with caution)

temps <- rawdata$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(station = "PIT Int'l Airport") %>%
  filter(datatype %in% c("TMAX", "TMIN", "TAVG")) %>%
  mutate(value = round(((value/10)*1.8)+32),0) %>%
  select(-`0`)
  
#in mph
#W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)

winds <- rawdata$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(station = "PIT Int'l Airport") %>%
  filter(datatype %in% c("AWND", "WSF2", "WSF5")) %>%
  mutate(value = round(((value/10)*2.23693629),1))

#in inches
#T(fl_m) = trace of precipitation, snowfall, or snow depth
#W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)

precip <- rawdata$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(station = "PIT Int'l Airport") %>%
  filter(datatype %in% c("PRCP")) %>%
  mutate(value = round(((value/10)*0.03937008),2))

#in inches
#T(fl_m) = trace of precipitation, snowfall, or snow depth
#W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)

snowfall <- rawdata$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(station = "PIT Int'l Airport") %>%
  filter(datatype %in% c("SNOW")) %>%
  mutate(value = round((value*0.03937008),1))

#in inches
#W(fl_so) = WBAN/ASOS Summary of the Day from NCDC's Integrated Surface Data (ISD)
  
snow_depth <- rawdata$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(station = "PIT Int'l Airport") %>%
  filter(datatype %in% c("SNWD")) %>%
  mutate(value = round((value*0.03937008),0))

weather <- rbind(temps, winds, precip, snow_depth, snowfall)

  
  