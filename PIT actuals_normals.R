#https://cran.r-project.org/web/packages/rnoaa/README.html
#https://ropensci.org/tutorials/rnoaa_tutorial.html
#ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/readme.txt

library(rnoaa)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

options(noaakey = Sys.getenv("NOAAKEY"))

station_info <- read.csv(file = "C:\\Users\\John\\Documents\\GitHub\\Wx_Charts\\Data\\isd-history.csv")

obs <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823", datatypeid = c("TMAX", "TMIN"),
            startdate = "2016-10-01", enddate = "2017-01-07", limit = 1000)

every <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823",
            startdate = "2016-10-01", enddate = "2016-10-07", limit = 1000)

obsv <- obs$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date)),
         value = ((value/10)*1.8)+32) %>%
  mutate(value = round(value,1)) %>%
  select(date,datatype,value) %>%
  mutate(value = as.numeric(value))%>%
  mutate(month = month(date)) 

norms <- ncdc(datasetid='NORMAL_DLY', stationid='GHCND:USW00094823', datatypeid = c("DLY-TMAX-NORMAL", "DLY-TMIN-NORMAL"),
                    startdate = '2010-01-01', enddate = '2010-12-31', limit = 1000)

normals <- norms$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date)) + years(7),
         value = (value/10) ) %>%
  select(date,datatype,value) %>%
  mutate(value = as.numeric(value))%>%
  mutate(month = month(date))

write.csv(normals, "C:/Users/John/Desktop/R/Pitt_norms.csv" )
####################
#manually made edits
####################
normals <- read.csv("C:/Users/John/Desktop/R/Pitt_norms.csv") %>%
  mutate(date = mdy(date))

#normals_max <-  filter(normals, datatype == "DLY-TMAX-NORMAL") %>%
#  select(date, value, month)
#colnames(normals_max)[3] <- "max"
#normals_min <-  filter(normals, datatype == "DLY-TMIN-NORMAL") %>%
#  select(date, value, month)
#colnames(normals_min)[3] <- "min"

#clean <- bind_rows(normals_max, normals_min)

#http://stackoverflow.com/questions/19643234/fill-region-between-two-loess-smoothed-lines-in-r-with-ggplot

ggplot(normals) +
  geom_ribbon(aes(x = normals$date, ymin = min, ymax = max), fill = "blue", alpha = 0.3)

#practice

a <- seq(1,10,1)
b <- seq(11,30,2)
c <- seq(1,5.5, 0.5)

g <- data.frame(a,b,c)

ggplot(g) +
  geom_ribbon(aes(x = a, ymin = c, ymax = b), fill = "blue")


#################################################################################################
#################################################################################################

#Precip



out <- ncdc(datasetid='NORMAL_DLY', stationid='GHCND:USW00094823', datatypeid='YTD-PRCP-NORMAL', startdate = '2010-01-01', enddate = '2010-12-31', limit = 400)
prcp <- out$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(value = (value/100)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  mutate(year = "Normal") %>%
  select(date, datatype, station, value, year, dayofyear)

precip_act <- filter(all_weather, station == "KPIT", datatype == "PRCP", year == 2015) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  mutate(value = cumsum(value)) %>%
  select(date, datatype, station, value, year, dayofyear)

great <- rbind(precip_act, prcp) %>%
  mutate(dayofyear = as.Date(dayofyear, "%m-%d"))

ggplot(great, aes(x= dayofyear, y = value, col = year, group = year,
                  linetype = year, size = year)) +
  geom_line() +
  scale_color_manual(values=c( "blue","red")) +
  scale_linetype_manual(values=c("solid","solid")) +
  scale_size_manual(values=c(1.25,1.75)) +
  #scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") 
  scale_x_date(date_breaks = "1 month", date_labels = "%B")
