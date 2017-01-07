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

ggplot(normals) +
  geom_ribbon(aes(x = normals$date, ymin = min, ymax = max), fill = "blue", alpha = 0.3)

#practice

a <- seq(1,10,1)
b <- seq(11,30,2)
c <- seq(1,5.5, 0.5)

g <- data.frame(a,b,c)

ggplot(g) +
  geom_ribbon(aes(x = a, ymin = c, ymax = b), fill = "blue")
