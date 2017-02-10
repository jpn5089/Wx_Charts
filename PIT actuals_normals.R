#https://cran.r-project.org/web/packages/rnoaa/README.html
#https://ropensci.org/tutorials/rnoaa_tutorial.html
#ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/readme.txt
#ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/documentation/precipitation-filenames.txt

library(rnoaa)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(data.table)

options(noaakey = Sys.getenv("NOAAKEY"))

#2/9/2017 ftp://ftp.ncdc.noaa.gov/pub/data/noaa
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
  geom_ribbon(aes(x = normals$date, ymin = min, ymax = max), fill = "blue", alpha = 0.15)

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
obsvred <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823", datatypeid = "PRCP",
            startdate = "2017-01-01", enddate = "2017-02-05", limit = 1000)

obs_rain <- obsvred$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(date = as.Date(date)) %>%
  mutate(value = round(((value/10)*0.03937008),2)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  #mutate(value = cumsum(value)) %>%
  mutate(year = year(date)) %>%
  select(date, datatype, station, value, year, dayofyear)
  
prcp <- out$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))- years(100)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(value = (value/100)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  mutate(year = "Normal") %>%
  select(date, datatype, station, value, year, dayofyear)

all_weather <- filter(precip_act, station == "KPIT", datatype == "PRCP") %>%
  mutate(date = mdy(date)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  #mutate(value = cumsum(value)) %>%
  select(date, datatype, station, value, year, dayofyear)

ttl <- data.table(precip_act)
ttl[, value := cumsum(value), by=list(year)] 

great <- rbind(prcp, total) %>%
  mutate(dayofyear = as.Date(dayofyear, "%m-%d")) %>%
  filter(dayofyear <= as.Date("2017-02-28")) 

write.csv(great, file = "C:\\Users\\John\\Desktop\\rain.csv")
rain <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\PIT_06_17.csv") %>%
  select(-X) %>%
  mutate(date = mdy(date)) %>%
  mutate(CDate = mdy(CDate)) %>%
  mutate(CDate = as.Date(CDate))
  #mutate(dayofyear = mdy(dayofyear)) %>%
  #mutate(dayofyear = as.Date(dayofyear))

ggplot(great, aes(x= dayofyear, y = value, col = year, group = year,
                  linetype = year, size = year)) +
  geom_line() +
  scale_color_manual(values=c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#D7C1B1", "#C0717C", "#CBD588", "#5F7FC7", 
                     "#673770", "#D3D93E", "#38333E", "#508578", "#3F4921")) +
  scale_linetype_manual(values=c(rep("solid",12), "dotted")) +
  scale_size_manual(values=c(rep(0.8,11), 1.5, 1.5)) +
  #scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") 
  scale_x_date() +
  labs(title = "Observed Precipitation",
       y="Precipiation (in)",
       subtitle = "Pittsburgh",
       caption = "Data Source: GHCND and NCEI (formerly NCDC)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank()) +
  theme(legend.title = element_blank())
ggsave(filename = "C:\\Users\\John\\Desktop\\ObsPrecip.jpeg", width = 10, height = 7)


#####################################################################################################
# SNOW 
#####################################################################################################

snow <- filter(precip_act, datatype == "SNOW") %>%
  mutate(date = mdy(date)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  #mutate(value = cumsum(value)) %>%
  select(date, datatype, station, value, year, dayofyear) 

#total_snow <- data.table(snow)
#total_snow[, value := cumsum(value), by=list(year)]

sn <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823", datatypeid = "SNOW",
                startdate = "2017-01-01", enddate = "2017-01-31", limit = 1000)

snw <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\snow_norm.csv") %>%
  mutate(date = mdy(date)) 

hellosnow <- sn$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(value = round(((value)*0.03937008),1)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  mutate(year = year(date)) %>%
  select(date, datatype, station, value, year, dayofyear)

great_snow <- rbind(total_snow, hellosnow) %>%
  mutate(dayofyear = as.Date(dayofyear, "%m-%d")) %>%
  mutate(year = year(date)) %>%
  mutate(year = factor(year)) %>%
  filter(dayofyear <= as.Date("2017-01-31")) 

ggplot(great_snow, aes(x= dayofyear, y = value, col = year, group = year,
                 linetype = year, size = year)) +
  geom_line() +
  scale_color_manual(values=c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#D7C1B1", "#C0717C", "#CBD588", "#5F7FC7", 
                              "#673770", "#D3D93E", "#38333E", "#508578")) +
  scale_linetype_manual(values=c(rep("solid",11), "dotted")) +
  scale_size_manual(values=c(rep(0.8,11), 1.5)) +
  #scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") 
  scale_x_date() +
  labs(title = "Observed Snowfall",
       y="Snowfall (in)",
       subtitle = "Pittsburgh",
       caption = "Data Source: GHCND and NCEI (formerly NCDC)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank()) +
  theme(legend.title = element_blank())
print()
ggsave(filename = "C:\\Users\\John\\Desktop\\ObsSnow.jpeg", width = 10, height = 7)
