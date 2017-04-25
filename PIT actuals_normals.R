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

standard_dev <- ncdc(datasetid='NORMAL_DLY', stationid='GHCND:USW00094823', datatypeid = c("DLY-TAVG-STDDEV", "DLY-TMAX-STDDEV"),
              startdate = '2010-01-01', enddate = '2010-12-31', limit = 1000)

standard_devs <- ncdc(datasetid='NORMAL_DLY', stationid='GHCND:USW00094823', datatypeid = "DLY-TMIN-STDDEV",
                     startdate = '2010-01-01', enddate = '2010-12-31', limit = 1000)

stdev <- standard_dev$data
std_d <- standard_devs$data

st <- rbind(stdev, std_d)

std_devs <- st %>%
  mutate(date = ymd_hms(gsub("T"," ",date)) + years(7),
         value = (value/10)) %>%
  mutate(value = as.numeric(value))%>%
  mutate(month = month(date)) %>%
  mutate(month_day = format(date, format="%m-%d")) %>%
  filter(datatype == "DLY-TAVG-STDDEV") %>%
  select(month_day,datatype,value)

#write.csv(std_devs, "C:/Users/John/Desktop/R/Pitt_StDev.csv" )

##############################################################################################################
#http://stackoverflow.com/questions/19643234/fill-region-between-two-loess-smoothed-lines-in-r-with-ggplot

#Plot 1 standard deviation too. Get all values from 1981-2010 and average high temp devs from norm and same 
#with low temps 
##############################################################################################################

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

precip_act <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\PIT_act_06_16.csv")

#precip_act <- subset(precip_act, year != 2006 && month < 10)

#out <- ncdc(datasetid='NORMAL_DLY', stationid='GHCND:USW00094823', datatypeid='YTD-PRCP-NORMAL', startdate = '2010-01-01', enddate = '2010-12-31', limit = 400)

rain_norm <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\rain_norm.csv") %>%
  select(-X) %>%
  mutate(date = mdy(date))

obsvred <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823", datatypeid = "PRCP",
            startdate = "2017-01-01", enddate = "2017-04-30", limit = 1000)

rain_2017 <- obsvred$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(date = as.Date(date)) %>%
  mutate(value = round(((value/10)*0.03937008),2)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  #mutate(value = cumsum(value)) %>%
  mutate(year = year(date)) %>%
  select(date, datatype, station, value, year, dayofyear)

rain_pre2017 <- filter(precip_act, station == "KPIT", datatype == "PRCP") %>%
  mutate(date = mdy(date)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  #mutate(value = cumsum(value)) %>%
  select(date, datatype, station, value, year, dayofyear)

rain <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\PIT_06_17.csv") %>%
  select(-X) %>%
  mutate(date = mdy(date)) %>%
  mutate(CDate = mdy(CDate)) %>%
  mutate(CDate = as.Date(CDate))
  #filter(CDate >= "1900-10-01" & CDate <= "1901-03-28")
  #mutate(dayofyear = mdy(dayofyear)) %>%
  #mutate(dayofyear = as.Date(dayofyear))

#################################################################
#Below is for caledar year
#################################################################

#ttl <- data.table(precip_act)
#ttl[, value := cumsum(value), by=list(year)] 

great <- rbind(rain_norm, ttl) %>%
  mutate(dayofyear = as.Date(dayofyear, "%m-%d")) %>%
  filter(dayofyear <= as.Date("2017-02-28")) 

#write.csv(great, file = "C:\\Users\\John\\Desktop\\R\\rain.csv")

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

snow_pre17 <- filter(precip_act, datatype == "SNOW") %>%
  mutate(date = mdy(date)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  #mutate(value = cumsum(value)) %>%
  select(date, datatype, station, value, year, dayofyear)

sn_norm <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\snow_norm.csv") %>%
  mutate(date = mdy(date)) 

sn <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823", datatypeid = "SNOW",
                startdate = "2017-01-01", enddate = "2017-04-28", limit = 1000)

snow_17 <- sn$data %>%
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(value = round(((value)*0.03937008),1)) %>%
  mutate(dayofyear = format(date, format="%m-%d")) %>%
  mutate(year = year(date)) %>%
  select(date, datatype, station, value, year, dayofyear)

#write.csv(snow_17,file = "C:\\Users\\John\\Desktop\\R\\snowfix.csv" )
#snow_17 <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\snowfix.csv") %>%
#  select(-X) %>%
#  mutate(date = mdy(date))

# Below is for Calendar Year

#total_snow <- data.table(snow)
#total_snow[, value := cumsum(value), by=list(year)]

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
