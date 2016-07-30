#devtools::install_github("ALShum/rwunderground")
library(rwunderground)
library(ggplot2)
library(dplyr)
library(lubridate)
#library(RCurl)
#library(data.table)
#library(devtools)
#library(reshape2)
#library(tidyr)
library(scales)

#setwd("~/")

rwunderground::set_api_key("d30db447d19d9927")

#table <- list_airports()

# pointed this to the raw data on the web - this way we don't ever need to worry about the file path!
Locations <- read.csv("https://raw.githubusercontent.com/jpn5089/Wx_Charts/master/Data/StationNames.csv",stringsAsFactors = FALSE)

LocationsRow <- c(17,14,20)

cities <- list()

for (i in 1:3){
  temp_wx <- hourly10day(set_location(airport_code = as.character(Locations[LocationsRow[i],1]))) %>%
    select(date,value = temp) %>%
    mutate(date = ymd_hms(date)) %>%
    mutate(format(date, tz = as.character(Locations[LocationsRow[i],5]))) %>%
    mutate(station = as.character(Locations[LocationsRow[i],1])) %>%
    mutate(value = as.numeric(value), datatype = "Forecast")%>%
    mutate(day = floor_date(date,unit = "day"),
           hour = hour(date)) %>%
    mutate(date = as.character(date)) %>%
    mutate(day = as.character(day))
  
  
  cities[[i]] <- temp_wx
}

FcstAll <-do.call(rbind,cities)

colnames(FcstAll) <- c("X", "value", "date", "station", "datatype", "day", "hour")

FcstAll <- FcstAll %>% 
  mutate(date = ymd_hms(date)) %>%
  mutate(day = floor_date(date,unit = "day"),
         hour = hour(date))

slocation <- paste("~/GitHub/Wx_Charts/Open Source Charts/Data/Forecast_", Sys.Date(),".csv",sep = "")

write.csv(FcstAll, file = slocation)

today <- read.csv(slocation) %>%
  mutate(datatype = "Today's Forecast")

yesterday <- read.csv(paste("~/GitHub/Wx_Charts/Open Source Charts/Data/Forecast_", Sys.Date()-1,".csv", sep = "")) %>%
  mutate(datatype = "Yesterday's Forecast")

forecasts <- rbind(today, yesterday) %>%
  select(-X, -X.1) %>%
  mutate(date = ymd_hms(date))

all_normals <-read.csv("~/GitHub/Wx_Charts/Data/Temp_Normals.csv", stringsAsFactors = FALSE) %>%
  select(-X) %>%
  mutate(date = mdy_hm(date)) %>%
  mutate(day = floor_date(date,unit = "day"))
#  mutate(date = as.character(date))

fcst_norm <- rbind(all_normals,forecasts) %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(day = floor_date(date,unit = "day")) %>%
  filter(floor_date(date,unit ="day") <= ymd(today())+days(8) & floor_date(date,unit ="day") >= ymd(today())+days(1)) %>%
  filter(station %in% c("KTPA", "KPIT", "KBHB")) 

for (i in 1:3){
  plots <- ggplot(filter(fcst_norm, station == Locations$short[LocationsRow[i]]), aes(x=hour, y=value, col = datatype, group = datatype, linetype = datatype, size = datatype, alpha = datatype)) +
    geom_line() +
    scale_color_manual(values=c( "blue","red", "black"))+
    scale_linetype_manual(values=c("solid","solid","dotted"))+
    scale_size_manual(values=c(1.25,1.75,1.75))+
    scale_alpha_manual(values = c(1,1,1))+
    theme(legend.title=element_blank())+
    labs(title = Locations[LocationsRow[i],4], x="Hour of Day (0 is Midnight/12:00am)", y=expression(paste("Temperature ( ",degree ~ F," )"))) + 
    facet_wrap(~day,ncol = 4, scales = "free_x") + 
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(vjust = 2))+
    theme(legend.title = element_blank())+
    theme(axis.title.x = element_text(vjust=-0.25))+
    theme(axis.text.x  = element_text(size=7))+
    scale_x_continuous(breaks = c(seq(0,23,by=3)))
  print(plots)
  ggsave(plots, file = paste("C:\\Users\\John\\Desktop\\Temp_Plots\\Temp_Plot_",Locations[LocationsRow[i],3],"_",Sys.Date(),".jpeg",sep = ""), width = 10, height = 7)
}




