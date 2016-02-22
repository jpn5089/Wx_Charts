devtools::install_github("ALShum/rwunderground")
library(rwunderground)
library(ggplot2)
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
library(scales)

rwunderground::set_api_key("d30db447d19d9927")

table <- list_airports()

Locations <- read.csv("c:/users/jnicola/Documents/GitHub/Wx_Charts/Data/StationNames.csv",stringsAsFactors = FALSE)

LocationsRow <- c(14,17)

cities <- list()

for (i in 1:2){
  temp_wx <- hourly10day(set_location(airport_code = as.character(Locations[LocationsRow[i],1]))) %>%
    select(date,value = temp) %>%
    mutate(station = as.character(Locations[LocationsRow[i],1])) %>%
    mutate(value = as.numeric(value), datatype = "Forecast")%>%
    mutate(day = floor_date(date,unit = "day"),
           hour = hour(date)) %>%
    mutate(day = as.character(day)) %>%
    mutate(date = as.character(date))

  cities[[i]] <- temp_wx

}

FcstAll <-do.call(rbind,cities)

all_normals <-read.csv("C:/Users/jnicola/Documents/GitHub/Wx_Charts/Data/Temp_Normals.csv", stringsAsFactors = FALSE) %>%
  select(-X) %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(date = as.character(date))

fcst_norm <- rbind(all_normals,FcstAll) %>%
  mutate(date = ymd_hms(date)) %>%
  filter(station %in% c("KPIT","KTPA")) %>%
  filter(floor_date(date,unit ="day") < ymd(today())+days(9) & floor_date(date,unit ="day") > ymd(today()))


for (i in 1:2){
  plots <- ggplot(filter(fcst_norm, station == Locations$short[StationsRow[i]]), aes(x=hour, y=value, col = datatype, group = datatype, linetype = datatype, size = datatype)) +
    geom_line() +
    scale_color_manual(values=c( "red","black", "red"))+
    scale_linetype_manual(values=c("solid","longdash","dotted"))+
    scale_size_manual(values=c(1,1,1.5))+
    theme(legend.title=element_blank())+
    labs(title = Stations[StationsRow[i],4], x="Hour of Day (0 is Midnight/12:00am)", y=expression(paste("Temperature ( ",degree ~ F," )"))) + 
    facet_wrap(~day,ncol = 4, scales = "free_x") + 
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(vjust = 2))+
    theme(legend.title = element_blank())+
    theme(axis.title.x = element_text(vjust=-0.25))+
    theme(axis.text.x  = element_text(size=7))+
    scale_x_continuous(breaks = c(seq(0,23,by=3)))
  
  ggsave(plots, file = paste("C:\\Users\\jnicola\\Desktop\\Temp_Plot_",Stations[StationsRow[i],3],"_",Sys.Date(),".pdf",sep = ""), width = 10, height = 7)
}


