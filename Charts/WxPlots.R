library(ggplot2)
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

setwd("~/")

Stations <- read.csv("~/GitHub/Wx_Charts/Data/StationNames.csv",stringsAsFactors = FALSE)

StationsRow <- c(6,14,1,13,7,16,5)

normals_full <-read.csv("~/GitHub/Wx_Charts/Data/Temp_Normals.csv", stringsAsFactors = FALSE) %>%
  select(-X)

fcast <- read.csv(file = paste("T:\\Scheduling\\Power\\load forecasting\\zzz pjm files folder\\Weather/Forecast_Data_",Sys.Date(),".csv", sep = ""), stringsAsFactors = FALSE) %>%
  select(-X,-variable) 

all <- rbind(normals_full,fcast) %>%
  mutate(date = ymd_hms(date)) %>%
  filter(floor_date(date,unit ="day") < ymd(today())+days(11) & floor_date(date,unit ="day") > ymd(today()))

for (i in 1:7){
  
  plots <- ggplot(filter(all, station == Stations$short[StationsRow[i]]),aes(x=hour, y=value, col = datatype, group = datatype, linetype = datatype, size = datatype)) +
    geom_line() +
    scale_color_manual(values=c( "blue","black", "red"))+
    scale_linetype_manual(values=c("solid","longdash","dotted"))+
    scale_size_manual(values=c(1,1,1.5))+
    theme(legend.title=element_blank())+
    labs(title = Stations[StationsRow[i],4], x="Hour of Day (0 is Midnight/12:00am)", y=expression(paste("Temperature ( ",degree ~ F," )"))) + 
    facet_wrap(~day,ncol = 5, scales = "free_x") + 
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(vjust = 2))+
    theme(legend.title = element_blank())+
    theme(axis.title.x = element_text(vjust=-0.25))+
    theme(axis.text.x  = element_text(size=7))+
    scale_x_continuous(breaks = c(seq(0,23,by=3)))
  
  ggsave(plots, file = paste("T:\\Weather Charts\\Temp_Plot_",Stations[StationsRow[i],3],"_",Sys.Date(),".pdf",sep = ""), width = 10, height = 7)
}
