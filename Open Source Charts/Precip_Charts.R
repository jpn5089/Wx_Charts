library(rwunderground)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(grid)
library(plotly)
library(tibble)

#setwd("~/")
#table <- list_airports()

rwunderground::set_api_key(Sys.getenv("GET_API_KEY"))

Locations <- read.csv("https://raw.githubusercontent.com/jpn5089/Wx_Charts/master/Data/StationNames.csv",stringsAsFactors = FALSE)
LocationsRow <- c(17,14,35)
cities <- list()

for (i in 1:3){
  precip <- hourly10day(set_location(lat_long = paste(as.character(Locations[LocationsRow[i],8]),",",as.character(Locations[LocationsRow[i],9]),sep = ""))) %>%
    select(date, rain, pop) %>%
    mutate(date = ymd_hms(date) - hours(Locations[LocationsRow[i],6]) + hours(1)) %>%
    mutate(station = as.character(Locations[LocationsRow[i],1])) %>%
    mutate(rain = as.numeric(rain), datatype = "Forecast")%>%
    mutate(pop = as.numeric(pop), datatype = "Forecast")%>%
    mutate(day = floor_date(date,unit = "day"),
           hour = hour(date)) %>%
    mutate(date = as.character(date)) %>%
    mutate(day = as.character(day)) %>%
    mutate(total = cumsum(rain))
  
  cities[[i]] <- precip
}

FcstPrecip <-do.call(rbind,cities)

#colnames(FcstMuggy) <- c("X", "dew", "hum", "date", "station", "datatype", "day", "hour")

FcstPrecip <- FcstPrecip %>% 
  mutate(date = ymd_hms(date)) %>%
  mutate(day = floor_date(date,unit = "day"),
         hour = hour(date))

slocation <- paste("~/GitHub/Wx_Charts/Open Source Charts/Data/Precip_Forecast_", Sys.Date(),".rds",sep = "")

saveRDS(FcstPrecip, file = slocation)

today_precip <- readRDS(slocation) %>%
  mutate(datatype = "Today's Forecast")

yesterday_precip <- readRDS(paste("~/GitHub/Wx_Charts/Open Source Charts/Data/Precip_Forecast_", Sys.Date()-1,".rds", sep = "")) %>%
  mutate(datatype = "Yesterday's Forecast")

forecasts_precip <- rbind(today_precip, yesterday_precip) %>%
  #select(-X) %>%
  mutate(date = ymd_hms(date))

FcstPrecip <- forecasts_precip %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(day = floor_date(date,unit = "day")) %>%
  filter(floor_date(date,unit ="day") <= ymd(today())+days(5) & floor_date(date,unit ="day") >= ymd(today())+days(1)) %>%
  filter(station %in% c("KPIT","KTPA", "KDRT", "KPAPITTS201")) 

for (i in 1:4){
  plot1 <- ggplot(filter(FcstPrecip, station == Locations$short[LocationsRow[i]]),
                  aes(x = hour, y = pop, col = datatype, group = datatype,
                      linetype = datatype, size = datatype, alpha = datatype)) +
    geom_line() +
    scale_color_manual(values=c( "dark green","green"))+
    scale_linetype_manual(values=c("solid","dotted"))+
    scale_size_manual(values=c(1.25,1.75))+
    scale_alpha_manual(values = c(1,1)) +
    facet_wrap(~day,ncol = 5, scales = "free_x") +
    scale_x_continuous(breaks = c(seq(0,23,by=3))) +
    labs(title = Locations[LocationsRow[i],3], 
         x = "Local Time (0 is 12:00am)", 
         y=expression(paste("Probability of Precip (%)"))) +
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(vjust = 2)) +
    theme(legend.title=element_blank())
  
  plot2 <- ggplot(filter(FcstPrecip, station == Locations$short[LocationsRow[i]]), aes(x = hour, y = total, col = datatype, group = datatype, linetype = datatype, size = datatype, alpha = datatype)) +
    geom_line() +
    scale_color_manual(values=c( "dark blue","light blue"))+
    scale_linetype_manual(values=c("solid","dotted"))+
    scale_size_manual(values=c(1.25,1.75))+
    scale_alpha_manual(values = c(1,1)) +
    facet_wrap(~day,ncol = 5, scales = "free_x") +
    scale_x_continuous(breaks = c(seq(0,23,by=3))) +
    labs(title = Locations[LocationsRow[i],3], x = "Local Time (0 is 12:00am)", 
         y = "Total Rainfall (inches)",
         caption = "Source: Weather Underground") +
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(vjust = 2)) +
    theme(plot.caption = element_text(size=9.5)) +
    theme(legend.title=element_blank())
  print(plot2)
  
  grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))
  
  ggsave(plot2, file = paste("C:\\Users\\John\\Desktop\\Precip_Plots\\Precip_Plot_",Locations[LocationsRow[i],3],"_",Sys.Date(),".jpeg",sep = ""), width = 10, height = 7)
}

