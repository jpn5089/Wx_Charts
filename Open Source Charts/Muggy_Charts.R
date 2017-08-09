#' When plotting multiple data series that share a common x axis but different y axes,
#' we can just plot each graph separately. This suffers from the drawback that the shared axis will typically
#' not align across graphs due to different plot margins.
#' One easy solution is to reshape2::melt() the data and use ggplot2's facet_grid() mapping. However, there is
#' no way to label individual y axes.
#' facet_grid() and facet_wrap() were designed to plot small multiples, where both x- and y-axis ranges are
#' shared acros all plots in the facetting. While the facet_ calls allow us to use different scales with
#' the \code{scales = "free"} argument, they should not be used this way.
#' A more robust approach is to the grid package grid.draw(), rbind() and ggplotGrob() to create a grid of 
#' individual plots where the plot axes are properly aligned within the grid.
# Thanks to https://rpubs.com/MarkusLoew/13295 for the grid.arrange() idea.

#devtools::install_github("ALShum/rwunderground")
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
LocationsRow <- c(17,14,34,35)
cities <- list()

for (i in 1:4){
  muggy_wx <- hourly10day(set_location(lat_long = paste(as.character(Locations[LocationsRow[i],8]),",",as.character(Locations[LocationsRow[i],9]),sep = ""))) %>%
    select(date, dew = dew_pt, hum = humidity) %>%
    mutate(date = ymd_hms(date) - hours(Locations[LocationsRow[i],6])) %>%
    #mutate(format(date, tz = as.character(Locations[LocationsRow[i],5]))) %>%
    mutate(station = as.character(Locations[LocationsRow[i],1])) %>%
    mutate(dew = as.numeric(dew), datatype = "Forecast")%>%
    mutate(hum = as.numeric(hum), datatype = "Forecast")%>%
    mutate(day = floor_date(date,unit = "day"),
           hour = hour(date)) %>%
    mutate(date = as.character(date)) %>%
    mutate(day = as.character(day))
  
  cities[[i]] <- muggy_wx
}

FcstMuggy <-do.call(rbind,cities)

#colnames(FcstMuggy) <- c("X", "dew", "hum", "date", "station", "datatype", "day", "hour")

FcstMuggy <- FcstMuggy %>% 
  mutate(date = ymd_hms(date)) %>%
  mutate(day = floor_date(date,unit = "day"),
         hour = hour(date))

slocation <- paste("~/GitHub/Wx_Charts/Open Source Charts/Data/Muggy_Forecast_", Sys.Date(),".rds",sep = "")

saveRDS(FcstMuggy, file = slocation)

today_muggy <- readRDS(slocation) %>%
  mutate(datatype = "Today's Forecast")

yesterday_muggy <- readRDS(paste("~/GitHub/Wx_Charts/Open Source Charts/Data/Muggy_Forecast_", Sys.Date()-1,".rds", sep = "")) %>%
  mutate(datatype = "Yesterday's Forecast")

forecasts_muggy <- rbind(today_muggy, yesterday_muggy) %>%
  #select(-X) %>%
  mutate(date = ymd_hms(date))

FcstMuggy <- forecasts_muggy %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(day = floor_date(date,unit = "day")) %>%
  filter(floor_date(date,unit ="day") <= ymd(today())+days(4) & floor_date(date,unit ="day") >= ymd(today())+days(1)) %>%
  filter(station %in% c("KPIT","KTPA","KDRT","KPAPITTS201")) 

for (i in 1:4){
  plot1 <- ggplot(filter(FcstMuggy, station == Locations$short[LocationsRow[i]]), aes(x = hour, y = dew, col = datatype, group = datatype, linetype = datatype, size = datatype, alpha = datatype)) +
    geom_line() +
    scale_color_manual(values=c( "dark green","green"))+
    scale_linetype_manual(values=c("solid","dotted"))+
    scale_size_manual(values=c(1.25,1.75))+
    scale_alpha_manual(values = c(1,1)) +
    facet_wrap(~day,ncol = 4, scales = "free_x") +
    scale_x_continuous(breaks = c(seq(0,23,by=3))) +
    labs(title = Locations[LocationsRow[i],3], 
         x = "Local Time (0 is 12:00am)",
         y = expression(paste("Dew Point (",degree ~ F,")")),
         caption = "(Source: Weather Underground)") +
    theme_bw(base_size = 15) +
    theme(plot.caption = element_text(size = 9.5)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(vjust = 2)) +
    theme(legend.title = element_blank()) 

  plot2 <- ggplot(filter(FcstMuggy, station == Locations$short[LocationsRow[i]]), aes(x = hour, y = hum, col = datatype, group = datatype, linetype = datatype, size = datatype, alpha = datatype)) +
    geom_line() +
    scale_color_manual(values=c( "dark blue","purple"))+
    scale_linetype_manual(values=c("solid","dotted"))+
    scale_size_manual(values=c(1.25,1.75))+
    scale_alpha_manual(values = c(1,1)) +
    facet_wrap(~day,ncol = 4, scales = "free_x") +
    scale_x_continuous(breaks = c(seq(0,23,by=3))) +
    labs(x = "Local Time (0 is 12:00am)", y = expression(paste("Humidity (%)"))) +
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(vjust = 2)) +
    theme(legend.title = element_blank())

grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))


ggsave(plot1, file = paste("C:\\Users\\John\\Desktop\\Muggy_Plots\\Muggy_Plot_",Locations[LocationsRow[i],3],"_",Sys.Date(),".jpeg",sep = ""), width = 10, height = 7)
}






