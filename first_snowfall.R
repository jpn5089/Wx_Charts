library(dplyr)
library(lubridate)
library(rnoaa)
library(ggplot2)
library(ggrepel)
library(scales)

options(noaakey = Sys.getenv("NOAAKEY"))

stations <- isd_stations()

years <- seq(1944,2017, by=1)

output <- list()

for (i in 1:length(years)) {
  
  snow <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00094823", datatypeid = "SNOW",
             startdate = paste("",years[i],"-01-01",sep = ""), enddate = paste("",years[i],"-12-31",sep = ""), limit = 1000)
  
  output[[i]] <- snow$data
  print(years[i])
}

snowfall <- do.call(rbind, output) %>% 
  mutate(date = ymd_hms(gsub("T"," ",date))) %>%
  mutate(station = "Pittsburgh") %>%
  mutate(value = round((value*0.03937008),1)) %>% 
  mutate(snow_year = wtr_yr(date)) %>% 
  mutate(snow_binary = value != 0) %>% 
  filter(snow_year > 1948) %>% 
  group_by(snow_year, snow_binary) %>% 
  arrange(snow_year) %>% 
  mutate(count= row_number(), 
         first_snow_of_year = snow_binary & count==1) %>% 
  filter(first_snow_of_year == T & count == 1) %>% 
  mutate(year = year(date), 
         month = month(date),
         day = day(date),
         day_month = format(date, format="%m-%d"), 
         day_month = as.Date(day_month, format = "%m-%d")) %>% 
  ungroup() %>% 
  select(day_month, year, count)

snowfall <- data.frame(day_month = ymd("2017-12-09"), year = "2017", count = 1) %>% 
  rbind(snowfall, .)

ggplot(snowfall, aes(x = day_month, y = count)) +
  geom_point() +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  geom_count(aes(y = count)) +
  geom_label_repel(data = snowfall[snowfall$day_month > "2017-11-25" & snowfall$year < 2017,], aes(x = day_month, y = count, label = year), 
                   size = 2.5,
                   box.padding = unit(0.5, "lines"),
                   point.padding = unit(0.7, "lines"),
                   arrow = arrow(length = unit(0.01, 'npc')),
                   segment.color = "black", segment.size = 0.5) +
  geom_label_repel(data = snowfall[snowfall$year == 2017,], aes(x = day_month, y = count, label = year), 
                   size = 2.5, color = "red",
                   box.padding = unit(0.5, "lines"),
                   point.padding = unit(0.7, "lines"),
                   arrow = arrow(length = unit(0.01, 'npc')),
                   segment.color = "red", segment.size = 0.5) +
  scale_x_date(date_breaks = "3 days", date_labels = "%m-%d", date_minor_breaks = "1 day") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        axis.title.x=element_blank()) +
  labs(title = "First day of measureable snowfall in Pittsburgh (1948-2017)", 
       subtitle = "Size of point represents number of occurrence for that date") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(filename = "C://Users/johnp/Documents/GitHub/Wx_Charts/first_snowfall.jpeg", width = 10, height = 7)




