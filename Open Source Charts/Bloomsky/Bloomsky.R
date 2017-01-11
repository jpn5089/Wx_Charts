library(dplyr)
library(lubridate)
library(rnoaa)
library(ggplot2)

bloomsky <- read.csv(file = "C:\\Users\\John\\Downloads\\BloomSky_JP's Wx Station_01-02-17@0248PM-01-03-17@0248PM.csv") %>%
  mutate(Temperature = round((1.8*Temperature + 32),2)) %>%
  mutate(TS = ymd_hms(gsub("T"," ",TS)))
  
ggplot(bloomsky, aes(x = TS, y = Temperature)) +
  geom_line()