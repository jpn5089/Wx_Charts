wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

precip_act <- read.csv(file = "C:\\Users\\John\\Desktop\\R\\PIT_act_06_16.csv")

#precip_act <- subset(precip_act, year != 2006 && month < 10)

obs_water_yr <- rbind(all_weather, obs_rain) %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))
normal_water_yr <- prcp %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  mutate(wtr_yrVAR= "Normal") %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))
total <- data.table(obs_water_yr)
total[, value := cumsum(value), by=list(wtr_yrVAR)] 

final <- rbind(total, normal_water_yr) %>% 
  filter(CDate >= "1900-01-01" & CDate <= "1901-02-28") 

write.csv(final, file = "C:\\Users\\John\\Desktop\\R\\PIT_06_17.csv")



#####################################################################################################
#Snow
#####################################################################################################

obs_snow_yr <- rbind(snow, hellosnow) %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))
normal_snow_yr <- snw %>%
  select(-X) %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  mutate(wtr_yrVAR= "Normal") %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))
total <- data.table(obs_snow_yr)
total[, value := cumsum(value), by=list(wtr_yrVAR)]   

final_sn <- rbind(total, normal_snow_yr) %>% 
  filter(CDate >= "1900-10-01" & CDate <= "1901-02-28") 

################################################################################################

ggplot(final_sn, aes(x= CDate, y = value, col = wtr_yrVAR, group = wtr_yrVAR,
                  linetype = wtr_yrVAR, size = wtr_yrVAR)) +
  geom_line() +
  scale_color_manual(values=c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#D7C1B1", "#C0717C", "#CBD588", "#5F7FC7", 
                              "#673770", "#D3D93E", "#38333E", "#508578")) +
  scale_linetype_manual(values=c(rep("solid",11), "dotted")) +
  scale_size_manual(values=c(rep(0.8,10),1.5, 1.5)) +
  #scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") 
  scale_x_date() +
  labs(title = "Water Year - Observed Snowfall",
       y="Snowfall (in)",
       subtitle = "Pittsburgh",
       caption = "Data Source: GHCND and NCEI (formerly NCDC)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank()) +
  theme(legend.title = element_blank())
ggsave(filename = "C:\\Users\\John\\Desktop\\WaterYear_Snow.jpeg", width = 10, height = 7)
