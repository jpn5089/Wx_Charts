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

obs_water_yr <- rbind(rain_pre2017, rain_2017) %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))
normal_water_yr <- rain_norm %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  mutate(wtr_yrVAR= "Normal") %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))

total_obs_rain <- data.table(obs_water_yr)
total_obs_rain[, value := cumsum(value), by=list(wtr_yrVAR)] 

final <- rbind(normal_water_yr, total_obs_rain) %>% 
  filter(CDate >= "1900-10-01" & CDate <= "1901-04-30") 

#write.csv(final, file = "C:\\Users\\John\\Desktop\\R\\PIT_06_17.csv")

################################################################################################
#Rain Plot 
################################################################################################

ggplot(final, aes(x= CDate, y = value, col = wtr_yrVAR, group = wtr_yrVAR,
                 linetype = wtr_yrVAR, size = wtr_yrVAR)) +
  geom_line() +
  scale_color_manual(values=c("navyblue", "#DA5724", "#74D944", "#CE50CA", "#D7C1B1", "#C0717C", "#CBD588", "#508578", 
                              "#673770", "#D3D93E", "#5F7FC7", "#38333E")) +
  scale_linetype_manual(values=c("dotted", rep("solid",11))) +
  scale_size_manual(values=c(1.5, rep(0.8,10),1.5)) +
  #scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") 
  scale_x_date() +
  labs(title = "Water Year - Observed Precipitation",
       y="Precipitation (in)",
       subtitle = "Pittsburgh",
       caption = "Data Source: GHCND and NCEI (formerly NCDC)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank()) +
  theme(legend.title = element_blank())
ggsave(filename = "C:\\Users\\John\\Desktop\\WaterYear_Precip.jpeg", width = 10, height = 7)

#####################################################################################################
#Snow Plot
#####################################################################################################

obs_snow_yr <- rbind(snow_pre17, snow_17) %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))
normal_snow_yr <- sn_norm %>%
  select(-X) %>%
  mutate(wtr_yrVAR=factor(wtr_yr(date))) %>%
  mutate(wtr_yrVAR= "Normal") %>%
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date))))
total_obs_snow <- data.table(obs_snow_yr)
total_obs_snow[, value := cumsum(value), by=list(wtr_yrVAR)]   

final_sn <- rbind(normal_snow_yr, total_obs_snow) %>% 
  filter(CDate >= "1900-10-01" & CDate <= "1901-04-30") 

###################################################################################################
#Plot
###################################################################################################

ggplot(final_sn, aes(x= CDate, y = value, col = wtr_yrVAR, group = wtr_yrVAR,
                 linetype = wtr_yrVAR, size = wtr_yrVAR)) +
  geom_line() +
  scale_color_manual(values=c("navyblue", "#DA5724", "#74D944", "#CE50CA", "#D7C1B1", "#C0717C", "#CBD588", "#5F7FC7", 
                              "#673770", "#D3D93E", "#508578", "#38333E")) +
  scale_linetype_manual(values=c("dotted", rep("solid",11))) +
  scale_size_manual(values=c(1.5, rep(0.8,10),1.5)) +
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
