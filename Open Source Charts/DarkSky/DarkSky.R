#https://blog.rstudio.org/2016/11/14/ggplot2-2-2-0/

#https://github.com/hrbrmstr/darksky

#https://rstudio.github.io/tensorflow/
#https://spark.apache.org/docs/latest/sparkr.html

#http://forecastwatch.com/static/Three_Region_Overview_2010_201606.pdf

#https://github.com/rudeboybert/fivethirtyeight

devtools::install_github("hrbrmstr/darksky")
library(darksky)
library(dplyr)
library(ggplot2)
library(purrr)


Sys.getenv("DARKSKY_API_KEY")

station_info <- read.csv(file = "C:\\Users\\johnp/Documents\\GitHub\\Wx_Charts\\Data\\isd-history.csv") %>% 
  filter(STATE == "PA")

now <- get_current_forecast(43.2672, -70.8617)
plot(now)
forecast <- now$daily

then <- get_forecast_for(43.2672, -70.8617, "2013-05-06T12:00:00-0400", add_headers=TRUE)

then$hourly

print(sprintf("You have used %s API calls.", then$`x-forecast-api-calls`))

seq(Sys.Date()-10, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(40.485, -80.2, .x)) %>% 
  map_df("hourly") %>% 
  ggplot(aes(x=time, y=windSpeed)) +
  geom_line()

########################################################################

#You are required to display the message "Powered by Dark Sky" 

########################################################################