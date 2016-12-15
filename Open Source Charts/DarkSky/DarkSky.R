#https://blog.rstudio.org/2016/11/14/ggplot2-2-2-0/

#https://github.com/hrbrmstr/darksky

#https://public.tableau.com/views/TornadoDensity-AlternateHexbins/TornadoDensity-Alternatehexbins?:showVizHome=no

#https://www.blackrock.com/investing/literature/whitepaper/bii-climate-change-2016-us.pdf

#https://www.ametsoc.org/cwwce/index.cfm/committees/committee-on-financial-weatherclimate-risk-management-comm/terms-of-reference/

#https://rstudio.github.io/tensorflow/
#https://spark.apache.org/docs/latest/sparkr.html

library(darksky)
library(purrr)
library(httr)

# current verison
packageVersion("darksky")

Sys.getenv("DARKSKY_API_KEY")

darksky_api_key = "a6eb6ed5246892a0d0988713d3b17102"

station_info <- read.csv(file = "C:\\Users\\John\\Documents\\GitHub\\Wx_Charts\\Data\\isd-history.csv")

now <- get_current_forecast(43.2672, -70.8617)
plot(now)
forecast <- now$hourly

########################################################################

#You are required to display the message "Powered by Dark Sky" 

########################################################################