library(darksky)
library(purrr)

# current verison
packageVersion("darksky")

darksky_api_key(...)

now <- get_current_forecast(43.2672, -70.8617)
plot(now)
forecast <- now$hourly
