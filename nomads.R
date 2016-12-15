#Get model data:
#Planetary temperature, relative humidity, winds
#at 2 m above ground and at 300 mb (jet stream level)
library(GEOmap)
library(rNOMADS)
library(fields)
#Get latest GFS model

model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)
#Get data

time <- c(0,0) #Analysis model
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points

variables <- c("tmp2m", "rh2m")
ground.data <- DODSGrab(latest.model, latest.model.run, variables, time, lon, lat, display.url = FALSE)
ground <- ModelGrid(ground.data, c(0.5, 0.5), "latlon")
lev <- c(28,28) #get 300 mb level
variables <- c("tmpprs", "ugrdprs", "vgrdprs")
atmos.data <- DODSGrab(latest.model, latest.model.run, variables, time, lon, lat, levels = lev, display.url = FALSE)
atmos <- ModelGrid(atmos.data, c(0.5, 0.5), "latlon")

#FIGURE 1
#Temperature at ground level

#Make model grid

#Set up color scale
colormap <- rev(rainbow(500, start = 0 , end = 5/6))
ind <- which(ground$variables == "tmp2m")
#Save image to PNG file
#Omit this line if you want to display image
#rather than save to file
png(file = "C:/Users/John/Desktop/fig_ground_temp2.png", width = 1000, height = 750)
#Make forecast image
image(ground$x, sort(ground$y), ground$z[1,ind,,], col = colormap, xlab = "Longitude", ylab = "Latitude", main = paste("World Temperature at Ground Level:", ground$fcst.date))
#Plot coastlines
plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA)
#Turn of PNG device
#Omit this line if you want to display image
#rather than save to file
dev.off()

#FIGURE 2 
#Temperature at 300 mb 
ind <- which(atmos$variables == "tmpprs") 
colormap <- rev(rainbow(500, start = 0 , end = 5/6)) 
png(file = "C:/Users/John/Desktop/fig_300mb_temp.png", width = 1000, height = 750) 
image(atmos$x, atmos$y, atmos$z[1,ind,,], col = colormap, xlab = "Longitude", ylab = "Latitude", main = paste("World Temperature at 300 mb:", atmos$fcst.date)) 
plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA) 
dev.off()

variables <- c("ugrd10m", "vgrd10m")
time <- c(2,2) #6 hr forecast 
ground.wind.data <- DODSGrab(latest.model, latest.model.run, variables, time, lon, lat, display.url = FALSE) 
#Make an array for quick indexing 
ground.wind <- ModelGrid(ground.wind.data, c(0.5, 0.5)) 
#Wind magnitude 
winds.vel <- sqrt(ground.wind$z[1,1,,]^2 + ground.wind$z[1,2,,]^2) 
#FIGURE 5 
png(file = "C:/Users/John/Desktop/fig_ground_gust.png", width = 1000, height = 750) 
image(ground.wind$x, ground.wind$y, winds.vel, col = colormap, xlab = "Longitude", ylab = "Latitude", main = paste("World Wind Speed at 10 m above ground:", ground.wind$fcst.date)) 
plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA) 
dev.off()

#First, define each location 
lon <- -79.9764 
lat <- 40.4397
#Get latest GFS 0.5 model, use analysis forecast
## Not run:
model.url <- CrawlModels(abbrev = "gfs_0p50", depth = 1)[1] 
pred <- ParseModelPage(model.url)$pred[1]
## End(Not run)
#Get levels 
pressure <- c(1, 2, 3, 5, 7, 10, 20, 30, 50, 70, seq(100, 1000, by = 25)) 
levels <- paste(pressure, " mb", sep = "")
#Variables - temperature and height only
variables <- c("TMP", "HGT")
## Not run: 
grib.info <- GribGrab(model.url, pred, levels, variables, 
                      model.domain = c(-85, -75, 37, 32)) 
grib.data <- ReadGrib(grib.info$file.name, levels, variables) 
gridded.data <- ModelGrid(grib.data, c(0.5, 0.5))
profile <- BuildProfile(gridded.data, lon, lat, TRUE) 
plot(profile[,2] - 273.15, profile[,1], xlab = "Temperature (C)", ylab = "Height (m)", main = "Temperature Profile above Pittsburgh, PA")


