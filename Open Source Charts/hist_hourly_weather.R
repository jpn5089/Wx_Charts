library(R.utils)
# Get all the possible stations
st <- read.csv(file = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv") %>%
  mutate( LAT = LAT / 1000,
          LON = LON / 1000,
          ELEV.M. = ELEV.M.,
          BEGIN = as.numeric(substr(BEGIN, 1, 4)),
          END = as.numeric(substr(END, 1, 4)))

# Keep only the staions with pittburgh in the name
pa.list <- st %>%
    filter(grepl("PITTSBURGH",STATION.NAME))

# download the historic file
<<<<<<< HEAD

out <- list()
for (i in c(2013,2014,2015,2016)){
path <- sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/%s-%s-%s.gz",i,pa.list$USAF[1],pa.list$WBAN[1],i)
download.file(path, destfile = sprintf("/Users/cmohan/Desktop/data_%s.gz", i))

# unzip
gunzip(sprintf("/Users/cmohan/Desktop/data_%s.gz", i))
#=======
# path <- sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/%s-%s-2016.gz",2016,pa.list$USAF[1],pa.list$WBAN[1])
# download.file(path, destfile = "C://Users/John/Desktop/data.gz")
# 
# # unzip
# library(R.utils)
# gunzip("C:/Users/John/Desktop/data.gz")
# >>>>>>> origin/master
=======
path <- sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/%s-%s-2016.gz",2016,pa.list$USAF[1],pa.list$WBAN[1])
download.file(path, destfile = "C://Users/John/Desktop/data.gz")

# unzip
library(R.utils)
gunzip("C:/Users/John/Desktop/data.gz")
>>>>>>> origin/master
#will save a text file under the same name as the zip file
column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6,
                   1, 1, 1, 5, 1, 5, 1, 5, 1)

<<<<<<< HEAD
#<<<<<<< HEAD
raw_data <- read.fwf(sprintf("/Users/cmohan/Desktop/data_%s", i), column.widths)
# =======
# raw_data <- read.fwf("C:/Users/John/Desktop/data", column.widths)
# >>>>>>> origin/master
=======
raw_data <- read.fwf("C:/Users/John/Desktop/data", column.widths)
>>>>>>> origin/master

selected_data <- raw_data[, c(2:8, 10:11, 13, 16, 19, 29,
                  31, 33)]

names(selected_data) <- c("USAFID", "WBAN", "YR", "M",
                 "D", "HR", "MIN", "LAT", "LONG", "ELEV",
                 "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT",
                 "ATM.PRES")

<<<<<<< HEAD
#<<<<<<< HEAD
x <- selected_data %>%
  mutate_each(funs(replace(., . %in% c(999,9999,99999), NA)))

name <- paste(i)
out[[name]] <- x
}

# In UTC time...so need to subtract 4 hours to get to EST local
# TEMP and DEW.POINT in degrees F
# ATM.PRES in mb
# WIND.SPD in mph
# final_data <- selected_data %>%
#   mutate_each(funs(replace(., . %in% c(999,9999,99999), NA))) %>%
#   mutate (TEMP = round(((TEMP/10)*(9/5) +32),0),
#           DEW.POINT = round(((DEW.POINT/10)*(9/5) +32)),
#           ATM.PRES = ATM.PRES/10,
#           WIND.SPD = round(((WIND.SPD/10)*2.23694), 0))
# 
# out <- list()
# 
# out <- final_data

# final_data <- do.call(rbind,out) %>%
#   group_by(YR, M, D, HR) %>%
#   summarize_each(funs(mean(., na.rm = TRUE)))
# >>>>>>> origin/master
=======
# In UTC time...so need to subtract 4 hours to get to EST local
# TEMP and DEW.POINT in degrees F
# ATM.PRES in mb
# WIND.SPD in mph
final_data <- selected_data %>%
  mutate_each(funs(replace(., . %in% c(999,9999,99999), NA))) %>%
  mutate (TEMP = round(((TEMP/10)*(9/5) +32),0),
          DEW.POINT = round(((DEW.POINT/10)*(9/5) +32)),
          ATM.PRES = ATM.PRES/10,
          WIND.SPD = round(((WIND.SPD/10)*2.23694), 0))

out <- list()

out <- final_data

final_data <- do.call(rbind,out) %>%
  group_by(YR, M, D, HR) %>%
  summarize_each(funs(mean(., na.rm = TRUE)))
>>>>>>> origin/master

final_data <- do.call(rbind,out) %>%
  group_by(YR, M, D, HR) %>%
  summarize_each(funs(mean(., na.rm = TRUE)))
  
write.csv(final_data, file = "/Users/cmohan/Desktop/duq_weather.csv")
