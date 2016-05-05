
# Get all the possible stations
st <- read.csv(file = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv") %>%
  mutate( LAT = LAT / 1000,
          LON = LON / 1000,
          ELEV.M. = ELEV.M. / 10,
          BEGIN = as.numeric(substr(BEGIN, 1, 4)),
          END = as.numeric(substr(END, 1, 4)))

# Keep only the staions with pittburgh in the name
pa.list <- st %>%
    filter(grepl("PITTSBURGH",STATION.NAME))

# download the historic file
path <- sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/%s/%s-%s-2016.gz",2016,pa.list$USAF[1],pa.list$WBAN[1])
download.file(path, destfile = "/Users/cmohan/Desktop/data.gz")

# unzip
library(R.utils)
gunzip("/Users/cmohan/Desktop/data.gz")
#will save a text file under the same name as the zip file
column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6,
                   1, 1, 1, 5, 1, 5, 1, 5, 1)

raw_data <- read.fwf("/Users/cmohan/Desktop/data", column.widths)

selected_data <- raw_data[, c(2:8, 10:11, 13, 16, 19, 29,
                  31, 33)]

names(selected_data) <- c("USAFID", "WBAN", "YR", "M",
                 "D", "HR", "MIN", "LAT", "LONG", "ELEV",
                 "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT",
                 "ATM.PRES")

final_data <- selected_data %>%
  mutate_each(funs(replace(., . %in% c(999,9999,99999), NA)))

