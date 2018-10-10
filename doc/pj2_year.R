#process the subsetted data for use in the shiny app
library(readr)
data = read_csv("../data/data_20160104.csv")

head(data)
tail(data)

# loc <- Sys.getlocale("LC_TIME")
# Sys.setlocale("LC_TIME", "C") 
# 
# pudt = strptime(data$tpep_pickup_datetime, "%m/%d/%Y %I:%M:%S %p")
# pudt = as.POSIXct(format(pudt),tz="UTC")
# 
# dodt = strptime(data$tpep_dropoff_datetime, "%m/%d/%Y %I:%M:%S %p")
# dodt = as.POSIXct(format(dodt),tz="UTC")
# 
# Sys.setlocale("LC_TIME", loc) 

trip_time = data$tpep_dropoff_datetime-data$tpep_pickup_datetime
dollar_per_mile = data$fare_amount/data$trip_distance
mph = data$speed_milesPerMin*60

per = data.frame(data, trip_time, dollar_per_mile, mph)
# names(per)[1:2] = c("tpep_pickup_datetime", "tpep_dropoff_datetime")
head(per)
tail(per)
write.csv(per,"../data/per.csv")

