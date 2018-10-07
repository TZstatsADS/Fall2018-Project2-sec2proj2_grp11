#process the subsetted data for use in the shiny app
library(readr)
data = read_csv("../data/subsetted_data.csv")

head(data)
tail(data)
data = data[1:480000,]

loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") 

pudt = strptime(data$tpep_pickup_datetime, "%m/%d/%Y %I:%M:%S %p")
pudt = as.POSIXct(format(pudt),tz="UTC")

dodt = strptime(data$tpep_dropoff_datetime, "%m/%d/%Y %I:%M:%S %p")
dodt = as.POSIXct(format(dodt),tz="UTC")

Sys.setlocale("LC_TIME", loc) 

trip_time = dodt - pudt
dollar_per_mile = data$fare_amount/data$trip_distance
mph = data$trip_distance/(as.numeric(trip_time)/3600)

per = data.frame(pudt, dodt, trip_time, dollar_per_mile, mph, data[,5:8])
names(per)[1:2] = c("tpep_pickup_datetime", "tpep_dropoff_datetime")
head(per)
tail(per)
write.csv(per,"../data/per.csv")

