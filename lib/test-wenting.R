library(fasttime)
combine_data <- function(firstFileName = 'data/data_20160110.csv'){
  filenames <- c()
  newdata <- fread(firstFileName)
  idx <- seq(4,9,1)
  
  for (i in 1:length(idx)){
    filenames[i] <- paste(paste('data/data_2016010', idx[i], sep = ''), '.csv', sep = '')
    newdata = rbind(newdata, fread(filenames[i]))
  }
  newdata <- newdata[order(newdata$tpep_pickup_datetime), ]
  newdata$tpep_pickup_datetime <- fastPOSIXct(newdata$tpep_pickup_datetime)
  newdata$tpep_dropoff_datetime <- fastPOSIXct(newdata$tpep_dropoff_datetime)
  newdata <- newdata[(newdata$trip_duration_inMins < 180) & (newdata$speed_milesPerMin < 5), ]
  return(newdata)
}
dat <- combine_data()
boroughs <- readOGR(dsn="data/BB.geojson")
boroughs <- spTransform(boroughs,CRS("+init=epsg:4326"))
cor1  <-  data.frame(Longitude1=dat$pickup_longitude,Latitude1=dat$pickup_latitude)
cor2 <- data.frame(Longitude2=dat$dropoff_longitude,Latitude2=dat$dropoff_latitude)
coordinates(cor1) <- ~Longitude1 + Latitude1
coordinates(cor2) <- ~Longitude2 + Latitude2
proj4string(cor1) <- CRS("+proj=longlat")
proj4string(cor2) <- CRS("+proj=longlat")
cor1 <- spTransform(cor1,proj4string(boroughs))
cor2 <- spTransform(cor2,proj4string(boroughs))
boro1 <- over(cor1,boroughs)
boro2 <- over(cor2,boroughs)
data_final <- cbind(dat,boro_pickup=boro1$boro_name,boro_dropoff=boro2$boro_name)
save(data_final,file='MyApp/output/oneweekwithboro.RData')