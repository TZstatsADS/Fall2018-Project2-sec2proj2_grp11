data <- read.csv('MyApp/data/processed_data.csv')
data <- data[(data["TPD"]>0)&(data["FPD"])<10,]
data = data %>% subset(select= c("pickup_longitude","pickup_latitude",
                                 "FPD","TPD","Month"))
load('MyApp/output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
coordinates(subdat)