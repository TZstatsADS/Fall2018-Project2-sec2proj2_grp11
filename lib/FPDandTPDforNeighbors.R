data <- read.csv('MyApp/data/processed_data.csv')
data <- data[(data["TPD"]>0)&(data["FPD"])<10,]
data = data %>% subset(select= c("pickup_longitude","pickup_latitude",
"FPD","TPD","Month"))
FPD_result = matrix(0,195,12)
TPD_result = matrix(0,195,12)
load('MyApp/output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
for(i in 1:12){
  data_selected <- data %>% subset(Month == i)
  dat = data.frame(Longitude=data_selected$pickup_longitude,Latitude=data_selected$pickup_latitude)
  coordinates(dat) <- ~Longitude + Latitude
  proj4string(dat) <- CRS("+proj=longlat")
  dat <- spTransform(dat,proj4string(myShape1))
  r = over (dat,myShape1)
  r = r %>% subset(select=c("NTACode")) %>% cbind(data_selected)
  FPD_mean = as.vector(tapply(r$FPD,r$NTACode,mean))
  TPD_mean = as.vector(tapply(r$TPD,r$NTACode,mean))
  for(a in 1:length(FPD_mean)){
    if(is.na(FPD_mean[a])){
      FPD_mean[a]=0
    }
  }
  for(b in 1:length(TPD_mean)){
    if(is.na(TPD_mean[b])){
      TPD_mean[b]=0
    }
  }
  FPD_result[,i] = as.vector((FPD_mean))
  TPD_result[,i] = as.vector((TPD_mean))
}
save(FPD_result,file='MyApp/output/meanFarePerDistance.RData')
save(TPD_result,file='MyApp/output/meanTipsPerDistance.RData')
