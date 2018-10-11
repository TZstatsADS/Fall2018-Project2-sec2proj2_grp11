data <- read.csv('MyApp/data/processed_data.csv')
data <- data[(data["TPD"]>0)&(data["FPD"])<10,]
data = data %>% subset(select= c("pickup_longitude","pickup_latitude",
                                 "FPD","TPD","Month"))
load('MyApp/output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
load('MyApp/output/meanFarePerDistance.RData')
load('MyApp/output/meanTipsPerDistance.RData')
boroughs <- readOGR(dsn="data/BB.geojson")
boroughs <- spTransform(boroughs,CRS("+init=epsg:4326"))
dat = data.frame(Longitude=data$pickup_longitude,Latitude=data$pickup_latitude)
coordinates(dat) <- ~Longitude + Latitude
proj4string(dat) <- CRS("+proj=longlat")
dat <- spTransform(dat,proj4string(boroughs))
r = over (dat,boroughs)
r = r %>% subset(select=c("boro_name")) %>% cbind(data)
FPD_result_1 <- data.frame(FPD_result,subdat@data$BoroName)
TPD_result_1 <- data.frame(TPD_result,subdat@data$BoroName)
FPD_final <- matrix(0,5,12)
TPD_final <- matrix(0,5,12)
borough_pick <- matrix(0,5,12)
month_fare_matrix <- matrix(0,5,12)
name <- c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
for(i in 1:12){
  d <- r$boro_name[r$Month==i]
  for(j in 1:5){
    borough_pick[j,i] <- sum(as.vector(d)==name[j],na.rm=TRUE)
  }
}
borough_final <- data.frame(borough_pick, borough=name)
colnames(borough_final) <- c("Jan","Feb","Mar","Apri","May","June","July","Aug","Sept","Oct","Nov","Dec","borough")
df_1 <- FPD_result_1[FPD_result_1$subdat.data.BoroName=="Manhattan",]
df_2 <- FPD_result_1[FPD_result_1$subdat.data.BoroName=="Brooklyn",]
df_3 <- FPD_result_1[FPD_result_1$subdat.data.BoroName=="Queens",]
df_4 <- FPD_result_1[FPD_result_1$subdat.data.BoroName=="Bronx",]
df_5 <- FPD_result_1[FPD_result_1$subdat.data.BoroName=="Staten Island",]
df_11 <- TPD_result_1[TPD_result_1$subdat.data.BoroName=="Manhattan",]
df_22 <- TPD_result_1[TPD_result_1$subdat.data.BoroName=="Brooklyn",]
df_33 <- TPD_result_1[TPD_result_1$subdat.data.BoroName=="Queens",]
df_44 <- TPD_result_1[TPD_result_1$subdat.data.BoroName=="Bronx",]
df_55 <- TPD_result_1[TPD_result_1$subdat.data.BoroName=="Staten Island",]
FPD_final[1,] <- as.vector(apply(df_1[,1:12],2,mean))
FPD_final[2,] <- as.vector(apply(df_2[,1:12],2,mean))
FPD_final[3,] <- as.vector(apply(df_3[,1:12],2,mean))
FPD_final[4,] <- as.vector(apply(df_4[,1:12],2,mean))
FPD_final[5,] <- as.vector(apply(df_5[,1:12],2,mean))
TPD_final[1,] <- as.vector(apply(df_11[,1:12],2,mean))
TPD_final[2,] <- as.vector(apply(df_22[,1:12],2,mean))
TPD_final[3,] <- as.vector(apply(df_33[,1:12],2,mean))
TPD_final[4,] <- as.vector(apply(df_44[,1:12],2,mean))
TPD_final[5,] <- as.vector(apply(df_55[,1:12],2,mean))
save(TPD_final,file ='MyApp/output/MeanTipPerDistanceBorough.RData')
save(FPD_final,file='MyApp/output/MeanFarePerDistanceBorough.RData')
month1 <- FPD_result_1[,1]
month2 <- FPD_result_1[,2]
month3 <- FPD_result_1[,3]
month4 <- FPD_result_1[,4]
month5 <- FPD_result_1[,5]
month6 <- FPD_result_1[,6]
group <- function(vector){
	m <- rep(NA,5)
	m[1] <- sum((vector>=0)&(vector<2))
	m[2] <- sum((vector>=2)&(vector<4))
	m[3] <- sum((vector>=4)&(vector<6))
	m[4] <- sum((vector>=6)&(vector<8))
	m[5] <- sum((vector>=8)&(vector<10))
	return(m)
}
month_fare_matrix[,1] <- group(month1)
month_fare_matrix[,2] <- group(month2)
month_fare_matrix[,3] <- group(month3)
month_fare_matrix[,4] <- group(month4)
month_fare_matrix[,5] <- group(month5)
month_fare_matrix[,6] <- group(month6)
save(month_fare_matrix,file='MyApp/output/month_fare_matrix.RData')
save(borough_final, file="MyApp/output/picks.RData")






