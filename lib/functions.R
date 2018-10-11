# Load Data
combine_data <- function(firstFileName = '../data/data_20160110.csv'){
  filenames <- c()
  newdata <- fread(firstFileName)
  idx <- seq(4,9,1)
  
  for (i in 1:length(idx)){
    filenames[i] <- paste(paste('../data/data_2016010', idx[i], sep = ''), '.csv', sep = '')
    newdata = rbind(newdata, fread(filenames[i]))
  }
  newdata <- newdata[order(newdata$tpep_pickup_datetime), ]
  newdata$tpep_pickup_datetime <- fastPOSIXct(newdata$tpep_pickup_datetime)
  newdata$tpep_dropoff_datetime <- fastPOSIXct(newdata$tpep_dropoff_datetime)
  newdata <- newdata[(newdata$trip_duration_inMins < 180) & (newdata$speed_milesPerMin < 5), ]
  return(newdata)
}



# Estimation of Fare based on past 5 mins ho=istorical data
est_amount_past5mins <- function(data, currentTime){
  #https://www1.nyc.gov/nyc-resources/service/1271/yellow-taxi-fares
  left_time_interval <- currentTime - 5*60
  right_time_interval <- currentTime
  dataPast5Mins <- data[(data$tpep_dropoff_datetime <= right_time_interval) & (data$tpep_dropoff_datetime >= left_time_interval), ]
  mean_tip_amount_percent <- sum(dataPast5Mins$tip_amount)/sum(dataPast5Mins$total_amount - dataPast5Mins$tip_amount)
  current_mean_speed <- mean(dataPast5Mins$speed_milesPerMin)
  other_fixed_amount <- mean(abs(abs(data$total_amount) - abs(data$fare_amount) - abs(dataPast5Mins$tip_amount)))
  result <- c(current_mean_speed,mean_tip_amount_percent,other_fixed_amount)
  return(result)
}

drawDrivingSpeed <- function(data, currentDate_time){
  one_data = data[(data$tpep_pickup_datetime >=  format(currentDate_time, '%Y-%m-%d')) & (data$tpep_dropoff_datetime <= currentDate_time), ]
  one_data.summary = one_data %>% group_by(by3=cut(tpep_dropoff_datetime, "3 min")) %>%
    summarise(mean_speed =mean(speed_milesPerMin)) %>% as.data.frame
  
  one_data2 <- one_data[(one_data$tip_amount>0) & (one_data$tip_amount< 20), ]
  one_data2$mean_tip_amount_percent <- one_data2$tip_amount/(one_data2$total_amount - one_data2$tip_amount)
  
  one_data.summary2 = one_data2 %>% group_by(by3=cut(tpep_dropoff_datetime, "3 min")) %>%
    summarise(mean_tip =mean(mean_tip_amount_percent)) %>% as.data.frame      
  
  times <- format(strptime(one_data.summary$by3, "%Y-%m-%d %H:%M:%S"), "%H:%M")
  par(mfrow = c(1,1),mar=c(4, 4, 3.5, 1))
  
  plot(one_data.summary$mean_speed, type = 'l', col = 'blue',
       ylab = "Value", xlab = 'Time',xaxt='n', main = 'Driving Speed vs Tip Amount')
  lines(one_data.summary2$mean_tip, col = 'red' )
  axis(1,at=seq(1,length(times), by = 6),labels=times[seq(1,length(times), by = 6)])
  legend("topright", legend = c('Speed (miles/min)','Tip'), col = c('blue', 'red'), lty=1:2, cex=0.8)
}
