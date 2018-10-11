# data preprocessing
library(lubridate)
# calc the NAs and zeros in each variable
# sum(test$pickup_longitude == 0)/nrow(test)
# sum(test$dropoff_longitude == 0)
# sum(test$dropoff_latitude == 0)
# sum(test$pickup_latitude == 0)

# delete the rows where one of the pu or do GIS infor is missing
# boo_delete <- ( (test$dropoff_longitude == 0) | (test$dropoff_latitude == 0) ) &
#               ( (test$pickup_longitude == 0) | (test$pickup_latitude == 0) )
Sys.time()
for (file in dir(path ="~/Google Drive/cloud/ADS/column subset/")){
  filepath = paste("/Users/siyuzhu/Google Drive/cloud/ADS/column subset/", file, sep = "")
  test <- fread(filepath, stringsAsFactors = F)
  boo_delete <- ( (test$dropoff_longitude == 0) | (test$dropoff_latitude == 0) ) &
    ( (test$pickup_longitude == 0) | (test$pickup_latitude == 0) )
  test <- test[-which(boo_delete == T), ]
  write.csv(file = as.character(file), x = test)
  print("write done")
  #### [todo] ####
  # more data preprocess code, till the output for each month, used for shiny app directly
  
  rm(test)
}
Sys.time()
# strptime("2018/10/09 10:17:19 PM", "%Y/%m/%d %I:%M:%S %p") # for parsing the time
# format(temp_time, format = "%H:%M")
# format(temp_time, format = "%Y/%M/%d")




test<-fread("data/2016_01_new.csv", header = T, stringsAsFactors = F)
test <- test %>% select(-PULocationID, -DOLocationID) %>% mutate_if(is.numeric, round, 4) %>% 
  mutate(pickup_time = format(strptime(tpep_pickup_datetime, "%m/%d/%Y %I:%M:%S %p"), format = "%H:%M"), 
         dropoff_time = format(strptime(tpep_dropoff_datetime, "%m/%d/%Y %I:%M:%S %p"), format = "%H:%M")) %>% 
  select(-tpep_pickup_datetime, -tpep_dropoff_datetime)

# tmp <- tmp %>% mutate(pickup_time = format(strptime(tpep_pickup_datetime, "%m/%d/%Y %I:%M:%S %p"), format = "%H:%M"), dropoff_time = format(strptime(tpep_dropoff_datetime, "%m/%d/%Y %I:%M:%S %p"), format = "%H:%M"))

# temp %>% mutate(pickup_date = strsplit(tpep_pickup_datetime, split = " ")[[1]][1],
#                 dropoff_date = strsplit(tpep_dropoff_datetime, split = " ")[[1]][1],
#                 pickup_time_hour = strsplit(tpep_pickup_datetime, split = " ")[1])
write.csv(file = "test01.csv", x = test)
 


