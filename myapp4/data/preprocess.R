# data preprocessing

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