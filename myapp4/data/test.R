library(data.table)
library(DT)
test <- fread("/Users/siyuzhu/Google Drive/cloud/ADS/column subset/2016_02_new.csv", stringsAsFactors = F)
datatable(head(test, 10))
setwd("/Users/siyuzhu/Documents/Github/ADS/Fall2018-Project2-sec2_proj2_grp11/data")

