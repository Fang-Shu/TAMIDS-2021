### 2021 Data Science Competition
### Data Cleaning and Saving for Presidential Election Result

library(datasets)
library(tidyverse)
library(stringr)

rm(list=ls())

# 2020
data2020 = read.csv("2020PopularVotes.csv")
data2020 = data2020[,c("state","stateid","dem_percent","rep_percent")]
data2020 = data2020[data2020$stateid!="",]
data2020 = data2020[data2020$stateid==substr(data2020$stateid,1,2),]
data2020 = data2020 %>% arrange(state)
data2020$dem_percent = substr(data2020$dem_percent,1,nchar(data2020$dem_percent)-1)
data2020$rep_percent = substr(data2020$rep_percent,1,nchar(data2020$rep_percent)-1)
data2020$dem_percent = as.numeric(data2020$dem_percent)
data2020$rep_percent = as.numeric(data2020$rep_percent)
colnames(data2020) = c("state","stateid","2020dem","2020rep")

# 2016
data2016 = read.csv("2016ElectionResult.csv")
colnames(data2016) = c("state","2016dem","2016rep","percentage")

# 2012
data2012 = read.csv("2012ElectionResult.csv")
colnames(data2012) = c("state","2012dem","2012rep","percentage")
data2012$state = stringr::word(data2012$state, 1)

# 2008
data2008 = read.csv("2008ElectionResult.csv")
colnames(data2008) = c("state","2008dem","2008rep","percentage")

# combine and save data
# check if data matches, then combine
data2016$state == data2020$state
data2012$state == data2020$state # in fact, they match.
data2008$state == data2020$state
data = cbind(data2020,data2016[,2:3],data2012[,2:3],data2008[,2:3])
saveRDS(data,"ElectionResults.RData")


