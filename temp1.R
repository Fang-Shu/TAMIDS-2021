### 2021 Data Science Competition
### Data Cleaning and Saving for Campaign Operating Expenditure

library(datasets)
library(tidyverse)
library(stringr)

rm(list=ls())


#### a function that clean data ####
# input: file name in csv format
# output: return data frame with state, categories, spending

data_cleaning = function(filename){
  data = read.csv(filename)
  data = data[,c("committee_name","report_year","recipient_state","disbursement_amount","disbursement_description","disbursement_purpose_category")]
  data = data[data$recipient_state!="",] # remove empty state
  # set some categories as description
  data$disbursement_description[data$disbursement_purpose_category=="MATERIALS"]="MATERIALS"
  data$disbursement_description[data$disbursement_purpose_category=="EVENTS"]="EVENTS"
  data$disbursement_description[data$disbursement_purpose_category=="CONTRIBUTIONS"]="CONTRIBUTIONS"
  data$disbursement_description[data$disbursement_purpose_category=="TRAVEL"]="TRAVEL"
  data$disbursement_description[data$disbursement_purpose_category=="ADMINISTRATIVE"]="ADMINISTRATIVE"
  data$disbursement_description[data$disbursement_purpose_category=="FUNDRAISING"]="FUNDRAISING"
  
  # find the unique first word from all strings
  data_description = data %>% 
    group_by(disbursement_description) %>% 
    summarise(spending = sum(disbursement_amount))
  data_description = data_description[data_description$disbursement_description!="",]
  data_description = stringr::word(data_description$disbursement_description, 1)
  data_description = stringr::word(data_description, 1, sep="/")
  data_description = stringr::word(data_description, 1, sep=";")
  data_description = stringr::word(data_description, 1, sep=",")
  data_description[grepl("E-MAIL",data_description)] = "EMAIL"
  data_description = stringr::word(data_description, 1, sep="-")
  data_description = unique(data_description)
  
  
  # concentrate naming in descriptions 
  
  for (i in 1:length(data_description)){
    grep_word = data_description[i]
    grep_result = grepl(grep_word,data$disbursement_description)
    data$disbursement_description[grep_result]=grep_word
  }
  
  # summing spending for each description
  final_data = data %>% 
    group_by(recipient_state,disbursement_description) %>% 
    summarise(spending = sum(disbursement_amount))
  
  # return df: state name, categories, spending
  return(final_data)
}



#### Function that save RDS files ####
# Input: file name in csv format
# output: save RData file

save_cleaned_RData = function(filename){
  data = data_cleaning(filename)
  filename_for_saving = paste(substr(filename,1,nchar(filename)-4),".RData",sep="")
  saveRDS(data,filename_for_saving)
}



## Data Cleaning and Saving for 2012, 2016, 2020 presidential elections
save_cleaned_RData("2012Obama.csv")
save_cleaned_RData("2012Romney.csv")
save_cleaned_RData("2016Hillary.csv")
save_cleaned_RData("2016Trump.csv")
save_cleaned_RData("2020Biden.csv")
save_cleaned_RData("2020Trump.csv")


