### 2021 Data Science Competition
### Construct Linear Model for 2020 election results

library(datasets)
library(tidyverse)
library(stringr)
library(coefplot)
library(leaps)
library(caret)

rm(list=ls())

## import data
data_election = readRDS("ElectionResults.RData")
data_trump = readRDS("2020Trump.RData")
data_biden = readRDS("2020Biden.RData")

data_trump = data_trump %>% spread(disbursement_description,spending)
data_biden = data_biden %>% spread(disbursement_description,spending)

# rank by state abbreviation
data_election = data_election %>% arrange(stateid)
data_trump = data_trump %>% arrange(recipient_state)
data_biden = data_biden %>% arrange(recipient_state)

# check order of rows
data_trump = data_trump[c(-26,-53),]
data_trump$recipient_state == data_election$stateid # All Trues
data_biden = data_biden[c(-4,-41,-54),]
data_biden$recipient_state == data_election$stateid # All Trues

## set up data frame for building model
colnames(data_trump) = paste("rep_",colnames(data_trump))
colnames(data_biden) = paste("dem_",colnames(data_biden))
data_trump = data_trump[,-1]
data_biden = data_biden[,-1]
data_complete_dem = cbind(data_election,data_biden)
data_complete_rep = cbind(data_election,data_trump)

data_complete_dem = data_complete_dem[,c(-1,-2,-4,-6,-8,-10)]
data_complete_rep = data_complete_rep[,c(-1,-2,-4,-6,-8,-10)]
data_complete_dem[is.na(data_complete_dem)] = 0
data_complete_rep[is.na(data_complete_rep)] = 0

temp = colnames(data_complete_dem)
temp[1]="Percentage"
colnames(data_complete_dem) = temp

temp = colnames(data_complete_rep)
temp[1]="Percentage"
colnames(data_complete_rep) = temp

## forward selection
set.seed(123)
train_control = trainControl(method = "cv", number = 10)
mod_dem = train(Percentage ~., data=data_complete_dem,
            method="leapForward",
            tuneGrid = data.frame(nvmax = 1:10),
            trControl = train_control)
mod_rep = train(Percentage ~., data=data_complete_rep,
            method="leapForward",
            tuneGrid = data.frame(nvmax = 1:10),
            trControl = train_control)

mod_dem$results # suggest 2 variables
mod_rep$results # suggest 1 variables

# select best 10 variables
mod_dem = train(Percentage ~., data=data_complete_dem,
                method="leapForward",
                tuneGrid = data.frame(nvmax = 10),
                trControl = train_control)

mod_rep = train(Percentage ~., data=data_complete_rep,
                method="leapForward",
                tuneGrid = data.frame(nvmax = 10),
                trControl = train_control)

summary(mod_dem$finalModel) ## 2016dem,CAMPAIGN,FLORIST,FLOWER,ILLUSTRATION,INTERPRETING,POLITICAL,POLLING,REPAIR,SIGN,TOLLS
summary(mod_rep$finalModel) ## 2016dem,ABATE,AUDIO,AUTO,BALLOT,CYBER,HAHN,IT,MATERIALS,MILEAGE,TAXES
coef(mod_dem$finalModel,11)
coef(mod_rep$finalModel,11)






