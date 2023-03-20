library(usmap)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(caret)
library(e1071)
library(tidyverse)
library(plotly)
library(class)
library(GGally)
library(RANN)


file1beer=read.csv(file="Beers.csv",header=TRUE)  #loading first data
file2brewery=read.csv(file="Breweries.csv",header=TRUE) #loading second data
all=full_join(file1beer,file2brewery,by =c("Brewery_id"="Brew_ID")) #full join them together

#address missing values
#replacing all NA values with average ABV

all$ABV=replace_na(all$ABV,mean(all[!is.na(all$ABV),]$ABV))

#Clearing NA values using KNN
knn_imp_model <- preProcess(all %>%
                              select(ABV,IBU),
                            method = c("knnImpute"),
                            k = 20,
                            knnSummary = mean)

all <- predict(knn_imp_model, all,na.action = na.pass)

procNames <- data.frame(col = names(knn_imp_model$mean), mean = knn_imp_model$mean, sd = knn_imp_model$std)
for(i in procNames$col){
  all[i] <- all[i]*knn_imp_model$std[i]+knn_imp_model$mean[i] 
}

brewerybystate=table(file2brewery$State)
brewerybystate= data.frame(brewerybystate)
colnames(brewerybystate)[1]="state"
brewerybystate$Freq=as.double(brewerybystate$Freq)
brewerybystate$state=as.character(brewerybystate$state)
all$State=str_replace_all(all$State," ","")
brewerybystate$state=str_replace_all(brewerybystate$state," ","")

