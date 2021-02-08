#https://www.inovancetech.com/blogML3.html

#install.packages("quantmod")
#install.packages("rpart")

library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(lattice)
library(psych)
library(DataExplorer)
library(reshape2)
library(car)
library(caret)
library(cowplot)
library(caTools)
library(rpart.plot)
library(e1071)
library(leaps)
library(rpart)
library(randomForest)
library(scales)
library(RColorBrewer)
library(packHV)
options(warn=-1)

library("quantmod")
#Allows us to import the data we need and calculate the technical indicators
library("rpart")
#Gives us access to the decision trees we will be using. (I had to update my version of R in order to install this one.)

#install.packages("rpart.plot")
library("rpart.plot")
#Let’s us easily create good looking diagrams of the trees.

startDate = as.Date("2000-01-01")
#The beginning of the date range we want to look at

endDate = as.Date("2020-11-01")
#The end of the date range we want to look at

getSymbols("BAC", src = "yahoo", from = startDate, to = endDate)
#Retrieving the daily OHLCV of Bank of America’s stock from Yahoo Finance


RSI3<-RSI(Op(BAC), n= 3)
#Calculate a 3-period relative strength index (RSI) off the open price

EMA5<-EMA(Op(BAC),n=5)
#Calculate a 5-period exponential moving average (EMA)
EMAcross<- Op(BAC)-EMA5
#Let’s explore the difference between the open price and our 5-period EMA


MACD<-MACD(Op(BAC),fast = 12, slow = 26, signal = 9)
#Calculate a MACD with standard parameters
MACDsignal<-MACD[,2]
#Grab just the signal line to use as our indicator.


SMI<-SMI(Op(BAC),n=13,slow=25,fast=2,signal=9)
#Stochastic Oscillator with standard parameters
SMI<-SMI[,1]
#Grab just the oscillator to use as our indicator

PriceChange<- Cl(BAC) - Op(BAC)
#Calculate the difference between the close price and open price
Class<-ifelse(PriceChange>0,"UP","DOWN")
#Create a binary classification variable, the variable we are trying to predict.

dfdecisiontree<-data.frame(RSI3,EMAcross,MACDsignal,SMI,Class)
#Create our data set
colnames(dfdecisiontree)<-c("RSI3","EMAcross","MACDsignal","Stochastic","Class")
#Name the columns
dfdecisiontree<-dfdecisiontree[-c(1:33),]
#Get rid of the data where the indicators are being calculated


intrain <- createDataPartition(y = dfdecisiontree$Class, p= 0.75, list = FALSE)
training <- dfdecisiontree[intrain,]
testing <- dfdecisiontree[-intrain,]

trainX <- training[,names(training) != "Class"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
ctrl <- trainControl(method="repeatedcv", number = 10,repeats = 3)

set.seed(400)
dtree_fit <- train( Class~ ., data = training, method = "rpart",
                    parms = list(split = "information"),
                    trControl = ctrl,
                    control = rpart.control(maxdepth = 6),
                    preProcess = c("center","scale"), 
                    tuneLength = 10)

plot(dtree_fit)
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

dtreePREDICT <- predict(dtree_fit,newdata = testing )
confusionMatrix(dtreePREDICT, (as.factor(testing$Class))) 




