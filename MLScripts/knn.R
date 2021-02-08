#https://datascienceplus.com/knn-classifier-to-predict-price-of-stock/

library(class)
library(dplyr)
library(lubridate)
library(class)
library(gmodels)
library(caret)
set.seed(100)

bny_raw <- read.csv(file = "/Users/nselvarajan/Desktop/R/finals/datasets1/bankofmelon.csv", header = T,stringsAsFactors = F)
bny_raw <- data.frame(bny_raw, stringsAsFactors = FALSE)
str(bny_raw)
bny_raw<-subset(bny_raw,select =c(Date,Open,increase,Volume,Close,High))

bny_raw$Date <- mdy(bny_raw$Date)
bny_raw$Date <- as.numeric(bny_raw$Date)
bny_raw$Open <- as.numeric((bny_raw$Open))
bny_raw$increase <- as.factor((bny_raw$increase))
bny_raw$Volume <- as.numeric((bny_raw$Volume))
bny_raw$Close <- as.numeric((bny_raw$Close))
bny_raw$High <- as.numeric((bny_raw$High))


str(bny_raw)

indxTrain <- createDataPartition(y = bny_raw$increase,p = .75,list = FALSE)
training <- bny_raw[indxTrain,]
testing <- bny_raw[-indxTrain,]
trainX <- training[,names(training) != "increase"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train( increase~ ., data = training, method = "knn",
                 trControl = ctrl, preProcess = c("center","scale"),
                 tuneLength = 20)
knnFit
plot(knnFit)
knnPredict <- predict(knnFit,newdata = testing )
confusionMatrix(knnPredict, testing$increase )
mean(knnPredict == testing$increase)




bny_raw<-subset(bny_raw,select =c(Open,Volume,Close,High))
indxTrain <- createDataPartition(y = bny_raw$Close,p = .75,list = FALSE)
training <- bny_raw[indxTrain,]
testing <- bny_raw[-indxTrain,]
trainX <- training[,names(training) != "Close"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
knnFit <- train( Close~ ., data = training, method = "knn",
                 trControl = ctrl, preProcess = c("center","scale"), 
                 tuneLength = 20)

plot(varImp(knnFit), top = 3,color="red")

print(knnFit)

plot(knnFit)




knnPredict <- predict(knnFit,newdata = testing , type="response")
confusionMatrix(knnPredict, testing$Close, dnn = c( "Reference"))
mean(knnPredict == testing$Close)



