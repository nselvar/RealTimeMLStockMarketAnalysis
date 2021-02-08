#https://inovancetech.com/ann.html
#https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/

#install.packages("quantmod")
library("quantmod")
#Allows us to import the market data and indicators we need
#install.packages("neuralnet")
library("neuralnet")
#Provides the artificial neural network algorithm (there are a variety of R packages that allow you build ANNs, however neuralnet allows us to easily plot the network and has all the functionality we need for this example)

startDate<-as.Date('2000-01-01')
endDate<-as.Date('2020-11-01')
getSymbols("GOOGL",src="yahoo",from=startDate,to=endDate)
#Retrieve the data we need

RSI3<-RSI(Op(GOOGL),n=3)
#Calculate a 3-period RSI

EMA5<-EMA(Op(GOOGL),n=5)
EMAcross<-Op(GOOGL)-EMA5
#Look at the difference between the open price and a 5-period EMA

MACD<-MACD(Op(GOOGL),fast = 12, slow = 26, signal = 9)
MACDsignal<-MACD[,2]
#Grab the signal line of the MACD


BB<-BBands(Op(GOOGL),n=20,sd=2)
BBp<-BB[,4]
#We will use the Bollinger Band %B, which measures the price relative to the upper and lower Bollinger Bands

Price<-Cl(GOOGL)-Op(GOOGL)
#For this example we will be looking to predict the numeric change in price

DataSetGoogle<-data.frame(RSI3,EMAcross,MACDsignal,BBp,Price)
DataSetGoogle<-DataSetGoogle[-c(1:33),]
colnames(DataSetGoogle)<-c("RSI3","EMAcross","MACDsignal","BollingerB","Price")
#Create our data set, remove the data where the indicator values are being calculated, and name our columns



startDateAmazon<-as.Date('2000-01-01')
endDateAmazon<-as.Date('2020-11-01')
getSymbols("AMZN",src="yahoo",from=startDate,to=endDate)
#Retrieve the data we need

RSI3Amazon<-RSI(Op(AMZN),n=3)
#Calculate a 3-period RSI

EMA5Amazon<-EMA(Op(AMZN),n=5)
EMAcrossAmazon<-Op(AMZN)-EMA5Amazon
#Look at the difference between the open price and a 5-period EMA

MACDAmazon<-MACD(Op(AMZN),fast = 12, slow = 26, signal = 9)
MACDsignalAmazon<-MACDAmazon[,2]
#Grab the signal line of the MACD


BBAmazon<-BBands(Op(AMZN),n=20,sd=2)
BBpAmazon<-BBAmazon[,4]
#We will use the Bollinger Band %B, which measures the price relative to the upper and lower Bollinger Bands

PriceAmazon<-Cl(AMZN)-Op(AMZN)
#For this example we will be looking to predict the numeric change in price

DataSetAmazon<-data.frame(RSI3Amazon,EMAcrossAmazon,MACDsignalAmazon,BBpAmazon,PriceAmazon)
DataSetAmazon<-DataSetAmazon[-c(1:33),]
colnames(DataSetAmazon)<-c("RSI3","EMAcross","MACDsignal","BollingerB","Price")
#Create our data set, remove the data where the indicator values are being calculated, and name our columns




startDateMicrosoft<-as.Date('2000-01-01')
endDateMicrosoft<-as.Date('2020-11-01')
getSymbols("MSFT",src="yahoo",from=startDate,to=endDate)
#Retrieve the data we need

RSI3Microsoft<-RSI(Op(MSFT),n=3)
#Calculate a 3-period RSI

EMA5Microsoft<-EMA(Op(MSFT),n=5)
EMAcrossMicrosoft<-Op(MSFT)-EMA5Microsoft
#Look at the difference between the open price and a 5-period EMA

MACDMicrosoft<-MACD(Op(MSFT),fast = 12, slow = 26, signal = 9)
MACDsignalMicrosoft<-MACDMicrosoft[,2]
#Grab the signal line of the MACD


BBMicrosoft<-BBands(Op(MSFT),n=20,sd=2)
BBpMicrosoft<-BBMicrosoft[,4]
#We will use the Bollinger Band %B, which measures the price relative to the upper and lower Bollinger Bands

PriceMicrosoft<-Cl(MSFT)-Op(MSFT)
#For this example we will be looking to predict the numeric change in price

DataSetMicrosoft<-data.frame(RSI3Microsoft,EMAcrossMicrosoft,MACDsignalMicrosoft,BBpMicrosoft,PriceMicrosoft)
DataSetMicrosoft<-DataSetMicrosoft[-c(1:33),]
colnames(DataSetMicrosoft)<-c("RSI3","EMAcross","MACDsignal","BollingerB","Price")
#Create our data set, remove the data where the indicator values are being calculated, and name our columns


combinedDfs <- rbind(DataSetMicrosoft, DataSetAmazon,DataSet)

Normalized <-function(x) {(x-min(x))/(max(x)-min(x))}
NormalizedData<-as.data.frame(lapply(combinedDfs,Normalized))
#We are normalizing our data to be bound between 0 and 1

set.seed(212)
trainIndex <- createDataPartition(NormalizedData$Price, p = 0.8, list=FALSE, times=3)
subTrain <- NormalizedData[trainIndex,]
subTest <- NormalizedData[-trainIndex,]
TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=3,classProbs = TRUE)

nnetGrid <-  expand.grid(size = seq(from = 1, to = 5, by = 1)
                         ,decay = seq(from = 0.1, to = 0.2, by = 0.1)
)
nn_model <- train(Price~RSI3+EMAcross+MACDsignal+BollingerB, subTrain,
                  method = "nnet",  algorithm = 'backprop',     
                  trControl= TrainingParameters,
                  preProcess=c("scale","center"),
                  na.action = na.omit,
                  #metric = "ROC",
                  tuneGrid = nnetGrid,
                  trace=FALSE,
                  verbose=FALSE)      

nn_model$results   
plot(nn_model)


prediction <- predict(nn_model, subTest)                           
#table(prediction, subTest$Price)  


results <- data.frame(actual = subTest$Price, prediction = prediction)
results

predicted=results$prediction * abs(diff(range(Price))) + min(Price)
actual=results$actual * abs(diff(range(Price))) + min(Price)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy


library(NeuralNetTools)
varImp_nn<-varImp(nn_model)
print(varImp_nn)
ggplot(varImp_nn)
plot(varImp_nn)

library(NeuralNetTools)
plotnet(nn_model, y_names = "Price")
title("Graphical Representation of our Neural Network")
