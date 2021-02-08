#https://inovancetech.com/blogML2.html
#https://inovancetech.com/blogML.html
#install.packages("lubridate")


library("quantmod")
#Allows us to import the data we need
library("lubridate")
library("e1071")
startDate = as.Date("1990-01-01")
endDate = as.Date("2020-11-01")
getSymbols("AAPL", src = "yahoo", from = startDate, to = endDate)
DayofWeek<-wday(AAPL, label=TRUE)
PriceChange<- Cl(AAPL) - Op(AAPL)
#Find the difference between the close price and open price
Class<-ifelse(PriceChange>0,"UP","DOWN")
#Convert to a binary classification. (In our data set, there are no bars with an exactly 0 price change so, for simplicity sake, we will not address bars that had the same open and close price.)
DataSet<-data.frame(DayofWeek,Class)

EMA5<-EMA(Op(AAPL),n = 5)
EMA10<-EMA(Op(AAPL),n = 10)

EMACross <- EMA5 - EMA10
#Positive values correspond to the 5-period EMA being above the 10-period EMA
EMACross<-round(EMACross,2)
DataSet2<-data.frame(DayofWeek,EMACross, Class)
DataSet2<-DataSet2[-c(1:10),]

set.seed(3033)
intrain <- createDataPartition(y = DataSet2$Class, p= 0.75, list = FALSE)
training <- DataSet2[intrain,]
testing <- DataSet2[-intrain,]

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  Class~ ., data = training, 
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

plot(nb.m2)
# results for best model
confusionMatrix(nb.m2)

pred <- predict(nb.m2, newdata = testing)
confusionMatrix(pred, test$Class)


