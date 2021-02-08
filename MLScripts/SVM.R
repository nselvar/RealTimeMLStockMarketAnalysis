library(quantmod)
library(lubridate)
library(e1071)
library(rpart)
library(rpart.plot)
library(ROCR)
options(warn	= -1)
library("quantmod")


SYM2	<- 'CVS'
print(paste('Predicting	the	output	for',	SYM,	sep	= '	'))
startDate = as.Date("1990-01-01")
endDate = as.Date("2020-11-01")
STOCK2	<- getSymbols(
  SYM2,
  src	= "yahoo",
  from	= startDate,
  to	= endDate
)
RSICvs	<- RSI(Op(CVS),	n	= 3)
#Calculate	a	3-period	relative	strength	index	(RSI)	off	the	open	price
EMA5Cvs	<- EMA(Op(CVS),	n	= 5)
#Calculate	a	5-period	exponential	moving	average	(EMA)
EMAcrossCvs	<- Op(CVS)	- EMA5Cvs
#Let	us	explore	the	difference	between	the	open	price	and	our	5-period	EMA
MACDCvs	<- MACD(Op(CVS),
                fast	= 12,
                slow	= 26,
                signal	= 9)
#Calculate	a	MACD	with	standard	parameters
MACDCvs	<- MACDCvs[,	2]
#Grab	just	the	signal	line	to	use	as	our	indicator.
SMICvs	<- SMI(
  Op(CVS),
  n	= 13,
  slow	= 25,
  fast	= 2,
  signal	= 9
)
#Stochastic	Oscillator	with	standard	parameters
SMICvs	<- SMICvs[,	1]
#Grab	just	the	oscillator	to	use	as	our	indicator
WPRCvs	<- WPR(Cl(CVS),	n	= 14)
WPRCvs	<- WPRCvs[,	1]
#Williams	%R	with	standard	parameters
ADXCvs	<- ADX(CVS,	n	= 14)
ADXCvs	<- ADXCvs[,	1]
#Average	Directional	Index	with	standard	parameters
CCICvs	<- CCI(Cl(CVS),	n	= 14)
CCICvs	<- CCICvs[,	1]
#Commodity	Channel	Index	with	standard	parameters
CMOCvs	<- CMO(Cl(CVS),	n	= 14)
CMOCvs	<- CMOCvs[,	1]
#Collateralized	Mortgage	Obligation	with	standard	parameters
ROCCvs	<- ROC(Cl(CVS),	n	= 2)
ROCCvs	<- ROCCvs[,	1]
#Price	Rate	Of	Change	with	standard	parameters
PriceChangeCvs	<- Cl(CVS)	- Op(CVS)
#Calculate	the	difference	between	the	close	price	and	open	price
ClassCvs	<- ifelse(PriceChangeCvs	> 0,	'UP',	'DOWN')
#Create	a	binary	classification	variable,	the	variable	we	are	trying	to	predict
DataSetCvs	<-
  data.frame(ClassCvs,	RSICvs,	EMAcrossCvs,	MACDCvs,	SMICvs,	WPRCvs,	ADXCvs,	CCICvs,	CMOCvs,	ROCCvs)
#Create	our	data	set
colnames(DataSetCvs)	<-
  c("Class",
    "RSI",
    "EMAcross",
    "MACD",
    "SMI",
    "WPR",
    "ADX",
    "CCI",
    "CMO",
    "ROC")
#Name	the	columns
#DataSet	<- DataSet[-c(1:33),	]
#Get	rid	of	the	data	where	the	indicators	are	being	calculated










SYM1	<- 'MSFT'
print(paste('Predicting	the	output	for',	SYM1,	sep	= '	'))
STOCK1	<- getSymbols(
  SYM1,
  src	= "yahoo",
  from	= startDate,
  to	= endDate
)
RSIMicrosoft	<- RSI(Op(MSFT),	n	= 3)
#Calculate	a	3-period	relative	strength	index	(RSI)	off	the	open	price
EMA5Microsoft	<- EMA(Op(MSFT),	n	= 5)
#Calculate	a	5-period	exponential	moving	average	(EMA)
EMAcrossMicrosoft	<- Op(MSFT)	- EMA5Microsoft
#Let	us	explore	the	difference	between	the	open	price	and	our	5-period	EMA
MACDMicrosoft	<- MACD(Op(MSFT),
                      fast	= 12,
                      slow	= 26,
                      signal	= 9)
#Calculate	a	MACD	with	standard	parameters
MACDMicrosoft	<- MACDMicrosoft[,	2]
#Grab	just	the	signal	line	to	use	as	our	indicator.
SMIMicrosoft	<- SMI(
  Op(MSFT),
  n	= 13,
  slow	= 25,
  fast	= 2,
  signal	= 9
)
#Stochastic	Oscillator	with	standard	parameters
SMIMicrosoft	<- SMIMicrosoft[,	1]
#Grab	just	the	oscillator	to	use	as	our	indicator
WPRMicrosoft	<- WPR(Cl(MSFT),	n	= 14)
Microsoft	<- WPRMicrosoft[,	1]
#Williams	%R	with	standard	parameters
ADXMicrosoft	<- ADX(MSFT,	n	= 14)
ADXMicrosoft	<- ADXMicrosoft[,	1]
#Average	Directional	Index	with	standard	parameters
CCIMicrosoft	<- CCI(Cl(MSFT),	n	= 14)
CCIMicrosoft	<- CCIMicrosoft[,	1]
#Commodity	Channel	Index	with	standard	parameters
CMOMicrosoft	<- CMO(Cl(MSFT),	n	= 14)
CMOMicrosoft	<- CMOMicrosoft[,	1]
#Collateralized	Mortgage	Obligation	with	standard	parameters
ROCMicrosoft	<- ROC(Cl(MSFT),	n	= 2)
ROCMicrosoft	<- ROCMicrosoft[,	1]
#Price	Rate	Of	Change	with	standard	parameters
PriceChangeMicrosoft	<- Cl(MSFT)	- Op(MSFT)
#Calculate	the	difference	between	the	close	price	and	open	price
ClassMicrosoft	<- ifelse(PriceChangeMicrosoft	> 0,	'UP',	'DOWN')
#Create	a	binary	classification	variable,	the	variable	we	are	trying	to	pre
#dict.
DataSetMicrosoft	<-
  data.frame(ClassMicrosoft,	RSIMicrosoft,	EMAcrossMicrosoft,	MACDMicrosoft,	SMIMicrosoft,	WPRMicrosoft,	ADXMicrosoft,	CCIMicrosoft,	CMOMicrosoft,	ROCMicrosoft)
#Create	our	data	set
colnames(DataSetMicrosoft)	<-
  c("Class",
    "RSI",
    "EMAcross",
    "MACD",
    "SMI",
    "WPR",
    "ADX",
    "CCI",
    "CMO",
    "ROC")
#Name	the	columns
#DataSet	<- DataSetMicrosoft[-c(1:33),	]
#Get	rid	of	the	data	where	the	indicators	are	being	calculated






SYM3	<- 'WMT'
print(paste('Predicting	the	output	for',	SYM,	sep	= '	'))
STOCK3	<- getSymbols(
  SYM3,
  src	= "yahoo",
  from	= startDate,
  to	= endDate
)
RSIWalmart	<- RSI(Op(WMT),	n	= 3)
#Calculate	a	3-period	relative	strength	index	(RSI)	off	the	open	price
EMA5Walmart	<- EMA(Op(WMT),	n	= 5)
#Calculate	a	5-period	exponential	moving	average	(EMA)
EMAcrossWalmart	<- Op(WMT)	- EMA5
#Let	us	explore	the	difference	between	the	open	price	and	our	5-period	EMA
MACDWalmart	<- MACD(Op(WMT),
                    fast	= 12,
                    slow	= 26,
                    signal	= 9)
#Calculate	a	MACD	with	standard	parameters
MACDWalmart	<- MACDWalmart[,	2]
#Grab	just	the	signal	line	to	use	as	our	indicator.
SMIWalmart	<- SMI(
  Op(WMT),
  n	= 13,
  slow	= 25,
  fast	= 2,
  signal	= 9
)
#Stochastic	Oscillator	with	standard	parameters
SMIWalmart	<- SMIWalmart[,	1]
#Grab	just	the	oscillator	to	use	as	our	indicator
WPRWalmart	<- WPR(Cl(WMT),	n	= 14)
WPRWalmart	<- WPRWalmart[,	1]
#Williams	%R	with	standard	parameters
ADXWalmart	<- ADX(WMT,	n	= 14)
ADXWalmart	<- ADXWalmart[,	1]
#Average	Directional	Index	with	standard	parameters
CCIWalmart	<- CCI(Cl(WMT),	n	= 14)
CCIWalmart	<- CCIWalmart[,	1]
#Commodity	Channel	Index	with	standard	parameters
CMOWalmart	<- CMO(Cl(WMT),	n	= 14)
CMOWalmart	<- CMOWalmart[,	1]
#Collateralized	Mortgage	Obligation	with	standard	parameters
ROCWalmart	<- ROC(Cl(WMT),	n	= 2)
ROCWalmart	<- ROCWalmart[,	1]
#Price	Rate	Of	Change	with	standard	parameters
PriceChangeWalmart	<- Cl(WMT)	- Op(WMT)
#Calculate	the	difference	between	the	close	price	and	open	price
ClassWalmart	<- ifelse(PriceChangeWalmart	> 0,	'UP',	'DOWN')
#Create	a	binary	classification	variable,	the	variable	we	are	trying	to	pre
#dict.
DataSetWalmart	<-
  data.frame(ClassWalmart,	RSIWalmart,	EMAcrossWalmart,	MACDWalmart,	SMIWalmart,	WPRWalmart,	ADXWalmart,	CCIWalmart,	CMOWalmart,	ROCWalmart)
#Create	our	data	set
colnames(DataSetWalmart)	<-
  c("Class",
    "RSI",
    "EMAcross",
    "MACD",
    "SMI",
    "WPR",
    "ADX",
    "CCI",
    "CMO",
    "ROC")
#Name	the	columns
#DataSet	<- DataSetWalmart[-c(1:33),	]
#Get	rid	of	the	data	where	the	indicators	are	being	calculated

ncol(DataSetMicrosoft)
ncol(DataSetCvs)
ncol(DataSetWalmart)

combinedDfs <- rbind(DataSetMicrosoft, DataSetCvs,DataSetWalmart)
combinedDfs <- rbind(DataSetMicrosoft)

combinedDfs$Class <- as.factor(combinedDfs$Class)
combinedDfs$RSI <- as.numeric(combinedDfs$RSI)
combinedDfs$ROC <- as.numeric(combinedDfs$ROC)
combinedDfs$CMO <- as.numeric(combinedDfs$CMO)
combinedDfs$CCI <- as.numeric(combinedDfs$CCI)
combinedDfs$EMAcross <- as.numeric(combinedDfs$EMAcross)
combinedDfs$MACD <- as.numeric(combinedDfs$MACD)
combinedDfs$SMI <- as.numeric(combinedDfs$SMI)
combinedDfs$WPR <- as.numeric(combinedDfs$WPR)
combinedDfs$ADX <- as.numeric(combinedDfs$ADX)
combinedDfs[is.na(combinedDfs$RSI), "RSI"] <- 0
combinedDfs[is.na(combinedDfs$ROC), "ROC"] <- 0
combinedDfs[is.na(combinedDfs$CMO), "CMO"] <- 0
combinedDfs[is.na(combinedDfs$CCI), "CCI"] <- 0
combinedDfs[is.na(combinedDfs$EMAcross), "EMAcross"] <- 0
combinedDfs[is.na(combinedDfs$MACD), "MACD"] <- 0
combinedDfs[is.na(combinedDfs$SMI), "SMI"] <- 0
combinedDfs[is.na(combinedDfs$WPR), "WPR"] <- 0
combinedDfs[is.na(combinedDfs$ADX), "ADX"] <- 0

set.seed(212)
trainIndexSVM <- createDataPartition(combinedDfs$Class, p = 0.8, list=FALSE, times=3)
subTrainSVM <- combinedDfs[trainIndexSVM,]
subTestSVM <- combinedDfs[-trainIndexSVM,]

# SVM Classifier using Linear Kernel


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
set.seed(323)
grid <- expand.grid(C = c( 0.25, 0.5, 1))
svm_Linear_Grid <- train(	Class	~ RSI	+ EMAcross	+ WPR	+ ADX	+ CMO	+ CCI	+ ROC, data = subTrainSVM, method = "svmLinear",
                          trControl=trctrl, preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid


plot(svm_Linear_Grid)

predictionsvm <- predict(svm_Linear_Grid, subTestSVM[-1]) 
table(predictionsvm, subTestSVM$Class)   

accuracysvm <- sum(predictionsvm == (subTestSVM$Class))/length(subTestSVM$Class)
print(accuracysvm)

#confusionNNSvm <-confusionMatrix(as.factor(predictionsvm),as.factor(subTestSVM$Term_Deposit))
#print(confusionNNSvm)


# SVM Classifier using Non-Linear Kernel

set.seed(323) 
grid_radial <- expand.grid(sigma = c(0.25, 0.5,0.9),
                           C = c(0.25, 0.5,1))
svm_Radial <- train(Class	~ RSI	+ EMAcross	+ WPR	+ ADX	+ CMO	+ CCI	+ ROC, data = subTrainSVM, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),tuneGrid = grid_radial,
                    tuneLength = 10)

svm_Radial

predictionnonlinearsvm <- predict(svm_Radial, subTestSVM[-14])                          
accuracynonlinearsvm <- sum(predictionnonlinearsvm == (subTestSVM$Class))/length(subTestSVM$Class)
print(accuracynonlinearsvm)



algo_results <- resamples(list(SVM_RADIAL=svm_Radial, SVM_LINEAR=svm_Linear_Grid))

summary(algo_results)

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(algo_results, scales=scales)

splom(algo_results)




