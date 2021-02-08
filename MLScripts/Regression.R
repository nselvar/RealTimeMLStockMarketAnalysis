library(leaps)
library(knitr)
library(fmsb)
library(ggplot2)
library(reshape2)
library(lubridate)
library(AppliedPredictiveModeling)
library(broom)
library(caret)
library(caTools)
library(class)
library(corrplot)
library(DataExplorer)
library(dplyr)
library(e1071) 
library(funModeling)
library(ggfortify) 
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(ISLR)
library(kableExtra)
library(kknn)
library(knitr)
library(lattice)
library(mgcv)
library(MLeval)
library(multiROC)
library(nnet)
library(pander)
library(party)
library(pROC)
library(quantmod) 
library(readxl)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library( splines)
library(tidyverse)
library(visreg)
theme_set(theme_classic())

#LOAD
fund <- read.csv("/Users/nselvarajan/Desktop/R/finals/datasets1/fundamentals.csv", header=T)
sprice <- read.csv("/Users/nselvarajan/Desktop/R/finals/datasets1/prices-split-adjusted.csv", header=T)
sec <- read.csv("/Users/nselvarajan/Desktop/R/finals/datasets1/securities.csv", header=T)

sprice$date <- ymd(sprice$date)
sprice$year <- year(sprice$date)
sprice$quarter <- quarter(sprice$date)

annual.sp <- aggregate(close ~ symbol + year, sprice, mean)
quarter.sp <- aggregate(close ~ symbol + year+quarter, sprice, mean)


#Add Industry (From SEC) to fundamentals (FUND)
names(sec)[names(sec)=="Ticker.symbol"] <- "Ticker.Symbol"
sec.short <- sec[,c(1,4,5)]
data <- merge(fund,sec.short, by="Ticker.Symbol", all.x=TRUE)

#Time Parse
data$Period.Ending <- ymd(data$Period.Ending)
data$endyear <- year(data$Period.Ending)
data$quarter <- quarter(data$Period.Ending)



#Add annual split-adj Stock Price to Fundamentals
annual.sp$index <- paste(as.character(annual.sp$symbol), annual.sp$year, sep=" ")
quarter.sp$index <- paste(as.character(quarter.sp$symbol), quarter.sp$year,quarter.sp$quarter, sep=" ")

data$index <- paste(as.character(data$Ticker.Symbol), data$endyear, sep=" ")
data<- merge(data, annual.sp[,3:4], by = "index", all.x=TRUE)
names(data)[names(data)=="close"] <- "Stock.p"
names(data)[names(data)=="GICS.Sector"] <- "Industry"
data <- subset(data, endyear > 2012 & endyear < 2016) #Remove Outliers
data$Period.Ending <- as.Date(data$Period.Ending)
data$Year <- year(data$Period.Ending)

str(data)
data <- data[,c(2,9,11,15,19,20,22,24,27,32,33,34,35,39,43,61,62,65,71,72,73,74,76,79,80,81,83,84,85,86)]

str(data)
data$Effect.of.Exchange.Rate <- NULL
data$Market.Cap <- data$Stock.p*data$Estimated.Shares.Outstanding
data <- subset(data, Estimated.Shares.Outstanding > 0)

#2013 to 2015, FULL DATA
data <- subset(data, Year > 2013 & Year < 2016)
data <- na.omit(data)
data <- subset(data, Industry != "Telecommunications Services")
data$Industry <- as.factor(as.character(data$Industry))

#Since Telecom is too small, Remove from dataset
tt <- table(data$Ticker.Symbol)
data <- subset(data, Ticker.Symbol %in% names(tt[tt == 2]))
data$Ticker.Symbol <- as.factor(as.character(data$Ticker.Symbol))
data <- data[,-c(12,14)]

load <- data
data$X <- NULL
nam <- names(data)

plot_missing(data)


str(data)

correlated_data <- data
correlated_data$Ticker.Symbol <-as.factor(data$Ticker.Symbol)
correlated_data$quarter <-as.factor(data$quarter)
correlated_data$Year <-as.factor(data$Year)
correlated_data$Industry <-as.factor(data$Industry)
correlated_data$endyear <-as.factor(data$endyear)
correlateddata$capital.Expenditures <-as.numeric(data$capital.Expenditures)
correlated_data$Cost.of.Revenue <-as.numeric(data$Cost.of.Revenue)
correlated_data$Earnings.Before.Interest.and.Tax <-as.numeric(data$Earnings.Before.Interest.and.Tax)
correlated_data$Gross.Profit <-as.numeric(data$Gross.Profit)
correlated_data$Liabilities <-as.numeric(data$Liabilities)
correlated_data$Net.Cash.Flow <-as.numeric(data$Net.Cash.Flow)
correlated_data$Profit.Margin <-as.numeric(data$Profit.Margin)
correlated_data$Total.Current.Assets <-as.numeric(data$Total.Current.Assets)
correlated_data$Total.Equity <-as.numeric(data$Total.Equity)
correlated_data$Total.Revenue <-as.numeric(data$Total.Revenue)
correlated_data$Estimated.Shares.Outstanding <-as.numeric(data$Estimated.Shares.Outstanding)
correlated_data$Stock.p <-as.numeric(data$Stock.p)
correlated_data$Market.Cap <-as.numeric(data$Market.Cap)
correlated_data$Cash.Ratio <-as.numeric(data$Cash.Ratio)
correlated_data$Depreciation <-as.numeric(data$Depreciation)
correlated_data$Fixed.Assets <-as.numeric(data$Fixed.Assets)
correlated_data$Investments <-as.numeric(data$Investments)
correlated_data$Long.Term.Debt <-as.numeric(data$Long.Term.Debt)
correlated_data$Pre.Tax.ROE <-as.numeric(data$Pre.Tax.ROE)
correlated_data$Retained.Earnings <-as.numeric(data$Retained.Earnings)
correlated_data$Total.Current.Liabilities <-as.numeric(data$Total.Current.Liabilities)
correlated_data$Total.Liabilities <-as.numeric(data$Total.Liabilities)
correlated_data$Earnings.Per.Share <-as.numeric(data$Earnings.Per.Share)




correlated_dataforanalysis <- correlated_data[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,28)]
#correlation plot
nam <- names(correlated_dataforanalysis)

correlation_r <- rcorr(as.matrix(correlated_dataforanalysis))
correlation_Matrix <- correlation_r$r
p_mat <- correlation_r$P



#col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corr_graph<-corrplot(correlation_Matrix, type = "upper", order = "hclust", 
                     p.mat = p_mat, sig.level = 0.05)
par(mfrow = c(1, 1))
corsig<-corrplot(correlation_Matrix,number.cex=0.75,
                 method = "color",
                 col = col(200),  
                 type = "upper", order = "hclust", 
                 addCoef.col = "black", # Add coefficient of correlation
                 tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
                 # Combine with significance level
                 p.mat = p_mat, sig.level = 0.05, insig = "blank", 
                 # hide correlation coefficient on the principal diagonal
                 diag = FALSE,
                 title = "Correlation Between Significant Biomarkers",
                 mar=c(0,0,1,0)
)


cor <- cor(correlated_dataforanalysis)

colnames(cor) <- c("Capital.Expenditures", "Cash.Ratio", 
                   "Cost.of.Revenue","Depreciation",
                   "Earnings.Before.Interest.and.Tax","Fixed.Assets",
                   "Gross.Profit","Investments",
                   "Liabilities","Long.Term.Debt",
                   "Net.Cash.Flow","Pre.Tax.ROE",
                   "Profit.Margin","Retained.Earnings",
                   "Total.Current.Assets","Total.Current.Liabilities",
                   "Total.Equity" ,"Total.Liabilities", 
                   "Total.Revenue" ,"Earnings.Per.Share",
                   "Estimated.Shares.Outstanding","Stock.p","Market.Cap")
rownames(cor) <-c("Capital.Expenditures", "Cash.Ratio", 
                  "Cost.of.Revenue","Depreciation",
                  "Earnings.Before.Interest.and.Tax","Fixed.Assets",
                  "Gross.Profit","Investments",
                  "Liabilities","Long.Term.Debt",
                  "Net.Cash.Flow","Pre.Tax.ROE",
                  "Profit.Margin","Retained.Earnings",
                  "Total.Current.Assets","Total.Current.Liabilities",
                  "Total.Equity" ,"Total.Liabilities", 
                  "Total.Revenue" ,"Earnings.Per.Share",
                  "Estimated.Shares.Outstanding","Stock.p","Market.Cap")
corrplot(correlation_Matrix, method = "number",  type="full",
         tl.srt = 50, tl.col = "black", tl.cex = 0.6, title = "Correlation of Variables",
         number.cex=0.25,
         mar=c(0,0,1,0)
)



par(mfrow=c(2,2))

outliers <- boxplot(correlated_data$Cost.of.Revenue, main = "Revenue",
                    ylab = "Cost.of.Revenue",
                    col = "orange",
                    border = "brown")$out
#mean(correlated_data$Cost.of.Revenue)
correlated_data[correlated_data$Cost.of.Revenue %in% outliers, "Cost.of.Revenue"] = 13995000000
boxplot(correlated_data$Cost.of.Revenue,
        main = "Cleaned Revenue",
        ylab = "Cost.of.Revenue",
        col = "orange",
        border = "brown")



outliersDepreciation <- boxplot(correlated_data$Depreciation,
                                main = "Depreciation",
                                ylab = "Depreciation",
                                col = "orange",
                                border = "brown")$out
mean(correlated_data$Depreciation)
correlated_data[correlated_data$Depreciation %in% outliersDepreciation, "Depreciation"] = 1141264753
boxplot(correlated_data$Depreciation,
        main = "Cleaned Depreciation",
        ylab = "Depreciation",
        col = "orange",
        border = "brown")


outliersEarningsBeforeInterestTax <- boxplot(correlated_data$Earnings.Before.Interest.and.Tax,
                                             main = "Earnings Before InterestTax",
                                             ylab = "Earnings Before InterestTax",
                                             col = "orange",
                                             border = "brown")$out
mean(correlated_data$Earnings.Before.Interest.and.Tax)
correlated_data[correlated_data$Earnings.Before.Interest.and.Tax %in% outliersEarningsBeforeInterestTax, "Earnings.Before.Interest.and.Tax"] = 2428463098
boxplot(correlated_data$Earnings.Before.Interest.and.Tax,
        main = "Cleaned EarningsBeforeInterestTax",
        ylab = "Earnings Before InterestTax",
        col = "orange",
        border = "brown")


outliersFixed.Assets <- boxplot(correlated_data$Fixed.Assets,
                                main = "Fixed.Assets ",
                                ylab = "Fixed.Assets",
                                col = "orange",
                                border = "brown")$out
mean(correlated_data$Fixed.Assets)
correlated_data[correlated_data$Fixed.Assets %in% outliersFixed.Assets, "Fixed.Assets"] = 9510363527
boxplot(correlated_data$Fixed.Assets,
        main = "Cleaned Fixed Assets",
        ylab = "Fixed.Assets",
        col = "orange",
        border = "brown")



outliersGross.Profit <- boxplot(correlated_data$Gross.Profit,
                                main = "Gross Profit",
                                ylab = "Gross Profit",
                                col = "orange",
                                border = "brown")$out
mean(correlated_data$Gross.Profit)
correlated_data[correlated_data$Gross.Profit %in% outliersGross.Profit, "Gross.Profit"] = 7329930088
boxplot(correlated_data$Gross.Profit,
        main = "Cleaned Gross Profit",
        ylab = "Gross.Profit",
        col = "orange",
        border = "brown")




outliersLiabilities <- boxplot(correlated_data$Liabilities,
                               main = "Liabilities",
                               ylab = "Liabilities",
                               col = "orange",
                               border = "brown")$out
mean(correlated_data$Liabilities)
correlated_data[correlated_data$Liabilities %in% outliersLiabilities, "Liabilities"] = 141345628
boxplot(correlated_data$Liabilities,
        main = "Cleaned Liabilities",
        ylab = "Liabilities",
        col = "orange",
        border = "brown")



outliersLongTermDebt <- boxplot(correlated_data$Long.Term.Debt,
                                main = "Long.Term.Debt",
                                ylab = "Long.Term.Debt",
                                col = "orange",
                                border = "brown")$out
mean(correlated_data$Long.Term.Debt)
correlated_data[correlated_data$Long.Term.Debt %in% outliersLongTermDebt, "Long.Term.Debt"] = 6612529131
boxplot(correlated_data$Long.Term.Debt,
        main = "Cleaned Long Term Debt",
        ylab = "Long Term Debt",
        col = "orange",
        border = "brown")




outliersRetained.Earnings <- boxplot(correlated_data$Retained.Earnings,
                                     main = "Retained.Earnings",
                                     ylab = "Retained.Earnings",
                                     col = "orange",
                                     border = "brown")$out
mean(correlated_data$Retained.Earnings)
correlated_data[correlated_data$Retained.Earnings %in% outliersRetained.Earnings, "Retained.Earnings"] = 6612529131
boxplot(correlated_data$Retained.Earnings,
        main = "Cleaned Retained.Earnings",
        ylab = "Retained.Earnings",
        col = "orange",
        border = "brown")




outliersTotal.Liabilities <- boxplot(correlated_data$Total.Liabilities,
                                     main = "Total.Liabilities",
                                     ylab = "Total.Liabilities",
                                     col = "orange",
                                     border = "brown")$out
mean(correlated_data$Total.Liabilities)
correlated_data[correlated_data$Total.Liabilities %in% outliersTotal.Liabilities, "Total.Liabilities"] = 16917262983
boxplot(correlated_data$Total.Liabilities,
        main = "Cleaned Total.Liabilities",
        ylab = "Total.Liabilities",
        col = "orange",
        border = "brown")



outliersTotal.Revenue <- boxplot(correlated_data$Total.Revenue,
                                 main = "Total.Revenue",
                                 ylab = "Total.Revenue",
                                 col = "orange",
                                 border = "brown")$out
mean(correlated_data$Total.Revenue)
correlated_data[correlated_data$Total.Revenue %in% outliersTotal.Revenue, "Total.Revenue"] = 21324933067
boxplot(correlated_data$Total.Revenue,
        main = "Cleaned Total.Revenue",
        ylab = "Total.Revenue",
        col = "orange",
        border = "brown")



outliersTotal.Equity <- boxplot(correlated_data$Total.Equity,
                                main = "Total.Equity",
                                ylab = "Total.Equity",
                                col = "orange",
                                border = "brown")$out
mean(correlated_data$Total.Equity)
correlated_data[correlated_data$Total.Equity %in% outliersTotal.Equity, "Total.Equity"] = 9407030214
boxplot(correlated_data$Total.Equity,
        main = "Cleaned Total.Equity",
        ylab = "Total.Equity",
        col = "orange",
        border = "brown")



outliersEarnings.Per.Share <- boxplot(correlated_data$Earnings.Per.Share,
                                      main = "Earnings.Per.Share ",
                                      ylab = "Earnings.Per.Share",
                                      col = "orange",
                                      border = "brown")$out
mean(correlated_data$Earnings.Per.Share)
correlated_data[correlated_data$Earnings.Per.Share %in% outliersEarnings.Per.Share, "Earnings.Per.Share"] = 3.273266
boxplot(correlated_data$Earnings.Per.Share,
        main = "Cleaned Earnings.Per.Shares",
        ylab = "Earnings.Per.Share",
        col = "orange",
        border = "brown")




outliersStock.p <- boxplot(correlated_data$Stock.p,
                           main = "Stock.p",
                           ylab = "Stock.p",
                           col = "orange",
                           border = "brown")$out
mean(correlated_data$Stock.p)
correlated_data[correlated_data$Stock.p %in% outliersStock.p, "Stock.p"] = 83.45985
boxplot(correlated_data$Stock.p,
        main = "Cleaned Stock.p",
        ylab = "Stock.p",
        col = "orange",
        border = "brown")



outliersEstimated.Shares.Outstanding <- boxplot(correlated_data$Estimated.Shares.Outstanding,
                                                main = "Estimated.Shares.Outstanding",
                                                ylab = "Estimated.Shares.Outstanding",
                                                col = "orange",
                                                border = "brown")$out
mean(correlated_data$Estimated.Shares.Outstanding)
correlated_data[correlated_data$Estimated.Shares.Outstanding %in% outliersEstimated.Shares.Outstanding, "Estimated.Shares.Outstanding"] = 83.45985
boxplot(correlated_data$Estimated.Shares.Outstanding,
        main = "Cleaned Estimated.Shares.Outstanding",
        ylab = "Estimated.Shares.Outstanding",
        col = "orange",
        border = "brown")


outliersMarket.Cap <- boxplot(correlated_data$Market.Cap,
                              main = "Market.Cap",
                              ylab = "Market.Cap",
                              col = "orange",
                              border = "brown")$out
mean(correlated_data$Market.Cap)
correlated_data[correlated_data$Market.Cap %in% outliersMarket.Cap, "Market.Cap"] = 13239613534
outliersMarket.Cap <- boxplot(correlated_data$Market.Cap,
                              main = "Market.Cap",
                              ylab = "Market.Cap",
                              col = "orange",
                              border = "brown")$out
mean(correlated_data$Market.Cap)
correlated_data[correlated_data$Market.Cap %in% outliersMarket.Cap, "Market.Cap"] = 13239613534


boxplot(correlated_data$Market.Cap,
        main = "Market.Cap",
        ylab = "Market.Cap",
        col = "orange",
        border = "brown")

par(mfrow=c(1,1))


#Regression Plot 
w1 <- ggplot(correlated_data, aes(y=Market.Cap, x=Cost.of.Revenue)) + geom_point(colour="blue")
w1<- w1 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Cost.of.Revenue")

w2 <- ggplot(correlated_data, aes(y=Market.Cap, x=Depreciation)) + geom_point(colour="blue")
w2<- w2 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Depreciation")

w3 <- ggplot(correlated_data, aes(y=Market.Cap, x=Earnings.Before.Interest.and.Tax)) + geom_point(colour="blue")
w3<- w3 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Earnings.Before.Interest.and.Tax")

w4 <- ggplot(correlated_data, aes(y=Market.Cap, x=Fixed.Assets)) + geom_point(colour="blue")
w4<- w4 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Fixed.Assets")

w5 <- ggplot(correlated_data, aes(y=Market.Cap, x=Gross.Profit)) + geom_point(colour="blue")
w5<- w5 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Gross.Profit")

w6 <- ggplot(correlated_data, aes(y=Market.Cap, x=Liabilities)) + geom_point(colour="blue")
w6<- w6 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Liabilities")

w7 <- ggplot(correlated_data, aes(y=Market.Cap, x=Long.Term.Debt)) + geom_point(colour="blue")
w7<- w7 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Long.Term.Debt")

w8 <- ggplot(correlated_data, aes(y=Market.Cap, x=Retained.Earnings)) + geom_point(colour="blue")
w8<- w8 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Retained.Earnings")


w10 <- ggplot(correlated_data, aes(y=Market.Cap, x=Total.Revenue)) + geom_point(colour="blue")
w10<- w10 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Total.Revenue")

w11 <- ggplot(correlated_data, aes(y=Market.Cap, x=Total.Equity)) + geom_point(colour="blue")
w11<- w11 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Total.Equity")

w12 <- ggplot(correlated_data, aes(y=Market.Cap, x=Earnings.Per.Share)) + geom_point(colour="blue")
w12<- w12 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Earnings.Per.Share")

w13 <- ggplot(correlated_data, aes(y=Market.Cap, x=Stock.p)) + geom_point(colour="blue")
w13<- w13 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Stock.p")

w14 <- ggplot(correlated_data, aes(y=Market.Cap, x=Estimated.Shares.Outstanding)) + geom_point(colour="blue")
w14<- w14 + stat_smooth(method="lm", formula = y~poly(x,2))+ ggtitle("Regression Plot \n Polynomial \n Estimated.Shares.Outstanding")


grid.arrange(w1,w2,ncol=2)
grid.arrange(w3,w4,ncol=2)
grid.arrange(w5,w6,ncol=2)
grid.arrange(w7,w8,ncol=2)
grid.arrange(w10,w11,ncol=2)
grid.arrange(w12,w13,ncol=2)



grid.arrange(w1,w2,w3,w4,w5,w6,ncol=2)
grid.arrange(w7,w8,w10,w11,w12,w13,ncol=2)
grid.arrange(w14,ncol=1)







correlated_dataforanalysis <- correlated_data[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,28)]
correlation_Matrix <- correlation_r$r
p_mat <- correlation_r$P



#col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corr_graph<-corrplot(correlation_Matrix, type = "upper", order = "hclust", 
                     p.mat = p_mat, sig.level = 0.05)
par(mfrow = c(1, 1))
corsig<-corrplot(correlation_Matrix,number.cex=0.75,
                 method = "color",
                 col = col(200),  
                 type = "upper", order = "hclust", 
                 addCoef.col = "black", # Add coefficient of correlation
                 tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
                 # Combine with significance level
                 p.mat = p_mat, sig.level = 0.05, insig = "blank", 
                 # hide correlation coefficient on the principal diagonal
                 diag = FALSE,
                 title = "Correlation Between Significant Biomarkers",
                 mar=c(0,0,1,0)
)


cor <- cor(correlated_dataforanalysis)

colnames(cor) <- c("Capital.Expenditures", "Cash.Ratio", 
                   "Cost.of.Revenue","Depreciation",
                   "Earnings.Before.Interest.and.Tax","Fixed.Assets",
                   "Gross.Profit","Investments",
                   "Liabilities","Long.Term.Debt",
                   "Net.Cash.Flow","Pre.Tax.ROE",
                   "Profit.Margin","Retained.Earnings",
                   "Total.Current.Assets","Total.Current.Liabilities",
                   "Total.Equity" ,"Total.Liabilities", 
                   "Total.Revenue" ,"Earnings.Per.Share",
                   "Estimated.Shares.Outstanding","Stock.p","Market.Cap")
rownames(cor) <-c("Capital.Expenditures", "Cash.Ratio", 
                  "Cost.of.Revenue","Depreciation",
                  "Earnings.Before.Interest.and.Tax","Fixed.Assets",
                  "Gross.Profit","Investments",
                  "Liabilities","Long.Term.Debt",
                  "Net.Cash.Flow","Pre.Tax.ROE",
                  "Profit.Margin","Retained.Earnings",
                  "Total.Current.Assets","Total.Current.Liabilities",
                  "Total.Equity" ,"Total.Liabilities", 
                  "Total.Revenue" ,"Earnings.Per.Share",
                  "Estimated.Shares.Outstanding","Stock.p","Market.Cap")
corrplot(correlation_Matrix, method = "number",  type="full",
         tl.srt = 50, tl.col = "black", tl.cex = 0.6, title = "Correlation of Variables",
         number.cex=0.25,
         mar=c(0,0,1,0)
)

















####Linear Regression
linearregdataset<-correlated_data
trainIndex1 <- createDataPartition(linearregdataset$Market.Cap, p = 0.8, list=FALSE, times=3)
subTrain1 <- linearregdataset[trainIndex1,]
subTest1 <- linearregdataset[-trainIndex1,]

# setup cross validation and control parameters
control <- trainControl(method="repeatedcv", number=3, repeats = 3, verbose = TRUE, search = "grid")
metric <- "RMSE"
tuneLength <- 10

# Training process 
# Fit / train a Linear Regression model to  dataset



linearModelReg <- caret::train(Market.Cap~
                                 Total.Equity+Total.Revenue+ Estimated.Shares.Outstanding+Earnings.Before.Interest.and.Tax+Gross.Profit+Stock.p
                               ,data=subTrain1, method="lm", metric=metric, 
                               preProc=c("center", "scale"), trControl=control, tuneLength = tuneLength)

linearplotmodel<-lm( Market.Cap~
                       Total.Equity+Total.Revenue+ Estimated.Shares.Outstanding+Earnings.Before.Interest.and.Tax+Gross.Profit+Stock.p
                     ,data = subTrain1)


summary(linearModelReg)

predictions<-predict(linearModelReg,newdata = subTest1)

rmse<-RMSE( predictions, subTest1$Market.Cap)
rmse

error.rate.linear=rmse/mean(subTest1$Market.Cap)
error.rate.linear

linearr2= R2( predictions, subTest1$Market.Cap) 
linearr2

lineardf <- data.frame( RMSE = rmse, R2 = linearr2 , Error =error.rate.linear) 



####Polynominal Regression

set.seed(400)

polyregdataset<-correlated_data
polyregdatasettrainIndex <- createDataPartition(polyregdataset$Market.Cap, p = 0.8, list=FALSE, times=3)
polyregdatasetsubTrain <- polyregdataset[polyregdatasettrainIndex,]
polyregdatasetsubTest <- polyregdataset[-polyregdatasettrainIndex,]

# setup cross validation and control parameters
control <- trainControl(method="repeatedcv", number=3, repeats = 3, verbose = TRUE, search = "grid")
metric <- "RMSE"
tuneLength <- 10


poly_reg <- caret::train(Market.Cap~
                           poly( Total.Equity,2)+ poly( Total.Revenue,2)+
                           poly( Estimated.Shares.Outstanding,2)+ poly( Earnings.Before.Interest.and.Tax,2)+
                           poly( Gross.Profit,2)
                         +poly( Stock.p,2)
                         ,data=polyregdatasetsubTrain, method="lm", metric=metric, 
                         preProc=c("center", "scale"), trControl=control, tuneLength = tuneLength)

predictionpoly<-predict(poly_reg,newdata = polyregdatasetsubTest)


rmsepoly<-RMSE( predictionpoly, polyregdatasetsubTest$Market.Cap)
rmsepoly

error.rate.poly=rmse/mean(polyregdatasetsubTest$Market.Cap)
error.rate.poly

polyrsquare = R2( predictionpoly, polyregdatasetsubTest$Market.Cap) 
polyrsquare

polydf <- data.frame( RMSE = rmsepoly, R2 = polyrsquare , Error =error.rate.poly) 


###Spline


set.seed(400)
splineDataset<-correlated_data
splineDatasetTrainIndex <- createDataPartition(splineDataset$Market.Cap, p = 0.8, list=FALSE, times=3)
splineDatasetSubTrain <- splineDataset[splineDatasetTrainIndex,]
splineDatasetSubTest <- splineDataset[-splineDatasetTrainIndex,]

knots <- quantile( splineDatasetSubTrain$Market.Cap, p = c( 0.25,0.50,0.75,1))

splineModel <- caret::train(Market.Cap~
                              bs( Total.Equity, knots = knots) +
                              bs( Total.Revenue, knots = knots)
                            +bs( Estimated.Shares.Outstanding, knots = knots)
                            + bs( Earnings.Before.Interest.and.Tax, knots = knots)
                            +bs( Gross.Profit, knots = knots)
                            ,data=splineDatasetSubTrain, method="lm", metric=metric, 
                            preProc=c("center", "scale"), trControl=control, tuneLength = tuneLength)

summary(splineModel)

predictionSpline<-predict(splineModel,newdata = splineDatasetSubTest)

rmseSpline<-RMSE( predictionSpline, splineDatasetSubTest$Market.Cap)
rmseSpline

error.rate.spline=rmseSpline/mean(splineDatasetSubTest$Market.Cap)
error.rate.spline

splinerSquare = R2( predictionSpline, splineDatasetSubTest$Market.Cap) 
splinerSquare
# Model performance 

splinedf <- data.frame( RMSE = rmseSpline, R2 = splinerSquare , Error =error.rate.spline) 


### Generalized Linear Model


set.seed(200)
gamDataset<-correlated_data
gamDatasetDatasetTrainIndex <- createDataPartition(gamDataset$Market.Cap, p = 0.8, list=FALSE, times=3)
gamDatasetDatasetSubTrain <- gamDataset[gamDatasetDatasetTrainIndex,]
gamDatasetDatasetSubTest <- gamDataset[-gamDatasetDatasetTrainIndex,]


gamModel <- gam(Market.Cap ~ Total.Equity+Total.Revenue+Earnings.Before.Interest.and.Tax+Estimated.Shares.Outstanding+Gross.Profit, data=gamDatasetDatasetSubTrain)
gamFit <- gam(Market.Cap ~ Total.Equity+Earnings.Before.Interest.and.Tax, data=gamDatasetDatasetSubTrain)

summary(gamFit)

predictiongam<-predict(gamFit,newdata = gamDatasetDatasetSubTest)

rmsegam<-RMSE( predictiongam, gamDatasetDatasetSubTest$Market.Cap)
rmsegam

error.rate.gam=rmsegam/mean(gamDatasetDatasetSubTest$Market.Cap)
error.rate.gam

rsquaregam = R2( predictiongam, gamDatasetDatasetSubTest$Market.Cap) 
rsquaregam

gamdf <- data.frame( RMSE = rmsegam, R2 = rsquaregam , Error =error.rate.gam) 

s3d <- scatterplot3d(correlated_data$Market.Cap,
                     correlated_data$Total.Equity,
                     correlated_data$Earnings.Before.Interest.and.Tax, 
                     pch=16, highlight.3d = TRUE, type = "h", 
                     main = "Multi-Variable Regression 
                     \nMarket.Cap ~ Total.Equity + Earnings.Before.Interest.and.Tax", 
                     xlab="Market.Cap", 
                     zlab="Earnings.Before.Interest.and.Tax", 
                     ylab="Total.Equity", 
                     angle=35)
s3d$plane3d(gamFit)

#Plotting the Model
par(mfrow=c(1,1)) #to partition the Plotting Window
plot(gamFit, all.terms = TRUE) 
#se stands for standard error Bands
gam.check(gamFit, k.rep=1000)


#Specify A Smoothing Spline Fit In A GAM Formula.Cubic regression splines


set.seed(200)
cubicRegressionSplineDataset<-correlated_data
cubicRegressionSplineDatasetTrainIndex <- createDataPartition(cubicRegressionSplineDataset$Market.Cap, p = 0.8, list=FALSE, times=3)
cubicRegressionDatasetSubTrain <- cubicRegressionSplineDataset[cubicRegressionSplineDatasetTrainIndex,]
cubicRegressionDatasetSubTest <- cubicRegressionSplineDataset[-cubicRegressionSplineDatasetTrainIndex,]


cubicModel <- gam(Market.Cap ~ s(Total.Equity, bs="cr")+s(Total.Revenue, bs="cr")+
                 s(Earnings.Before.Interest.and.Tax, bs="cr"),
               data=cubicRegressionSplineDataset)

summary(cubicModel)
gam.check(cubicModel, k.rep=1000)

vis.gam(cubicModel, type='response', plot.type='persp',
        phi=30, theta=30, n.grid=500, border=NA)
visreg2d(cubicModel, xvar='Total.Equity', yvar='Market.Cap', scale='response')

predictiongamsmooth<-predict(cubicModel,newdata = cubicRegressionDatasetSubTest)

rmsegamsmooth<-RMSE( predictiongamsmooth, cubicRegressionDatasetSubTest$Market.Cap)
rmsegamsmooth

error.rate.gam.smooth=rmsegamsmooth/mean(cubicRegressionDatasetSubTest$Market.Cap)
error.rate.gam.smooth

rsquaregamsmooth = R2( predictiongamsmooth, cubicRegressionDatasetSubTest$Market.Cap) 
rsquaregamsmooth

gamsmoothdf <- data.frame( RMSE = rmsegamsmooth, R2 = rsquaregamsmooth , Error =error.rate.gam.smooth) 



#### Extreme Gradient Boosting
set.seed(200)
extremeGradinetBoostingDataset<-correlated_data
extremeGradinetBoostingDatasetTrainIndex <- createDataPartition(extremeGradinetBoostingDataset$Market.Cap, p = 0.8, list=FALSE, times=3)
extremeGradinetBoostingDatasetSubTrain <- extremeGradinetBoostingDataset[extremeGradinetBoostingDatasetTrainIndex,]
extremeGradinetBoostingDatasetSubTest <- extremeGradinetBoostingDataset[-extremeGradinetBoostingDatasetTrainIndex,]


xgbGrid <- expand.grid(nrounds = c(140,160),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.3,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)



model_xgb <- train(Market.Cap ~ Total.Equity+Total.Revenue+ Estimated.Shares.Outstanding+Earnings.Before.Interest.and.Tax+Gross.Profit+Stock.p,
                   data = extremeGradinetBoostingDatasetSubTrain,
                   method = "xgbTree",
                   preProcess = c("scale", "center"),
                   trControl = trainControl(method = "repeatedcv", 
                                            number = 5, 
                                            repeats = 3, 
                                            verboseIter = FALSE),
                   tuneGrid = xgbGrid,
                   verbose = 0)

model_xgb$results   
plot(model_xgb)

pred_xgb = predict(model_xgb, extremeGradinetBoostingDatasetSubTest)
str(extremeGradinetBoostingDatasetSubTest)

#changed from 30
mse = mean((extremeGradinetBoostingDatasetSubTest[, 28] - pred_xgb)^2)
mae = caret::MAE(extremeGradinetBoostingDatasetSubTest[, 28], pred_xgb)
rmse = caret::RMSE(extremeGradinetBoostingDatasetSubTest[, 28], pred_xgb)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(extremeGradinetBoostingDatasetSubTest[, 1])
plot(x, extremeGradinetBoostingDatasetSubTest[, 1], col = "red", type = "l",lty=3, lwd=3, xlab='x', ylab='y')
lines(x, pred_xgb, col = "blue", type = "l")
legend(x = 1, y = 5000,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))


rmseXgb<-RMSE( pred_xgb, extremeGradinetBoostingDatasetSubTest$Market.Cap)
rmseXgb

error.rate.Xgb=rmseXgb/mean(extremeGradinetBoostingDatasetSubTest$Market.Cap)
error.rate.Xgb

rsquarexgb = R2( pred_xgb, extremeGradinetBoostingDatasetSubTest$Market.Cap) 
rsquarexgb

gamstochasticdf <- data.frame( RMSE = rmseXgb, R2 = rsquarexgb , Error =error.rate.Xgb) 





linearplotmodel<-lm( Market.Cap~
                       Total.Equity+Total.Revenue+ Estimated.Shares.Outstanding+Earnings.Before.Interest.and.Tax+Gross.Profit+Stock.p
                     ,data = subTrain1)


plot(linearplotmodel,col="lightblue")
plot(linearplotmodel, 4,col="lightblue")
plot(linearplotmodel, 5,col="lightblue")




