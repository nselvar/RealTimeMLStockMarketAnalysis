#https://rpubs.com/SayakChakraborty/GoogleStockPrice_COVID19
#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html#selecting-a-candidate-arima-model
library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(dplyr)
library(readxl)
library(kableExtra)
library(data.table)
library(DT)
library(tsfknn)
library(ggplot2)

getSymbols("EXPE", src = "yahoo", from = "2019-07-01", to = "2020-03-28")
google_data_before_covid <- as.data.frame(EXPE)
str(google_data_before_covid)
tsData_before_covid_close <- ts(google_data_before_covid$EXPE.Close)
tsData_before_covid_start <- ts(google_data_before_covid$EXPE.Open)

getSymbols("EXPE", src = "yahoo", from = "2020-04-01")
google_data_after_covid <- as.data.frame(EXPE)
tsData_after_covid_close <- ts(google_data_after_covid$EXPE.Close)
tsData_after_covid_start <- ts(google_data_after_covid$EXPE.Open)



par(mfrow = c(1,2))
plot.ts(tsData_before_covid_close, ylab = "Closing Price", main = "Before COVID-19")
plot.ts(tsData_after_covid_close, ylab = "Closing Price", main = "During COVID-19")

plot.ts(tsData_before_covid_start, ylab = "Open Price", main = "Before COVID-19")
plot.ts(tsData_after_covid_start, ylab = "Open Price", main = "During COVID-19")



par(mfrow = c(2,2))
acf(tsData_before_covid_close, main = "Before COVID-19")
pacf(tsData_before_covid_close, main = "Before COVID-19")

acf(tsData_after_covid_close, main = "After COVID-19")
pacf(tsData_after_covid_close, main = "After COVID-19")

modelfit_before_covid <- auto.arima(tsData_before_covid_close, lambda = "auto")
summary(modelfit_before_covid)


modelfit_after_covid <- auto.arima(tsData_after_covid_close, lambda = "auto")
summary(modelfit_after_covid)



library(tseries)
adf.test(tsData) # p-value < 0.05 indicates the TS is stationary
kpss.test(tsData)


print(adf.test(tsData_before_covid_close))
print(adf.test(tsData_after_covid_close))


print(kpss.test(tsData_before_covid_close))
print(kpss.test(tsData_after_covid_close))



modelfit_before_covid <- auto.arima(tsData_before_covid, lambda = "auto")
summary(modelfit_before_covid)
modelfit_after_covid <- auto.arima(tsData_after_covid, lambda = "auto")
summary(modelfit_after_covid)



par(mfrow = c(2,3))

plot(modelfit_before_covid$residuals, ylab = 'Residuals', main = "Before COVID-19")
acf(modelfit_before_covid$residuals,ylim = c(-1,1), main = "Before COVID-19")
pacf(modelfit_before_covid$residuals,ylim = c(-1,1), main = "Before COVID-19")

plot(modelfit_after_covid$residuals, ylab = 'Residuals', main = "After COVID-19")
acf(modelfit_after_covid$residuals,ylim = c(-1,1), main = "After COVID-19")
pacf(modelfit_after_covid$residuals,ylim = c(-1,1), main = "After COVID-19")


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(modelfit_before_covid$residuals) # make a histogram
plotForecastErrors(modelfit_after_covid$residuals) # make a histogram



skirtsseriesforecasts <- HoltWinters(tsData_before_covid_close, gamma=FALSE)
plot(skirtsseriesforecasts)

#Load forecast package
require(forecast)
#Apply model into forecast
m <- forecast(skirtsseriesforecasts)

require(forecast)
library(forecast)
souvenirtimeseriesforecasts2 <-forecast:::forecast.HoltWinters(skirtsseriesforecasts,h=45)
plot(souvenirtimeseriesforecasts2)


p <- predict(skirtsseriesforecasts, 10)   # predict 10 periods ahead
plot(m, p)
lines(tsData_before_covid_close)
require(graphics)

Box.test(modelfit_after_covid$residuals, type = "Ljung-Box")
Box.test(modelfit_before_covid$residuals, type = "Ljung-Box")









