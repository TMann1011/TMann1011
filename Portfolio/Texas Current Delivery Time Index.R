rm(list=ls())


##  Load packages.
library(fpp)
library(forecast)
library(ggplot2)
library(tis)
library(readxl)
library(fpp2)
library(GGally)
library(corrplot)
library(ggcorrplot)
library(gridExtra)

setwd('C:/Users/tyler/OneDrive/Desktop/Tyler stuff/R Forecasting Methods')
getwd()

data<-read.csv("DTMSAMFRBDAL.csv",header=TRUE, stringsAsFactors = FALSE)
View(data)
attributes(data)

head(data,12)
tail(data,12)

tsdata <- ts(data[,2],  start = c(2004,1), frequency = 12)
class(tsdata)
str(tsdata)
autoplot(tsdata)

##  Split the data into a training and a test set.
trainData <- window(tsdata, end = c(2019,4))
testData  <- window(tsdata, start = c(2019,5))

trainData
testData

plot(trainData)
plot(testData)

autoplot(trainData, series='TrainData') + autolayer(testData, series='TestData') + 
  ggtitle("Texas Manufacturing Outlook: Time Series Analysis")+
  xlab("Months") +
  ylab("Delivery Time") 

fit <- decompose(tsdata, type='multiplicative')
autoplot(fit) + ggtitle('Multiplicative Decomposition of Delivery Time Index')
fit


#forecast
fcast_snaive <- snaive(fit$seasonal, h=12)
fcast_meanf <- meanf(fit$seasonal, h=12)
fcast_naive <- naive(fit$seasonal, h=12)
fcast_rwf <- rwf(fit$seasonal, h=12, drift=T)

autoplot(fcast_snaive)
fcast_snaive

autoplot(fcast_meanf)
fcast_meanf

autoplot(fcast_naive)
fcast_naive

autoplot(fcast_rwf)
fcast_rwf

#   Reseasonalize and produce the point forecast.
afp <- ts(c(tsdata, fcast_naive$mean*fcast_meanf$mean*fcast_snaive$mean*fcast_rwf$mean), start=c(2004,1), frequency = 12)
afp

#   produce the 95% confidence
#   prediction interval.
fl  <- fcast_snaive$lower[,2] * fcast_rwf$lower[,2]
fu  <- fcast_snaive$upper[,2] * fcast_rwf$upper[,2]


#   Plot the actual and forecasted values.
autoplot(window(afp, end=c(2021,12))) +
  autolayer(window(afp, start=c(2022,1)), series='forecast') +
  autolayer(fl, series='95% bound') +
  autolayer(fu, series='95% bound') +
  ggtitle('Texas Delivery Time Index\nForecast with Forecast Bounds\nClassical Decomposition')+
  ylab('Delivery Index')


accuracy(fcast_naive,afp)
accuracy(fcast_snaive,afp)
accuracy(fcast_meanf,afp)
accuracy(fcast_rwf,afp)
# Naive and the rwf has the lowest MAPE, RMSE, etc. These two are the best models to use
# You can pick the model with the lowest MAPE value. This is true given the abnormal rise and fall of 


checkresiduals(fcast_naive)     # visually, residual is not rising or declining, and around the mean 0, also the ACF plot did not quickly go to zero, and but after that stays within the blue significance lines for the most part, histogram is not normal, it does not appear to be a whitenoise 
checkresiduals(fcast_snaive)    # visually, residual has varying levels of variations over time, it is not really centered around 0, positive values are more common, ACF plot does not quickly go to 0 and stays above the significance line for the most part, histogram is not normal, it does not appear to be a whitenoise
checkresiduals(fcast_meanf)     # visually, residual is rising with higher variations, not centered around 0, ACF plot is significantly higher than the blue significance lines,histogram is not normal, it does not appear to be close to whitenoise
checkresiduals(fcast_rwf)       # visually, residual is not rising or declining, and around the mean 0, also the ACF plot did not quickly go to zero, and but after that stays within the blue significance lines for the most part,histogram is not normal, it does not appear to be a whitenoise 


# You can also do do a test of stationarity / white noise

residuals(fcast_naive)
residuals(fcast_snaive)
residuals(fcast_meanf)
residuals(fcast_rwf)


Box.test(residuals(fcast_naive), lag=10, type="Ljung-Box")    # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(fcast_snaive), lag=10, type="Ljung-Box")   # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(fcast_meanf), lag=10, type="Ljung-Box")    # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(fcast_rwf), lag=10, type="Ljung-Box")      # Not stationary or a whitenoise, p-value is less than 0.05
