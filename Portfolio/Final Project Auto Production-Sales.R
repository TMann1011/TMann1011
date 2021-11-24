rm(list=ls())

## Load following packages.
## Install them if you don't have them installed already.
install.packages('doBy')
library(fpp)
library(quantmod)
library(lubridate)
library(ggplot2)
library(cowplot)
library(doBy)
library(dplyr)
library(urca)
library(forecast)
library(corrplot)
library(gridExtra)

setwd('C:\\Users\\tyler\\OneDrive\\Desktop\\Tyler stuff\\R Forecasting Methods')
getwd()

##Read and view the original data
data_Sales <- read.csv('TOTALSA.csv', header=TRUE, stringsAsFactors=FALSE)
View(data_Sales)
class(data_Sales)

##Read and view the original data for second dataset
data_Production <- read.csv('DAUPSA.csv', header=TRUE, stringsAsFactors=FALSE)
View(data_Production)
class(data_Production)

##Change the data from a dataframe to a time series
tsdata_Sales <- ts(data_Sales[,2], start=c(1976,01), frequency=12)
class(tsdata_Sales)
str(tsdata_Sales)
autoplot(tsdata_Sales)

##Change the data from a datafrom to a time series
tsdata_Production <- ts(data_Production[,2], start=c(1993,01), frequency=12)
class(tsdata_Production)
str(tsdata_Production)
autoplot(tsdata_Production)

##Split the data into a training and test set, then plot.
trainSales <- window(tsdata_Sales, end=c(2016,1))
testSales <- window(tsdata_Sales, start=c(2016,2))
autoplot(trainSales)
autoplot(testSales)

##Split the data into a training and test set, then plot.
trainProduction <- window(tsdata_Production, end=c(2016,1))
testProduction <- window(tsdata_Production, start=c(2016,2))
autoplot(trainProduction)
autoplot(testProduction)

##Plot the training and test sales.
autoplot(trainSales, series="Training")+ autolayer(testSales, series="Test")+
  ggtitle('Auto Sales')+ ylab('Millions of Units')

autoplot(trainProduction, series="Training")+ autolayer(testProduction, series="Test")+
  ggtitle('Auto Production')+ ylab('Millions of Units')


################################################################################

#Forecast Auto Sales
fcast_snaive_Sales <- snaive(trainSales, h=69)
fcast_meanf_Sales <- meanf(trainSales, h=69)
fcast_naive_Sales <- naive(trainSales, h=69)
fcast_rwf_Sales <- rwf(trainSales, h=69, drift=T)

autoplot(fcast_snaive_Sales)
fcast_snaive_Sales

autoplot(fcast_meanf_Sales)
fcast_meanf_Sales

autoplot(fcast_naive_Sales)
fcast_naive_Sales

autoplot(fcast_rwf_Sales)
fcast_rwf_Sales

#Forecast Auto Production
fcast_snaive_Production <- snaive(trainProduction, h=68)
fcast_meanf_Production <- meanf(trainProduction, h=68)
fcast_naive_Production <- naive(trainProduction, h=68)
fcast_rwf_Production <- rwf(trainProduction, h=68, drift=T)

autoplot(fcast_snaive_Production)
fcast_snaive_Production

autoplot(fcast_meanf_Production)
fcast_meanf_Production

autoplot(fcast_naive_Production)
fcast_naive_Production

autoplot(fcast_rwf_Production)
fcast_rwf_Production

################################################################################

naiveSalesFcast <- accuracy(fcast_naive_Sales, testSales)
snaiveSalesFcast <- accuracy(fcast_snaive_Sales, testSales)
meanfSalesFcast <- accuracy(fcast_meanf_Sales, testSales)
rwfwithdriftSalesFcast <- accuracy(fcast_rwf_Sales, testSales)

naiveProductionFcast <- accuracy(fcast_naive_Production, testProduction)
snaiveProductionFcast <- accuracy(fcast_snaive_Production, testProduction)
meanfProductionFcast <- accuracy(fcast_meanf_Production, testProduction)
rwfwithdriftProductionFcast <- accuracy(fcast_rwf_Production, testProduction)

# Naive and the rwf has the lowest MAPE, RMSE, etc. These two are the best models to use
# You can pick the model with the lowest MAPE value. This is true given the abnormal rise and fall of 


checkresiduals(naiveSalesFcast)     # visually, residual is not rising or declining, and around the mean 0, also the ACF plot did not quickly go to zero, and but after that stays within the blue significance lines for the most part, histogram is not normal, it does not appear to be a whitenoise 
checkresiduals(snaiveSalesFcast)    # visually, residual has varying levels of variations over time, it is not really centered around 0, positive values are more common, ACF plot does not quickly go to 0 and stays above the significance line for the most part, histogram is not normal, it does not appear to be a whitenoise
checkresiduals(meanfSalesFcast)     # visually, residual is rising with higher variations, not centered around 0, ACF plot is significantly higher than the blue significance lines,histogram is not normal, it does not appear to be close to whitenoise
checkresiduals(rwfwithdriftSalesFcast)       # visually, residual is not rising or declining, and around the mean 0, also the ACF plot did not quickly go to zero, and but after that stays within the blue significance lines for the most part,histogram is not normal, it does not appear to be a whitenoise 

checkresiduals(naiveProductionFcast)     # visually, residual is not rising or declining, and around the mean 0, also the ACF plot did not quickly go to zero, and but after that stays within the blue significance lines for the most part, histogram is not normal, it does not appear to be a whitenoise 
checkresiduals(snaiveProductionFcast)    # visually, residual has varying levels of variations over time, it is not really centered around 0, positive values are more common, ACF plot does not quickly go to 0 and stays above the significance line for the most part, histogram is not normal, it does not appear to be a whitenoise
checkresiduals(meanfProductionFcast)     # visually, residual is rising with higher variations, not centered around 0, ACF plot is significantly higher than the blue significance lines,histogram is not normal, it does not appear to be close to whitenoise
checkresiduals(rwfwithdriftProductionFcast)       # visually, residual is not rising or declining, and around the mean 0, also the ACF plot did not quickly go to zero, and but after that stays within the blue significance lines for the most part,histogram is not normal, it does not appear to be a whitenoise 


# You can also do do a test of stationarity / white noise

residuals(naiveSalesFcast)
residuals(snaiveSalesFcast)
residuals(meanfSalesFcast)
residuals(rwfwithdriftSalesFcast)

residuals(naiveProductionFcast)
residuals(snaiveProductionFcast)
residuals(meanfProductionFcast)
residuals(rwfwithdriftProductionFcast)

Box.test(residuals(naiveSalesFcast), lag=10, type="Ljung-Box")    # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(snaiveSalesFcast), lag=10, type="Ljung-Box")   # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(meanfSalesFcast), lag=10, type="Ljung-Box")    # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(rwfwithdriftSalesFcast), lag=10, type="Ljung-Box")      # Not stationary or a whitenoise, p-value is less than 0.05

Box.test(residuals(naiveProductionFcast), lag=10, type="Ljung-Box")    # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(snaiveProductionFcast), lag=10, type="Ljung-Box")   # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(meanfProductionFcast), lag=10, type="Ljung-Box")    # Not stationary or a whitenoise, p-value is less than 0.05
Box.test(residuals(rwfwithdriftProductionFcast), lag=10, type="Ljung-Box")      # Not stationary or a whitenoise, p-value is less than 0.05

################################################################################

ARIMA_Sales_Train <- auto.arima(trainSales, stepwise=FALSE, approximation=FALSE)
ARIMA_Sales_Train
class(ARIMA_Sales_Train)

tsdisplay(trainSales)
ur.kpss(trainSales)%>%summary()
ARIMA_Sales_Train$aicc
checkresiduals(ARIMA_Sales_Train)
accuracy(ARIMA_Sales_Train)

ARIMA_Sales_Test <- auto.arima(testSales, stepwise=FALSE, approximation=FALSE)
ARIMA_Sales_Test
class(ARIMA_Sales_Test)

tsdisplay(testSales)
ur.kpss(testSales)%>%summary()
ARIMA_Sales_Test$aicc
checkresiduals(ARIMA_Sales_Test)
accuracy(ARIMA_Sales_Test)

FCastARIMA_Sales <- forecast(ARIMA_Sales_Test, h=36)
autoplot(FCastARIMA_Sales)
Fcast_Accuracy_Sales <- accuracy(FCastARIMA_Sales,h=36)
Fcast_Accuracy_Sales
checkresiduals(FCastARIMA_Sales)


ARIMA_Production_Train <- auto.arima(trainProduction, stepwise=FALSE, approximation=FALSE)
ARIMA_Production_Train
class(ARIMA_Production_Train)

tsdisplay(trainProduction)
ur.kpss(trainProduction)%>%summary()
ARIMA_Production_Train$aicc
checkresiduals(ARIMA_Production_Train)
accuracy(ARIMA_Production_Train)

ARIMA_Production_Test <- auto.arima(testProduction, stepwise=FALSE, approximation=FALSE)
ARIMA_Production_Test
class(ARIMA_Production_Test)

tsdisplay(testProduction)
ur.kpss(testProduction)%>%summary()
ARIMA_Production_Test$aicc
checkresiduals(ARIMA_Production_Test)
accuracy(ARIMA_Production_Test)

FCastARIMA_Production <- forecast(ARIMA_Production_Test, h=36)
autoplot(FCastARIMA_Production)
Fcast_Accuracy_Production <- accuracy(FCastARIMA_Production,h=24)
Fcast_Accuracy_Production
checkresiduals(FCastARIMA_Production)