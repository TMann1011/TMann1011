rm(list=ls())
setwd("C:\\Users\\tyler\\OneDrive\\Desktop\\Tyler stuff\\R Forecasting Methods")
getwd()

coaldata <- read.csv("Coal Power.csv", header=TRUE, stringsAsFactors=TRUE)
coaldata

tsdata <- ts(coaldata[,2], start=c(2001,1), end=c(2020,4), frequency=12)

class(tsdata)
str(tsdata)

mycoal.train <- window(tsdata, start=c(2001,1), end=c(2017,12))
mycoal.train

mycoal.test <- window(tsdata, start=c(2018,1), end=c(2020,4))
mycoal.test

autoplot(mycoal.train, series="Training")+ autolayer(mycoal.test, series="Test")+
  ggtitle('Texas Coal Consumption')+ ylab('Quarterly Short Tons')

mycoal.train <- window(tsdata, start=c(2001,1), end=c(2020,4))

meanfCoalForecast = meanf(mycoal.train, h=12)
naiveCoalForecast = naive(mycoal.train, h=12)
snaiveCoalForecast = snaive(mycoal.train, h=12)
rwfwithdriftCoalForecast = rwf(mycoal.train, drift=TRUE, h=12)

meanfCoalForecast
naiveCoalForecast
snaiveCoalForecast
rwfwithdriftCoalForecast

pdf('ForecastCoalPlots.pdf')
autoplot(meanfCoalForecast) + xlab('Time') + ylab('Short Tons')
autoplot(naiveCoalForecast) + xlab('Time') + ylab('Short Tons')
autoplot(snaiveCoalForecast) + xlab('Time') + ylab('Short Tons')
autoplot(rwfwithdriftCoalForecast) + xlab('Time') + ylab('Short Tons')

mycoal.test <- window(tsdata, start=c(2018,1), end=c(2020,4))

naiveCoalFcast <- accuracy(naiveCoalForecast, mycoal.test)
snaiveCoalFcast <- accuracy(snaiveCoalForecast, mycoal.test)
meanfCoalFcast <- accuracy(meanfCoalForecast, mycoal.test)
rwfwithdriftCoalFcast <- accuracy(rwfwithdriftCoalForecast, mycoal.test)


naiveCoalFcast
snaiveCoalFcast
meanfCoalFcast
rwfwithdriftCoalFcast


pdf('residualPlotsCoal.pdf')
checkresiduals(naiveCoalForecast)
checkresiduals(snaiveCoalForecast)
checkresiduals(meanfCoalForecast)
checkresiduals(rwfwithdriftCoalForecast)
dev.off()