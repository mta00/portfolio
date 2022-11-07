#load the libraries
library(data.table)
library(PerformanceAnalytics)
library(tseries)
library(TSA)
library(rugarch)

#1

#load the datasets
dt1<-fread("Downloads/Downloads/PFE.csv")
dt2<-fread("Downloads/Downloads/PFE (1).csv")

#extract the Close column
close1<-dt1$Close #30.01.2015-30.01.2020
close2<-dt2$Close #30.01.2015-30.01.2019

#constructing log-returns

#take the log
logr1<-log(r1)
logr2<-log(r2)
#then construct returns
r1<-diff(close1)
r2<-diff(close2)

#the above operation produced some NaNs and infinity values. let's get rid of them
logr1[!is.finite(logr1)] <- NA
logr2[!is.finite(logr2)] <- NA
logr1<-na.omit(logr1)
logr2<-na.omit(logr2)

#descriptive statistics for the given data
#mean
m1<-mean(logr1)
m1

m2<-mean(logr2)
m2

#st dev
sd1<-sd(logr1)
sd1

sd2<-sd(logr2)
sd2

#quartiles
quantile(logr1, prob=c(.25,.5,.75,1))
quantile(logr2, prob=c(.25,.5,.75,1))

#skewness
skewness(logr1,method="sample")
skewness(logr2,method="sample")

#kurtosis
kurtosis(logr1,method="sample")
kurtosis(logr2,method="sample")

#Jarque-Bera
jarque.bera.test(logr1)
jarque.bera.test(logr1)[1] #statistic

jarque.bera.test(logr2)
jarque.bera.test(logr2)[1] #statistic

#histograms
hist(logr1)
hist(logr2)

#2

test<-McLeod.Li.test(y=logr1)
test

m <- arima(logr1, c(0,1,0))
BIC(m)

m <- arima(logr1, c(1,0,0))
BIC(m)

m <- arima(logr1, c(0,0,1))
BIC(m)

m <- arima(logr1, c(2,0,0))
BIC(m)

m <- arima(logr1, c(2,0,1))
BIC(m)

m <- arima(logr1, c(2,1,1)) #
BIC(m)

m <- arima(logr1, c(2,1,2))
BIC(m)

m <- arima(logr1, c(3,1,1))
BIC(m)

m <- arima(logr1, c(2,2,1))
BIC(m)

m <- arima(logr1, c(4,1,1))
BIC(m) #no improvement have been achieved since then

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(2,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(0,1)), list(armaOrder=c(2,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model 

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,0)), list(armaOrder=c(2,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(0,1)), list(armaOrder=c(1,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model #

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(0,1)), list(armaOrder=c(1,0), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(0,1)), list(armaOrder=c(0,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model


spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(1,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,0)), list(armaOrder=c(1,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,0)), list(armaOrder=c(1,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model
spec <- ugarchspec(list(model="sGARCH", garchOrder=c(0,2)), list(armaOrder=c(1,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model
spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,1)), list(armaOrder=c(1,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model
spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,2)), list(armaOrder=c(1,1), include.mean = TRUE))
model <- ugarchfit(spec, logr1)
model

#asymmetric GARCH, for instance TGARCH

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2 

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(0,1)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(1,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,1), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(1,1), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2 #

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(2,1), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(1,2), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(2,2), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2



spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(2,0)), list(armaOrder=c(1,1), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(0,2)), list(armaOrder=c(1,1), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,2)), list(armaOrder=c(1,1), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(2,1)), list(armaOrder=c(1,1), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, logr1)
model2
