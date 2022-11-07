rm(list=ls())
library(data.table)
library(urca)
library(ggplot2)

btc<-fread('BTC-USD.csv')
eth<-fread('ETH-USD.csv')
xmr<-fread('XMR-USD.csv')

btc_price<-as.numeric(btc$Close)
btc_price<-btc_price[!is.na(btc_price)]

eth_price<-as.numeric(eth$Close)
eth_price<-eth_price[!is.na(eth_price)]

xmr_price<-as.numeric(xmr$Close)
xmr_price<-xmr_price[!is.na(xmr_price)]

#let us first explore the graphs of the time series

plot(btc$Close)

plot(eth$Close)

plot(xmr$Close)

#none of these look stationary (apart from the volume of XMR, maybe). will carry out stationarity tests to prove it
#some appear to have a time trend, however, it is not steady all over the period

#dickey-fuller test
summary(ur.df(btc_price, "trend", 20, "BIC"))
summary(ur.df(btc_vol, "trend", 20, "BIC"))

summary(ur.df(eth_price, "trend", 20, "BIC"))
summary(ur.df(eth_vol, "trend", 20, "BIC"))

summary(ur.df(xmr_price, "trend", 20, "BIC"))
summary(ur.df(xmr_vol, "trend", 20, "BIC"))

#kpss test - tests the hypothesis that an observable time series is stationary around a deterministic trend

summary(ur.kpss(btc_price, "tau", "short"))
summary(ur.kpss(btc_vol, "tau", "short"))

summary(ur.kpss(eth_price, "tau", "short"))
summary(ur.kpss(eth_vol, "tau", "short"))

summary(ur.kpss(xmr_price, "tau", "short"))
summary(ur.kpss(xmr_vol, "tau", "short"))

summary(ur.kpss(btc_price, "tau", "long"))
summary(ur.kpss(btc_vol, "tau", "long"))

summary(ur.kpss(eth_price, "tau", "long"))
summary(ur.kpss(eth_vol, "tau", "long"))

summary(ur.kpss(xmr_price, "tau", "long"))
summary(ur.kpss(xmr_vol, "tau", "long"))

#pp test

summary(ur.pp(btc_price, "Z-tau", "trend", "short"))
summary(ur.pp(btc_vol, "Z-tau", "trend", "short"))

summary(ur.pp(eth_price, "Z-tau", "trend", "short"))
summary(ur.pp(eth_vol, "Z-tau", "trend", "short"))

summary(ur.pp(xmr_price, "Z-tau", "trend", "short"))
summary(ur.pp(xmr_vol, "Z-tau", "trend", "short"))

summary(ur.pp(btc_price, "Z-tau", "trend", "long"))
summary(ur.pp(btc_vol, "Z-tau", "trend", "long"))

summary(ur.pp(eth_price, "Z-tau", "trend", "long"))
summary(ur.pp(eth_vol, "Z-tau", "trend", "long"))

summary(ur.pp(xmr_price, "Z-tau", "trend", "long"))
summary(ur.pp(xmr_vol, "Z-tau", "trend", "long"))

#fitting several appropriate models
#ARMA

db<-diff(btc_price)
summary(ur.df(db, "trend", 20, "BIC")) #nst

summary(ur.kpss(db, "tau", "short")) #st
summary(ur.kpss(db, "tau", "long")) #st

summary(ur.pp(db, "Z-tau", "trend", "short")) #st
summary(ur.pp(db, "Z-tau", "trend", "long")) #st

de<-diff(eth_price)
summary(ur.df(de, "trend", 20, "BIC")) #nst

summary(ur.kpss(de, "tau", "short")) #reject at 5, st at 1
summary(ur.kpss(de, "tau", "long")) #reject at 5, st at 1

summary(ur.pp(de, "Z-tau", "trend", "short")) #st
summary(ur.pp(de, "Z-tau", "trend", "long")) #st

dx<-diff(xmr_price)
summary(ur.df(dx, "trend", 20, "BIC")) #nst

summary(ur.kpss(dx, "tau", "short")) #st
summary(ur.kpss(dx, "tau", "long")) #st

summary(ur.pp(dx, "Z-tau", "trend", "short")) #st
summary(ur.pp(dx, "Z-tau", "trend", "long")) #st

#so d=1

m <- arima(btc_price, c(0,1,0))
BIC(m)

m <- arima(btc_price, c(1,1,1))
BIC(m)

m <- arima(btc_price, c(2,1,2))
BIC(m)

m <- arima(btc_price, c(3,1,3))
BIC(m)

m <- arima(btc_price, c(4,1,4))
BIC(m)

m <- arima(btc_price, c(5,1,5))
BIC(m)

m <- arima(btc_price, c(1,1,0))
BIC(m)

m <- arima(btc_price, c(2,1,0))
BIC(m)

m <- arima(btc_price, c(3,1,0))
BIC(m)

m <- arima(btc_price, c(4,1,0))
BIC(m)

m <- arima(btc_price, c(5,1,0))
BIC(m)

m <- arima(btc_price, c(0,1,1))
BIC(m)

m <- arima(btc_price, c(0,1,2))
BIC(m)

m <- arima(btc_price, c(0,1,3))
BIC(m)

m <- arima(btc_price, c(0,1,4))
BIC(m)

m <- arima(btc_price, c(0,1,5))
BIC(m)

#eth

m <- arima(eth_price, c(0,1,0))
BIC(m)

m <- arima(eth_price, c(1,1,1))
BIC(m)

m <- arima(eth_price, c(2,1,2))
BIC(m)

m <- arima(eth_price, c(3,1,3))
BIC(m)

m <- arima(eth_price, c(4,1,4))
BIC(m)

m <- arima(eth_price, c(4,1,5))
BIC(m)

m <- arima(eth_price, c(5,1,4))
BIC(m)

m <- arima(eth_price, c(5,1,5))
BIC(m)

m <- arima(eth_price, c(1,1,0))
BIC(m)

m <- arima(eth_price, c(2,1,0))
BIC(m)

m <- arima(eth_price, c(3,1,0))
BIC(m)

m <- arima(eth_price, c(4,1,0))
BIC(m)

m <- arima(eth_price, c(5,1,0))
BIC(m)

m <- arima(eth_price, c(0,1,1))
BIC(m)

m <- arima(eth_price, c(0,1,2))
BIC(m)

m <- arima(eth_price, c(0,1,3))
BIC(m)

m <- arima(eth_price, c(0,1,4))
BIC(m)

m <- arima(eth_price, c(0,1,5))
BIC(m)

#xmr

m <- arima(xmr_price, c(0,1,0))
BIC(m)

m <- arima(xmr_price, c(1,1,1))
BIC(m)

m <- arima(xmr_price, c(2,1,2))
BIC(m)

m <- arima(xmr_price, c(3,1,3))
BIC(m)

m <- arima(xmr_price, c(4,1,4))
BIC(m)

m <- arima(xmr_price, c(5,1,5))
BIC(m)

m <- arima(xmr_price, c(4,1,5))
BIC(m)

m <- arima(xmr_price, c(5,1,4))
BIC(m)

m <- arima(xmr_price, c(1,1,0))
BIC(m)

m <- arima(xmr_price, c(2,1,0))
BIC(m)

m <- arima(xmr_price, c(3,1,0))
BIC(m)

m <- arima(xmr_price, c(4,1,0))
BIC(m)

m <- arima(xmr_price, c(5,1,0))
BIC(m)

m <- arima(xmr_price, c(0,1,1))
BIC(m)

m <- arima(xmr_price, c(0,1,2))
BIC(m)

m <- arima(xmr_price, c(0,1,3))
BIC(m)

m <- arima(xmr_price, c(0,1,4))
BIC(m)

m <- arima(xmr_price, c(0,1,5))
BIC(m)

#we can also observe from the plots that variance has definitely changed over time, so it might be of interest to 
#construct a model capturing changes in variance, e.g., GARCH

library(rugarch)

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,2)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(3,3)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(4,4)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(5,5)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

###

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(3,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(4,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(5,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, btc_price)
model

###
###

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,2)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(3,3)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(4,4)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(5,5)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

###

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(3,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(4,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(5,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, eth_price)
model

###
###

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,2)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(3,3)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(4,4)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_pricee)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(5,5)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

###

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(3,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(4,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(5,0)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(2,1)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, xmr_price)
model

###
#tgarch

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, btc_price)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, btc_price)
model2



spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, eth_price)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, eth_price)
model2



spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, xmr_price)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,0)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, xmr_price)
model2

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(2,0)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, xmr_price)
model2

#structural breaks

library(strucchange)
library(sandwich)
library(lubridate)

#HAC variance was not used due to long computations

Fs <-Fstats(btc_price~1, from = 1, to = NULL, data = btc, vcov. = NULL)
sctest(Fs)
breakpoints(Fs)

Fs <-Fstats(eth_price~1, from = 1, to = NULL, data = eth, vcov. = NULL)
sctest(Fs)
breakpoints(Fs)

Fs <-Fstats(xmr_price~1, from = 1, to = NULL, data = xmr, vcov. = NULL)
sctest(Fs)
breakpoints(Fs)


Fs <-Fstats(btc_price~1, from = 500, to = 2000, data = btc, vcov. = NULL)
sctest(Fs)

Fs <-Fstats(btc_price~1, from = 2000, to = 2288, data = btc, vcov. = NULL)
sctest(Fs)

Fs <-Fstats(eth_price~1, from = 500, to = 2000, data = eth, vcov. = NULL)
sctest(Fs)

Fs <-Fstats(eth_price~1, from = 2000, to = 2288, data = eth, vcov. = NULL)
sctest(Fs)



Fs <-Fstats(xmr_price~1, from = 500, to = 1500, data = xmr, vcov. = NULL)
sctest(Fs)

Fs <-Fstats(xmr_price~1, from = 2000, to = 2288, data = xmr, vcov. = NULL)
sctest(Fs)


#forecast

btc_train<-btc_price[1:1964]
btc_test<-btc_price[1964:2288]

eth_train<-eth_price[1:2002]
eth_test<-eth_price[2002:2288]

xmr_train<-xmr_price[1:2010]
xmr_test<-xmr_price[2010:2288]

library(forecast)
library(MLmetrics)
#Create samples
train_btc<-btc_price[1:1964]
mean(train_btc)
act_btc<-btc_price[1965]
act_btc
naive_btc<-btc_price[1964]
naive_btc
db<-diff(btc_price)
drift_btc<-btc_price[1964]+1/(length(btc_price)-1)*sum(db)
drift_btc

train_eth<-eth_price[1:2002]
mean(train_eth)
act_eth<-eth_price[2003]
act_eth
naive_eth<-eth_price[2002]
naive_eth
de<-diff(eth_price)
drift_eth<-eth_price[2002]+1/(length(eth_price)-1)*sum(de)
drift_eth

train_xmr<-xmr_price[1:2010]
mean(train_xmr)
act_xmr<-xmr_price[2011]
act_xmr
naive_xmr<-xmr_price[2010]
naive_xmr
dx<-diff(xmr_price)
drift_xmr<-xmr_price[2010]+1/(length(xmr_price)-1)*sum(dx)
drift_xmr

#cointegration
library(aTSA)
coint.test(btc_price,eth_price)
coint.test(btc_price,xmr_price)
coint.test(xmr_price,eth_price)

#granger causality
library(lmtest)

grangertest(btc_price ~ eth_price, order = 1)
grangertest(btc_price ~ xmr_price, order = 1)

grangertest(eth_price ~ btc_price, order = 1)
grangertest(eth_price ~ xmr_price, order = 1)

grangertest(xmr_price ~ btc_price, order = 1)
grangertest(xmr_price ~ eth_price, order = 1)


