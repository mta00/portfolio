rm(list=ls())
library(data.table)
library(ggplot2)
library(rugarch)
library("readxl")
library(urca)
library("fGarch")

my_data <- read_excel("HW2.xls")

log_sp<-log(my_data$SP500)
log_sp_diff<-diff(log_sp)
plot(log_sp_diff)

log_exchange<-log(my_data$Euro)
log_exchange_diff<-diff(log_exchange)
plot(log_exchange_diff)

#3

#standard specification
spec = ugarchspec() 
def.fit = ugarchfit(spec = spec, data = log_sp_diff)
print(def.fit)

spec=ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(5, 5))) 
def.fit1 = ugarchfit(spec = spec, data = log_sp_diff)
print(def.fit1)

spec = ugarchspec() 
def.fit2 = ugarchfit(spec = spec, data = log_exchange_diff)
print(def.fit2)

spec=ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(3, 3))) 
def.fit3 = ugarchfit(spec = spec, data = log_exchange_diff)
print(def.fit3)

#trying TGARCH
fit.garch = garchFit(~garch(2,2),data=log_sp_diff,trace=F,include.mean=F)
fit.garch

fit.tgarch = garchFit(~garch(2,2),delta=1,leverage=T,data=log_sp_diff,trace=F,include.mean=F)
fit.tgarch

fit.garch = garchFit(~garch(1,1),data=log_exchange_diff,trace=F,include.mean=F)
fit.garch

fit.tgarch = garchFit(~garch(1,1),delta=1,leverage=T,data=log_exchange_diff,trace=F,include.mean=F)
fit.tgarch

#trying GARCH-M
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 2), external.regressors = NULL),
                   mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = TRUE, archpow = 1),
                   fixed.pars = list(mu = 0))
def.fit4 = ugarchfit(spec = spec, data = log_sp_diff)
print(def.fit4)

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = NULL),
                   mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = TRUE, archpow = 1),
                   fixed.pars = list(mu = 0))
def.fit5 = ugarchfit(spec = spec, data = log_exchange_diff)
print(def.fit5)

#4

data <- read_excel("NLSY2000_HA.xls")
View(data)

#a

library("plm")
pool<-plm(EARNINGS~S+AGE+MALE+ETHBLACK+ETHHISP+TENURE,data=data,model='pooling')
summary(pool)

#c

rand<-plm(EARNINGS~S+AGE+MALE+ETHBLACK+ETHHISP+TENURE,data=data,model='random')
summary(rand)

#e

plmtest(pool, effect="twoways", type="bp")

pbsytest(EARNINGS~S+AGE+MALE+ETHBLACK+ETHHISP+TENURE,data=data, test="j")

#f

fe<-plm(EARNINGS~S+AGE+MALE+ETHBLACK+ETHHISP+TENURE,data=data,model='within')
summary(fe)

fd<-plm(EARNINGS~S+AGE+MALE+ETHBLACK+ETHHISP+TENURE,data=data,model='fd')
summary(fd)

#g

pwfdtest(EARNINGS~S+AGE+MALE+ETHBLACK+ETHHISP+TENURE, data=data)

#h

#Hausman test

phtest(fe, rand)

summary(rand)$ercomp


