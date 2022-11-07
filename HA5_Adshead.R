library(data.table)
library(zoo)
library(PerformanceAnalytics)
library(readxl)

#Problem 3

#load the datasets for each year
data0<-fread("Downloads/Downloads/Download Data - INDEX_US_S&P US_SPX.csv")
data1<-fread("Downloads/Downloads/Download Data - INDEX_US_S&P US_SPX (1).csv")
data2<-fread("Downloads/Downloads/Download Data - INDEX_US_S&P US_SPX (2).csv")
data3<-fread("Downloads/Downloads/Download Data - INDEX_US_S&P US_SPX (3).csv")
data4<-fread("Downloads/Downloads/Download Data - INDEX_US_S&P US_SPX (4).csv")
data5<-fread("Downloads/Downloads/Download Data - INDEX_US_S&P US_SPX (5).csv")

#bind them into one
data<-rbind(data0,data1,data2,data3,data4,data5)

#function that will help us transform the strings for daily Close prices into numeric 
transform<-function(x){
  x<-gsub(',', '', x)
  return(x)
}

#get the list of prices in the numeric format
close<-as.numeric(lapply(data$Close,transform))

#calculate logarithmic returns 
r<-diff(log(close))

#Risk metrics
lambda<-0.94 #since we have daily data
sigma0<-var(head(r,500))
sigmas<-list()

#calculate variances by formula sigma[i]=lambda*sigma[i-1]+(1-lambda)*r^2[i]
for (i in 1:(length(r)-1)){
  sigmas[i]<-lambda*unlist(sigma0)+(1-lambda)*(r[i])^2
  sigma0<-sigmas[i]
}

sig<-var(head(r,500))
#obtain a complete list of variances
s<-c(sig,sigmas)

#get the volatility as the square root
s<-sqrt(unlist(s))

#inverse standard normal for 1% and 5% (acquired from an online calculator)
inv1<--2.3263
inv5<--1.6449

#calculate 1% VaR assuming normal distribution 
rm_var1<--s*inv1

#calculate 5% VaR assuming normal distribution 
rm_var5<--s*inv5

#create a data table for convenience
dt<-data.table(
  returns=r,
  VaR1RM=rm_var1,
  VaR5RM=rm_var5
)

#constructing Hit sequences for both 1% and 5% VaR
dt[,hit1rm:=fifelse(returns < -VaR1RM,1,0)]
dt[,hit5rm:=fifelse(returns < -VaR5RM,1,0)]

#Historical simulation
var1hs<-list()
var5hs<-list()

for (i in 0:(length(r)-1-500)){
  var<- -quantile(r[(i+1):(500+i)],0.01)
  var1hs<-append(var1hs,var[[1]])
}

for (i in 0:(length(r)-1-500)){
  var<- -quantile(r[(i+1):(500+i)],0.05)
  var5hs<-append(var5hs,var[[1]])
}

zeros<-as.list(rep(0, 500))

varhs5<-c(zeros,unlist(var5hs))
varhs1<-c(zeros,unlist(var1hs))

#append our values to the datatable (first 500 values of our dataset are 0)
dt[,VaR1HS:=unlist(varhs1)]
dt[,VaR5HS:=unlist(varhs5)]

#constructing Hit sequences for both 1% and 5% VaR
dt[,hit1hs:=fifelse(returns < -VaR1HS,1,0)]
dt[,hit5hs:=fifelse(returns < -VaR5HS,1,0)]

#Risk Metrics

#VaR 1%
t1<-sum(dt$hit1rm)
t0<-nrow(dt)-t1

#t-statistic and p-value
tst<-(t1/(t1+t0)-0.01)/sqrt(0.01*(1-0.01)/(t1+t0))
pv<-1-pnorm(tst, mean = 0, sd = 1, lower.tail = TRUE)

#UC test
phat<-t1/(t1+t0)
likp<-phat^t1*(1-phat)^t0
lika<-0.01^t1*(1-0.01)^t0

lr<- -2*log(lika/likp)

#independence test
t00<-0
t01<-0
t11<-0
t10<-0

for (i in 2:length(dt$hit1rm)){
  if ((dt$hit1rm[i]==1)&(dt$hit1rm[i-1]==1)){
    t11<-t11+1
  }
  if ((dt$hit1rm[i]==1)&(dt$hit1rm[i-1]==0)){
    t10<-t10+1
  }
  if ((dt$hit1rm[i]==0)&(dt$hit1rm[i-1]==1)){
    t01<-t01+1
  }
  if ((dt$hit1rm[i]==0)&(dt$hit1rm[i-1]==0)){
    t00<-t00+1
  }
}

p01<-t01/(t00+t01)
p00<-1-p01
p11<-t11/(t11+t10)
p10<-1-p11

lur<-p00^t00*p01^t01*p10^t10*p11^t11
lrind<- -2*log(likp/lur)

#CC test
lrcc<- -2*log(lika/lur)

#VaR 5%
t1<-sum(dt$hit5rm)
t0<-nrow(dt)-t1

#t-statistic and p-value
tst<-(t1/(t1+t0)-0.05)/sqrt(0.05*(1-0.05)/(t1+t0))
pv<-1-pnorm(tst, mean = 0, sd = 1, lower.tail = TRUE)

#UC test
phat<-t1/(t1+t0)
likp<-phat^t1*(1-phat)^t0
lika<-0.05^t1*(1-0.05)^t0

lr<- -2*log(lika/likp)

#independence test
t00<-0
t01<-0
t11<-0
t10<-0

for (i in 2:length(dt$hit5rm)){
  if ((dt$hit5rm[i]==1)&(dt$hit5rm[i-1]==1)){
    t11<-t11+1
  }
  if ((dt$hit5rm[i]==1)&(dt$hit5rm[i-1]==0)){
    t10<-t10+1
  }
  if ((dt$hit5rm[i]==0)&(dt$hit5rm[i-1]==1)){
    t01<-t01+1
  }
  if ((dt$hit5rm[i]==0)&(dt$hit5rm[i-1]==0)){
    t00<-t00+1
  }
}

p01<-t01/(t00+t01)
p00<-1-p01
p11<-t11/(t11+t10)
p10<-1-p11

lur<-p00^t00*p01^t01*p10^t10*p11^t11
lrind<- -2*log(likp/lur)

#CC test
lrcc<- -2*log(lika/lur)

#Historical Simulation

dths<-dt[501:nrow(dt),]

#VaR 1%
t1<-sum(dths$hit1hs)
t0<-nrow(dths)-t1

#t-statistic and p-value
tst<-(t1/(t1+t0)-0.01)/sqrt(0.01*(1-0.01)/(t1+t0))
pv<-1-pnorm(tst, mean = 0, sd = 1, lower.tail = FALSE)

#UC test
phat<-t1/(t1+t0)
likp<-phat^t1*(1-phat)^t0
lika<-0.01^t1*(1-0.01)^t0

lr<- -2*log(lika/likp)

#independence test
t00<-0
t01<-0
t11<-0
t10<-0

for (i in 2:length(dths$hit1hs)){
  if ((dths$hit1hs[i]==1)&(dths$hit1hs[i-1]==1)){
    t11<-t11+1
  }
  if ((dths$hit1hs[i]==1)&(dths$hit1hs[i-1]==0)){
    t10<-t10+1
  }
  if ((dths$hit1hs[i]==0)&(dths$hit1hs[i-1]==1)){
    t01<-t01+1
  }
  if ((dths$hit1hs[i]==0)&(dths$hit1hs[i-1]==0)){
    t00<-t00+1
  }
}

p01<-t01/(t00+t01)
p00<-1-p01
p11<-t11/(t11+t10)
p10<-1-p11

lur<-p00^t00*p01^t01*p10^t10*p11^t11
lrind<- -2*log(likp/lur)

#CC test
lrcc<- -2*log(lika/lur)

#VaR 5%
t1<-sum(dths$hit5hs)
t0<-nrow(dths)-t1

#t-statistic and p-value
tst<-(t1/(t1+t0)-0.05)/sqrt(0.05*(1-0.05)/(t1+t0))
pv<-1-pnorm(tst, mean = 0, sd = 1, lower.tail = F)

#UC test
phat<-t1/(t1+t0)
likp<-phat^t1*(1-phat)^t0
lika<-0.05^t1*(1-0.05)^t0

lr<- -2*log(lika/likp)

#independence test
t00<-0
t01<-0
t11<-0
t10<-0

for (i in 2:length(dths$hit5hs)){
  if ((dths$hit5hs[i]==1)&(dths$hit5hs[i-1]==1)){
    t11<-t11+1
  }
  if ((dths$hit5hs[i]==1)&(dths$hit5hs[i-1]==0)){
    t10<-t10+1
  }
  if ((dths$hit5hs[i]==0)&(dths$hit5hs[i-1]==1)){
    t01<-t01+1
  }
  if ((dths$hit5hs[i]==0)&(dths$hit5hs[i-1]==0)){
    t00<-t00+1
  }
}

p01<-t01/(t00+t01)
p00<-1-p01
p11<-t11/(t11+t10)
p10<-1-p11

lur<-p00^t00*p01^t01*p10^t10*p11^t11
lrind<- -2*log(likp/lur)

#CC test
lrcc<- -2*log(lika/lur)

#Problem 4

goog<-fread("Downloads/Downloads/Google.csv")
View(goog)

#function that will help us transform the strings into numeric 
transform<-function(x){
  x<-gsub(',', '.', x)
  return(x)
}

goog[,`log-return`:=as.numeric(lapply(`log-return`,transform))]
goog[,Variance:=as.numeric(lapply(Variance,transform))]

goog<-na.omit(goog)

#a

#transform into standard normal
goog[,rnew:=(`log-return`-mean(`log-return`))/Variance]

#obtain uniform
y <- pnorm(goog$rnew)
hist(y)

#b

x<-qnorm(y)
hist(x)

#dropping infinite values
x[!is.finite(x)] <- NA
x<-na.omit(x)

mean(x)
sd(x)
skewness(x)
kurtosis(x)

#c

a<-y[y<=0.1]
a<-a*10
hist(a)

#d

d<-qnorm(a)
hist(d)

#dropping infinite values
d[!is.finite(d)] <- NA
d<-na.omit(d)

mean(d)
sd(d)
skewness(d)
kurtosis(d)

#BONUS TASK
#Problem 5

hf <- read_excel("Downloads/Downloads/HF_data.xlsx")

#a
l<-list()

#go through all unique values of Hour; inside each hour go through each unique value of Minute;
#calculate return from the first and the last seconds for each minute
for (i in 1:length(unique(hf$Hour))){
  d<-subset(hf,Hour==unique(hf$Hour)[i])
  for (j in 1:length(unique(d$Minute))){
    q<-subset(d,Minute==unique(d$Minute)[j])
    r<-log(q$PRICE[nrow(q)]/q$PRICE[1])
    l<-c(l,r)
  }
}

plot(unlist(l))

#b


