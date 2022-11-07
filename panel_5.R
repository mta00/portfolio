#Problem 5

#sources:
#https://www.rdocumentation.org/packages/stats/versions/3.3.1/topics/acf

#a

#load the package and read the file
library("readxl")
library(urca)
my_data <- read_excel("HW2.xls")

#calculating the logarithmic exchange rates and their differences
log_exchange<-log(my_data$Euro)
log_exchange_diff<-diff(log_exchange)

#first 50 autocorrelations for log exchange rates
acf_log<-acf(log_exchange, type = c("correlation"), lag.max = 50, plot = FALSE)
acf_log<-acf_log[1:50]
acf_log
#    1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19 
#0.999 0.999 0.998 0.997 0.996 0.995 0.995 0.994 0.993 0.992 0.991 0.991 0.990 0.989 0.988 0.987 0.986 0.986 0.985 
#20    21    22    23    24    25    26    27    28    29    30    31    32    33    34    35    36    37    38 
#0.984 0.983 0.982 0.981 0.980 0.979 0.978 0.977 0.976 0.975 0.974 0.973 0.972 0.971 0.970 0.969 0.968 0.967 0.967 
#39    40    41    42    43    44    45    46    47    48    49    50 
#0.966 0.965 0.964 0.963 0.962 0.961 0.960 0.959 0.958 0.958 0.957 0.956 

#first 50 autocorrelations for log exchange rates differences
acf_log_diff<-acf(log_exchange_diff, type = c("correlation"), lag.max = 50, plot = FALSE)
acf_log_diff<-acf_log_diff[1:50]
acf_log_diff
#1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17 
#0.072 -0.012 -0.014  0.013  0.011 -0.012  0.038 -0.007 -0.022 -0.005 -0.017  0.035  0.012  0.003  0.031 -0.018  0.033 
#18     19     20     21     22     23     24     25     26     27     28     29     30     31     32     33     34 
#0.001  0.038 -0.002 -0.007 -0.004 -0.001 -0.013  0.000  0.018  0.003  0.002  0.016 -0.030  0.038 -0.026 -0.007  0.004 
#35     36     37     38     39     40     41     42     43     44     45     46     47     48     49     50 
#-0.013 -0.013 -0.023  0.001 -0.012  0.007 -0.009 -0.004  0.025 -0.002 -0.015 -0.003  0.005 -0.001 -0.020  0.019 

#In case of log exchange we can expect the data to be serially correlated, e.g., the values of the exchange rate in 
#the current period are very likely to be highly correlated with the past ones, especially the more recent ones.
#Moreover, this is in line with the autocorrelations printed above, which are very high. The test is also robust
#in presence of heteroskedasticity: judging by the plot, this might be the case as well.

#The test that allows serial correlation is the Phillips-Perron test, 
#Below I conduct the test. I decided to include the trend in the specification since I suspect the presence of it
#after plotting the graph.
#H_0: the time series is integrated of order 1
#H_1: the time series is stationary
plot(log_exchange)
summary(ur.pp(log_exchange, "Z-tau", "trend", "short"))
summary(ur.pp(log_exchange, "Z-tau", "trend", "long"))
#For both short and long lag structure the Z statistic is smaller than critical values by absolute value, e.g.
#we fail to reject the null. The process has a unit root, e.g., is integrated of order 1.

#Judjing by the plot of the return, I do not see signs of autocorrelation or heteroskedasticity. 
#KPSS test is more meant to test trend stationarity rather than just stationarity. PP test on one hand deals with
#autocorrelation and heteroskedasticity, but if these are not present, the test may be less precise than the DF test
#all else equal. So, let us conduct the DF test here.
plot(log_exchange_diff)
#H_0: rho=1 (the process is integrated of order 1)
#H_1: rho<1 (the process is stationary)
summary(ur.df(log_exchange_diff, "none", 50, "BIC"))
#Did not include a trend or a drift because presence of these is not obvious from the graph. Used BIC because this 
#information criteria chooses asymptotically correct model and is good on large samples.
#Test statistics is larger by absolute value than the critical value at 1%, hence reject the null. The returns 
#are stationary.

#Ljung-Box test
#H_0: autocorrelations up to lag k are small, the model provides a good fit
#H_1: autocorrelations up to lag k are significant, the model does not provide a good fit
Box.test(log_exchange_diff, lag = 20, type = "Ljung-Box")
#p-value=0.0005134, hence we reject the null at 1% SL
Box.test(log_exchange_diff, lag = 40, type = "Ljung-Box")
#p-value=0.007527, hence we reject the null at 1% SL
Box.test(log_exchange_diff, lag = 60, type = "Ljung-Box")
#p-value=0.03494, e.g. the null is not rejected at 1% SL.
#Conclusion: with a large number of lags the model can provide a relatively good fit (relatively because in the
#last case we caj still reject the null at 5%)

library(forecast)
Acf(log_exchange, plot = TRUE)
Pacf(log_exchange, plot = TRUE)

m <- arima(log_exchange, c(10,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange, c(15,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange, c(5,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange, c(1,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange, c(4,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange, c(3,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange, c(2,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange, c(1,0,0))
AIC(m)
BIC(m)

#b
log_sp<-log(my_data$SP500)

Acf(log_sp, plot = TRUE)
Pacf(log_sp, plot = TRUE)

m <- arima(log_sp, c(1,0,0))
AIC(m)
BIC(m)

m <- arima(log_sp, c(2,0,0))
AIC(m)
BIC(m)

m <- arima(log_sp, c(3,0,0))
AIC(m)
BIC(m)

#c

log_sp_diff<-diff(log_sp)
log_sp_diff_abs<-abs(log_sp_diff)


Acf(log_sp_diff_abs, plot = TRUE)
Pacf(log_sp_diff_abs, plot = TRUE)

m <- arima(log_sp_diff_abs, c(1,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(2,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(3,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(4,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(5,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(6,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(7,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(8,0,0))
AIC(m)
BIC(m)


m <- arima(log_sp_diff_abs, c(9,0,0))
AIC(m)
BIC(m)


log_exchange_diff_abs<-abs(log_exchange_diff)

Acf(log_exchange_diff_abs, plot = TRUE)
Pacf(log_exchange_diff_abs, plot = TRUE)

m <- arima(log_exchange_diff_abs, c(1,0,0))
AIC(m)
BIC(m)


m <- arima(log_exchange_diff_abs, c(2,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(3,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(4,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(5,0,0))
AIC(m)
BIC(m)


m <- arima(log_exchange_diff_abs, c(6,0,0))
AIC(m)
BIC(m)


m <- arima(log_exchange_diff_abs, c(7,0,0))
AIC(m)
BIC(m)


m <- arima(log_exchange_diff_abs, c(8,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(9,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(10,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(11,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(12,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(13,0,0))
AIC(m)
BIC(m)

m <- arima(log_exchange_diff_abs, c(14,0,0))
AIC(m)
BIC(m)


m <- arima(log_exchange_diff_abs, c(15,0,0))
AIC(m)
BIC(m)

#d

plot(log_sp)
plot(log_exchange)

#source: https://cran.r-project.org/web/packages/egcm/egcm.pdf
library(egcm)
egcm(log_sp,log_exchange)

#e

#source: https://www.statology.org/granger-causality-test-in-r/
library(lmtest)

grangertest(log_sp_diff ~ log_exchange_diff, order = 1)
grangertest(log_exchange_diff ~ log_sp_diff, order = 1)

#f

library(strucchange)
library(sandwich)
library(tibble)

dat <- tibble(ylag0 = log_exchange,
              ylag1 = lag(log_exchange)
) 


qlr <- Fstats(ylag0 ~ ylag1, data = dat)

breakpoints(qlr)

sctest(qlr, type = "supF")

dat <- tibble(ylag0 = log_sp_diff,
              ylag1 = lag(log_sp_diff)
) 


qlr <- Fstats(ylag0 ~ ylag1, data = dat)

breakpoints(qlr)

sctest(qlr, type = "supF")

