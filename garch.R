install.packages("AER")
library(AER)
data("NYSESW")
y <- diff(log(NYSESW[-3500:-1]))*100

install.packages("rugarch")
library(rugarch)

spec <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE))
model <- ugarchfit(spec, y)
model

spec1 <- ugarchspec(list(model="sGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model1 <- ugarchfit(spec1, y)
model1

spec2 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE), "sstd")
model2 <- ugarchfit(spec2, y)
model2

spec3 <- ugarchspec(list(model="gjrGARCH", garchOrder=c(1,1)), list(armaOrder=c(0,0), include.mean = TRUE, archm=TRUE), "sstd")
model3 <- ugarchfit(spec3, y)
model3
