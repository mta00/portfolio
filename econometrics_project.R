rm(list=ls())
library(data.table)

#Define where the data is goint to be stored:
dir_data<-"/Users/masha/Downloads/archive"
setwd(dir_data)
data1<-fread("dataset-of-60s.csv")
data2<-fread("dataset-of-70s.csv")
data3<-fread("dataset-of-80s.csv")
data4<-fread("dataset-of-90s.csv")
data5<-fread("dataset-of-00s.csv")
data6<-fread("dataset-of-10s.csv")

#data1[,six:=1]
data1[,sev:=0]
data1[,eig:=0]
data1[,nin:=0]
data1[,zer:=0]
data1[,ten:=0]

#data2[,six:=0]
data2[,sev:=1]
data2[,eig:=0]
data2[,nin:=0]
data2[,zer:=0]
data2[,ten:=0]

#data3[,six:=0]
data3[,sev:=0]
data3[,eig:=1]
data3[,nin:=0]
data3[,zer:=0]
data3[,ten:=0]

#data4[,six:=0]
data4[,sev:=0]
data4[,eig:=0]
data4[,nin:=1]
data4[,zer:=0]
data4[,ten:=0]

#data5[,six:=0]
data5[,sev:=0]
data5[,eig:=0]
data5[,nin:=0]
data5[,zer:=1]
data5[,ten:=0]

#data6[,six:=0]
data6[,sev:=0]
data6[,eig:=0]
data6[,nin:=0]
data6[,zer:=0]
data6[,ten:=1]

data<-rbind(data1,data2,data3,data4,data5,data6)

data_numeric<-data[,-c('track','artist','uri')] #change to data_numeric
data_numeric.cor<-cor(data_numeric)
View(data_numeric.cor)

library(aod)
#library(ggplot2)

summary(data_numeric)
summary(data_numeric,sd)

mylogit <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit)

mylogit_ex_duration <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit_ex_duration)

mylogit_ex_sections <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit_ex_sections)

mylogit_ex_duration_sections <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+chorus_hit+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit_ex_duration_sections)

mylogit_onlyconst<-glm(target ~ 1, data = data_numeric, family = "binomial")
summary(mylogit_onlyconst)
logLik(mylogit_onlyconst)


logLik(mylogit)
logLik(mylogit_ex_duration)
logLik(mylogit_ex_sections)
logLik(mylogit_ex_duration_sections)

#probit

myprobit <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, 
                data = data_numeric, family = binomial(link = "probit") )

myprobit0 <- glm(target ~ target, 
                data = data_numeric, family = binomial(link = "probit") )

summary(myprobit0)
logLik(myprobit0)

myprobit_exdur <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, 
                data = data_numeric, family = binomial(link = "probit") )
  
summary(myprobit_exdur)

myprobit_exsec <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sev+eig+nin+zer+ten, 
                data = data_numeric, family = binomial(link = "probit") )
summary(myprobit_exsec)

myprobit_exsecanddur <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sev+eig+nin+zer+ten, 
                      data = data_numeric, family = binomial(link = "probit") )
summary(myprobit_exsecanddur)

## model summary
summary(myprobit)

logLik(myprobit)

#LPM
library(AER)

lpm<-lm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data=data_numeric)
coeftest(lpm, vcov. = vcovHC, type = "HC1")
summary(lpm)

lpm_exdur<-lm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data=data_numeric)
coeftest(lpm_exdur, vcov. = vcovHC, type = "HC1")
summary(lpm_exdur)

lpm_exval<-lm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+duration_ms+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data=data_numeric)
coeftest(lpm_exval, vcov. = vcovHC, type = "HC1")
summary(lpm_exval)

lpm_exsec<-lm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sev+eig+nin+zer+ten, data=data_numeric)
coeftest(lpm_exsec, vcov. = vcovHC, type = "HC1")
summary(lpm_exsec)

lpm_exdursecval<-lm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+chorus_hit+sev+eig+nin+zer+ten, data=data_numeric)
coeftest(lpm_exdursecval, vcov. = vcovHC, type = "HC1")
summary(lpm_exdursecval)

plot(residuals(lpm_exdursecval))
plot(residuals(mylogit_ex_duration))
plot(residuals(myprobit_exdur))

data_numeric[,lpm_prob:=predict.glm(lpm_exdursecval,data_numeric)]
data_numeric[,lpm_pred:=fifelse(lpm_prob>0.5,1,0)]
data_numeric[,lpm_error:=fifelse(target!=lpm_pred,1,0)]

data_numeric[,logit_prob:=predict.glm(mylogit_ex_duration,data_numeric)]
data_numeric[,logit_pred:=fifelse(logit_prob>0.5,1,0)]
data_numeric[,logit_error:=fifelse(target!=logit_pred,1,0)]

data_numeric[,probit_prob:=predict.glm(myprobit_exdur,data_numeric)]
data_numeric[,probit_pred:=fifelse(probit_prob>0.5,1,0)]
data_numeric[,probit_error:=fifelse(target!=probit_pred,1,0)]

lpm_error_rate<-sum(data_numeric$lpm_error)/nrow(data_numeric)
logit_error_rate<-sum(data_numeric$logit_error)/nrow(data_numeric)
probit_error_rate<-sum(data_numeric$probit_error)/nrow(data_numeric)

#library(hexbin)
#hexbin(residuals(lpm_exdursecval))
#plot(hexbin(residuals(lpm_exdursecval),xbins=10))


lpm_error_rate
logit_error_rate
probit_error_rate

View(data_numeric)

#white_aux<-lm(residuals(lpm_exdursecval) ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+chorus_hit+sev+eig+nin+zer+ten+danceability^2+energy^2+loudness^2+speechiness^2+acousticness^2+instrumentalness^2+liveness^2+tempo^2+time_signature^2+chorus_hit^2+danceability*energy+danceability*loudness+danceability*mode+danceability*speechiness+danceability*acousticness+danceability*instrumentalness+danceability*liveness+danceability*tempo+danceability*time_signature+danceability*, data=data_numeric)
coeftest(lpm, vcov. = vcovHC, type = "HC1")
summary(lpm)
library(writexl)
write_xlsx(data,"/Users/masha/Downloads/file name.xlsx")
white_aux<-lm(residuals(lpm_exdursecval) ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+chorus_hit+sev+eig+nin+zer+ten+danceability^2+energy^2+loudness^2+speechiness^2+acousticness^2+instrumentalness^2+liveness^2+tempo^2+time_signature^2+chorus_hit^2, data=data_numeric)
summary(white_aux)