rm(list=ls())
#load all required libraries
library(data.table)
library(aod)
library(AER)
library(readxl)

#load data (please change the path to yours)
data <- read_excel("/Users/masha/Downloads/Spotify_Hit_Predictor_Dataset.xlsx")
data<-as.data.table(data)

#include only columns with numeric data
data_numeric<-data[,-c('track','artist','uri')] 

#create the correlation matrix
data_numeric.cor<-cor(data_numeric)
View(data_numeric.cor)

#Linear Probability Model (LPM)
lpm<-lm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data=data_numeric)
summary(lpm)

lpm_ex_dur_sec_val<-lm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+chorus_hit+sev+eig+nin+zer+ten, data=data_numeric)
summary(lpm_ex_dur_sec_val)

#Logit
mylogit <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit)
logLik(mylogit)

mylogit_onlyconst<-glm(target ~ 1, data = data_numeric, family = "binomial")
summary(mylogit_onlyconst)
logLik(mylogit_onlyconst)

mylogit_ex_duration <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit_ex_duration)
logLik(mylogit_ex_duration)

mylogit_ex_sections <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit_ex_sections)
logLik(mylogit_ex_sections)

mylogit_ex_duration_sections <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+chorus_hit+sev+eig+nin+zer+ten, data = data_numeric, family = "binomial")
summary(mylogit_ex_duration_sections)
logLik(mylogit_ex_duration_sections)

#Probit

myprobit <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, 
                data = data_numeric, family = binomial(link = "probit") )
summary(myprobit)
logLik(myprobit)

myprobit <- glm(target ~ 1, 
                 data = data_numeric, family = binomial(link = "probit") )
summary(myprobit0)
logLik(myprobit0)

myprobit_ex_duration <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+chorus_hit+sections+sev+eig+nin+zer+ten, 
                      data = data_numeric, family = binomial(link = "probit") )
summary(myprobit_ex_duration)
logLik(myprobit_ex_duration)

myprobit_ex_sections <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sev+eig+nin+zer+ten, 
                      data = data_numeric, family = binomial(link = "probit") )
summary(myprobit_ex_sections)
logLik(myprobit_ex_sections)

myprobit_ex_secdur <- glm(target ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit+sev+eig+nin+zer+ten, 
                            data = data_numeric, family = binomial(link = "probit") )
summary(myprobit_ex_secdur)
logLik(myprobit_ex_secdur)

#Residuals plots
plot(residuals(lpm_ex_dur_sec_val))
plot(residuals(mylogit_ex_duration))
plot(residuals(myprobit_ex_duration))

white_aux<-lm(residuals(lpm_ex_dur_sec_val) ~ danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+chorus_hit+sev+eig+nin+zer+ten+danceability^2+energy^2+loudness^2+speechiness^2+acousticness^2+instrumentalness^2+liveness^2+tempo^2+time_signature^2+chorus_hit^2, data=data_numeric)
summary(white_aux)



