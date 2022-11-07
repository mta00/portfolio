rm(list=ls())
#load all required libraries
library(data.table)
library(devtools)
library(spotifyr)
library(stringr)
library(aod)
library(AER)
library(sentimentr)

data<-fread('Downloads/music1.csv')
#sp<-fread('Downloads/Untitled.csv')
msp<-fread('Downloads/music_sp.csv')
emo<-fread('Downloads/emotions.csv')
emo<-emo[1:778,2:9]


data[,sp:=msp$`s&p500`]
data[,ggr:=data$`gdp growth, % quarterly`]
data[,folk:=data$`folk, world & country`]
data[,funk:=data$`funk/soul`]
data[,hiphop:=data$`hip hop/rap`]

data<-data[danceability!=0]
colnames(data)[1]<-'Date'
#sp <- sp[-1,] 
#sp<-sp[,-1]

#setnames(sp, "V2", "Date")
#setnames(sp, "V3", "Open")
#setnames(sp, "V4", "High")
#setnames(sp, "V5", "Low")
#setnames(sp, "V6", "Close")
#setnames(sp, "V7", "Adj Close")
#setnames(sp, "V8", "Volume")

#dates<-sp$Date
#sp<-sp[,2:7]

data[,anticipation:=emo$anticipation]
data[,fear:=emo$fear]
data[,joy:=emo$joy]
data[,sadness:=emo$sadness]
data[,surprise:=emo$surprise]
data[,trust:=emo$trust]
data[,anger:=emo$anger]
data[,disgust:=emo$disgust]

View(emo)
View(data)

data_numeric<-data[,-c('Date','Artist(s)','Single','psychedelic')] 


#create the correlation matrix
data_numeric.cor<-cor(data_numeric)
View(data_numeric.cor)

#linear model for sp
lsp<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp)

lsp1<-lm(sp ~ jazz+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp1)

lsp2<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp2)

lsp3<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp3)

lsp4<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp4)

lsp5<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp5)

lsp6<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp6)

lsp7<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp7)

lsp8<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp8)

lsp9<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp)

lsp10<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp)

lsp11<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp11)

lsp12<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp12)

lsp13<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp13)

lsp14<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+surprise+trust+anger+disgust, data=data)
summary(lsp14)

lsp15<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+trust+anger+disgust, data=data)
summary(lsp15)

lsp16<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp16)

lsp17<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+disgust, data=data)
summary(lsp17)

lsp18<-lm(sp ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger, data=data)
summary(lsp18)

#linear model for sp after excluding reggae

lsp21<-lm(sp ~ jazz+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp21)

lsp22<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp22)

lsp23<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp23)

lsp24<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp24)

lsp25<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp25)

lsp26<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp26)

lsp27<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp27)

lsp28<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp28)

lsp29<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp29)

lsp210<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp210)

lsp211<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp211)

lsp212<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp212)

lsp213<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+surprise+trust+anger+disgust, data=data)
summary(lsp213)

lsp214<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+trust+anger+disgust, data=data)
summary(lsp214)

lsp215<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp215)

lsp216<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+disgust, data=data)
summary(lsp216)

lsp217<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger, data=data)
summary(lsp217)

#excluding trust

lsp2151<-lm(sp ~ jazz+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2151)

lsp2152<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2152)

lsp2153<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2153)


lsp2154<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2154)

lsp2155<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2155)


lsp2156<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2156)

lsp2157<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2157)

lsp2158<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+time_signature+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2158)

lsp2159<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp2159)

lsp21510<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+fear+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp21510)

lsp21511<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+joy+sadness+surprise+anger+disgust, data=data)
summary(lsp21511)

lsp21512<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+sadness+surprise+anger+disgust, data=data)
summary(lsp21512)

lsp21513<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+surprise+anger+disgust, data=data)
summary(lsp21513)

lsp21514<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+anger+disgust, data=data)
summary(lsp21514)

lsp21515<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515)

lsp21516<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+anger, data=data)
summary(lsp21516)

#ex anger 

lsp215151<-lm(sp ~ jazz+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215151)

lsp215152<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215152)

lsp215153<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215153)

lsp215154<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215154)

lsp215155<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215155)

lsp215156<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215156)

lsp215157<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215157)

lsp215158<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+time_signature+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215158)

lsp215159<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp215159)

lsp2151510<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151510)

lsp2151511<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+joy+sadness+surprise+disgust, data=data)
summary(lsp2151511)

lsp2151512<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+sadness+surprise+disgust, data=data)
summary(lsp2151512)

lsp2151513<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+surprise+disgust, data=data)
summary(lsp2151513)

lsp2151514<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp2151514)

lsp2151515<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise, data=data)
summary(lsp2151515)

#ex time signature

lsp2151591<-lm(sp ~ jazz+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151591)

lsp2151592<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151592)

lsp2151593<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151593)

lsp2151594<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151594)

lsp2151595<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151595)

lsp2151596<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151596)

lsp2151597<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151597)

lsp2151598<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151598)

lsp2151599<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp2151599)

lsp21515910<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+joy+sadness+surprise+disgust, data=data)
summary(lsp21515910)

lsp21515911<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+sadness+surprise+disgust, data=data)
summary(lsp21515911)

lsp21515912<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+surprise+disgust, data=data)
summary(lsp21515912)

lsp21515913<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp21515913)

lsp21515914<-lm(sp ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise, data=data)
summary(lsp21515914)

#ex metal

lsp21515921<-lm(sp ~ jazz+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515921)
lsp21515922<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515922)
lsp21515923<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515923)
lsp21515924<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515924)
lsp21515925<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515925)
lsp21515926<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515926)
lsp21515927<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+anticipation+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515927)
lsp21515928<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness+surprise+disgust, data=data)
summary(lsp21515928)
lsp21515929<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+joy+sadness+surprise+disgust, data=data)
summary(lsp21515929)
lsp215159210<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+sadness+surprise+disgust, data=data)
summary(lsp215159210)
lsp215159211<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+surprise+disgust, data=data)
summary(lsp215159211)
lsp215159212<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp215159212)
lsp215159213<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise, data=data)
summary(lsp215159213)

#ex surprise

lsp1<-lm(sp ~ jazz+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp1)
lsp2<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp2)
lsp3<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp3)
lsp4<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp4)
lsp5<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp5)
lsp6<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp6)
lsp7<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+anticipation+fear+joy+sadness+disgust, data=data)
summary(lsp7)
lsp8<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp8)
lsp9<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+joy+sadness+disgust, data=data)
summary(lsp9)
lsp10<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+sadness+disgust, data=data)
summary(lsp10)
lsp11<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+disgust, data=data)
summary(lsp11)
lsp12<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness, data=data)
summary(lsp12)

#ex anticipation

lsp1<-lmlm(sp ~ jazz+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp1)
lsp2<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp2)
lsp3<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp3)
lsp4<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp4)
lsp5<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp5)
lsp6<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp6)
lsp7<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+fear+joy+sadness+disgust, data=data)
summary(lsp7)
lsp8<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+joy+sadness+disgust, data=data)
summary(lsp8)
lsp9<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+sadness+disgust, data=data)
summary(lsp9)
lsp10<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+disgust, data=data)
summary(lsp10)
lsp11<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+fear+joy+sadness, data=data)
summary(lsp11)

#ex liveness

lsp1<-lm(sp ~ jazz+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp1)
lsp2<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp2)
lsp3<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp3)
lsp4<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+valence+tempo+fear+joy+sadness+disgust, data=data)
summary(lsp4)
lsp5<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy+sadness+disgust, data=data)
summary(lsp5)
lsp6<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+joy+sadness+disgust, data=data)
summary(lsp6)
lsp7<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+fear+sadness+disgust, data=data)
summary(lsp7)
lsp8<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+fear+joy+disgust, data=data)
summary(lsp8)
lsp9<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+tempo+fear+joy+sadness, data=data)
summary(lsp9)

#ex tempo

lsp1<-lm(sp ~ jazz+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy+sadness+disgust, data=data)
summary(lsp1)
lsp2<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy+sadness+disgust, data=data)
summary(lsp2)
lsp3<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+valence+fear+joy+sadness+disgust, data=data)
summary(lsp3)
lsp4<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+joy+sadness+disgust, data=data)
summary(lsp4)
lsp5<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+sadness+disgust, data=data)
summary(lsp5)
lsp6<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy+disgust, data=data)
summary(lsp6)
lsp7<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy+sadness, data=data)
summary(lsp7)



#ex disgust

lsp1<-lm(sp ~ jazz+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy+sadness, data=data)
summary(lsp1)
lsp2<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy+sadness, data=data)
summary(lsp2)
lsp3<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+valence+fear+joy+sadness, data=data)
summary(lsp3)
lsp4<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+joy+sadness, data=data)
summary(lsp4)
lsp5<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+sadness, data=data)
summary(lsp5)
lsp6<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+fear+joy, data=data)
summary(lsp6)

#ex fear

lsp1<-lm(sp ~ jazz+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+joy+sadness, data=data)
summary(lsp1)
lsp2<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+joy+sadness, data=data)
summary(lsp2)
lsp3<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+valence+joy+sadness, data=data)
summary(lsp3)
lsp4<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+sadness, data=data)
summary(lsp4)
lsp5<-lm(sp ~ jazz+folk+rock+funk+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+valence+joy, data=data)
summary(lsp5)


#linear model for gdp growth
lgg<-lm(ggr ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lgg)

lsp1<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp1)

lsp2<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp2)

lsp3<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp3)

lsp4<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp4)

lsp5<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp5)

lsp6<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp6)

lsp7<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp7)

lsp8<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp8)

lsp9<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp9)

lsp10<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp10)

lsp11<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp11)

lsp12<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+anticipation+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp12)

lsp13<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp13)

lsp14<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp14)

lsp15<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp15)

lsp16<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+surprise+trust+anger+disgust, data=data)
summary(lsp16)

lsp17<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+trust+anger+disgust, data=data)
summary(lsp17)

lsp18<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+disgust, data=data)
summary(lsp18)

lsp19<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature+anticipation+fear+joy+sadness+surprise+trust+anger, data=data)
summary(lsp19)



#ex anticipation

lsp1<-lm(ggr ~ jazz+folk+rock+funk+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp1)
lsp2<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp2)
lsp3<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp3)
lsp4<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp4)
lsp5<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp5)
lsp6<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp6)
lsp7<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp7)
lsp8<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp8)
lsp9<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp9)
lsp10<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp10)
lsp11<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp11)
lsp12<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp12)
lsp13<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp13)
lsp14<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp14)
lsp15<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp15)
lsp16<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp16)
lsp17<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp17)
lsp18<-lm(ggr ~ jazz+folk+rock+funk+reggae+disco+metal+hiphop+rnb+danceability+energy+loudness+mode+speechiness+acousticness+instrumentalness+liveness+tempo+time_signature+fear+joy+sadness+surprise+trust+anger+disgust, data=data)
summary(lsp18)



plot(data$sp)
plot(data$gdp)
plot(data$`gdp growth, % quarterly`)
plot(data$sp)

#sp$Open<-as.numeric(gsub(",","",sp$Open))
#sp$High<-as.numeric(gsub(",","",sp$High))
#sp$Low<-as.numeric(gsub(",","",sp$Low))
#sp$Close<-as.numeric(gsub(",","",sp$Close))
#sp$`Adj Close`<-as.numeric(gsub(",","",sp$`Adj Close`))
#sp$Volume<-as.numeric(gsub(",","",sp$Volume))
#sp[,Date:=dates]

#mean(sp$Open)

#function_convert_date<-function(x){
  x<-x[[1]]
  m<-str_split(x,' ')[[1]][1]
  if (m=='January'){
    m<-'Jan'
  } else if (m=='February'){
    m<-'Feb'
  } else if (m=='March'){
    m<-'Mar'
  } else if (m=='April'){
    m<-'Apr'
  } else if (m=='June'){
    m<-'Jun'
  } else if (m=='July'){
    m<-'Jul'
  } else if (m=='August'){
    m<-'Aug'
  } else if (m=='September'){
    m<-'Sep'
  } else if (m=='October'){
    m<-'Oct'
  } else if (m=='November'){
    m<-'Nov'
  } else if (m=='December'){
    m<-'Dec'
  }
  x<-paste(m, str_split(x,' ')[[1]][2],str_split(x,' ')[[1]][3],sep=' ')
  return(x)
}

#data[,Date:=sapply(data$Date,function_convert_date)]
#View(data)

#40s - 87 obs
#50s - 92 obs
#60s - 122 obs
#70s - 151 obs
#80s - 146 obs
#90s - 79 obs
#00s - 59 obs
#10s - 42 obs
#20s - 7 obs

#fwrite(data, file='music.csv')
#fwrite(sp, file='spnew.csv')
