library(foreign)
library(data.table)


data <- read.spss("Downloads/Downloads/r29h_os41.sav", to.data.frame=TRUE)

#data1<- read.spss("Downloads/Downloads/r28hall41.sav", to.data.frame=TRUE)

cols<-c("region","status","popul","y_nfm","yc9.623a","yc9.623b","yc9.624a","yc9.624b",
        "yc9.15a","yc9.15b","ye41.5","ye20.5d","yf14","YA4.2","yc6","yc5","yc5.1","yd3","ye3","ye4","ye6.1","ye11","ye12.1","ye13.31b","YB1.4","YB1.5")

dt<-data[,cols]

dt$yf14<-as.character(dt$yf14)
dt$yf14<-as.numeric(dt$yf14)

m<-mean(dt$yf14,na.rm=T)
dt$yf14[is.na(dt$yf14)]<-m

dt$ye20.5d<-as.character(dt$ye20.5d)
dt$ye20.5d<-as.numeric(dt$ye20.5d)

m<-mean(dt$ye20.5d,na.rm=T)
dt$ye20.5d[is.na(dt$ye20.5d)]<-m

#dt$ye20.4d<-as.character(dt$ye20.4d)
#dt$ye20.4d<-as.numeric(dt$ye20.4d)

#m<-mean(dt$ye20.4d,na.rm=T)
#dt$ye20.4d[is.na(dt$ye20.4d)]<-m

#dt$ye20.5c<-as.character(dt$ye20.5c)
#dt$ye20.5a<-as.character(dt$ye20.5a)
#dt$ye20.4c<-as.character(dt$ye20.4c)
#dt$ye20.4a<-as.character(dt$ye20.4a)

#dt$ye20.3d<-as.character(dt$ye20.3d)
#dt$ye20.3d<-as.numeric(dt$ye20.3d)

#m<-mean(dt$ye20.3d,na.rm=T)
#dt$ye20.3d[is.na(dt$ye20.3d)]<-m

#dt$ye20.3c<-as.character(dt$ye20.3c)
#dt$ye20.3a<-as.character(dt$ye20.3a)

#dt$ye20.2d<-as.character(dt$ye20.2d)
#dt$ye20.2d<-as.numeric(dt$ye20.2d)

#m<-mean(dt$ye20.2d,na.rm=T)
#dt$ye20.2d[is.na(dt$ye20.2d)]<-m

#dt$ye20.2c<-as.character(dt$ye20.2c)
#dt$ye20.2a<-as.character(dt$ye20.2a)

#dt$ye20.1d<-as.character(dt$ye20.1d)
#dt$ye20.1d<-as.numeric(dt$ye20.1d)

#m<-mean(dt$ye20.1d,na.rm=T)
#dt$ye20.1d[is.na(dt$ye20.1d)]<-m

#dt$ye20.1c<-as.character(dt$ye20.1c)
#dt$ye20.1a<-as.character(dt$ye20.1a)

#dt$ye41.1<-as.character(dt$ye41.1)
#dt$ye41.2<-as.character(dt$ye41.2)
#dt$ye41.3<-as.character(dt$ye41.3)
#dt$ye41.4<-as.character(dt$ye41.4)
dt$ye41.5<-as.character(dt$ye41.5)

dt$yc9.15b<-as.character(dt$yc9.15b)
dt$yc9.15b<-as.numeric(dt$yc9.15b)

m<-mean(dt$yc9.15b,na.rm=T)
dt$yc9.15b[is.na(dt$yc9.15b)]<-m

dt$yc9.624b<-as.character(dt$yc9.624b)
dt$yc9.624b<-as.numeric(dt$yc9.624b)

m<-mean(dt$yc9.624b,na.rm=T)
dt$yc9.624b[is.na(dt$yc9.624b)]<-m

dt$yc9.624a<-as.character(dt$yc9.624a)

dt$yc9.623b<-as.character(dt$yc9.623b)
dt$yc9.623b<-as.numeric(dt$yc9.623b)

m<-mean(dt$yc9.623b,na.rm=T)
dt$yc9.623b[is.na(dt$yc9.623b)]<-m

dt$yc9.623a<-as.character(dt$yc9.623a)

dt$y_nfm<-as.character(dt$y_nfm)
dt$y_nfm<-as.numeric(dt$y_nfm)

m<-mean(dt$y_nfm,na.rm=T)
dt$y_nfm[is.na(dt$y_nfm)]<-m

dt$popul<-as.character(dt$popul)
dt$popul<-as.numeric(dt$popul)

m<-mean(dt$popul,na.rm=T)
dt$popul[is.na(dt$popul)]<-m

dt$region<-as.factor(dt$region)
dt$status<-as.factor(dt$status)

dt$yc9.15a<-as.character(dt$yc9.15a)
dt$yc9.15a[dt$yc9.15a=="Да"]<-1
dt$yc9.15a[dt$yc9.15a=="Нет"]<-0
dt$yc9.15a[is.na(dt$yc9.15a)]<-0

#dt$ye20.5a[dt$ye20.5a=="Да"]<-1
#dt$ye20.5a[dt$ye20.5a=="Нет"]<-0
#dt$ye20.5a[is.na(dt$ye20.5a)]<-0

#dt$ye20.4a[dt$ye20.4a=="Да"]<-1
#dt$ye20.4a[dt$ye20.4a=="Нет"]<-0
#dt$ye20.4a[is.na(dt$ye20.4a)]<-0

#dt$ye20.3a[dt$ye20.3a=="Да"]<-1
#dt$ye20.3a[dt$ye20.3a=="Нет"]<-0
#dt$ye20.3a[is.na(dt$ye20.3a)]<-0

#dt$ye20.2a[dt$ye20.2a=="Да"]<-1
#dt$ye20.2a[dt$ye20.2a=="Нет"]<-0
#dt$ye20.2a[is.na(dt$ye20.2a)]<-0

#dt$ye20.1a[dt$ye20.1a=="Да"]<-1
#dt$ye20.1a[dt$ye20.1a=="Нет"]<-0
#dt$ye20.1a[is.na(dt$ye20.1a)]<-0


#dt$ye20.5c[dt$ye20.5c=="Да"]<-1
#dt$ye20.5c[dt$ye20.5c=="Нет"]<-0
#dt$ye20.5c[is.na(dt$ye20.5c)]<-0

#dt$ye20.4c[dt$ye20.4c=="Да"]<-1
#dt$ye20.4c[dt$ye20.4c=="Нет"]<-0
#dt$ye20.4c[is.na(dt$ye20.4c)]<-0

#dt$ye20.3c[dt$ye20.3c=="Да"]<-1
#dt$ye20.3c[dt$ye20.3c=="Нет"]<-0
#dt$ye20.3c[is.na(dt$ye20.3c)]<-0

#dt$ye20.2c[dt$ye20.2c=="Да"]<-1
#dt$ye20.2c[dt$ye20.2c=="Нет"]<-0
#dt$ye20.2c[is.na(dt$ye20.2c)]<-0

#dt$ye20.1c[dt$ye20.1c=="Да"]<-1
#dt$ye20.1c[dt$ye20.1c=="Нет"]<-0
#dt$ye20.1c[is.na(dt$ye20.1c)]<-0
#
#dt$ye41.1[dt$ye41.1=="Да"]<-1
#dt$ye41.1[dt$ye41.1=="Нет"]<-0
#dt$ye41.1[is.na(dt$ye41.1)]<-0

#dt$ye41.2[dt$ye41.2=="Да"]<-1
#dt$ye41.2[dt$ye41.2=="Нет"]<-0
#dt$ye41.2[is.na(dt$ye41.2)]<-0

#dt$ye41.3[dt$ye41.3=="Да"]<-1
#dt$ye41.3[dt$ye41.3=="Нет"]<-0
#dt$ye41.3[is.na(dt$ye41.3)]<-0

#dt$ye41.4[dt$ye41.4=="Да"]<-1
#dt$ye41.4[dt$ye41.4=="Нет"]<-0
#dt$ye41.4[is.na(dt$ye41.4)]<-0

dt$ye41.5[dt$ye41.5=="Да"]<-1
dt$ye41.5[dt$ye41.5=="Нет"]<-0
dt$ye41.5[is.na(dt$ye41.5)]<-0


dt$yc9.15b[dt$yc9.15b=="Да"]<-1
dt$yc9.15b[dt$yc9.15b=="Нет"]<-0
dt$yc9.15b[is.na(dt$yc9.15b)]<-0

dt$yc9.623a[dt$yc9.623a=="Да"]<-1
dt$yc9.623a[dt$yc9.623a=="Нет"]<-0
dt$yc9.623a[is.na(dt$yc9.623a)]<-0

dt$yc9.624a[dt$yc9.624a=="Да"]<-1
dt$yc9.624a[dt$yc9.624a=="Нет"]<-0
dt$yc9.624a[is.na(dt$yc9.624a)]<-0

dt$YB1.4<-as.character(dt$YB1.4)
dt$YB1.4[dt$YB1.4=="Мужской"]<-1
dt$YB1.4[dt$YB1.4=="Женский"]<-0
dt$YB1.4<-as.numeric(dt$YB1.4)

dt$YB1.5<-as.numeric(dt$YB1.5)

dt<-as.data.table(dt)

dt[,yearinc:=yf14*12]
dt[,age:=2020-YB1.5]

functax<-function(x){
  if (x>=5000000){
    m<-(650000+(x-5000000)*0.15)/x
  } else if (x>144000){
    m<-0.13
  } else {
    m<-0
  }
}

dt[,mtax:=lapply(dt$yearinc,functax)]
dt$mtax<-as.numeric(dt$mtax)
dt$yc9.623a<-as.numeric(dt$yc9.623a)
dt$yc9.624a<-as.numeric(dt$yc9.624a)
dt$yc9.15a<-as.numeric(dt$yc9.15a)
dt$ye41.5<-as.numeric(dt$ye41.5)

dt$yc6<-as.character(dt$yc6)
dt$yc6<-as.numeric(dt$yc6)

m<-mean(dt$yc6,na.rm=T)
dt$yc6[is.na(dt$yc6)]<-m

dt$yc5<-as.character(dt$yc5)
dt$yc5<-as.numeric(dt$yc5)

m<-mean(dt$yc5,na.rm=T)
dt$yc5[is.na(dt$yc5)]<-m

dt$yc5.1<-as.character(dt$yc5.1)
dt$yc5.1<-as.numeric(dt$yc5.1)

m<-mean(dt$yc5.1,na.rm=T)
dt$yc5.1[is.na(dt$yc5.1)]<-m

dt$yd3<-as.character(dt$yd3)
dt$yd3<-as.numeric(dt$yd3)

m<-mean(dt$yd3,na.rm=T)
dt$yd3[is.na(dt$yd3)]<-m

dt$ye3<-as.character(dt$ye3)
dt$ye3<-as.numeric(dt$ye3)

m<-mean(dt$ye3,na.rm=T)
dt$ye3[is.na(dt$ye3)]<-m

dt$ye4<-as.character(dt$ye4)
dt$ye4<-as.numeric(dt$ye4)

m<-mean(dt$ye4,na.rm=T)
dt$ye4[is.na(dt$ye4)]<-m

dt$ye6.1<-as.character(dt$ye6.1)
dt$ye6.1<-as.numeric(dt$ye6.1)

m<-mean(dt$ye6.1,na.rm=T)
dt$ye6.1[is.na(dt$ye6.1)]<-m

dt$ye11<-as.character(dt$ye11)
dt$ye11<-as.numeric(dt$ye11)

m<-mean(dt$ye11,na.rm=T)
dt$ye11[is.na(dt$ye11)]<-m

dt$ye12.1<-as.character(dt$ye12.1)
dt$ye12.1<-as.numeric(dt$ye12.1)

m<-mean(dt$ye12.1,na.rm=T)
dt$ye12.1[is.na(dt$ye12.1)]<-m

dt$ye13.31b<-as.character(dt$ye13.31b)
dt$ye13.31b<-as.numeric(dt$ye13.31b)

m<-mean(dt$ye13.31b,na.rm=T)
dt$ye13.31b[is.na(dt$ye13.31b)]<-m

#yc6 - ye13.31b possible instruments
dt$region<-as.character(dt$region)
dt$region[dt$region=="Ханты-Мансийский АО, Сургут и Сургутский район (1994-2002 гг)"]<-NA
dt[,cl1:=0]
dt[,cl2:=0]
dt[,cl3:=0]
dt[,cl4:=0]
dt[,cl5:=0]

dt$cl4[dt$region=="Алтайский край, Бийск и Бийский район"]<-1
dt$cl5[dt$region=="Алтайский край, Курьинский район"]<-1
dt$cl5[dt$region=="Амурская область, Тамбовский район"]<-1
dt$cl3[dt$region=="Владивосток"]<-1
dt$cl5[dt$region=="Волгоградская область, Руднянский район"]<-1
dt$cl2[dt$region=="Казань"]<-1
dt$cl5[dt$region=="Калужская область, Куйбышевский район"]<-1
dt$cl5[dt$region=="Коми Республика, Печора и Печорский район  (с 2020 года)"]<-1
dt$cl4[dt$region=="Коми Республика, Сыктывкар"]<-1
dt$cl2[dt$region=="Краснодар"]<-1
dt$cl2[dt$region=="Красноярск"]<-1
dt$cl5[dt$region=="Краснодарский край, Кущевский район"]<-1
dt$cl5[dt$region=="Красноярский край, Назарово и Назаровский район"]<-1
dt$cl3[dt$region=="Курган"]<-1
dt$cl5[dt$region=="Ленинградская область, Волосовский район"]<-1
dt$cl3[dt$region=="Липецк"]<-1
dt$cl1[dt$region=="Москва"]<-1
dt$cl2[dt$region=="Московская область"]<-1
dt$cl2[dt$region=="Нижний Новгород"]<-1
dt$cl5[dt$region=="Новая Москва"]<-1
dt$cl4[dt$region=="Новосибирская область, Бердск и Бердский район (с 2003 года)"]<-1
dt$cl4[dt$region=="Оренбургская область, Орск"]<-1
dt$cl5[dt$region=="Новосибирская область, Бердск и Бердский район (с 2003 года)"]<-1
dt$cl5[dt$region=="Пензенская область, Земетчинский район"]<-1
dt$cl5[dt$region=="Пермский край, Соликамск и Соликамский район"]<-1
dt$cl5[dt$region=="Республика Кабардино-Балкария, Залукокоаже и Зольский район"]<-1
dt$cl5[dt$region=="Республика Чувашия, Шумерля и Шумерлинский район"]<-1
dt$cl4[dt$region=="Ростовская область, Батайск"]<-1
dt$cl1[dt$region=="Санкт-Петербург"]<-1
dt$cl2[dt$region=="Саратов"]<-1
dt$cl5[dt$region=="Саратовская область, Вольск и Вольский район"]<-1
dt$cl3[dt$region=="Смоленск"]<-1
dt$cl4[dt$region=="Ставропольский край, Георгиевск и Георгиевский район"]<-1
dt$cl5[dt$region=="Тамбовская область, Уварово и Уваровский район"]<-1
dt$cl5[dt$region=="Тверская область, Ржев и Ржевский район"]<-1
dt$cl3[dt$region=="Томск"]<-1
dt$cl3[dt$region=="Тула"]<-1
dt$cl5[dt$region=="Удмуртская Республика, Глазов и Глазовский район"]<-1
dt$cl2[dt$region=="Челябинск"]<-1
dt$cl5[dt$region=="Челябинская область, Красноармейский район"]<-1


dt$cl[dt$region=="Алтайский край, Бийск и Бийский район"]<-4
dt$cl[dt$region=="Алтайский край, Курьинский район"]<-5
dt$cl[dt$region=="Амурская область, Тамбовский район"]<-5
dt$cl[dt$region=="Владивосток"]<-3
dt$cl[dt$region=="Волгоградская область, Руднянский район"]<-5
dt$cl[dt$region=="Казань"]<-2
dt$cl[dt$region=="Калужская область, Куйбышевский район"]<-5
dt$cl[dt$region=="Коми Республика, Печора и Печорский район  (с 2020 года)"]<-5
dt$cl[dt$region=="Коми Республика, Сыктывкар"]<-4
dt$cl[dt$region=="Краснодар"]<-2
dt$cl[dt$region=="Красноярск"]<-2
dt$cl[dt$region=="Краснодарский край, Кущевский район"]<-5
dt$cl[dt$region=="Красноярский край, Назарово и Назаровский район"]<-5
dt$cl[dt$region=="Курган"]<-3
dt$cl[dt$region=="Ленинградская область, Волосовский район"]<-5
dt$cl[dt$region=="Липецк"]<-3
dt$cl[dt$region=="Москва"]<-1
dt$cl[dt$region=="Московская область"]<-2
dt$cl[dt$region=="Нижний Новгород"]<-2
dt$cl[dt$region=="Новая Москва"]<-5
dt$cl[dt$region=="Новосибирская область, Бердск и Бердский район (с 2003 года)"]<-4
dt$cl[dt$region=="Оренбургская область, Орск"]<-4
dt$cl[dt$region=="Новосибирская область, Бердск и Бердский район (с 2003 года)"]<-5
dt$cl[dt$region=="Пензенская область, Земетчинский район"]<-5
dt$cl[dt$region=="Пермский край, Соликамск и Соликамский район"]<-5
dt$cl[dt$region=="Республика Кабардино-Балкария, Залукокоаже и Зольский район"]<-5
dt$cl[dt$region=="Республика Чувашия, Шумерля и Шумерлинский район"]<-5
dt$cl[dt$region=="Ростовская область, Батайск"]<-4
dt$cl[dt$region=="Санкт-Петербург"]<-1
dt$cl[dt$region=="Саратов"]<-2
dt$cl[dt$region=="Саратовская область, Вольск и Вольский район"]<-5
dt$cl[dt$region=="Смоленск"]<-3
dt$cl[dt$region=="Ставропольский край, Георгиевск и Георгиевский район"]<-4
dt$cl[dt$region=="Тамбовская область, Уварово и Уваровский район"]<-5
dt$cl[dt$region=="Тверская область, Ржев и Ржевский район"]<-5
dt$cl[dt$region=="Томск"]<-3
dt$cl[dt$region=="Тула"]<-3
dt$cl[dt$region=="Удмуртская Республика, Глазов и Глазовский район"]<-5
dt$cl[dt$region=="Челябинск"]<-2
dt$cl[dt$region=="Челябинская область, Красноармейский район"]<-5

dt$cl<-as.factor(dt$cl)
dt$region<-as.factor(dt$region)

dt<-na.omit(dt)

View(dt)


dt$status<-as.character(dt$status)
dt[,city:=fifelse(dt$status=="Город",1,0)]
dt[,pgt:=fifelse(dt$status=="ПГТ",1,0)]
dt[,city:=fifelse(dt$status=="Областной центр",1,0)]
dt[,selo:=fifelse(dt$status=="Село",1,0)]

dt[,relprice:=ye20.5d/yf14]
dt$relprice<-as.numeric(dt$relprice)

dt$YA4.2<-as.character(dt$YA4.2)
funcmon<-function(x){
  if (x=="Январь"){
    m<-504.7
  }
  if (x=="Февраль"){
    m<-403.3
  }
  if (x=="Март"){
    m<-509.4
  }
  if (x=="Апрель"){
    m<-729
  }
  if (x=="Май"){
    m<-357.1
  }
  if (x=="Июнь"){
    m<-692.2
  }
  if (x=="Июль"){
    m<-510.4
  }
  if (x=="Август"){
    m<-518.7
  }
  if (x=="Сентябрь"){
    m<-543.9
  }
  if (x=="Октябрь"){
    m<-700.3
  }
  if (x=="Ноябрь"){
    m<-404.5
  }
  if (x=="Декабрь"){
    m<-1117.7
  }
  return(m)
}

dt[,govexp:=lapply(YA4.2,funcmon)]
dt$govexp<-as.numeric(dt$govexp)

#dt$govexp<-dt$govexp*1000000000

#trying different instruments
library(AER)
library(sandwich)

#modify the columns
dt[,mtaxmod:=mtax*100]
dt[,relpricemod:=relprice*100]
dt[,ye4mod:=ye4/1000]
dt[,yf14mod:=yf14/1000]
dt[,populmod:=popul/1000]

#divide the dataset intoo two by income
dt<-dt[dt$relprice!=Inf]

income<-dt$yf14
meaninc<-mean(income)

dtlow<-dt[dt$yf14<meaninc]
dthigh<-dt[dt$yf14>meaninc]

library(ivprobit)
library(margins)

m_iv <- ivprobit(ye41.5~cl1+cl2+cl3+cl4+cl5+selo+pgt+y_nfm+mtaxmod+govexp+YB1.4+YB1.5 | yf14mod | cl1+cl2+cl3+cl4+cl5+selo+pgt+obcenter+y_nfm+ye4mod+mtaxmod+govexp+YB1.4+YB1.5, data=dt)
summary(m_iv)
m <- margins(m_iv)
summary(m)



myprobit <- glm(ye41.5~cl1+cl2+cl3+cl4+cl5+selo+pgt+y_nfm+mtaxmod+govexp+YB1.4+YB1.5+yf14mod, family = binomial(link = "probit"), 
                data = dt)

## model summary
summary(myprobit)

m <- margins(myprobit)
summary(m)

myprobit <- glm(ye41.5~cl1+cl2+cl3+cl4+cl5+selo+pgt+y_nfm+mtaxmod+govexp+YB1.4+YB1.5+yf14mod, family = binomial(link = "probit"), 
                data = dtlow)

## model summary
summary(myprobit)
m <- margins(myprobit)
summary(m)


myprobit <- glm(ye41.5~cl1+cl2+cl3+cl4+cl5+selo+pgt+y_nfm+mtaxmod+govexp+YB1.4+YB1.5+yf14mod, family = binomial(link = "probit"), 
                data = dthigh)

summary(myprobit)
m <- margins(myprobit)
summary(m)

myprobit <- glm(ye41.5~cl1+cl2+cl3+cl4+cl5+selo+pgt+y_nfm+relpricemod+govexp+YB1.4+YB1.5+yf14mod, family = binomial(link = "probit"), 
                data = dt)

## model summary
summary(myprobit)
m <- margins(myprobit)
summary(m)

myprobit <- glm(ye41.5~cl1+cl2+cl3+cl4+cl5+selo+pgt+y_nfm+relpricemod+govexp+YB1.4+YB1.5+yf14mod, family = binomial(link = "probit"), 
                data = dtlow)

## model summary
summary(myprobit)


m <- margins(myprobit)
summary(m)

myprobit <- glm(ye41.5~cl1+cl2+cl3+cl4+cl5+selo+pgt+y_nfm+relpricemod+govexp+YB1.4+YB1.5+yf14mod, family = binomial(link = "probit"), 
                data = dthigh)

## model summary
summary(myprobit)

m <- margins(myprobit)
summary(m)

library(moments)






cl<-dt[,c("region","popul")]
View(cl)

aggregate(cl$popul,by=list(Group=cl$region), FUN=sum)

library(dplyr)
options(dplyr.print_max = 1e9)
cl %>%
  group_by(region) %>%
  summarise(Sum = sum(unique(popul), na.rm = TRUE))

#cluster by population
#cluster 1 - moscow st pete
#cluster 2 - chelyabinsk, new moscow, moscow oblast, krasnoyarsk, kazan, saratov, krasnodar, nizhny novgorod
#cluster 3- vladivostok, kurgan, lipetsk, smolensk, tomsk, tula
#cluster 4 - altay biysk, komi republic syktyvkar, orenburg oblast, stavropol kray, rostov oblast, novosib oblast,
#cluster 5 - altay kuryinsky rayon, amur oblast, volgograd oblast, kaluzh oblast, komi pechora, 
#krasnodarsky kray, krasnoyarsky kray, leningrad oblast, new moscow, penza oblast, perm kray, kabardino, chuvash,
#saratov oblast, tambov oblast, tver oblast, udmurt, chelyab oblast



View(dt)
#cluster by volunteers
#cluster 1 - moscow, st pete, kurgan, orenburg oblast orsk
#cluster 2 - tomsk, chuvash republic, tuka, kaluzhskaya oblast kyub rayon
#cluster 3 - altay kray and biysk, volgogr oblast, kazan, komi republic, krasnodar, leningrad oblast, 
#lipetsk, moscow oblast, new moscow, novosib oblast, perm kray, saratov, smolensk, stavropol kray,
#tambov oblast, tver oblast, udmurt republic, chelyab, chelyab oblast
#cluster 4 - amur oblast, altay kray and kyruinsky region, vladivostok, krasnodar kray, krasnoyarsk, 
#krasnoyarsk kray, nizhny novgorod, penza oblast, kabardino balkariya, rostov oblast, saratov oblast,



m_iv <- ivreg(ye41.5~region+city+pgt+populmod+y_nfm+yf14mod+mtaxmod+govexp | region+city+pgt+populmod+y_nfm+ye4mod+mtaxmod+govexp, data=dtlow)
summary(m_iv,vcov = vcovHC, type = "HC1")

lm1<-lm(ye41.5~region+city+pgt+populmod+y_nfm+yf14mod+mtaxmod+govexp,data=dtlow)
summary(lm1)

lm2<-lm(ye41.5~mtaxmod,data=dtlow)
summary(lm2)
  
m_iv <- ivreg(ye41.5~region+city+pgt+populmod+y_nfm+yf14mod+mtaxmod+govexp | region+city+pgt+populmod+y_nfm+ye4mod+mtaxmod+govexp, data=dthigh)
summary(m_iv,vcov = vcovHC, type = "HC1")

m_iv <- ivreg(ye41.5~region+city+pgt+populmod+y_nfm+yf14mod+relpricemod+govexp | region+city+pgt+populmod+y_nfm+ye4mod+relpricemod+govexp, data=dt)
summary(m_iv,vcov = vcovHC, type = "HC1")

m_iv <- ivreg(ye41.5~region+city+pgt+populmod+y_nfm+yf14mod+relpricemod+govexp | region+city+pgt+populmod+y_nfm+ye4mod+relpricemod+govexp, data=dtlow)
summary(m_iv,vcov = vcovHC, type = "HC1")

m_iv <- ivreg(ye41.5~region+city+pgt+populmod+y_nfm+yf14mod+relpricemod+govexp | region+city+pgt+populmod+y_nfm+ye4mod+relpricemod+govexp, data=dthigh)
summary(m_iv,vcov = vcovHC, type = "HC1")

plot(dt$yf14)

#region
#city - if they live in a city
#pgt - poselok gorodskogo tipa, selo is a reference cateogry
#popul - population
#y_nfm - number of family members
#yf14 - incomecome per month 
#mtax - marginal tax
#govexp in bln roubles source: https://minfin.gov.ru/ru/statistics/fedbud/




