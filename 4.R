rm(list=ls())
library(data.table)
library(ggplot2)
library(rugarch)
library("readxl")
library(urca)
library("fGarch")
library(plm)

#3
data <- read_excel("NLSY2000_HA.xls")
data<-as.data.table(data)
#a
#time invariant regressors are id, male, s, ethblack and ethhisp
data$lag_earnings <- c(NA, data$EARNINGS[seq_along(data$EARNINGS) -1])
View(data)

pgmm(EARNINGS ~ lag_earnings+TENURE+AGE,data=data,model='onestep')


#4

dt <- read_excel("German.xls")
dt<-as.data.table(dt)

View(dt)

#a
library(AER)
model <- lm(doctor ~ age+married+working+hhninc+newhsat, data = dt)
summary(model)

coeftest(model, vcov. = vcovHC, type = "HC1")

model1 <- lm(doctor ~ age+married+working, data = dt)
summary(model1)

#b
logLik(model)
logLik(model1)

#c
m_probit <- glm(doctor ~ age+married+working+hhninc+newhsat, family = binomial(link = "probit"), data = dt)
summary(m_probit)

m_logit <- glm(doctor ~ age+married+working+hhninc+newhsat, family = binomial(link = "logit"), data = dt)
summary(m_logit)

library("margins")
margins(m_probit)
margins(m_logit)

mage<-mean(dt$age)
mmarr<-mean(dt$married)
mwork<-mean(dt$working)
mhh<-mean(dt$hhninc)
mnewh<-mean(dt$newhsat)

margins(m_probit, data=data.frame(dt),at = list(age=mage,married=mmarr,working=mwork,hhninc=mhh,newhsat=mnewh))

margins(m_logit, data=data.frame(dt),at = list(age=mage,married=mmarr,working=mwork,hhninc=mhh,newhsat=mnewh))

margins(m_probit, data=data.frame(dt),at = list(hhninc=mhh))

margins(m_logit, data=data.frame(dt),at = list(hhninc=mhh))

margins(m_probit, data=data.frame(dt),at = list(newhsat=mnewh))

margins(m_logit, data=data.frame(dt),at = list(newhsat=mnewh))

margins(m_probit, data=data.frame(dt),at = list(age=mage))

margins(m_logit, data=data.frame(dt),at = list(age=mage))

margins(m_probit, data=data.frame(dt),at = list(married=1))

margins(m_logit, data=data.frame(dt),at = list(married=1))

margins(m_probit, data=data.frame(dt),at = list(working=0))

margins(m_logit, data=data.frame(dt),at = list(working=0))

margins(m_probit, data=data.frame(dt),at = list(age=mage,married=1,working=0,hhninc=mhh,newhsat=mnewh))

margins(m_logit, data=data.frame(dt),at = list(age=mage,married=1,working=0,hhninc=mhh,newhsat=mnewh))

#d

pred_probit<-predict(m_probit,type='response')
pred_logit<-predict(m_logit,type='response')

dt[,pred_probit:=pred_probit]
dt[,pred_logit:=pred_logit]

#assume 0.5 is the threshold
dt[,res_probit:=fifelse(pred_probit>0.5,1,0)]
dt[,res_logit:=fifelse(pred_logit>0.5,1,0)]

dt[,prob_go:=fifelse(res_probit==doctor & doctor==1,1,0)]
dt[,prob_gont:=fifelse(res_probit==doctor & doctor==0,1,0)]

dt[,log_go:=fifelse(res_logit==doctor & doctor==1,1,0)]
dt[,log_gont:=fifelse(res_logit==doctor & doctor==0,1,0)]

n1<-sum(dt$prob_go)/nrow(dt)*100
n2<-sum(dt$prob_gont)/nrow(dt)*100
n3<-sum(dt$log_go)/nrow(dt)*100
n4<-sum(dt$log_gont)/nrow(dt)*100

#e
library(bife)

stat <- bife(doctor ~ age+married+working+hhninc+newhsat | id, dt, "logit")
summary(stat)


library(survival)

fe <- clogit(doctor ~ age+married+working+hhninc+newhsat+strata(id), data = dt)
logLik(fe)
summary(fe)

fee <- clogit(doctor ~ age+married+working+strata(id), data = dt)
logLik(fee)

#g
library(lme4)

m <- glmer(doctor ~ age+married+working+hhninc+newhsat +
             (1 | id), data = dt, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

print(m, corr = FALSE)

m <- glmer(doctor ~ age+married+working+hhninc+newhsat +
             (1 | id), data = dt, family = binomial(link='probit'), control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

print(m, corr = FALSE)


m <- glmer(doctor ~ age+married+working+
             (1 | id), data = dt, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

print(m, corr = FALSE)

m <- glmer(doctor ~ age+married+working+
             (1 | id), data = dt, family = binomial(link='probit'), control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

print(m, corr = FALSE)


