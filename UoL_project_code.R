#Import the required libraries
library(data.table) 
library(ggplot2) 
library(factoextra) 
library(leaps)
library(ranger)
library(margins)
#Part 1

data<-fread("Downloads/Downloads/EWCS_2016.csv")

#Cleaning the data from "-999"
data[data=="-999"]<-NA
data<-na.omit(data)

#Transforming the data to make the variables more intuitive
data$Q2a[data$Q2a==2]<-0 #female is now 0 instead of 2 and male is 1 (make the variable binary)

transform_87<-function(x){
  if (x==1){
    x<-6
  } else if (x==2){
    x<-5
  } else if (x==3){
    x<-4
  } else if (x==4){
    x<-3
  } else if (x==5){
    x<-2
  } else {
    x<-1
  }
  x
}

transform_90<-function(x){
  if (x==1){
    x<-5
  } else if (x==2){
    x<-4
  } else if (x==3){
    x<-3
  } else if (x==4){
    x<-2
  } else {
    x<-1
  }
  x
}

data[,Q87a:=as.numeric(lapply(data$Q87a,transform_87))]
data[,Q87b:=as.numeric(lapply(data$Q87b,transform_87))]
data[,Q87c:=as.numeric(lapply(data$Q87c,transform_87))]
data[,Q87d:=as.numeric(lapply(data$Q87d,transform_87))]
data[,Q87e:=as.numeric(lapply(data$Q87e,transform_87))]

data[,Q90a:=as.numeric(lapply(data$Q90a,transform_90))]
data[,Q90b:=as.numeric(lapply(data$Q90b,transform_90))]
data[,Q90c:=as.numeric(lapply(data$Q90c,transform_90))]
data[,Q90f:=as.numeric(lapply(data$Q90f,transform_90))]

#Correlation matrix
print(cor(data), digits = 2)

#PCA
data_smol<-data[1:100,]

#PCA with scaling
pca1 <- prcomp(data_smol, scale. = T, center = T)
summary(pca1)
data_transform = as.data.frame(-pca1$x[,1:2])
fviz_nbclust(data_transform[1:100,], kmeans, method = 'wss')
k = 4
kmeans1 = kmeans(data_transform, centers = k, nstart = 25)
fviz_cluster(kmeans1, data = data_transform[1:100,])

sort(table(kmeans1$clust))
clust <- names(sort(table(kmeans1$clust)))
#looking at clusters
data_smol[kmeans1$clust==clust[1],]
data_smol[kmeans1$clust==clust[2],]
data_smol[kmeans1$clust==clust[3],]
data_smol[kmeans1$clust==clust[4],]

#Taking averages for Q87 and Q90 variables and implelenting PCA again
data$Q87av<-rowMeans(subset(data, select = c(Q87a,Q87b,Q87c,Q87d,Q87e)), na.rm = TRUE)
data$Q90av<-rowMeans(subset(data, select = c(Q90a,Q90b,Q90c,Q90f)), na.rm = TRUE)

data2<-data[,c('Q2a','Q2b','Q87av','Q90av')]
data_smol2<-data2[1:100,]

pca2 <- prcomp(data_smol2, scale. = T, center = T)
summary(pca2)
data_transform = as.data.frame(-pca2$x[,1:2])
fviz_nbclust(data_transform[1:100,], kmeans, method = 'wss')
k = 4
kmeans2 = kmeans(data_transform, centers = k, nstart = 25)
fviz_cluster(kmeans2, data = data_transform[1:100,])

sort(table(kmeans2$clust))
clust <- names(sort(table(kmeans2$clust)))

#looking at clusters
data_smol2[kmeans2$clust==clust[1],]
data_smol2[kmeans2$clust==clust[2],]
data_smol2[kmeans2$clust==clust[3],]
data_smol2[kmeans2$clust==clust[4],]

#Part 2

data1<-fread("Downloads/Downloads/student-mat.csv")
data2<-fread("Downloads/Downloads/student-por.csv")

#Include the subject as a factor so that we can combine the two data tables and use this information
data1$subj<-"mat"
data2$subj<-"por"

#Combine the two data tables into one for convenience
data<-rbind(data1,data2)

#Removing G1 and G2 from the dataset
data<-data[,-c("G1","G2")]

#We need to appropriately transform our data so that it can be used for regression
data$school[data$school=="GP"]<-1
data$school[data$school=="MS"]<-0

data$sex[data$sex=="M"]<-1
data$sex[data$sex=="F"]<-0

data$address[data$address=="U"]<-1
data$address[data$address=="R"]<-0

data$famsize[data$famsize=="GT3"]<-1
data$famsize[data$famsize=="LE3"]<-0

data$Pstatus[data$Pstatus=="T"]<-1
data$Pstatus[data$Pstatus=="A"]<-0

data$Mjob<-as.factor(data$Mjob)
data$Fjob<-as.factor(data$Fjob)
data$reason<-as.factor(data$reason)
data$guardian<-as.factor(data$guardian)

data$schoolsup[data$schoolsup=="yes"]<-1
data$schoolsup[data$schoolsup=="no"]<-0

data$famsup[data$famsup=="yes"]<-1
data$famsup[data$famsup=="no"]<-0

data$paid[data$paid=="yes"]<-1
data$paid[data$paid=="no"]<-0

data$activities[data$activities=="yes"]<-1
data$activities[data$activities=="no"]<-0

data$nursery[data$nursery=="yes"]<-1
data$nursery[data$nursery=="no"]<-0

data$higher[data$higher=="yes"]<-1
data$higher[data$higher=="no"]<-0

data$internet[data$internet=="yes"]<-1
data$internet[data$internet=="no"]<-0

data$romantic[data$romantic=="yes"]<-1
data$romantic[data$romantic=="no"]<-0

#Divide our dataset randomly into train and test set
samplesize <- floor(0.8 * nrow(data))
set.seed(42)
tr <- sample(seq_len(nrow(data)), size = samplesize)
train <- data[tr, ]
test <- data[-tr, ]

#Adjusted R-squared of the linear regression including all variables
lin<-lm(G3~.,train)
summary(lin)
pr<-predict(lin,test)
resid<-test$G3-pr
rss<-sum(resid^2)
tss<-sum((test$G3-mean(test$G3))^2)
rsq<-1-rss/tss
adjrsq<-1-(1-rsq)*(nrow(test)-1)/(nrow(test)-ncol(data)-1)
adjrsq #0.06429495

#Best subset selection
#source: http://www.science.smith.edu/~jcrouser/SDS293/labs/lab8-r.html#:~:text=The%20regsubsets()%20function%20(part,variables%20for%20each%20model%20size.
bss = regsubsets(G3~., data = train, nvmax=ncol(data))

bss_summary = summary(bss)
rssmin<-which.min(bss_summary$rss) #32
adjrmax<-which.max(bss_summary$adjr2) #21
cpmin = which.min(bss_summary$cp) #17
rsqmax<-which.max(bss_summary$rsq) #32

#Model obtained through BSS using Adjusted R-squared as a basis
coef(bss, adjrmax)
lin_adj<-lm(G3~school+address+famsize+Medu+Mjob+Fjob+guardian+studytime+failures+schoolsup+famsup+higher+internet+
              romantic+famrel+goout+Dalc+health+subj,train)

#Calculating the Adjusted R-squared for this model
pr<-predict(lin_adj,test)
resid<-test$G3-pr
rss<-sum(resid^2)
tss<-sum((test$G3-mean(test$G3))^2)
rsq<-1-rss/tss
adjrsq<-1-(1-rsq)*(nrow(test)-1)/(nrow(test)-adjrmax-1)
adjrsq #0.1433867

#Model obtained through BSS using Cp as a basis
coef(bss, cpmin)
lin_cp<-lm(G3~school+famsize+Medu+Mjob+Fjob+guardian+studytime+failures+schoolsup+higher+
             romantic+famrel+goout+Dalc+health+subj,train)

#Calculating the Adjusted R-squared for this model
pr<-predict(lin_cp,test)
resid<-test$G3-pr
rss<-sum(resid^2)
tss<-sum((test$G3-mean(test$G3))^2)
rsq<-1-rss/tss
adjrsq<-1-(1-rsq)*(nrow(test)-1)/(nrow(test)-cpmin-1)
adjrsq #0.1540443

#Adjusted R-squared was very low for the linear model, so we will try tree-based methods
rf1<-ranger(G3~.,train,num.trees=50,min.node.size=5)
pr<-predict(rf1,test)
resid<-test$G3-pr$predictions
rss<-sum(resid^2)
tss<-sum((test$G3-mean(test$G3))^2)
rsq<-1-rss/tss
adjrsq<-1-(1-rsq)*(nrow(test)-1)/(nrow(test)-ncol(data)-1)
adjrsq #0.1273994

#Part 3

data<-fread("Downloads/Downloads/bank.csv")
#Transform the binary variables into a more interpretable form
data$y[data$y=="yes"]<-1
data$y[data$y=="no"]<-0

data$default[data$default=="yes"]<-1
data$default[data$default=="no"]<-0

data$housing[data$housing=="yes"]<-1
data$housing[data$housing=="no"]<-0

data$loan[data$loan=="yes"]<-1
data$loan[data$loan=="no"]<-0

#Transform categorical variables as factors
fac<-c("job","marital","education","contact","month","poutcome")
for(j in fac){
  set(data, i=NULL, j=j, value=as.factor(data[[j]]))
}

data$y<-as.numeric(data$y)

#imputing the "-1" values in pdays column
imp<-mean(data$pdays!=-1)
data$pdays[data$pdays==-1]<-imp

#Split the dataset into training and test sample
samplesize <- floor(0.8 * nrow(data))
set.seed(42)
tr <- sample(seq_len(nrow(data)), size = samplesize)
train <- data[tr, ]
test <- data[-tr, ]

#Build a simple preliminary linear model that includes all of the regressors
lin<-lm(y~.,train)
pr<-predict(lin,test)
pred<-fifelse(pr>0.5,1,0)
hits<-fifelse(pred==test$y,1,0)
metr<-sum(hits)/length(hits)
metr #0.8983425

#Quite a high accuracy was achieved even with a linear model
#Let us see whether we can improve it with best subset selection
bss = regsubsets(y~., data = train, nvmax=ncol(data))

bss_summary = summary(bss)
rssmin<-which.min(bss_summary$rss) #17
adjrmax<-which.max(bss_summary$adjr2) #17
cpmin = which.min(bss_summary$cp) #17
rsqmax<-which.max(bss_summary$rsq) #17

coef(bss,adjrmax)
lin2<-lm(y~job+marital+education+loan+contact+day+month+duration+poutcome,train)
pr<-predict(lin2,test)
pred<-fifelse(pr>0.5,1,0)
hits<-fifelse(pred==test$y,1,0)
metr<-sum(hits)/length(hits) #did not increase accuracy
metr #0.8972376

#There are several issues associated with the LPM that could lead to lower accuracy
#Let us try logit and probit models instead
logit <- glm(y~., family=binomial(link="logit"), data=train) 
summary(logit)
pr<-predict(logit,test)
pred<-fifelse(pr>0.5,1,0)
hits<-fifelse(pred==test$y,1,0)
metr<-sum(hits)/length(hits) 
metr #0.9071823

probit <- glm(y~., family=binomial(link="probit"), data=train) 
summary(probit)
pr<-predict(probit,test)
pred<-fifelse(pr>0.5,1,0)
hits<-fifelse(pred==test$y,1,0)
metr<-sum(hits)/length(hits) 
metr #0.8950276

#Probit hasn't even improved out result compared to LPM, while logit has
#Let us proceed by trying to improve the logit

#Best subset selection could not be implemented with the package bestglm due to computational issues
#Therefore, we implement other approaches instead

#Backward stepwise selection
b = step(logit)

logit_back <- glm(formula(b), family=binomial(link="logit"), data=train) 
summary(logit_back)
pr<-predict(logit_back,test)
pred<-fifelse(pr>0.5,1,0)
hits<-fifelse(pred==test$y,1,0)
metr<-sum(hits)/length(hits) 
metr #0.9060773

#Forward stepwise selection
#Model with no regressors
noreg <- glm(y~1, family=binomial(link="logit"), data=train) 
f = step(noreg,scope=list(lower=formula(noreg),upper=formula(logit)), direction="forward")

logit_forw <- glm(formula(f), family=binomial(link="logit"), data=train) 
summary(logit_forw)
pr<-predict(logit_forw,test)
pred<-fifelse(pr>0.5,1,0)
hits<-fifelse(pred==test$y,1,0)
metr<-sum(hits)/length(hits) 
metr #0.9060773

#marginal effects for the logit model
mar<-margins(logit)
summary(mar)


