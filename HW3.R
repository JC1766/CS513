############################################## ###
#  Company    : Stevens 
#  Project    : HW3
#  Purpose    : KKNN
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 3/17/21
#  Comments   : I pledge my honor that I have abided by the Stevens Honor System

rm(list=ls())
#install.packages("kknn")
library(kknn)
############################################## ###
file<-filename<-file.choose()
cancer<-read.csv(file,na.strings = "?")
cancer<-read.csv("breast-cancer-wisconsin.data.csv",na.strings = "?")
View(cancer)
summary(cancer)
#is.na(Cancer)
#missing<-Cancer[is.na(Cancer$F6),]
apply(is.na(Cancer), 2, which)
cancer2<-na.omit(cancer)
cancer2$Class <- factor(cancer2$Class)

#Split the data into 70% training, 30% test
idx<-sort(sample(nrow(cancer2),as.integer(.7*nrow(cancer2))))
training<-cancer2[idx,]
test<-cancer2[-idx,]

#k=3
predict_k3 <- kknn(formula=Class~., training, test[,-11], k=3,kernel ="rectangular")
fit <- fitted(predict_k3)
table(Actual=test$Class,Fitted=fit)
#k=5
predict_k5 <- kknn(formula=Class~., training, test[,-11], k=5,kernel ="rectangular")
fit2 <- fitted(predict_k5)
table(Actual=test$Class,Fitted=fit2)
#k=10
predict_k10 <- kknn(formula=Class~., training, test[,-11], k=10,kernel ="rectangular")
fit3 <- fitted(predict_k10)
table(Actual=test$Class,Fitted=fit3)
