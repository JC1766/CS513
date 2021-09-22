############################################## ###
#  Company    : Stevens 
#  Project    : HW3
#  Purpose    : Naive Bayes
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 3/17/21
#  Comments   : I pledge my honor that I have abided by the Stevens Honor System

rm(list=ls())
############################################## ###
library(e1071)
library(class) 

file<-filename<-file.choose()
cancer<-read.csv(file,na.strings = "?")
cancer<-read.csv("breast-cancer-wisconsin.data.csv",na.strings = "?")
cancer2<-na.omit(cancer[,-1])
cancer2$Class <- factor(cancer2$Class)

idx<-sort(sample(nrow(cancer2),as.integer(.7*nrow(cancer2))))
training<-cancer2[idx,]
test<-cancer2[-idx,]

nBayes_all <- naiveBayes(Class ~., data =cancer2)
category_all<-predict(nBayes_all,cancer2  )
table(NBayes_all=category_all,Actual=cancer2$Class)
NB_wrong<-sum(category_all!=cancer2$Class)

NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate
