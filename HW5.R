############################################## ###
#  Company    : Stevens 
#  Project    : HW5
#  Purpose    : CART
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 4/5/21
#  Comments   : I pledge my honor that I have abided by the Stevens Honor System

rm(list=ls())
#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
############################################## ###

filename<-file.choose()
bc<-  read.csv(filename )

set.seed(111)
bc<-bc[-1]

bcf <- lapply(bc,factor)
for (i in 1:ncol(bc)) {
  bc[i]<-bcf[i]
}

index<-sort(sample(nrow(bc),round(.3*nrow(bc))))
training<-bc[-index,]
test<-bc[index,]

CART_class<-rpart(Class~.,data=training)
rpart.plot(CART_class)
CART_predict<-predict(CART_class,test, type="class") 
table(Actual=test[,10],CART=CART_predict)
CART_wrong<-sum(test[,10]!=CART_predict)
CART_error_rate<-CART_wrong/length(test[,10])
CART_error_rate

library(rpart.plot)
prp(CART_class)


# much fancier graph
fancyRpartPlot(CART_class)
