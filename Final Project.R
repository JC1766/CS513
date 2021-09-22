############################################## ###
#  Company    : Stevens 
#  Project    : Final Project
#  Purpose    : To develop and test different models for finding fraudulent transactions
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 4/28/21
#  Comments   : I pledge my honor that I have abided by the Stevens Honor System

rm(list=ls())
#install.packages("neuralnet")
install.packages("PRROC")

library(PRROC)
#devtools::install_github("collectivemedia/tictoc")
library(tictoc)
############################################## ###

filename<-file.choose()
df<-  read.csv(filename)

view(df)
summary(df)
df2<-na.omit(df)


mmnorm3 <-function(x)
{z<-((x-min(x))/(max(x)-min(x))) 
return(z)                              
}
df2 = data.frame(lapply(df2,mmnorm3))
df2$Class<-factor(df2$Class)
#Setting a seed and splitting data into 70/30 train/test
set.seed(111)
index<-sort(sample(nrow(df2),round(.30*nrow(df2))))
training<-df2[-index,]
test<-df2[index,]

#CART Decision Tree
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)
tic("CART")
CART_class<-rpart(Class~.,data=training)
toc()
rpart.plot(CART_class)
CART_predict<-predict(CART_class,test, type="class") 
table(Actual=test[,31],CART=CART_predict)
CART_wrong<-sum(test[,31]!=CART_predict)
CART_error_rate<-CART_wrong/length(test[,31])
CART_error_rate

#Calculate baseline to compare with AUPRC
pos<-sum(test[,31]==1)
baseline<-pos/length(test[,31])

fg <- CART_predict[test$Class == 1]
bg <- CART_predict[test$Class == 0]
CART_pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
CART_pr

#Random Forest
library(randomForest)
tic("Random Forest")
#fit <- randomForest(Class~., data=training, importance=TRUE, ntree=100)
fit2 <- randomForest(Class~., data=training, importance=TRUE, ntree=50)
#fit3 <- randomForest(Class~., data=training, importance=TRUE, ntree=10)
toc()
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit2, test)
table(actual=test[,31],Prediction)

wrong<- (test[,31]!=Prediction )
RF_error_rate<-sum(wrong)/length(wrong)
RF_error_rate 

fg <- Prediction[test$Class == 1]
bg <- Prediction[test$Class == 0]
RF_pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
RF_pr

#K-nearest neighbors
library(kknn)
tic("KNN")
predict_k5 <- kknn(formula=Class~., training, test[,-31], k=5,kernel ="rectangular")
toc()
fit2 <- fitted(predict_k5)
table(Actual=test$Class,Fitted=fit2)
wrong<- (test[,31]!=fit2 )
Knn_error_rate<-sum(wrong)/length(wrong)
Knn_error_rate 

fg <- fit2[test$Class == 1]
bg <- fit2[test$Class == 0]
KNN_pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
KNN_pr

#Naive-Bayes
library(e1071)
library(class) 
tic("Naive Bayes")
nBayes_all <- naiveBayes(Class ~., data =training)
category_all<-predict(nBayes_all,test  )
toc()
table(NBayes_all=category_all,Actual=test$Class)
NB_wrong<-sum(category_all!=test$Class)

NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate

fg <- category_all[test$Class == 1]
bg <- category_all[test$Class == 0]
NB_pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
NB_pr

