############################################## ###
#  Company    : Stevens 
#  Project    : HW6
#  Purpose    : C5.0 and Random Forest
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 4/5/21
#  Comments   : I pledge my honor that I have abided by the Stevens Honor System

rm(list=ls())
#install.packages("C50")
library('C50')
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

C50_class <- C5.0(Class~.,data=training )
plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,10],C50=C50_predict)
wrong<- (test[,10]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,10])
c50_rate


############################################## ###
rm(list=ls())
#install.packages('randomForest')
library(randomForest)

############################################## ###
file<-file.choose()
bc<- read.csv(file)

set.seed(111)
bc<-bc[-1]

bcf <- lapply(bc,factor)
for (i in 1:ncol(bc)) {
  bc[i]<-bcf[i]
}

index<-sort(sample(nrow(bc),round(.3*nrow(bc))))
training<-bc[-index,]
test<-bc[index,]

fit <- randomForest(Class~., data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,10],Prediction)

wrong<- (test[,10]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 
