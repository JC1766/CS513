############################################## ###
#  Company    : Stevens 
#  Project    : HW7
#  Purpose    : ANN
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 5/2/21
#  Comments   : I pledge my honor that I have abided by the Stevens Honor System

rm(list=ls())
#install.packages("neuralnet")
library("neuralnet")
############################################## ###
filename<-file.choose()
breast_cancer<-  read.csv(filename)
breast_cancer$diagnosis<-factor(breast_cancer$diagnosis)
breast_cancer2<-data.frame(lapply(na.omit(breast_cancer[-1]),as.numeric))

mmnorm3 <-function(x)
{z<-((x-min(x))/(max(x)-min(x))) 
return(z)                              
}
breast_cancer2 = data.frame(lapply(breast_cancer2,mmnorm3))

index<-sort(sample(nrow(breast_cancer2),round(.30*nrow(breast_cancer2))))
training<-breast_cancer2[-index,]
test<-breast_cancer2[index,]

class(training$diagnosis)
net_cancer2<- neuralnet( diagnosis~radius_mean+texture_mean+perimeter_mean+
                           area_mean+smoothness_mean+compactness_mean+concavity_mean+
                           concave.points_mean+symmetry_mean+fractal_dimension_mean+
                           radius_se+texture_se+perimeter_se+area_se+smoothness_se+
                           compactness_se+concavity_se+concave.points_se+symmetry_se+
                           fractal_dimension_se+radius_worst+texture_worst+perimeter_worst+
                           area_worst+smoothness_worst+compactness_worst+concavity_worst+
                           concave.points_worst+symmetry_worst+fractal_dimension_worst
                         ,training, hidden=5, threshold=0.01)

#Plot the neural network
plot(net_cancer2)

## test should have only the input colum
ann <-compute(net_cancer2 , test[-1])
ann$net.result 

ann_cat<-ifelse(ann$net.result <0.5,0,1)
length(ann_cat)

table(Actual=test$diagnosis,prediction=ann_cat)

wrong<- (test$diagnosis!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate
