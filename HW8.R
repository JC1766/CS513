############################################## ###
#  Company    : Stevens 
#  Project    : HW8
#  Purpose    : KMEANS
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 5/2/21
#  Comments   : I pledge my honor that I have abided by the Stevens Honor System

rm(list=ls())
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


bc_dist<-dist(breast_cancer2[,-1])
hclust_results<-hclust(bc_dist)
plot(hclust_results)
dev.off()
hclust_2<-cutree(hclust_resutls,2)
table(hclust_2,breast_cancer2[,1])


?kmeans

kmeans_3<- kmeans(breast_cancer2[,-1],2,nstart = 10)
kmeans_3$cluster
kmeans_3$centers
table(kmeans_3$cluster,breast_cancer2[,1])

