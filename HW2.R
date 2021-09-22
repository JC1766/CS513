############################################## ###
#  Company    : Stevens 
#  Project    : HW2
#  Purpose    : Exploratory Data 
#  First Name  : Jerry
#  Last Name  : Chen
#  Id			    : 10439406
#  Date       : 3/16/21
#  Comments   :

rm(list=ls())
############################################## ###
Cancer<-
  read.csv("breast-cancer-wisconsin.data.csv",na.strings = "?")
View(Cancer)
summary(Cancer)
is.na(Cancer)
missing<-Cancer[is.na(Cancer$F6),]
apply(is.na(Cancer), 2, which)
